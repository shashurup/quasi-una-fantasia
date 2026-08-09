(ns shashurup.quf.sh
  "Shell process launchers"
  {:shashurup.quf/client-module :terminal}
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [shashurup.quf.data :as d]
            [shashurup.quf.fs :as fs]
            [shashurup.quf.view :as v]
            [shashurup.quf.vars :refer [*term-dimensions*]])
  (:import [java.lang ProcessBuilder ProcessBuilder$Redirect]
           [java.io Reader Writer IOException InputStreamReader]
           [com.pty4j PtyProcessBuilder PtyProcess WinSize]))

(def ^:dynamic *shell* (or  (System/getenv "SHELL") "/bin/sh"))

;; clojure.java.shell/sh always create a pipe for stdin
;; this ins't always what we need
;; for instance, ag thinks it needs to search stdin in this case
;; so here goes our own implementation

(defn- write-lines [dest lines]
  (let [wrtr (io/writer dest)]
    (doseq [line lines]
      (.write wrtr line)
      (.write wrtr "\n"))
    (.flush wrtr)))

(defn wrap-process-output [^java.io.InputStream subj
                           ^java.lang.Process proc]
  (io/reader
   (proxy [java.io.InputStreamReader] [subj]
     (read [buf offset len]
       (let [result (proxy-super read buf offset len)]
         (if (= result -1)
           (let [exit (.waitFor proc)]
             (if (= exit 0)
               result
               (throw (Exception. (str "Exit code " exit)))))
           result))))))

(defn pipe
  "Launches a subprocess to consume its output.

    (cmd \"ls -al\")

  Standard input may also be specified:

    (cmd \"sort\" [\"def\" \"ghi\" \"abc\"])

  it could be a list of string or anything that can be
  copied with clojure.java.io/copy.

  By default java.io.Reader is returned. This can be changed
  with options

    (cmd \"sort\" :lines [\"def\" \"ghi\" \"abc\"])

  returns process output as sequence of lines

    (cmd \"cat file.xml\" :binary)

  returns process output as java.io.InputStream so that
  it could be consumed by xml/parse for instance

  In place of a keyword a map can be used when there is
  more than one option:

  (cmd \"curl http://example.com\" {:dir \"/\" :lines true})
  
  curl is run in / and output is returned as a sequence of lines.

  When the process returns non zero exit code
  an exception is thrown. Standart error can be
  captured into a stream:

  (cmd \"curl http://example.com\" {:error *err*})
  "

  ([subj] (cmd subj {} nil))
  ([subj arg] (if (or (keyword? arg) (map? arg))
                (cmd subj arg nil)
                (cmd subj {} arg)))
  ([subj opts input]
   (let [{:keys [dir binary lines error]} (if (keyword? opts)
                                            {opts true}
                                            opts)
         dev-null (io/as-file "/dev/null")
         p (-> (ProcessBuilder. (into-array String
                                            [*shell* "-c" subj]))
               (.directory (io/as-file (or dir fs/*cwd*)))
               ;; TODO handle the environment
               (.redirectInput (if input ProcessBuilder$Redirect/PIPE dev-null))
               (.redirectError (if error ProcessBuilder$Redirect/PIPE dev-null))
               .start)
         output (.getInputStream p)]
     (v/defer #(.destroy p))
     (when input
       (future
         (try
           (with-open [in (.getOutputStream p)]
             (if (coll? input)
               (write-lines in input)
               (io/copy input in)))
           (catch IOException _))))
     (when error
       (future (io/copy (.getErrorStream p) error)))
     (cond
       binary output
       lines (line-seq (wrap-process-output output p))
       :else (io/reader output)))))

(defn- resize [^PtyProcess process [cols rows]]
  (.setWinSize process (WinSize. (int cols) (int rows))))

(defn- start-process-with-pty [cmd dir]
  (let [env (java.util.HashMap. (System/getenv))
        [cols rows] (or *term-dimensions* [80 24]) 
        _ (.put env "TERM" "xterm")
        p (-> (PtyProcessBuilder. (into-array String cmd))
              (.setRedirectErrorStream true)
              (.setDirectory dir)
              (.setEnvironment env)
              (.setInitialColumns (int cols))
              (.setInitialRows (int rows))
              .start)]
    {:in (io/writer (.getOutputStream p))
     :out (io/reader (.getInputStream p))
     :wait #(.waitFor p)
     :resize #(resize p %)}))

(defn stream-events [^Reader from]
  (let [buffer (char-array 1024)]
    (loop []
      (let [size (.read from buffer)]
        (when (pos? size)
          (v/report-event {:type :terminal
                           :out (String. buffer 0 size)})
          (recur))))))

(defn process-input [from ^Writer to resize]
  (loop []
    (let [data (edn/read from)]
      (if (= :resize (:cmd (meta data)))
        (resize data)
        (do 
          (.write to data)
          (.flush to)))
      (recur))))

(defn !
  "Launches a subprocess interactively.
  Subprocess is defined either by a single string:

    (! \"ls -al\")

  Return value is a process return code."
  [cmdline]
  (let [args [*shell* "-c" cmdline]
        {:keys [in out wait resize]} (start-process-with-pty args fs/*cwd*)]
    (let [in-handler (future (process-input *in* in resize))]
      (stream-events out)
      (future-cancel in-handler))
    (wait))
  (v/report-event {:type :terminal :status #{:done}}))
