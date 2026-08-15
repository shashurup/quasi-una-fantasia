(ns shashurup.quf.sh
  "Shell process launchers"
  {:shashurup.quf/client-module :terminal}
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [shashurup.quf.data :as d]
            [shashurup.quf.fs :as fs]
            [shashurup.quf.proc :as proc]
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

(defn pipe
  "Launches a subprocess to consume its output.

    (pipe \"ls -al\")

  Standard input may also be specified:

    (pipe \"sort\" [\"def\" \"ghi\" \"abc\"])

  it could be a list of string or anything that can be
  copied with clojure.java.io/copy.

  By default java.io.Reader is returned. This can be changed
  with options

    (pipe \"sort\" :lines [\"def\" \"ghi\" \"abc\"])

  returns process output as sequence of lines

    (pipe \"cat file.xml\" :binary)

  returns process output as java.io.InputStream so that
  it could be consumed by xml/parse for instance

  In place of a keyword a map can be used when there is
  more than one option:

  (pipe \"curl http://example.com\" {:dir \"/\" :lines true})
  (pipe \"set\" {:env {\"var1\" \"val1\"}})
  
  curl is run in / and output is returned as a sequence of lines.

  When the process returns non zero exit code
  an exception is thrown. Standart error can be
  captured into a stream:

  (pipe \"curl http://example.com\" {:error *err*})
  "

  ([subj] (pipe subj {} nil))
  ([subj arg] (if (or (keyword? arg) (map? arg))
                (pipe subj arg nil)
                (pipe subj {} arg)))
  ([subj opts input]
   (let [{:keys [dir env binary lines error]} (if (keyword? opts)
                                                {opts true}
                                                opts)
         b (proc/create [*shell* "-c" subj]
                        :dir (or dir fs/*cwd*)
                        :env env)
         p (proc/start b input error)]
     (v/defer #(proc/kill p))
     (cond
       binary (proc/output p)
       lines (line-seq (proc/reader p))
       :else (proc/reader p)))))

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
