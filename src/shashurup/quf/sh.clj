(ns shashurup.quf.sh
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [shashurup.quf.data :as d]
            [shashurup.quf.fs :as fs]
            [shashurup.quf.response :as resp]
            [shashurup.quf.vars :refer [*term-dimensions*]])
  (:import [java.lang ProcessBuilder ProcessBuilder$Redirect]
           [com.pty4j PtyProcessBuilder WinSize]))

;; clojure.java.shell/sh always create a pipe for stdin
;; this ins't always what we need
;; for instance, ag thinks it needs to search stdin in this case
;; so here goes our own implementation

(defn- start-process [cmd dir in]
  (let [p (-> (ProcessBuilder. (into-array String cmd))
              (.directory (io/as-file dir))
              (.redirectInput (if in
                                ProcessBuilder$Redirect/PIPE
                                (io/as-file "/dev/null")))
              .start)]
    {:in (.getOutputStream p)
     :out (io/reader (.getInputStream p))
     :err (io/reader (.getErrorStream p))
     :wait #(.waitFor p)}))

(defn !>
  "Launches a subprocess to cosume its output.
  Subprocess is defined either by a single string:

    (!> \"ls -al\")

  or by a list of strings:

    (!> \"ls\" \"-al\")
  
  Standard input may also be specified:

    (!> \"sort\" :input [\"def\" \"ghi\" \"abc\"])

  it could be a list of string or anything that can be
  copied with clojure.java.io/copy.
  Output conversion can be specified with :output

    (!> \"\" :output from-json)

  The function takes an instance of java.io.Reader, the result
  is used as ! result. shahsurup.quf.data/as-text is used by default.
  Another option is :dir for a directory to use.
  Return value is a process output as a list of strings.
  Exception is thrown when exit code is non zero."
  [& args]
  (let [[cmd {:keys [input output dir]
              :or {output d/as-text}}] (split-with string? args)
        cmd (if (> (count cmd) 1)
               cmd
               (remove empty? (s/split (first cmd) #"\s")))]
    (let [{:keys [in out err wait]} (start-process cmd
                                                   (or dir fs/*cwd*)
                                                   input)]
      (when input
        (future
          (let [input (if (coll? input)
                        (s/join "\n" input)
                        input)]
            (with-open [in' in]
              (io/copy input in')))))
      (let [error (future (slurp err))
            result (output out)
            exit (wait)]
        (when (not-empty @error)
          (print @error))
        (if (= exit 0)
          (if (coll? result)
            (doall result)
            result)
          (throw (Exception. (str "Exit code " exit))))))))

(defn- resize [process [cols rows]]
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

(defn flushing-copy [from to]
  (let [buffer (char-array 1024)]
    (loop []
      (let [size (.read from buffer)]
        (when (pos? size)
          (.write to buffer 0 size)
          (.flush to)
          (recur))))))

(defn process-input [from to resize]
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

    (!> \"ls -al\")

  or by a list of strings:

    (!> \"ls\" \"-al\")
  
  Return value is a process return code."
  [& args]
  (resp/print-with-hint {} :terminal)
  (let [args (if (> (count args) 1)
               args
               (remove empty? (s/split (first args) #"\s")))
        {:keys [in out wait resize]} (start-process-with-pty args fs/*cwd*)]
    (let [in-handler (future (process-input *in* in resize))]
      (flushing-copy out *out*)
      (future-cancel in-handler))
    (wait)))

(resp/client-require "shashurup.quf.terminal")
