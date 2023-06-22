(ns shashurup.quf.sh
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [shashurup.quf.fs :as fs])
  (:import [java.lang ProcessBuilder ProcessBuilder$Redirect]))

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

(defn !
  "Launches a subprocess.
  Subprocess is defined either by a single string:

    (! \"ls -al\")

  or by a list of strings:

    (! \"ls\" \"-al\")
  
  Standard input may also be specified:

    (! \"sort\" :input [\"def\" \"ghi\" \"abc\"])

  it could be a list of string or anything that can be
  copied with clojure.java.io/copy.
  Another option is :dir for a directory to use.
  Return value is a process output as a list of strings.
  Exception is thrown when exit code is non zero."
  [& args]
  (let [[cmd {:keys [input dir]}] (split-with string? args)
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
            output (vec (line-seq out))
            exit (wait)]
        (when (not-empty @error)
          (print @error))
        (if (= exit 0)
          (with-meta output {:shashurup.quf/hint :text})
          (throw (Exception. (str "Exit code " exit))))))))
