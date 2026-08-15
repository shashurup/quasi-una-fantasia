(ns shashurup.quf.proc
  "Java process wrapper functions"
  (:require [clojure.java.io :as io])
  (:import [java.lang Process ProcessBuilder ProcessBuilder$Redirect]
           [java.io InputStream FilterInputStream IOException]))

(defn create
  "Creates a process.
   cmd - a sequence: command and arguments
   dir - working directory
   env - map of environment variables
  (actually, and instance of ProcessBuilder)"
  [cmd & {dir :dir env :env}]
  (let [^ProcessBuilder p (ProcessBuilder. (into-array String cmd))]
    (when dir
      (.directory p (io/as-file dir)))
    (when env
      (doseq [[k v] env]
        (.put (.environment p) k v)))
    p))

(defn kill [^Process subj]
  (.destroy subj))

(defn- write-lines [dest lines]
  (let [wrtr (io/writer dest)]
    (doseq [line lines]
      (.write wrtr line)
      (.write wrtr "\n"))
    (.flush wrtr)))

(defn start
  "Start the process created by create.
   input - can be either a sequence of lines
           to feed as the process input, or
           anything suitable for io/copy. Optional.
   error - a target for io/copy for the process errors."
  ([subj] (start subj nil nil))
  ([subj input] (start subj input nil))
  ([^ProcessBuilder subj input error]
   (let [dev-null (io/as-file "/dev/null")]
     (when (nil? input) (.redirectInput subj dev-null))
     (when (nil? error) (.redirectError subj dev-null))
     (let [^Process proc (.start subj)]
       (when input
         (future
           (try
             (with-open [in (.getOutputStream proc)]
               (if (coll? input)
                 (write-lines in input)
                 (io/copy input in)))
             (catch IOException _))))
       (when error
         (future (io/copy (.getErrorStream proc) error)))
       proc))))

(defn output
  "An output stream from the process instance.
   Throws when the process returns non zero status."
  [^Process subj]
  (let [^InputStream out (.getInputStream subj)]
    (proxy [FilterInputStream] [out]
      (read [buf offset len]
        (let [result (proxy-super read buf offset len)]
          (if (= result -1)
            (let [exit (.waitFor subj)]
              (if (= exit 0)
                result
                (throw (Exception. (str "Exit code " exit)))))
            result))))))

(defn reader
  "A reader from the process from the process instance.
   Throws when the process returns non zero status."
  [subj]
  (io/reader (output subj)))

(defn execute [cmd & opts]
  (let [^ProcessBuilder b (apply create cmd opts)]
    (.redirectOutput b (io/as-file "/dev/null"))
    (.waitFor (start b))))
