(ns luby
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [donut.system :as ds]
            [donut.system.repl :as dsr]
            [juxt.dirwatch :refer [close-watcher watch-dir]]
            parse
            [print :as p])
  (:import [java.io PushbackReader]))

(defn read-clj-file [s]
  (with-open [r (PushbackReader. (io/reader s))]
    (doall
     (take-while some?
                 (repeatedly
                  #(try
                     (edn/read r)
                     (catch Throwable e
                       (case (.getMessage e)
                         "EOF while reading"
                         nil
                         (throw e)))))))))

(defn emit [file-name file]
  (spit
   file-name
   (str/join
    "\n"
    (map
     p/print-ast
     (parse/parse-file
      (read-clj-file
       file))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defonce _killhook
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. ^Runnable
                     dsr/stop)))

(defn extract-clj-filename [s]
  (re-find #"^([^\.]+)\.clj$" s))

(defn watch-and-transpile! [{:keys [src-dir]}]
  (watch-dir
   (fn [{f :file}]
     (when-let [fname (second (extract-clj-filename (.getName f)))]
       (prn "compile" fname)
       (emit fname (io/file fname))))
   (io/file src-dir)))

(defmethod ds/named-system :donut.system/repl
  [_]
  {::ds/defs {:compiler {:watcher {::ds/start watch-and-transpile!
                                   :src-dir "./mysrc"
                                   ::ds/stop (fn stop-watcher! [watcher]
                                               (close-watcher watcher))}}}})

(comment
  (dsr/start)
  (doseq [f ["draw" "update" "main" "data"]]
    (emit (format "%s.rb" f)
         (io/file (format "/Users/eloren/src/luby/game/src/%s.clj" f))))

  (parse/parse-file
   (read-clj-file (io/file "/Users/eloren/src/luby/game/src/main.clj")))
  (print
   (p/print-ast
    (parse/expr->node
     '(while (< a 2)
      (+= a 1)))))

  (p/print-ast
   (parse/expr->node
    '(somefn 1 2 3)))

  (print
   (p/print-ast
    (parse/expr->node
     '(.filter [1 2 3]
               (fn [x] (<= x 1))))))
  )
