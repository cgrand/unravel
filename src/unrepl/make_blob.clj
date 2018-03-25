(ns unrepl.make-blob
  (:require
    [clojure.java.io :as io]
    [clojure.edn :as edn]
    [clojure.string :as str]
    [unrepl.shade-libs :as shade]))

(defn- strip-spaces-and-comments [s]
  #_(I had this nice #"(?s)(?:\s|;[^\n\r]*)+|((?:[^;\"\\\s]|\\.|\"(?:[^\"\\]|\\.)*\")+)"
      but it generates stack overflows...
      so let's write the state machine!)
  (let [sb (StringBuilder.)]
    (letfn [(regular [c]
              (case c
                \; comment
                \# dispatch
                \" (do (.append sb c) string)
                \\ (do (.append sb c) regular-esc)
                (\newline \return) strip-nl
                (\tab \space \,) strip
                (do (.append sb c) regular)))
            (strip [c]
              (case c
                (\newline \return) strip-nl
                (\tab \space \,) strip
                \; comment
                (do (.append sb " ") (regular c))))
            (strip-nl [c]
              (case c
                (\newline \return \tab \space \,) strip-nl
                \; comment
                (do (.append sb "\n") (regular c))))
            (dispatch [c]
              (case c
                \! comment
                \" (do (.append sb "#\"") string)
                (do (-> sb (.append "#") (.append c)) regular)))
            (comment [c]
              (case c
                \newline strip-nl
                comment))
            (string [c]
              (.append sb c)
              (case c
                \" regular
                \\ string-esc
                string))
            (string-esc [c]
              (.append sb c)
              string)
            (regular-esc [c]
              (.append sb c)
              regular)]
      (reduce
        #(%1 %2)
        regular s))
    (str sb)))

(defn- gen-blob []
  (let [template (slurp (io/resource "unrepl/blob-template.clj"))
        shaded-code-sb (StringBuilder.)
        shaded-libs (shade/shade 'unrepl.repl
                      {:writer (fn [_ ^String code] (.append shaded-code-sb code))
                       :except [#"clojure\..*" 'unrepl.core]})
        code (-> template
               (str/replace "unrepl.repl"
                 (str (shaded-libs 'unrepl.repl)))
               (str/replace "<BLOB-PAYLOAD>" (str shaded-code-sb)))]
    (str (strip-spaces-and-comments code) "\n"))) ; newline to force eval by the repl

(defn -main
  ([] (-main "resources/unrepl/blob.clj"))
  ([target]
    (-> target io/file .getParentFile .mkdirs)
    (spit target (gen-blob))))
