(ns github-data.core
  (require [clj-http.client :as client]
           [clojure.data.json :as json]
           )
  )

(def repo_count  "https://api.github.com/search/repositories?q=language:")
(def bug_count "https://api.github.com/search/issues?q=label:bug+language:")

(def langs ["csharp", "fsharp", "clojure", "javascript", "coffeescript"])

(defn get-count [q lang]
  (-> (str q lang)
      (client/get token)
      :body
      json/read-str
      (get "total_count")))

(def all_bug_counts (map #(get-count bug_count %) langs))
(def all_repo_counts (map #(get-count repo_count %) langs))
(def bugs-per-repo (map #(float (/ %1 %2)) all_bug_counts all_repo_counts))


(defn get-json [q lang]
  (-> (str q lang)
      (client/get token)
      :body
      json/read-str))

(defn test-count [lang repo]
  (str "https://api.github.com/search/code?q=test+in:file+language:"
       lang
       "+repo:" repo))

(defn make-test-queries [lang]
  (map #(test-count lang (get % "full_name"))
       (take 15 (-> (get-json (str repo_count "+stars:\">10\"") lang)
                   (get "items")))))

(defn get-count-q [q]
  (-> q
      (client/get token)
      :body
      json/read-str
      (get "total_count")))

(def test-counts (map #(reduce + (map get-count-q (make-test-queries %))) langs))
(def clojures  (reduce + (map get-count-q (make-test-queries "clojure"))))
(def fsharps (reduce + (map get-count-q (make-test-queries "fsharp"))))
(def csharps (reduce + (map get-count-q (make-test-queries "csharp"))))
(def jss (reduce + (map get-count-q (make-test-queries "js"))))
