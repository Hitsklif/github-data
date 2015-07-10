(ns github-data.core
  (use [github-data.private])
  (require [clj-http.client :as client]
           [clojure.data.json :as json]
           )
  )

(def repo_count  "https://api.github.com/search/repositories?q=language:")
(def bug_count "https://api.github.com/search/issues?q=label:bug+language:")
(def bugs "https://api.github.com/search/issues?q=label:bug")

(def langs ["csharp", "fsharp", "clojure", "js", "coffeescript"])

(defn get-count [q lang]
  (-> (str q lang)
      (client/get token)
      :body
      json/read-str
      (get "total_count")))

(def all_bug_counts (map #(get-count bug_count %) langs))
(def all_repo_counts (map #(get-count repo_count %) langs))
(def bugs-per-repo (map #(float (/ %1 %2)) all_bug_counts all_repo_counts))


(defn get-json [q]
  (-> q
      (client/get token)
      :body
      json/read-str))

(defn test-count [lang repo]
  (str "https://api.github.com/search/code?q=test+filename:test+language:"
       lang
       repo))

(defn get-names [lang]
  (map #(get % "full_name")
       (-> (get-json (str repo_count lang "+stars:\"50..100\"&per_page=30"))
           (get "items"))))

(defn get-count-q [q]
  (-> (get-json q)
      (get "total_count")))

(def csharpnames (get-names "csharp"))
(def fsharpnames (get-names "fsharp"))
(def cljnames (get-names "clojure"))
(def jsnames (get-names "js"))
(def coffeeenames (get-names "coffeescript"))

(defn get-repo-names-for-lang [lang]
  (reduce str (map #(str "+repo:" %)
                   (get-names lang))))

(def bugs-in-top
  (map #(get-count-q (str bugs (get-repo-names-for-lang %)))
       langs))

(defn make-test-queries [lang]
  (test-count lang (get-repo-names-for-lang lang)))

(def test-counts (map #(get-count-q (make-test-queries %)) langs))
(def clojures  (reduce + (map get-count-q (make-test-queries "clojure"))))
(def fsharps (reduce + (map get-count-q (make-test-queries "fsharp"))))
(def csharps (reduce + (map get-count-q (make-test-queries "csharp"))))
(def jss (reduce + (map get-count-q (make-test-queries "js"))))
