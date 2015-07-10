(ns github-data.core
  (use [github-data.private])
  (require [clj-http.client :as client]
           [clojure.data.json :as json]))

(def repo_count  "https://api.github.com/search/repositories?q=language:")
(def bug_count "https://api.github.com/search/issues?q=label:bug+language:")
(def bugs "https://api.github.com/search/issues?q=label:bug")

;;(client/post "https://api.github.com/search/issues" (merge token {:label "bug"}))

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
       (-> (get-json (str repo_count lang "+forks:\">20\"&per_page=30"))
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

(def t (get-repo-names-for-lang "csharp"))
;; bugs-in-top  => >25 forks
;; ([149.3 1493/10 30] [19.466667 292/15 30] [14.766666 443/30 30] [125.36667 3761/30 30] [98.46667 1477/15 30])

;; page=2&
(def bugs-in-top 
  (map #(let [names (get-names %)
              counts (count names)
              namesstr (reduce str (map (fn [x] (str "+repo:" x)) names))
              ratio (/ (get-count-q (str bugs namesstr))
                       counts)]
          [(float ratio) ratio counts])
       langs))

(defn make-test-queries [lang]
  (test-count lang (get-repo-names-for-lang lang)))

(def test-counts (map #(get-count-q (make-test-queries %)) langs))
(def qs (map #(make-test-queries %) langs))
