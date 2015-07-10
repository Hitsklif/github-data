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

(defn get-names
  ([lang] (get-names lang 1))
  ([lang num]
   (map #(get % "full_name")
        (-> (get-json (str repo_count lang "+forks:\">20\"&per_page=30&page=" num))
            (get "items")))))

(defn get-count-q [q]
  (-> (get-json q)
      (get "total_count")))

(defn get-repo-names-for-lang [lang]
  (reduce str (map #(str "+repo:" %)
                   (get-names lang))))

(def t (get-repo-names-for-lang "csharp"))
;; bugs-in-top  => >25 forks
;; ([149.3 1493/10 30] [19.466667 292/15 30] [14.766666 443/30 30] [125.36667 3761/30 30] [98.46667 1477/15 30])

(defn get-many-paged-bugs []
  (let [bugs-in-top
        (for [lang langs]
          (reduce (fn [total page]
                    (let [names (get-names lang page)
                          counts (count names)
                          namesstr (reduce str (map (fn [x] (str "+repo:" x)) names))
                          bugcount (get-count-q (str bugs namesstr))]
                      (assoc total
                             :lang lang
                             :bugs (+ (:bugs total) bugcount)
                             :repos (+ (:repos total) counts))))
                  {:bugs 0 :repos 0}
                  (range 1 4)))]
    (map #(assoc % :ratio (float (/ (:bugs %) (:repos %))))
         bugs-in-top)))

(def bugs-many-paged (get-many-paged-bugs))

(defn make-test-queries [lang]
  (test-count lang (get-repo-names-for-lang lang)))

(def test-counts (map #(get-count-q (make-test-queries %)) langs))
(def qs (map #(make-test-queries %) langs))
