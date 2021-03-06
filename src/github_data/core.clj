(ns github-data.core
  (use [github-data.private]
       [github-data.cached])
  (require [clj-http.client :as client]
           [clojure.edn :as edn]
           [clojure.java.io :as io]
           [clojure.data.json :as json]))

(def repo_count  "hrepocounts ttps://api.github.com/search/repositories?q=language:")
(def bug_count "https://api.github.com/search/issues?q=label:bug+language:")
(def comitcounts "https://api.github.com/repos/")
(def date-range "+created:\"2011-01-01..2015-05-30\"")
(def bugs (str "https://api.github.com/search/issues?q=label:bug" date-range))

(def langs ["csharp" "fsharp" "clojure" "js" "coffeescript"
            "scala" "php" "go" "java" "haskell" "ruby" "python"])

;; (json/write-str langs)

;; (defn get-count [q lang]
;;   (-> (str q lang)
;;       (client/get token)
;;       :body
;;       json/read-str
;;       (get "total_count")))

;; (def all_bug_counts (map #(get-count bug_count %) langs))
;; (def all_repo_counts (map #(get-count repo_count %) langs))
;; (def bugs-per-repo (map #(float (/ %1 %2)) all_bug_counts all_repo_counts))

(defn get-json [q]
  (println "GET:" q)
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
        (-> (get-json (str repo_count lang date-range "+forks:\">15\"&per_page=200&page=" num))
            (get "items")))))

(defn get-count-q [q]
  (-> (get-json q)
      (get "total_count")))

(defn get-repo-names-for-lang [lang]
  (reduce str (map #(str "+repo:" %)
                   (get-names lang))))


;; bugs-in-top  => >25 forks
;; ([149.3 1493/10 30] [19.466667 292/15 30] [14.766666 443/30 30] [125.36667 3761/30 30] [98.46667 1477/15 30])
(def repo-names (atom {}))

(defn get-repos-commit-counts [repos]
  (reduce (fn [sum repo]
            (let [name (clojure.string/replace (second repo) "+repo:" "")
                  url (str comitcounts name "/contributors")
                  #_(Thread/sleep 4000)
                  json (get-json url)
                  contribs (map #(get % "contributions") json)
                  commits (reduce + contribs)]
              (+ sum commits)))
          0
          repos))

(defn get-test-data [repo lang file-search]
  (try
    (if (or (.contains repo "Test") (.contains repo "test")) 0
        (get (get-json
              (str "https://api.github.com/search/code?q="
                   (reduce #(str %1 "+filename:" %2) "" file-search)
                   "+language:" lang repo "&per_page=1"))
             "total_count"))
    (catch Exception e 0)
    ))

(def bugs-many-paged [])
(defn get-many-paged-commits []
  (map get-repos-commit-counts bugs-many-paged))

;; (def commits-time (get-many-paged-commits))

(defn lang-dir-name [lang]
  (str "cacheddata/" lang))

(defn write-to-file [lang name data]
  (with-open [w (clojure.java.io/writer
                 (str (lang-dir-name lang) "/"
                      (.replace name "/" "") ".clj"))]
    (binding [*print-length* false
              *out* w]
      (pr data))))

(defn get-data-for-repo [name lang]
  (Thread/sleep 4000)
  (let [namestr (str "+repo:" name)
        bug-count (get-count-q (str bugs namestr))
        x (Thread/sleep 4000)
        url (str comitcounts name "/contributors")
        json (get-json url)
        contribs (map #(get % "contributions") json)
        commits (reduce + contribs)
        x (Thread/sleep 4000)
        tests (get-test-data namestr lang ["spec" "test"])
        data {:bugs bug-count :commits commits :tests tests :repoqry namestr :name name}]
    (write-to-file lang name data)))

(defn get-all-repo-data []
  (.mkdir (java.io.File. "cacheddata"))
  (doseq [lang langs]
    (println "Lang:" lang)
    (do (.mkdir (java.io.File. (lang-dir-name lang)))
        (let [names (get-names lang)
              counts (count names)
              bugcounts (doall (map (fn [name x]
                                      (get-data-for-repo name lang)
                                      (println "Repo:" name " " (inc x) "/" counts))
                                    names (range counts)))]
          bugcounts))))
;; (get-all-repo-data)

(defn get-cached-lang-data [lang]
  (->> (str  "cacheddata/" lang)
       io/file
       file-seq
       (filter #(.endsWith (.getName %) ".clj") )
       (map (comp edn/read-string
                  slurp
                  str))))

(def clean-data (map get-cached-lang-data langs))

;; dont reeval!
;; (def bugs-many-paged (get-many-paged-bugs))
(def bugs-many-paged (map (fn [eachs]
                            (filter #(not= 0 (first %)) eachs))
                          over-fifteen-forks))


(defn take-interquartile [sort-fn coll]
  (let [coll (sort-by sort-fn coll)
        item-count (count coll)
        coll (drop (int (/ item-count 4)) coll)
        coll (take (int (/ item-count 2)) coll)]
    coll))

(def data-cleaned
  (map
   (fn [lang-data name score total-repos]
     (let [
           ;; remove records without data (not using bug tracker)
           not-zero (filter #(not= 0 (:bugs %)) lang-data)
           ;; not-zero (take-interquartile :bugs not-zero)
           repo-count (count not-zero)
           repo-data (map
                      (fn [repo]
                        (let [bug-count (:bugs repo)
                              test-count (:tests repo)
                              commit-count (:commits repo)
                              bug-and-test-commit-ratio (float (/ (+ bug-count test-count)))
                              ratiocommits (float (/ bug-count commit-count))
                              testspercommit  (float (/ test-count commit-count))]
                          (merge repo {:bug-and-test-commit-ratio bug-and-test-commit-ratio :tests-commits-ratio testspercommit :bug-commit-ratio ratiocommits})))
                      not-zero)

           not-zero (take-interquartile :bug-commit-ratio repo-data)

           min-bug-ratio (reduce min (map :bug-commit-ratio not-zero))
           max-bug-ratio (reduce max (map :bug-commit-ratio not-zero))
           bug-count (reduce + (map :bugs not-zero))

           test-count (reduce + (map :tests not-zero))
           commit-count (reduce + (map :commits not-zero))
           bug-repo-ratio (float (/ bug-count repo-count))
           bug-and-test-commit-ratio (float (/ (+ bug-count test-count)
                                               repo-count))
           ratiocommits  (float (/ bug-count commit-count))
           testspercommit (float (/ test-count commit-count))
           ]
       {:name name :max-bug-ratio max-bug-ratio :min-bug-ratio min-bug-ratio :bugs bug-count :test test-count :commits commit-count :total-repos total-repos :score score :repos repo-count :bug-and-test-commit-ratio bug-and-test-commit-ratio :tests-commits-ratio testspercommit :bug-commit-ratio ratiocommits}))
   ;;bugs-many-paged
   clean-data
   langs
   (list 3, 9,  3,  -3, -2,  6,  -1,  3,  3,  10,  -2,  -2,)
   (list 307572,4180,30257,1475201,49716,49937,630816,75802,1224939,37328,826600,721979)
   ))

(map (fn [l] [(:name l) (:tests-commits-ratio l) (:bug-commit-ratio l)])
     (sort-by :bug-and-test-commit-ratio data-cleaned))
(
["clojure" 0.00876137 0.011503478] 
["fsharp" 0.0014933478 0.023486288] 
["haskell" 0.0026110662 0.015551204] 
["js" 0.0027743443 0.039445132] 
["python" 0.019477962 0.02531419] 
["ruby" 0.03903771 0.020303702] 
["scala" 0.04074265 0.01904762] 
["csharp" 0.017439904 0.03261284] 
["java" 0.033524733 0.032567736] 
["coffeescript" 0.007376024 0.047242288] 
["go" 0.050468475 0.024698375] 
["php" 0.07703413 0.031669293])

(
["fsharp" 0.0014933478] 
["haskell" 0.0026110662] 
["js" 0.0027743443] 
["coffeescript" 0.007376024] 
["clojure" 0.00876137] 
["csharp" 0.017439904] 
["python" 0.019477962] 
["java" 0.033524733] 
["ruby" 0.03903771] 
["scala" 0.04074265] 
["go" 0.050468475] 
["php" 0.07703413])


;; (json/write-str data-cleaned)
(defn output-R-frame-by-lang []
  (let [sorted (sort-by :score data-cleaned)
        ratios  (clojure.string/join ","  (map :bug-commit-ratio sorted)) 
        minratios  (clojure.string/join ","  (map :min-bug-ratio sorted)) 
        maxratios  (clojure.string/join ","  (map :max-bug-ratio sorted))
        names  (clojure.string/join ","  (map :name sorted))
        output (str names "\n" ratios "\n" minratios "\n" maxratios)]
    (spit "/home/jack/programming/datasciencecoursera/langugagedata2.csv" output)
    names))
(output-R-frame-by-lang)


(defn output-R-frame []
  (spit "/home/jack/programming/datasciencecoursera/langugagedata.csv" 
        (reduce str
                "name, ratio, score,totalrepo, minratio, maxratio"
                (map (fn [lang]
                           (let [
                                 ratios (:bug-commit-ratio lang)
                                 minratios (:min-bug-ratio lang)
                                 maxratios (:max-bug-ratio lang)
                                 names (:name lang)
                                 scores (:score lang)
                                 total-repos (:total-repos lang)
                                 output (str names "," ratios "," scores "," total-repos "," minratios "," maxratios "\n")]
                             output))
                         (sort-by :score data-cleaned)))))
(output-R-frame)

(defn output-R []
  (let [sorted (sort-by :score data-cleaned)
        ratios (str "bugratios <- rescale(c" (toR (map :bug-commit-ratio sorted)) ")")
        minratios (str "minbugratios <- rescale(c" (toR (map :min-bug-ratio sorted)) ")")
        maxratios (str "maxbugratios <- rescale(c" (toR (map :max-bug-ratio sorted)) ")")
        names  (str "names <- c" (toR (map #(str "'" (:name %) "'") sorted)))
        scores (str "newscores <- rescale(c" (toR (map :score sorted)) ")")
        total-repos (str "repos <- rescale(c" (toR (map :total-repos sorted)) ")")
        output (str ratios "\n" names "\n" scores "\n" total-repos "\n" minratios "\n" maxratios)]
    (spit "/home/jack/programming/datasciencecoursera/langugagedata.R" output)
    names))



(def all_repo_counts (map #(get-count-q (str repo_count %)) langs))

(["clojure" 115]
 ["haskell" 155]
 ["scala" 190]
 ["ruby" 203]
 ["fsharp" 234]
 ["go" 246]
 ["python" 253]
 ["php" 316]
 ["java" 325]
 ["csharp" 326]
 ["js" 394]
 ["coffeescript" 472])

(defn toR [col]
  (str "(" (clojure.string/join "," col) ")"))

(defn get-r-lang-data [type]
  (str  "s <- c" (toR (map :bugcommits data-cleaned))))

(get-r-lang-data "bugcommits")

(defn get-r-field-data [type]
  (map (fn [lang name]
         (str name type " <- c(" (-> (map (keyword type) lang) toR) ")"))
       over-fifteen-forks-bugs-tests
       langs))

(get-r-field-data "bugs")
(get-r-field-data "tests")
(reduce str (map #(str "cor(" % "bugs, " % "tests)") langs))

;; (map  (comp int :commitratio) data-cleaned)
;; data-cleaned
(def sorted
  (list
   {:name "clojure",      :bugs 76, :repos 23, :ratio 3, :commitratio 33, :commits 22798}
   {:name "fsharp"    ,   :bugs 27, :repos 6, :ratio 4, :commitratio 45, :commits 5901}
   {:name "haskell",      :bugs 115, :repos 22, :ratio 5, :commitratio 23, :commits 48726}
   {:name "ruby",         :bugs 177, :repos 29, :ratio 6, :commitratio 17, :commits 101431}
   {:name "coffeescript", :bugs 203, :repos 27, :ratio 7, :commitratio 35, :commits 57124}
   {:name "scala",        :bugs 227, :repos 30, :ratio 7, :commitratio 30, :commits 74539}
   {:name "python",       :bugs 361, :repos 30, :ratio 12, :commitratio 44, :commits 81765}
   {:name "go",           :bugs 452, :repos 33, :ratio 13, :commitratio 48, :commits 93680}
   {:name "csharp",       :bugs 483, :repos 31, :ratio 15, :commitratio 63, :commits 76405}
   {:name "php",          :bugs 574, :repos 26, :ratio 22, :commitratio 65, :commits 87741}
   {:name "java",         :bugs 676, :repos 29, :ratio 23, :commitratio 60, :commits 112441}
   {:name "js",           :bugs 1037, :repos 35, :ratio 29, :commitratio 123, :commits 83930}))

;; (int (/ (reduce + (map :commitratio sorted)) (count sorted)))


(list
 {:name "ruby"        ,:bugs 177,:repos 29,:ratio 6.1034484,:commitratio 17,:commits 101431}
 {:name "haskell"     ,:bugs 115,:repos 22,:ratio 5.2272725,:commitratio 23,:commits 48726}

 {:name "scala"       ,:bugs 227,:repos 30,:ratio 7.5666666,:commitratio 30,:commits 74539}
 {:name "clojure"     ,:bugs 76,:repos 23,  :ratio 3.3043478,:commitratio 33,:commits 22798}
 {:name "coffeescript",:bugs 203,:repos 27,:ratio 7.5185184,:commitratio 35,:commits 57124}

 {:name "python"      ,:bugs 361,:repos 30,:ratio 12.033334,:commitratio 44,:commits 81765}
 {:name "fsharp"      ,:bugs 27,:repos 6,  :ratio 4.5      ,:commitratio 45,:commits 5901}
 {:name "go"          ,:bugs 452,:repos 33,:ratio 13.69697 ,:commitratio 48,:commits 93680}

 {:name "java"        ,:bugs 676,:repos 29,:ratio 23.310345,:commitratio 60,:commits 112441}
 {:name "csharp"      ,:bugs 483,:repos 31,:ratio 15.580646,:commitratio 63,:commits 76405}
 {:name "php"         ,:bugs 574,:repos 26,:ratio 22.076923,:commitratio 65,:commits 87741}

 {:name "js"          ,:bugs 1037,:repos 35,:ratio 29.62857,:commitratio 123,:commits 83930}
 )



;; (defn toR [col]
;;   (str "(" (clojure.string/join "," col) ")"))

;; (toR [15 4 3 29 7 7 22 13 23 5 6 12])

;; (toR (list 63 45 33 123 35 30 65 48 60 23 17 44))


;; data-cleaned

;; bugs-many-paged

;; filter for activity !!!

;;(take 5 (map second  (list ["csharp" 24] ["fsharp" 3] ["clojure" 2] ["js" 44] ["coffeescript" 23] ["scala" 10] ["php" 54] ["go" 22] ["java" 36] ["haskell" 11] ["ruby" 11] ["python" 18])))
(24 3 2 44 23)
;; extracted
(["csharp" 24] ["fsharp" 3] ["clojure" 2] ["js" 44] ["coffeescript" 23] ["scala" 10] ["php" 54] ["go" 22] ["java" 36] ["haskell" 11] ["ruby" 11] ["python" 18])
;; sorted
(["clojure" 2] ["fsharp" 3] ["scala" 10] ["haskell" 11] ["ruby" 11] ["python" 18] ["go" 22] ["coffeescript" 23] ["csharp" 24] ["java" 36] ["js" 44] ["php" 54])

(json/write-str '({:ratio 24.506666, :lang "csharp", :bugs 3676, :repos 150} {:ratio 3.6190476, :lang "fsharp", :bugs 76, :repos 21} {:ratio 2.53211, :lang "clojure", :bugs 276, :repos 109} {:ratio 44.226665, :lang "js", :bugs 6634, :repos 150} {:ratio 23.373333, :lang "coffeescript", :bugs 3506, :repos 150} {:ratio 10.786667, :lang "scala", :bugs 1618, :repos 150} {:ratio 54.62, :lang "php", :bugs 8193, :repos 150} {:ratio 22.693333, :lang "go", :bugs 3404, :repos 150} {:ratio 36.633335, :lang "java", :bugs 5495, :repos 150} {:ratio 11.171429, :lang "haskell", :bugs 782, :repos 70} {:ratio 11.36, :lang "ruby", :bugs 1704, :repos 150} {:ratio 18.3, :lang "python", :bugs 2745, :repos 150}))
"[{\"ratio\":24.506666,\"lang\":\"csharp\",\"bugs\":3676,\"repos\":150},{\"ratio\":3.6190476,\"lang\":\"fsharp\",\"bugs\":76,\"repos\":21},{\"ratio\":2.53211,\"lang\":\"clojure\",\"bugs\":276,\"repos\":109},{\"ratio\":44.226665,\"lang\":\"js\",\"bugs\":6634,\"repos\":150},{\"ratio\":23.373333,\"lang\":\"coffeescript\",\"bugs\":3506,\"repos\":150},{\"ratio\":10.786667,\"lang\":\"scala\",\"bugs\":1618,\"repos\":150},{\"ratio\":54.62,\"lang\":\"php\",\"bugs\":8193,\"repos\":150},{\"ratio\":22.693333,\"lang\":\"go\",\"bugs\":3404,\"repos\":150},{\"ratio\":36.633335,\"lang\":\"java\",\"bugs\":5495,\"repos\":150},{\"ratio\":11.171429,\"lang\":\"haskell\",\"bugs\":782,\"repos\":70},{\"ratio\":11.36,\"lang\":\"ruby\",\"bugs\":1704,\"repos\":150},{\"ratio\":18.3,\"lang\":\"python\",\"bugs\":2745,\"repos\":150}]"
(map (fn [lang] [(:lang lang) (-> lang :ratio int )]) bugs-many-paged)


(list {:ratio 32.493332, :lang "csharp", :bugs 4874, :repos 150} {:ratio 20.25926, :lang "fsharp", :bugs 547, :repos 27} {:ratio 3.360294, :lang "clojure", :bugs 457, :repos 136} {:ratio 42.366665, :lang "js", :bugs 6355, :repos 150} {:ratio 23.38, :lang "coffeescript", :bugs 3507, :repos 150} {:ratio 11.973333, :lang "scala", :bugs 1796, :repos 150} {:ratio 54.326668, :lang "php", :bugs 8149, :repos 150} {:ratio 28.386667, :lang "go", :bugs 4258, :repos 150} {:ratio 35.246666, :lang "java", :bugs 5287, :repos 150} {:ratio 12.229167, :lang "haskell", :bugs 1174, :repos 96} {:ratio 9.526667, :lang "ruby", :bugs 1429, :repos 150} {:ratio 21.206667, :lang "python", :bugs 3181, :repos 150})




;;(map (fn [lang] [(:lang lang) (-> lang :ratio int )]) bugs-many-paged)

;; 2013-now
(list ["csharp" 37] ["fsharp" 25] ["clojure" 4] ["js" 41] ["coffeescript" 13] ["scala" 11] ["php" 17] ["go" 25] ["java" 18] ["haskell" 15] ["ruby" 6] ["python" 16])
(list {:ratio 37.073334, :lang "csharp", :bugs 5561, :repos 150} {:ratio 25.714285, :lang "fsharp", :bugs 540, :repos 21} {:ratio 4.56962, :lang "clojure", :bugs 361, :repos 79} {:ratio 41.98, :lang "js", :bugs 6297, :repos 150} {:ratio 13.12, :lang "coffeescript", :bugs 1968, :repos 150} {:ratio 11.793333, :lang "scala", :bugs 1769, :repos 150} {:ratio 17.853333, :lang "php", :bugs 2678, :repos 150} {:ratio 25.92, :lang "go", :bugs 3888, :repos 150} {:ratio 18.973333, :lang "java", :bugs 2846, :repos 150} {:ratio 15.888889, :lang "haskell", :bugs 858, :repos 54} {:ratio 6.4466667, :lang "ruby", :bugs 967, :repos 150} {:ratio 16.813334, :lang "python", :bugs 2522, :repos 150})

;; 2013 - now

;; (map (comp #(- 0 %) :ratio) bugs-many-paged)

(list ["csharp" 32] ["fsharp" 20] ["clojure" 3] ["js" 44] ["coffeescript" 23] ["scala" 11] ["php" 54] ["go" 28] ["java" 35] ["haskell" 12] ["ruby" 9] ["python" 21])
(list ["clojure" 3] ["ruby" 9] ["scala" 11] ["haskell" 12] ["fsharp" 20] ["python" 21] ["coffeescript" 23] ["go" 28] ["csharp" 32] ["java" 35] ["js" 44] ["php" 54])
(list {:ratio 32.493332, :lang "csharp", :bugs 4874, :repos 150} {:ratio 20.25926, :lang "fsharp", :bugs 547, :repos 27} {:ratio 3.3851852, :lang "clojure", :bugs 457, :repos 135} {:ratio 44.633335, :lang "js", :bugs 6695, :repos 150} {:ratio 23.38, :lang "coffeescript", :bugs 3507, :repos 150} {:ratio 11.973333, :lang "scala", :bugs 1796, :repos 150} {:ratio 54.32, :lang "php", :bugs 8148, :repos 150} {:ratio 28.386667, :lang "go", :bugs 4258, :repos 150} {:ratio 35.246666, :lang "java", :bugs 5287, :repos 150} {:ratio 12.260417, :lang "haskell", :bugs 1177, :repos 96} {:ratio 9.493333, :lang "ruby", :bugs 1424, :repos 150} {:ratio 21.206667, :lang "python", :bugs 3181, :repos 150})
;; (map (fn [lang] [(:lang lang) (-> lang :ratio int )]) bugs-many-paged)


;; 90 many langs
;; ({:ratio 44.744446, :lang "csharp", :bugs 4027, :repos 90} {:ratio 20.25926, :lang "fsharp", :bugs 547, :repos 27} {:ratio 4.3, :lang "clojure", :bugs 387, :repos 90} {:ratio 46.455555, :lang "js", :bugs 4181, :repos 90} {:ratio 29.777779, :lang "coffeescript", :bugs 2680, :repos 90} {:ratio 16.722221, :lang "scala", :bugs 1505, :repos 90} {:ratio 83.23333, :lang "php", :bugs 7491, :repos 90} {:ratio 42.41111, :lang "go", :bugs 3817, :repos 90} {:ratio 24.48889, :lang "java", :bugs 2204, :repos 90} {:ratio 12.588889, :lang "haskell", :bugs 1133, :repos 90} {:ratio 10.611111, :lang "ruby", :bugs 955, :repos 90} {:ratio 21.977777, :lang "python", :bugs 1978, :repos 90})
;; (map (fn [lang] [(:lang lang) (-> lang :ratio int )]) bugs-many-paged)



;; repos and issues since 2012
(list {:ratio 32.493332, :lang "csharp", :bugs 4874, :repos 150} {:ratio 20.25926, :lang "fsharp", :bugs 547, :repos 27} {:ratio 3.3851852, :lang "clojure", :bugs 457, :repos 135} {:ratio 44.633335, :lang "js", :bugs 6695, :repos 150} {:ratio 23.38, :lang "coffeescript", :bugs 3507, :repos 150} {:ratio 11.973333, :lang "scala", :bugs 1796, :repos 150} {:ratio 55.52, :lang "php", :bugs 8328, :repos 150})

;; issues in two year span 2012-2014
(list {:ratio 10.906667, :lang "csharp", :bugs 1636, :repos 150} {:ratio 0.85714287, :lang "fsharp", :bugs 18, :repos 21} {:ratio 1.3981482, :lang "clojure", :bugs 151, :repos 108} {:ratio 14.886666, :lang "js", :bugs 2233, :repos 150} {:ratio 5.48, :lang "coffeescript", :bugs 822, :repos 150} {:ratio 2.3466666, :lang "scala", :bugs 352, :repos 150} {:ratio 20.86, :lang "php", :bugs 3129, :repos 150})

;; repos in two year span 2012-2014
(list {:ratio 27.22, :lang "csharp", :bugs 4083, :repos 150} {:ratio 236412.9, :lang "fsharp", :bugs 4964671, :repos 21} {:ratio 11495.037, :lang "clojure", :bugs 1241464, :repos 108} {:ratio 48.6, :lang "js", :bugs 7290, :repos 150} {:ratio 24.8, :lang "coffeescript", :bugs 3720, :repos 150} {:ratio 11.946667, :lang "scala", :bugs 1792, :repos 150} {:ratio 63.926666, :lang "php", :bugs 9589, :repos 150})


;; 150 each
(comment {:ratio 61.12, :lang "csharp", :bugs 9168, :repos 150} {:ratio 106400.4, :lang "fsharp", :bugs 3724014, :repos 35} {:ratio 6.326667, :lang "clojure", :bugs 949, :repos 150} {:ratio 66.333336, :lang "js", :bugs 9950, :repos 150} {:ratio 28.0, :lang "coffeescript", :bugs 4200, :repos 150} {:ratio 20.88, :lang "scala", :bugs 3132, :repos 150} {:ratio 112.24, :lang "php", :bugs 16836, :repos 150})

;; just 60 each
(comment {:ratio 107.433334, :lang "csharp", :bugs 6446, :repos 60} {:ratio 16.8, :lang "fsharp", :bugs 588, :repos 35} {:ratio 10.483334, :lang "clojure", :bugs 629, :repos 60} {:ratio 101.25, :lang "js", :bugs 6075, :repos 60} {:ratio 58.466667, :lang "coffeescript", :bugs 3508, :repos 60} {:ratio 38.0, :lang "scala", :bugs 2280, :repos 60} {:ratio 213.28334, :lang "php", :bugs 12797, :repos 60})({:ratio 107.433334, :lang "csharp", :bugs 6446, :repos 60} {:ratio 16.8, :lang "fsharp", :bugs 588, :repos 35} {:ratio 10.483334, :lang "clojure", :bugs 629, :repos 60} {:ratio 101.25, :lang "js", :bugs 6075, :repos 60} {:ratio 58.466667, :lang "coffeescript", :bugs 3508, :repos 60} {:ratio 38.0, :lang "scala", :bugs 2280, :repos 60} {:ratio 213.28334, :lang "php", :bugs 12797, :repos 60})

(map :ratio bugs-many-paged)
(map :bugs bugs-many-paged)
(map :repos bugs-many-paged)

(defn make-test-queries [lang]
  (test-count lang (get-repo-names-for-lang lang)))

(def test-counts (map #(get-count-q (make-test-queries %)) langs))
(def qs (map #(make-test-queries %) langs))
