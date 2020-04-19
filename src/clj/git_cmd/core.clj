(ns git-cmd.core
  "使用git来统计工作量"
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [com.hypirion.clj-xchart :as c]
            [java-time :as jt]
            [clojure.tools.cli :refer [parse-opts]]
            [clj-jgit.querying :refer [rev-list commit-info changed-files-with-patch]]
            [clj-jgit.porcelain :refer [git-blame load-repo git-log git-fetch git-clone git-add git-commit git-push]])
  (:gen-class))


;;; 配置部分, 最终会放到文件中
(def SRC-SUFFIX-SET #{"clj" "cljs" "js" "css" "sql" "vue" "java"})
;; 人员对应关系也是要到配置文件
(def AUTHORS {"sunx113@139.com" "孙岩"
              "sunx113"        "孙岩"
              "andyhuang"      "黄超"
              "redcreation"    "amdin"
              "Nie JianLong"   "聂建龙"
              "NieJianlong"    "聂建龙"
              "chuanwu zhu"    "聂建龙"
              "Kevin li"       "李照宇"
              "Kevin.li"       "李照宇"
              "kevin.li"       "李照宇"
              "lizy"           "李照宇"
              "dirk.sun"       "孙东和"
              "Dirk"           "孙东和"
              "Damon"          "沈友谊"
              "Tony"           "杨鲁鹏"
              "cisco.luo"      "罗德玉"
              "leilei.s"       "孙磊磊"
              "zhanghongyi"    "张弘毅"
              "David Wu"       "吴伟"
              "alisa.yang"     "杨柳"
              "visen_lu"       "陆卫新"
              "vise.lu"        "陆卫新"
              "Murphy"         "贺茂丰"
              "ElbertY"        "依力"
              "Anna"           "赵阳"
              "maofeng"        "贺茂丰"
              "MaoFeng"        "贺茂丰"
              "hcops"          "hcops..who??"
              "ranmingsheng"   "冉明生"
              "chris"          "冉明生"
              "chirs"          "冉明生"
              "ben"            "冉明生"
              "marvin ma"      "马海强"
              "strongfish"     "于壮壮"
              "eric shao"      "邵夔"
              "cui"            "崔云鹏"
              "eric"           "崔云鹏"
              "eric.cui"       "崔云鹏"
              "henrydf"        "丁凡"
              "henry"          "丁凡"
              "WYX"            "丁凡"
              "Henry"          "丁凡"})


;;; 目录操作

(defn- has-sub-dir?
  "判断dir下是否有名为 sub-dir-name 的子文件夹"
  [dir sub-dir-name]
  (let [files (vec (.listFiles dir))]
    (->> files
         (filter (fn [f] (and (.isDirectory f)
                             (= sub-dir-name (.getName f)))))
         seq)))

(defn- repo-dir?
  "判断当前文件夹是否是个git代码库"
  [dir]
  (and (.isDirectory dir)
       (has-sub-dir? dir ".git")))

(defn- src-dir?
  "判断当前src目录是不是代码目录, 主要用来排除node_modules的src目录"
  [dir]
  (and (.isDirectory dir)
       (#{"src" "sql"} (.getName dir) )
       (not (s/includes? (.getParent dir) "node_modules"))))


(defn file-end-with?
  "文件是否以某后缀结束"
  [f suffix]
  (s/ends-with? (.getName f) suffix))

(defn file-suffix
  "返回文件的后缀名"
  [file-name]
  (if (s/includes? file-name ".")
    (s/replace file-name #".*\." "")
    "") )

(defn- is-src-file?
  "是否为源代码"
  [f]
  (SRC-SUFFIX-SET (file-suffix (.getName f))))

(defn- repo-root
  "获得repo相对于当前执行目录的位置, 为库调用做准备"
  [file]
  (loop [parent (.getParent file)]
    (if (repo-dir? (io/file parent))
      parent
      (recur (.getParent (io/file parent))))))

(defn- file-path-in-repo
  "处理代码文件的名称:
   ../customplatform/store-pc/src/cljs/store_pc/quick_custom/information_view.cljs
  处理为:
  store-pc/src/cljs/store_pc/quick_custom/information_view.cljs"
  [file-name]
  (let [repo-root-path (repo-root (io/file file-name))]
    (-> file-name
        (s/replace repo-root-path "")
        (s/replace-first "/" "" ))))


(defn- src-files-lst
  "从src目录下返回该目录的所有代码文件"
  [src-dir]
  (->> src-dir
       file-seq
       (filter (fn [f]
                 (and (.isFile f)
                      (is-src-file? f))))))



(defn- file-full-name
  "拼接文件的全名"
  [io-file]
  (str (.getParent io-file) "/" (.getName io-file)))


(defn src-dirs
  "返回当前代码文件夹下所有的src目录"
  [dir]
  (let [all-files (file-seq dir)
        dirs (->> all-files
                  (filter src-dir?))]
    dirs))

(defn blame-file
  "使用blame处理代码, 得到每个人在该文件的行数"
  [repo file-name]
  (->> file-name
       file-path-in-repo
       (git-blame repo)
       (map (fn [m] {:name (get-in m [:author :name])
                    :file (file-suffix (get-in m [:source-path]))}))
       (group-by identity)
       (reduce-kv (fn [m k v]
                    (assoc m k (count v)))
                  {})))


(defn authors-stats [repo-dir-path]
  {:pre [(repo-dir? (io/file repo-dir-path))]}
  (let [repo (load-repo repo-dir-path)]
    (->> repo-dir-path
         io/file
         src-dirs
         (mapcat src-files-lst)
         (map file-full-name)
         (map (partial blame-file repo))
         (reduce  (fn [r m]
                    (merge-with + r m))
                  {}))))


(defn category-chart-data [m]
  (reduce-kv (fn [m k v]
               (assoc-in m [(:name k) (:file k)] v))
             {}
             m))

(defn date-formatter
  "foramt date to string"
  [date]
  (jt/format "YYYY-MM-dd" date))

(defn repo-name-from-dir [repo-dir]
  (-> repo-dir
      clojure.java.io/file
      .getAbsolutePath
      (s/replace #".*/" "")))

(defn stats-file
  "根据日期生成status缓存文件名"
  [repo-dir date]
  (let [repo-name (repo-name-from-dir repo-dir)
        date-str  (date-formatter date)]
    (str "./tmp/" repo-name "/" date-str "-status.edn"
         )))

(defn cached?
  "接受git库目录和日期判断缓存文件是否存在"
  [repo-dir date]
  (.exists (io/file (stats-file repo-dir date))))

(comment
  (stats-file "customplatform" (jt/local-date))
  (cached? "customplatform" (jt/local-date))
  )

(defn retrive-loc-from-file
  "从文件中读取[作者-行数]的统计数据"
  [repo-dir date]
  (->> (stats-file repo-dir date)
       slurp
       edn/read-string))

(defn generate-loc-anew
  "重新生成作者-行数统计数据"
  [repo-dir date data]
  (let [file (stats-file repo-dir date)
        _ (io/make-parents file )]
    (-> (stats-file repo-dir date)
        (spit data))
    data))

(defn repo-name
  "从全路径中得到repo的最后一集的文件夹名称"
  [repo-dir]
  (s/replace repo-dir #".*/" ""))

(defn sync-stats
  "得到[作者-行数]的统计数据"
  [repo-dir date]
  (if (cached? repo-dir date)
    (retrive-loc-from-file repo-dir date)
    (generate-loc-anew repo-dir date (authors-stats repo-dir) )))


(defn re-name [input]
  (reduce-kv (fn [m k v]
               (if (get AUTHORS (:name k))
                 (assoc m
                        {:name (get AUTHORS (:name k)) :file (:file k)}
                        (+ v (or (get m {:name (get AUTHORS (:name k)) :file (:file k)} ) 0)  ))
                 (assoc m k v))) {} input))

(comment

  (sync-stats "../tmp" (jt/local-date))

  (c/view
   (c/category-chart
    (category-chart-data (re-name (sync-stats "../cosmo-test-platform" (jt/local-date))))
    {:title "cosmo测试平台"
     :width 600
     :height 400
     :render-style :line
     :theme :xchart
     :y-axis {}
     :x-axis {:order ["cljs" "clj" "sql" "css" "js"]}}))

  (c/view
   (c/category-chart
    (category-chart-data (re-name (sync-stats "../customplatform" (jt/local-date))))
    {:title "多肽代码分析"
     :width 600
     :height 400
     :render-style :line
     :theme :xchart
     :y-axis {}
     :x-axis {:order ["cljs" "clj" "sql" "css" "js"]}}))
  )


(defn all-commits
  "获得repo的所有commit, 注意是所有branch的commit"
  [repo]
  (->>  (rev-list repo)
        (map  (partial commit-info repo))))

(defn- rev-diff
  "返回当前commit对应的patch, 返回值是patch的文本"
  [rev-m]
  (changed-files-with-patch (:repo rev-m) (:raw rev-m)))

(defn- count-commit-lines
  "为commit-map增加一个新的key, :add-delete 记录了修改的行数"
  [rev]
  (assoc
   rev
   :add-delete
   (if-let [patch (rev-diff rev)]
     {:add-line (count (re-seq #"\n\+" patch))
      :delete-line (count (re-seq #"\n\-" patch))}
     {:add-line 0
      :delete-line 0})))

#_(defn all-diffs [repo]
    (->>  (rev-list repo)
          (map  (partial count-commit-lines repo))))

(defn time->date
  "java.util.Date -> java-time的localdate"
  [date]
  (-> date
      jt/instant
      (.atZone (jt/zone-id))
      (.toLocalDate)))

(defn truncate-time-to-day
  "util.Date 得到当前时间对应的0点."
  ;; TODO: 这应该不是应该最好的方法....
  [date]
  (-> date
      .toInstant
      (.atZone (jt/zone-id))
      (.toLocalDateTime)
      (jt/truncate-to :days)
      (.atZone (jt/zone-id))
      .toInstant
      (java.util.Date/from)))

(defn rev->date
  "从时间到日期, 返回localdate类型的rev map的日期"
  [rev]
  (-> rev :time time->date ))

(defn commit-file-name
  "commit的edn文件的名称, 一天一个, 作为后续的缓存使用"
  [repo date]
  (format "./tmp/%s/%s-commit.edn" repo (jt/format "YYYY-MM-dd" date) ))

(defn commits-cached?
  "判断缓存文件是否存在"
  [repo date]
  (.exists (io/file (commit-file-name repo date))))

(defn gen-commits-anew
  "产生一个新的commit edn文件到对应的磁盘位置"
  [repo repo-dir date]
  (let [file-name (commit-file-name repo date)
        file (io/file file-name)
        _ (io/make-parents file)
        data (->> repo-dir
                  load-repo
                  all-commits
                  (map #(select-keys % [:email
                                        :time
                                        :branches
                                        :merge
                                        :author
                                        :id
                                        :message]))
                  vec)]
    (spit file-name data)
    data))

(defn retrive-commits-from-cache
  "从缓存读到相应的commit edn, 每天只执行一次就够了."
  [repo date]
  (let [file-name (commit-file-name repo date)]
    (->> file-name
         slurp
         edn/read-string
         )))

(defn sync-commits
  "写入commit edn, 做缓存用."
  [repo repo-dir date]
  (let [file-name (commit-file-name repo date)
        file (io/file file-name)
        _ (io/make-parents file)]

    (if (commits-cached? repo date)
      (retrive-commits-from-cache repo date)
      (gen-commits-anew repo repo-dir date))))

(defn jt-local-date->util-date [date]
  (-> date
      (.atStartOfDay)
      (.atZone (jt/zone-id))
      (.toInstant)
      (java.util.Date/from)))

(defn in-peroid [start end now]
  (let [s (jt-local-date->util-date start)
        e (jt-local-date->util-date end)]
    (and (neg? (compare s   now ))
         (neg? (compare now e)))))



(defn re-name-commit [commit]
  (if-let [new-name (AUTHORS (:author commit))]
    (assoc commit :author new-name)
    commit))

(defn mapping-author-names [git-author-name]
  (or (AUTHORS git-author-name) git-author-name))

(defn time->first-day-of-month [time]
  (-> time
      time->date
      (jt/adjust :first-day-of-month)))

(defn monthly-commits [commits]
  (->> commits
       (partition-by (fn [commit] (time->first-day-of-month (:time commit)))))
  )

(defn partion-commits-by [commits peroid]
  (case peroid
    :monthly (monthly-commits commits) ))

(defn commit-count-by-author [commits]
  (->> commits
       (map (fn [m] (assoc m :author (mapping-author-names (:author m)))))
       (group-by :author)
       (reduce-kv (fn [m k v]
                    (assoc m k (count v)))
                  {})))

(defn gen-commit-chart [repo-name
                        repo-dir
                        {:keys [last-commit-date period]
                         :or {last-commit-date (jt/local-date)
                              period           :monthly}}]
  (let [commits (sync-commits repo-name
                              repo-dir
                              last-commit-date)
        partition-commits (partion-commits-by commits period)]

    (c/view (c/pie-chart (commit-count-by-author (second partition-commits)))))
  )

(comment

  (commit-count-by-author (sync-commits "定制平台"
                                        "../customplatform"
                                        (jt/local-date)) )
  (gen-commit-chart "定制平台"
                    "../customplatform"
                    {:last-commit-date (jt/local-date)} )
  (sync-commits "定制平台"
                "../customplatform"
                (jt/local-date))
  )



(def cli-options
  ;; An option with a required argument
  [
   ["-r" "--repo repo-dir" "repo directory"
    :default "."
    :parse-fn (fn [dir] {:file (io/file dir)
                        :dir dir})
    :validate [#(.isDirectory (:file %)) "Must by a directory"]]
   ["-t" "--title project-name" "name"
    :default "代码库xxxx"
    :parse-fn str
    :validate [string? "必须是一个string"]]
   ["-c" "--config config.edn" "config file"
    :default "config.edn"
    :parse-fn #(io/file %)
    :validate [#(not (.isDirectory %)) "Must by a directory"]]

   ["-h" "--help"]])

(defn -main
  [& args]
  (let [options (parse-opts args cli-options)
        dir     (get-in options [:options :repo :dir])
        title   (get-in options [:options :title])]
    (c/view
     (c/category-chart
      (category-chart-data (re-name (sync-stats dir (jt/local-date))))
      {:title title
       :width 600
       :height 400
       :render-style :line
       :theme :xchart
       :y-axis {}
       :x-axis {:order ["cljs" "clj" "sql" "css" "js"]}}))
    ))
