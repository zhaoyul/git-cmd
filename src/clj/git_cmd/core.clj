(ns git-cmd.core
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

(def src-suffix-set #{"clj" "cljs" "js" "css" "sql" "vue" "java"})

(defn- file-suffix
  "返回文件的后缀名"
  [file-name]
  (s/replace file-name #".*\." "") )

(defn- is-src-file?
  "是否为源代码"
  [f]
  (src-suffix-set (file-suffix (.getName f))))

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

(defn current-date-str [date]
  (jt/format "YYYY-MM-dd" date))

(defn stats-file [repo date]
  (str "./tmp/" repo "/" (current-date-str date) "-status.edn"
       ))

(defn processed? [repo date]
  (.exists (io/file (stats-file repo date))))

(comment
  (stats-file "customplatform" (jt/local-date))
  (processed? "customplatform" (jt/local-date))
  )

(defn retrive-loc-from-file [repo date]
  (->> (stats-file repo date)
       slurp
       edn/read-string))

(defn generate-loc-anew [repo date data]
  (let [file (stats-file repo date)
        _ (io/make-parents file )]
    (-> (stats-file repo date)
        (spit data))
    data))

(defn repo-name [repo-dir]
  (s/replace repo-dir #".*/" ""))

(defn sync-stats [repo-dir date]
  (if (processed? (repo-name repo-dir) date)
    (retrive-loc-from-file (repo-name repo-dir) date)
    (generate-loc-anew (repo-name repo-dir) date (authors-stats repo-dir) )))


;; 人员对应关系也是要到配置文件
(def authors {"Nie JianLong"   "聂建龙"
              "NieJianlong"    "聂建龙"
              "chuanwu zhu"    "聂建龙"
              "Kevin li"       "李照宇"
              "Kevin.li"       "李照宇"
              "kevin.li"       "李照宇"
              "lizy"           "李照宇"
              "dirk.sun"       "孙东和"
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
              "WYX"            "丁凡"
              "Henry"          "丁凡"})

(defn category-chart-data [m]
  (reduce-kv (fn [m k v]
               (assoc-in m [(:name k) (:file k)] v))
             {}
             m))

(defn re-name [input]
  (reduce-kv (fn [m k v]
               (if (get authors (:name k))
                 (assoc m
                        {:name (get authors (:name k)) :file (:file k)}
                        (+ v (or (get m {:name (get authors (:name k)) :file (:file k)} ) 0)  ))
                 (assoc m k v))) {} input))

(comment
  (c/view
   (c/category-chart
    (category-chart-data (re-name (sync-stats "../peptide" (jt/local-date))))
    {:title "多肽代码分析"
     :width 600
     :height 400
     :render-style :line
     :theme :xchart
     :y-axis {}
     :x-axis {:order ["cljs" "clj" "sql" "css" "js"]}}))
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
  "I don't do a whole lot ... yet."
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
