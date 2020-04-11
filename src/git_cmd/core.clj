(ns git-cmd.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
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


(def cli-options
  ;; An option with a required argument
  [["-r" "--repo repo-dir" "repo directory"
    :default "."
    :parse-fn #(io/file %)
    :validate [#(.isDirectory %) "Must by a directory"]]
   ["-c" "--config config.edn" "config file"
    :default "config.edn"
    :parse-fn #(io/file %)
    :validate [#(not (.isDirectory %)) "Must by a directory"]]

   ["-h" "--help"]])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [options (parse-opts args cli-options)]
    (println options)))
