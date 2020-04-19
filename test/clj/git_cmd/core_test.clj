(ns git-cmd.core-test
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh with-sh-dir]]
            [git-cmd.core :as sut]))


(defmacro with-private-fns
  "Refers private fns from ns and runs tests in context."
  [[ns fns] & tests]
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))



(def ^:private tmp-dir "/tmp/git-test/")
(def ^:private tmp-src-dir "/tmp/git-test/src")

(defn prepare-before-test
  "测试之前执行"
  []
  (.mkdir (io/file tmp-dir))
  (with-sh-dir tmp-dir
    (sh "sh" "-c" "echo 'hello' > abc.clj")
    (sh "git" "init")
    (sh "git" "add" "--all")
    (sh "git" "commit" "-m" "commit"))
  (.mkdir (io/file tmp-src-dir))
  (with-sh-dir tmp-src-dir
    (sh "sh" "-c" "echo 'hello' > abc.clj")
    (sh "git" "add" "--all")
    (sh "git" "commit" "-m" "commit"))
  )
(defn clean-up-after-test
  "测试之后执行"
  []
  (with-sh-dir tmp-dir
    (sh "sh" "-c" "rm -rf  /tmp/git-test/ ")    )
  )

(t/use-fixtures
  :once
  ;; :each 用于每一组deftest 都执行的场景
  ;; :once fixture在当前namespace只执行一次
  (fn [f]
    (prepare-before-test)
    (f)
    (clean-up-after-test)
    ))

(with-private-fns
  [git-cmd.core [has-sub-dir?
                 repo-dir?
                 file-end-with?
                 src-dir?
                 authors-stats]]
  (t/deftest 测试目录和文件相关操作
    (t/testing "第一组test"
      (t/is (nil? (has-sub-dir? (io/file tmp-dir ) ".gt")) "没有.gt文件夹")
      (t/is (has-sub-dir? (io/file tmp-dir ) ".git") "还应该有.git文件夹")
      (t/is (repo-dir? (io/file tmp-dir )))
      (t/is (nil? (repo-dir? (io/file "/" ))))

      (t/is (file-end-with? (io/file "abc.abc" ) "abc"))
      (t/is (file-end-with? (io/file "abc.abc.xf" ) "xf"))
      (t/is (not (file-end-with? (io/file "abc.abc.xf" ) "x-f")))))


  (t/deftest 测试src-dirs
    (t/testing "不是src目录的情况"
      (t/is (not (src-dir? (clojure.java.io/file tmp-dir)))))
    (t/testing "是src的情况"
      (t/is (src-dir? (clojure.java.io/file tmp-src-dir))))
    )

  (t/deftest 测试函数file-end-with?
    (t/testing "正确以后缀.cpp结束的情况"
      (t/is (sut/file-end-with? (clojure.java.io/file "xxx.cpp")
                                "cpp") )
      )
    (t/testing "不是以后缀.cpp结束的情况"
      (t/is (not (sut/file-end-with? (clojure.java.io/file "xxx.pp")
                                     "cpp")) )
      ))

  (t/deftest 测试函数file-suffix
    (t/testing "正常后缀的情况"
      (t/is (= "xyz" (sut/file-suffix "abc.xyz")) )
      )
    (t/testing "多个后缀的情况"
      (t/is (= "xyz" (sut/file-suffix "www.abc.xyz")) )
      )
    (t/testing "没有后缀的情况"
      (t/is (= "xyz" (sut/file-suffix "xyz")) )
      )
    (t/testing "有乱字符的情况"
      (t/is (= "abc" (sut/file-suffix "xyz.abc\"")) )
      )
    (t/testing "有数字的情况"
      (t/is (= "abc" (sut/file-suffix "xyz.abc045\"")) )
      ))

  (t/deftest 测试repo相关
    (t/testing "第一组test"
      (t/is (let [m (sut/authors-stats tmp-dir)]
              (and (map? m)
                   (= 1 (first (vals m))))) )
      ))

  (t/deftest 测试配置文件相关
    (t/testing "第一组test"
      (t/is (let [m (sut/authors-stats tmp-dir)]
              (and (map? m)
                   (= 1 (first (vals m))))) )
      ))
  )
