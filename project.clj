(defproject git-cmd "0.1.0-SNAPSHOT"
  :jvm-opts ["-Dapple.awt.UIElement=false"]
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.cli "1.0.194"]
                 [clojure.java-time "0.3.2"]
                 [com.hypirion/clj-xchart "0.2.0"]
                 [clj-jgit "1.0.0-beta3"]]
  :main ^:skip-aot git-cmd.core
  :source-paths ["src/clj"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
