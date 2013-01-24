(defproject visic "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://visi.la"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.eclipse.jgit/org.eclipse.jgit "1.3.0.201202151440-r"]
                 [la.visi/visi_2.10 "0.1-SNAPSHOT"]]
  :source-paths      ["src/clojure"]
  :java-source-paths ["src/java"]
  :repositories [["java.net" "http://download.java.net/maven/2"]
                 ["sonatype" {:url "http://oss.sonatype.org/content/repositories/releases"
                              ;; If a repository contains releases only setting
                              ;; :snapshots to false will speed up dependencies.
                              ;; :snapshots false
                              ;; You can also set the policies for how to handle
                              ;; :checksum failures to :fail, :warn, or :ignore.
                              :checksum :fail
                              ;; How often should this repository be checked
                              ;; for updates? (:daily, :always, or :never)
                              :update :always}]
                ["Sonatype snapshot" {
                  :url "https://oss.sonatype.org/content/repositories/snapshots"
                  :checksum :ignore
                  :update :always}]
                ]
  :main visi.clcore)
