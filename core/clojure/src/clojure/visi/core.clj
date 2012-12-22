(ns visi.core
  (:gen-class))

(import
  '(visi Frog)
  '(java.util Date)
  '(java.util.concurrent ConcurrentHashMap))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
(let [frog (Frog.)]
(println "Plus 1 " (. frog foo 1))
  (println "Pow " (Math/pow 2 3))
  (println "Map is " (ConcurrentHashMap.))
  (println "It's " (Date.))
  (println "Hello, World!33"))
  44)
