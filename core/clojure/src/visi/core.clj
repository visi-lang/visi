(ns visi.core
  (:gen-class))

(import
  '(java.util Date)
  '(java.util.concurrent ConcurrentHashMap))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Pow " (Math/pow 2 3))
  (println "Map is " (ConcurrentHashMap.))
  (println "It's " (Date.))
  (println "Hello, World!"))
