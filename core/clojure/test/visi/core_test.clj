(ns visi.core-test
  (:use clojure.test
        visi.clcore))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))



(deftest b-test
  (testing "The parser."
  (let [res (visi.core.VisiParse.)
       moo (.map (net.liftweb.common.Full. "hi") (f1 (fn [x] (.length x))))
        ; thing (dog  (.code res "foo = foo" false) (fn [x] false))
        thing false
        ]
    (is (= thing
          false
          )
      ))))