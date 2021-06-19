(ns sdf.dsl-test
    (:require [clojure.test :refer :all]
     [sdf.dsl :refer :all]))

(deftest dsl-test
  (testing "DSL TEST"
           (is (= 1 1))))

(deftest spread-combine-test
  (testing "Valid pattern"
           (let [f (spread-combine list
                                   (fn [x y z] (list x y z))
                                   (fn [v w] (list v w)))]
             (is
               (= (f 'a 'b 'c 'd 'e)
                 '((a b c) (d e))))
             (is
               (= (get-arity f)
                  5))))

  (testing "Arity check"
           (let [f (spread-combine list
                                   (fn [x y z] (list x y z))
                                   (fn [v w] (list v w)))]
             (try
               (f 'a 'b 'c 'd) (is false) 
               (catch java.lang.AssertionError e (is true))))))
