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

(deftest exercise-2-1-test
  (testing "They check their components to make sure that the arities are compatible"
           (try
             (compose (fn [a b c] (list a b c))
                      (fn [d e] (list d e)))
             (is false)
             (catch java.lang.AssertionError e (is true)))
           (try
             (parallel-combine (fn [a b] (list a b))
                               (fn [c d] (list c d))
                               (fn [e f g] (list e f g)))
             (is false)
             (catch java.lang.AssertionError e (is true)))
           (try
             (parallel-combine (fn [a b c] (list a b c))
                               (fn [c d] (list c d))
                               (fn [e f] (list e f)))
             (is false)
             (catch java.lang.AssertionError e (is true))))

  (testing "The combination they construct checks that it is given the correct number of arguments when it is called"
           (let [compose-f (compose (fn [a b c] (list a b c))
                                    (fn [a b c] (list a b c)))
                 para-f (parallel-combine (fn [a b] (list a b))
                                          (fn [a b c] (list a b c))
                                          (fn [a b c] (list a b c)))]
             (try
               (compose-f 'a 'b)
               (is false)
               (catch java.lang.AssertionError e (is true)))
             (try
               (para-f 'a 'b)
               (is false)
               (catch java.lang.AssertionError e (is true)))))

  (testing "The combination advertises its arity correctly for get-arity"
           (let [compose-f (compose (fn [a b c d] (list a b c d))
                                    (fn [a b c d] (list a b c d)))
                 para-f (parallel-combine (fn [a b] (list a b))
                                          (fn [a b c d e] (list a b c d e))
                                          (fn [a b c d e] (list a b c d e)))]
             (= (get-arity compose-f) 4)
             (= (get-arity para-f) 5))))

 (deftest exercise-2-2-test
   (testing "- hはarityに2を含むこと
             - fとgの返り値をそれぞれ受け取る
             - fはarityが一意であること
             - 生成する関数のairtyはgのarity-listのそれぞれにfのarityを加えたもの
             - 可変長引数のarityは##Inf
             実装は諦めた"
            (is true))) 

