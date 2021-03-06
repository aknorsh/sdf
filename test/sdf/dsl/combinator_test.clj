(ns sdf.dsl.combinator_test
    (:require [clojure.test :refer :all]
     [sdf.dsl.combinator :refer :all]))

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
   (testing "- h???arity???2???????????????
             - f???g???????????????????????????????????????
             - f???arity????????????????????????
             - ?????????????????????airty???g???arity-list??????????????????f???arity??????????????????
             - ??????????????????arity???##Inf
             ??????????????????"
            (is true))) 

(deftest spread-apply-test
  (testing "Valid case"
           (let [sa (spread-apply (fn [x y] [(+ x y) (- x y)])
                                  (fn [x y] [(* x y) (/ x y) x y]))]
             (is (= (sa 1 2 3 4)
                    '(3 -1 12 3/4 3 4))))))

(deftest multi-compose-test
  (testing "Valid case"
           (let [mc (multi-compose (fn [x y z] [(+ x y z)])
                                   (fn [x y] [(+ x y) x y]))]
             (is (= (mc 1 2)
                    '(6))))))

(deftest composed-spread-combine-test
  (testing "Valid case"
           (let [f (composed-spread-combine 
                                   (fn [x y] [(list x y)])
                                   (fn [x y z] [(list x y z)] )
                                   (fn [v w] [(list v w)]))]
             (is
               (= (f 'a 'b 'c 'd 'e)
                 '(((a b c) (d e))) ))
             (is
               (= (get-arity f)
                  5)))
           (let [g (composed-spread-combine
                     (fn [x y z a b c] [(+ x y) (+ z a) (+ b c)])
                     (fn [x] [(* x 2) (- (* x 3) 1)])
                     (fn [x y] [(* x y) (+ x y) x y]))]
             (is
               (= (g 1 2 3) '[4 11 5])))))

(deftest exercise-2-3-test
  (testing "composed-parallel-combine Valid Case"
           (let [f (composed-parallel-combine 
                                   (fn [x y z] [(list x y z)])
                                   (fn [x y z] [(list x y z)] )
                                   (fn [x y z] [(list x ) (list y z)]))]
             (is
               (= (f 'a 'b 'c)
                 '(((a b c) (a) (b c))) ))
             (is
               (= (get-arity f)
                  3)))
           (let [g (composed-parallel-combine
                     (fn [x y z a b c] [(+ x y) (+ z a) (+ b c)])
                     (fn [x y] [(* x 2) (- (* y 3) 1)])
                     (fn [x y] [(* x y) (+ x y) x y]))]
             (is
               (= (g 1 2) '[7 5 3]))
             (is (= (get-arity g) 2)))))

(deftest remove-nth-test
  (testing "ValidCase"
           (is (= (remove-nth 0 [1 2 3 4 5])
              [2 3 4 5]))
           (is (= (remove-nth 1 [1 2 3 4 5])
              [1 3 4 5]))
           (is (= (remove-nth 2 [1 2 3 4 5])
              [1 2 4 5]))
           (is (= (remove-nth 3 [1 2 3 4 5])
              [1 2 3 5]))
           (is (= (remove-nth 4 [1 2 3 4 5])
              [1 2 3 4]))
           (is (= (remove-nth 5 [1 2 3 4 5])
              [1 2 3 4 5]))))

(deftest discard-argument-test
  (testing "Valid case"
           (is (= (((discard-argument 2) (fn [x y z] (list 'foo x y z)))
               'a 'b 'c 'd)
              '(foo a b d)))))

(deftest curry-argument-test
  (testing "Valid case"
           (is (= (((curry-argument 1 0 1 2) (fn [x y z w] [x y z w]))
              1)
              [0 1 1 2]))
           (is (= (((curry-argument 2 'a 'b 'c) (fn [x y z w] (list 'foo x y z w)))
               'd)
              '(foo a b d c)))))

(deftest permute-arguments-test
  (testing "Valid case"
           (is (= (((permute-arguments 1 2 0 3) (fn [x y z w] (list 'foo x y z w)))
               'a 'b 'c 'd)
              '(foo b c a d)))))

(deftest exercise-2-4
  (testing "multi-discard-argument Valid case"
           (is (= (((multi-discard-argument 2) (fn [x y z] ['foo x y z]))
               'a 'b 'c 'd)
              ['foo 'a 'b 'd])))
  (testing "multi-discard-argument Valid case"
           (is (= (((multi-curry-argument 1 0 1 2) (fn [x y z w] [x y z w]))
               1)
              [0 1 1 2]))
           (is (= (((multi-curry-argument 2 'a 'b 'c) (fn [x y z w] ['foo x y z w]))
               'd)
              ['foo 'a 'b 'd 'c])))
  (testing "multi-permute-arguments Valid case"
           (is (= (((multi-permute-arguments 1 2 0 3) (fn [x y z w] ['foo x y z w]))
               'a 'b 'c 'd)
              ['foo 'b 'c 'a 'd]))))

(deftest remove-nths-test
  (testing "Valid"
           (is 
             (= (remove-nths [1 2] [1 2 3 4 5])
                [1 4 5]))))

(deftest insert-nths-test
  (testing "Valid"
           (is
             (= (insert-nths [1 3 5] ['a 'b 'c] [1 2 3 4 5])
                [1 'a 2 'b 3 'c 4 5]))))

(deftest exercise-2-5-a-test
  (testing "discard-arguments"
           (is
            (= (((discard-argument 2) (fn [x y z] ['foo x y z]))
               'a 'b 'c 'd)
               (((discard-arguments 2) (fn [x y z] ['foo x y z]))
               'a 'b 'c 'd)))
           (is
             (= '(foo a d)
               (((discard-arguments 1 2) (fn [x y] ['foo x y]))
               'a 'b 'c 'd))))
  (testing "curry-arguments"
           (is
             (= (((curry-argument 1 0 1 2) (fn [x y z w] [x y z w]))
              1)
                (((curry-arguments [1] 0 1 2) (fn [x y z w] [x y z w]))
              1)))
           (is
             (= (((curry-argument 2 'a 'b 'c) (fn [x y z w] (list 'foo x y z w)))
               'd)
                  (((curry-arguments [2] 'a 'b 'c) (fn [x y z w] (list 'foo x y z w)))
               'd)))))

(deftest exercise-2-5-b-test
  (testing "fill-with-default"
           (is (= (fill-with-default 1 [2 nil 3 nil 4])
                  '(2 1 3 1 4))))
  (testing "force-default-arguments"
           (is (= (((force-default-arguments 'x) (fn [x y z] (list x y z)))
                   'a nil 'c)
                  '(a x c))))
  (testing "default-arguments"
           (is (= (((default-arguments) (fn [x y z] (list x y z)))
                   'x 'a nil 'c)
                  '(a x c)))))

(deftest exercise-2-5-c-test
  (testing "serial-compose Valid"
           (let [f (serial-compose (fn [x] (+ x 1)) (fn [x] (* x 2)) (fn [x] (+ x 3)))]
             (is (= (f 5)
                    17)))))
