(ns sdf.core-test
  (:require [clojure.test :refer :all]
            [sdf.core :refer :all]))

(deftest core-test
  (testing "Core is just core"
    (is (= 1 1))))
