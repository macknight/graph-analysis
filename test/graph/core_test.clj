(ns graph.core-test
  (:require [clojure.test :refer :all]
            [graph.core :refer :all]))

(deftest test-make-graph
  (let [g (make-graph 5 6)]
    (is (= 5 (count (keys g))))
    (is (every? vector? (mapcat identity (vals g))))))

(deftest test-shortest-path
  (let [g {:1 [[:2 1] [:3 4]]
           :2 [[:4 2]]
           :3 [[:4 1]]
           :4 []}]
    (is (= [:1 :2 :4] (shortest-path g :1 :4)))
    ;; 起点=终点情况
    (is (= [:1] (shortest-path g :1 :1)))
    ;; 不可达节点路径应返回 nil
    (is (nil? (shortest-path g :4 :1)))))

(deftest test-eccentricity
  (let [g {:1 [[:2 1] [:3 4]]
           :2 [[:4 2]]
           :3 [[:4 1]]
           :4 []}]
    (is (= 4 (eccentricity g :1))) ;; 这里改成4
    (is (= 0 (eccentricity g :4)))))

(deftest test-radius-and-diameter
  (let [g {:1 [[:2 1]]
           :2 [[:3 2]]
           :3 [[:4 3]]
           :4 []}]
    (is (= 5 (eccentricity g :2))) ;; 2->3->4 距离5
    (is (= 6 (diameter g)))          ;; 最大eccentricity
    (is (= 0 (radius g)))))          ;; 最小eccentricity（孤立节点4）

(deftest test-isolated-node
  (let [g {:1 []
           :2 []
           :3 []}]
    (is (= 0 (radius g)))
    (is (= 0 (diameter g)))))
