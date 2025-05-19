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
    ;; Start equals end
    (is (= [:1] (shortest-path g :1 :1)))
    ;; Unreachable node should return nil
    (is (nil? (shortest-path g :4 :1)))))

(deftest test-eccentricity
  (let [g {:1 [[:2 1] [:3 4]]
           :2 [[:4 2]]
           :3 [[:4 1]]
           :4 []}]
    (is (= 4 (eccentricity g :1))) ;; From :1 to :4 distance is 4
    (is (= 0 (eccentricity g :4))) ;; :4 has no outgoing edges, eccentricity is 0
    ))

(deftest test-radius-and-diameter
  (let [g {:1 [[:2 1]]
           :2 [[:3 2]]
           :3 [[:4 3]]
           :4 []}]
    (is (= 5 (eccentricity g :2))) ;; 2->3->4 = distance 5
    (is (= 6 (diameter g)))        ;; Max eccentricity
    (is (= 3 (radius g)))))        ;; Min eccentricity, node :3

(deftest test-isolated-node
  (let [g {:1 []
           :2 []
           :3 []}]
    ;; All nodes are isolated, eccentricity should be 0
    (is (= 0 (eccentricity g :1)))
    (is (= 0 (eccentricity g :2)))
    (is (= 0 (eccentricity g :3)))
    (is (= 0 (radius g)))
    (is (= 0 (diameter g)))))

(deftest test-single-node
  (let [g (make-graph 1 0)]
    (is (= {:1 []} g))
    (is (= 0 (eccentricity g :1)))))

(deftest test-dijkstra-negative-weight
  (let [g {:1 [[:2 -1]] :2 []}]
    (is (thrown? Exception (dijkstra g :1)))))
