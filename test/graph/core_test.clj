(ns graph.core-test
  (:require [clojure.test :refer :all]
            [graph.core :refer :all]))

(deftest test-make-graph
  (testing "生成图节点数和边结构"
    (let [g (make-graph 5 6)]
      (is (= 5 (count (keys g))))
      (is (every? vector? (mapcat identity (vals g))))))

  (testing "边数少于最小边数时自动调整"
    (with-out-str ;; 屏蔽打印输出
      (let [g (make-graph 5 3)] ;; 3 < 5-1=4 应自动调整为4边
        (is (= 5 (count (keys g))))))))

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
    (is (= 4 (eccentricity g :1))) ;; 从 :1 到最远节点 :4 距离4
    (is (= Double/POSITIVE_INFINITY (eccentricity g :4))) ;; :4 无出边且不可达其他节点，ecc=∞
    ))

(deftest test-radius-and-diameter
  (let [g {:1 [[:2 1]]
           :2 [[:3 2]]
           :3 [[:4 3]]
           :4 []}]
    (is (= 5 (eccentricity g :2))) ;; 2->3->4 距离5
    (is (= 6 (diameter g)))         ;; 最大eccentricity
    (is (= 3 (radius g)))))         ;; 最小eccentricity，即 :3 的离心率

(deftest test-isolated-node
  (let [g {:1 []
           :2 []
           :3 []}]
    ;; 所有节点都孤立，离心率应该是∞
    (is (= Double/POSITIVE_INFINITY (eccentricity g :1)))
    (is (= Double/POSITIVE_INFINITY (eccentricity g :2)))
    (is (= Double/POSITIVE_INFINITY (eccentricity g :3)))
    (is (= Double/POSITIVE_INFINITY (radius g)))
    (is (= Double/POSITIVE_INFINITY (diameter g)))
    ))

(deftest test-dijkstra-negative-weight
  (testing "负权边应抛异常"
    (let [g {:1 [[:2 -1]] :2 []}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"不支持负权边"
                            (dijkstra g :1))))))
