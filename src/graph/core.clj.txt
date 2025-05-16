(ns graph.core
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn make-graph [n s]
  ;; 保证连通性的生成图：先构造生成树，再加随机边
  (let [nodes (mapv #(keyword (str %)) (range 1 (inc n)))
        edges (loop [edges #{},
                     remaining (vec (rest nodes))]
                (if (empty? remaining)
                  edges
                  (let [from (rand-nth (vec nodes))
                        to (first remaining)]
                    (recur (conj edges [from to]) (rest remaining)))))
        all-edges (for [a nodes, b nodes :when (not= a b)] [a b])
        remaining-edges (filter #(not (contains? edges %)) all-edges)
        extra-edges (take (- s (count edges)) (shuffle remaining-edges))
        final-edges (vec (concat edges extra-edges))
        weights (repeatedly (count final-edges) #(inc (rand-int 10)))
        edge-map (reduce (fn [m [[from to] w]]
                           (update m from #(conj (or % []) [to w])))
                         {} (map vector final-edges weights))]
    ;; 确保每个节点都有entry，即使没有出边
    (reduce (fn [m node] (update m node #(or % []))) edge-map nodes)))

(defn dijkstra [graph start]
  (let [nodes (set (concat (keys graph) (map first (mapcat identity (vals graph)))))
        dist (into {} (map #(vector % Double/POSITIVE_INFINITY) nodes))
        dist (assoc dist start 0)
        queue (priority-map start 0)]
    (loop [dist dist
           prev {}
           queue queue]
      (if (empty? queue)
        [dist prev]
        (let [[node node-dist] (peek queue)
              queue (pop queue)
              neighbors (get graph node [])]
          (let [[dist' prev' queue']
                (reduce (fn [[d p q] [n w]]
                          (let [alt (+ (d node) w)]
                            (if (< alt (d n))
                              [(assoc d n alt) (assoc p n node) (assoc q n alt)]
                              [d p q])))
                        [dist prev queue]
                        neighbors)]
            (recur dist' prev' queue')))))))

(defn shortest-path [graph start end]
  (let [[dist prev] (dijkstra graph start)]
    (loop [v end, path []]
      (if (nil? v)
        nil
        (if (= v start)
          (reverse (conj path v))
          (recur (get prev v) (conj path v)))))))

(defn eccentricity [graph node]
  (let [[dist _] (dijkstra graph node)
        reachable-dists (filter #(not= Double/POSITIVE_INFINITY %) (vals dist))]
    (if (seq reachable-dists)
      (apply max reachable-dists)
      Double/POSITIVE_INFINITY)))

(defn radius [graph]
  (let [eccs (map #(eccentricity graph %) (keys graph))
        finite-eccs (filter #(not= Double/POSITIVE_INFINITY %) eccs)]
    (if (seq finite-eccs)
      (apply min finite-eccs)
      Double/POSITIVE_INFINITY)))

(defn diameter [graph]
  (let [eccs (map #(eccentricity graph %) (keys graph))
        finite-eccs (filter #(not= Double/POSITIVE_INFINITY %) eccs)]
    (if (seq finite-eccs)
      (apply max finite-eccs)
      Double/POSITIVE_INFINITY)))
