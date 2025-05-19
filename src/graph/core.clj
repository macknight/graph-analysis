(ns graph.core
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn make-graph
  "Generate a directed weighted graph with `n` nodes and approximately `s` edges.
   Ensures connectivity: generates a spanning tree first, then adds extra edges.
   Return format: {node [[neighbor weight] ...] ...}"
  [n s]
  {:pre [(> n 0) (>= s 0)]}
  (when (< s (dec n))
    (throw (ex-info "Number of edges `s` is insufficient to ensure connectivity; must be at least n-1" {:n n :s s})))
  (let [nodes (mapv #(keyword (str %)) (range 1 (inc n)))]
    ;; First, generate a spanning tree to ensure all nodes are connected
    (loop [edges #{}
           connected [(first nodes)]
           remaining (vec (rest nodes))]
      (if (empty? remaining)
        ;; After tree generation, add extra edges
        (let [all-edges (for [a nodes, b nodes :when (not= a b)] [a b])
              existing-edges edges
              possible-edges (remove existing-edges all-edges)
              extra-count (- s (count edges))
              extra-edges (take extra-count (shuffle possible-edges))
              final-edges (vec (concat edges extra-edges))
              weights (repeatedly (count final-edges) #(inc (rand-int 10)))
              edge-map (reduce (fn [m [[from to] w]]
                                 (update m from #(conj (or % []) [to w])))
                               {} (map vector final-edges weights))]
          ;; Ensure every node has an entry, even if it has no outgoing edges
          (reduce (fn [m node] (update m node #(or % []))) edge-map nodes))
        ;; Continue to expand the spanning tree
        (let [from (rand-nth connected)
              to (first remaining)
              new-edges (conj edges [from to])]
          (recur new-edges (conj connected to) (subvec remaining 1)))))))

(defn dijkstra
  "Dijkstra's algorithm: computes the shortest distance from `start` to all other nodes.
   Time complexity: O(|E| + |V|log|V|)
   Space complexity: O(|V|)
   Note: does not support negative weights; throws exception if found"
  [graph start]
  ;; Check for negative weights
  (when (some (fn [[_ neighbors]]
                (some (fn [[_ w]] (< w 0)) neighbors))
              graph)
    (throw (ex-info "Dijkstra's algorithm does not support negative weights" {})))
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

(defn shortest-path
  "Returns the shortest path from `start` to `end` as a sequence of nodes (inclusive).
   Example: (shortest-path g :1 :3) => [:1 :2 :3]
   Notes:
   - Returns a single-element list when start == end
   - Returns nil if no path exists
   - The path is ordered from start to end"
  [graph start end]
  (let [[dist prev] (dijkstra graph start)]
    (loop [v end, path []]
      (cond
        (nil? v) nil
        (= v start) (reverse (conj path v))
        :else (recur (get prev v) (conj path v))))))

(defn eccentricity
  "Eccentricity of a node: the longest shortest path from the node to any reachable node.
   For isolated or sink nodes, returns 0"
  [graph node]
  (let [[dist _] (dijkstra graph node)
        other-dists (remove #(= 0 %) (vals dist))
        reachable-dists (filter #(not= Double/POSITIVE_INFINITY %) other-dists)]
    (if (seq reachable-dists)
      (apply max reachable-dists)
      0)))

(defn radius
  "Graph radius: the smallest eccentricity among all nodes (ignoring unreachable or isolated nodes with 0)"
  [graph]
  (let [eccs (map #(eccentricity graph %) (keys graph))
        valid-eccs (filter #(and (> % 0) (not= % Double/POSITIVE_INFINITY)) eccs)]
    (if (seq valid-eccs)
      (apply min valid-eccs)
      0)))

(defn diameter
  "Graph diameter: the largest eccentricity among all nodes (ignoring unreachable or isolated nodes with 0)"
  [graph]
  (let [eccs (map #(eccentricity graph %) (keys graph))
        valid-eccs (filter #(and (> % 0) (not= % Double/POSITIVE_INFINITY)) eccs)]
    (if (seq valid-eccs)
      (apply max valid-eccs)
      0)))
