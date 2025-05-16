(ns graph.core
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn make-graph
  "生成一个有向加权图，节点数为 n，边数约为 s。
   保证连通性：先生成生成树，再添加额外边。
   返回格式：{node [[neighbor weight] ...] ...}"
  [n s]
  {:pre [(> n 0) (>= s 0)]}
  (when (< s (dec n))
    (throw (ex-info "边数 s 不足以保证图连通，至少为 n-1" {:n n :s s})))
  (let [nodes (mapv #(keyword (str %)) (range 1 (inc n)))]
    ;; 先生成连通树，确保所有节点被连接
    (loop [edges #{}
           connected [(first nodes)]
           remaining (vec (rest nodes))]
      (if (empty? remaining)
        ;; 生成树完成后，补充剩余边
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
          ;; 确保所有节点都有 entry，即使无出边也返回空列表
          (reduce (fn [m node] (update m node #(or % []))) edge-map nodes))
        ;; 继续扩展生成树
        (let [from (rand-nth connected)
              to (first remaining)
              new-edges (conj edges [from to])]
          (recur new-edges (conj connected to) (subvec remaining 1)))))))

(defn dijkstra
  "Dijkstra算法，计算从start节点到所有其他节点的最短距离
   时间复杂度: O(|E| + |V|log|V|)
   空间复杂度: O(|V|)
   注意: 不支持负权边，遇负权抛异常"
  [graph start]
  ;; 先检查负权边
  (when (some (fn [[_ neighbors]]
                (some (fn [[_ w]] (< w 0)) neighbors))
              graph)
    (throw (ex-info "Dijkstra算法不支持负权边" {})))
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
  "返回从 start 到 end 的最短路径节点序列（包含两端节点）。
   示例: (shortest-path g :1 :3) => [:1 :2 :3]
   注意:
   - 当 start == end 时返回单元素列表
   - 无路径时返回 nil
   - 路径按从 start 到 end 的顺序排列"
  [graph start end]
  (let [[dist prev] (dijkstra graph start)]
    (loop [v end, path []]
      (cond
        (nil? v) nil
        (= v start) (reverse (conj path v))
        :else (recur (get prev v) (conj path v))))))

(defn eccentricity
  "节点node的离心率：从node出发能到达的最远距离
   孤立节点或无出边时，返回0"
  [graph node]
  (let [[dist _] (dijkstra graph node)
        other-dists (remove #(= 0 %) (vals dist))
        reachable-dists (filter #(not= Double/POSITIVE_INFINITY %) other-dists)]
    (if (seq reachable-dists)
      (apply max reachable-dists)
      0)))

(defn radius
  "图的半径：所有节点离心率的最小值（忽略不可达节点和孤立节点0）"
  [graph]
  (let [eccs (map #(eccentricity graph %) (keys graph))
        valid-eccs (filter #(and (> % 0) (not= % Double/POSITIVE_INFINITY)) eccs)]
    (if (seq valid-eccs)
      (apply min valid-eccs)
      0)))

(defn diameter
  "图的直径：所有节点离心率的最大值（忽略不可达节点和孤立节点0）"
  [graph]
  (let [eccs (map #(eccentricity graph %) (keys graph))
        valid-eccs (filter #(and (> % 0) (not= % Double/POSITIVE_INFINITY)) eccs)]
    (if (seq valid-eccs)
      (apply max valid-eccs)
      0)))
