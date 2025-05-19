# Graph Analysis in Clojure

This project implements and tests a set of core graph algorithms using Clojure. It includes graph generation, Dijkstra's shortest path algorithm, eccentricity, radius, and diameter computations.

## 📁 Files Overview

### `src/graph/core.clj`

This file contains the main graph-related functions:

- **`make-graph [n s]`**  
  Generates a **connected directed weighted graph** with `n` nodes and approximately `s` edges. Ensures connectivity by first constructing a spanning tree, then adds extra edges randomly. All edge weights are randomly chosen positive integers from 1 to 10.

- **`dijkstra [graph start]`**  
  Implements **Dijkstra’s algorithm** to compute the shortest distances from a `start` node to all reachable nodes. Negative weights are not supported.

- **`shortest-path [graph start end]`**  
  Returns the **shortest path (node sequence)** from `start` to `end`. If no path exists, returns `nil`.

- **`eccentricity [graph node]`**  
  Calculates the **eccentricity** of a node, i.e., the longest shortest path from that node. Returns `0` for isolated nodes.

- **`radius [graph]`**  
  Returns the **radius** of the graph — the minimum eccentricity among all nodes (ignoring unreachable or isolated nodes).

- **`diameter [graph]`**  
  Returns the **diameter** of the graph — the maximum eccentricity among all nodes.

---

### `test/graph/core_test.clj`

This file contains test cases for all core functions, including:

- Graph structure validation (`make-graph`)
- Shortest path correctness
- Eccentricity, radius, and diameter computations
- Handling of edge cases like isolated nodes, single-node graphs, and negative weights

All tests can be run using:

```bash
lein test
