(ns graph.algo-test
  (:use clojure.test
        graph.algo
        graph.core))

(deftest depth-first-searching
  (testing "Search on the empty graph"
    (let [empty-graph (graph)]
      (is (empty? (depth-first-search empty-graph)))))
  (testing "Search on single vertex graph"
    (let [single-vertex-graph (add-node (graph) :a)]
      (is (= [:a] (depth-first-search single-vertex-graph)))
      (is (= [:a] (depth-first-search single-vertex-graph :a)))))
  (testing "Search on connected two vertex graph"
    (let [two-vertex-graph (-> (graph)
                               (add-node :a)
                               (add-node :b)
                               (add-edge :a :b))]
      (is (= [:a :b] (depth-first-search two-vertex-graph :a)))
      (is (= [:b :a] (depth-first-search two-vertex-graph :b))))))
