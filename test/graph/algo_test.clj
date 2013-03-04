(ns graph.algo-test
  (:use clojure.test)
  (:use [graph.algo] :reload) 
  (:use [graph.core] :reload))

(deftest depth-first-searching
  (testing "Search on the empty graph"
    (let [empty-graph (graph)]
      (is (empty? (depth-first-search empty-graph)))))
  (testing "Search on single vertex graph"
    (let [single-vertex-graph (add-node (graph) :a)]
      (is (= [:a] (depth-first-search single-vertex-graph)))
      (is (= [:a] (depth-first-search single-vertex-graph :a)))))
  (testing "Search on connected two vertex graph"
    ;; a <-> b
    (let [two-vertex-graph (-> (graph)
                               (add-node :a)
                               (add-node :b)
                               (add-edge :a :b))]
      (is (= [:a :b] (depth-first-search two-vertex-graph :a)))
      (is (= [:b :a] (depth-first-search two-vertex-graph :b)))))
  (testing "Search on connected three vertex line graph"
    ;; a <-> b <-> c
    (let [three-vertex-graph (-> (graph)
                                 (add-node :a)
                                 (add-node :b)
                                 (add-node :c)
                                 (add-edge :a :b)
                                 (add-edge :b :c))]
      (is (= [:a :b :c] (depth-first-search three-vertex-graph :a)))
      (is (= [:c :b :a] (depth-first-search three-vertex-graph :c)))
      (is (= [:b :a :c] (depth-first-search three-vertex-graph :b)))))
  (testing "Search on interconnected four vertex graph"
    ;; a <-> b <-> c
    ;;       ^     ^
    ;;       |     |
    ;;       V     V
    ;;       d <-> e
    (let [interconnect-graph (-> (graph)
                                 (add-node :a)
                                 (add-node :b)
                                 (add-node :c)
                                 (add-node :d)
                                 (add-node :e)
                                 (add-edge :a :b)
                                 (add-edge :b :c)
                                 (add-edge :b :d)
                                 (add-edge :c :e)
                                 (add-edge :d :e))]
      (is (= [:a :b :c :e :d] (depth-first-search interconnect-graph :a))))))
