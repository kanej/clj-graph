(ns clj-graph.algo.traversal-test
  (:use clojure.test)
  (:use [clj-graph.algo.traversal] :reload) 
  (:use [clj-graph.core] :reload))

(def empty-graph (graph))

(def single-vertex-graph (add-vertex (graph) :a))

;; a <-> b
(def two-vertex-graph 
  (-> (graph)
      (add-vertex :a)
      (add-vertex :b)
      (add-edge :a :b)))

;; a <-> b <-> c
(def three-vertex-graph 
  (-> (graph)
      (add-vertex :a)
      (add-vertex :b)
      (add-vertex :c)
      (add-edge :a :b)
      (add-edge :b :c)))

;; a <-> b <-> c
;;       ^     ^
;;       |     |
;;       V     V
;;       d <-> e
(def interconnected-graph 
  (-> (graph)
      (add-vertex :a)
      (add-vertex :b)
      (add-vertex :c)
      (add-vertex :d)
      (add-vertex :e)
      (add-edge :a :b)
      (add-edge :b :c)
      (add-edge :b :d)
      (add-edge :c :e)
      (add-edge :d :e)))

(deftest depth-first-searching
  (testing "Search on the empty graph"
    (is (empty? (depth-first-search empty-graph :nonexistant))))
  (testing "Search on single vertex graph"
    (is (= [:a] (depth-first-search single-vertex-graph :a))))
  (testing "Search on connected two vertex graph"
    (is (= [:a :b] (depth-first-search two-vertex-graph :a)))
    (is (= [:b :a] (depth-first-search two-vertex-graph :b))))
  (testing "Search on connected three vertex line graph"
    (is (= [:a :b :c] (depth-first-search three-vertex-graph :a)))
    (is (= [:c :b :a] (depth-first-search three-vertex-graph :c)))
    (is (= [:b :a :c] (depth-first-search three-vertex-graph :b))))
  (testing "Search on interconnected four vertex graph"
    (is (= [:a :b :c :e :d] (depth-first-search interconnected-graph :a)))))

(deftest breadth-first-searching
  (testing "Search on the empty graph"
    (is (empty? (breadth-first-search empty-graph :nonexistant))))
  (testing "Search on single vertex graph"
    (is (= [:a] (breadth-first-search single-vertex-graph :a))))
  (testing "Search on connected two vertex graph"
    (is (= [:a :b] (breadth-first-search two-vertex-graph :a)))
    (is (= [:b :a] (breadth-first-search two-vertex-graph :b))))
  (testing "Search on connected three vertex line graph"
    (is (= [:a :b :c] (breadth-first-search three-vertex-graph :a)))
    (is (= [:c :b :a] (breadth-first-search three-vertex-graph :c)))
    (is (= [:b :a :c] (breadth-first-search three-vertex-graph :b))))
  (testing "Search on interconnected four vertex graph"
    (is (= [:a :b :c :d :e] (breadth-first-search interconnected-graph :a)))))
