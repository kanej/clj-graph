(ns clj-graph.algo.edge-collection-test
  (:refer-clojure :exclude [inc])
  (:use clojure.test)
  (:use [clj-graph.algo.edge-collection] :reload))

(deftest edge-matrixes 
  (testing "Incrementing the matrix"
    (let [empty-array (edge-matrix)]
      (is (= [[nil]] (inc empty-array)))
      (is (= [[nil nil] [nil nil]] (inc (inc empty-array))))))
  (testing "Setting edges in the matrix"
    (let [matrix (-> (edge-matrix)
                     (inc)
                     (inc))]
      ;; Standard set
      (is (= [[:a  nil] [nil nil]] (update matrix 0 0 :a)))
      (is (= [[nil :b ] [nil nil]] (update matrix 0 1 :b)))
      (is (= [[nil nil] [:c  nil]] (update matrix 1 0 :c)))
      (is (= [[nil nil] [nil :d ]] (update matrix 1 1 :d)))
      ;; TODO
      ;; Setting with co-ordinates outside the boundaries
      ;; has no effect
      ;;(is (= [[nil nil] [nil nil]] (update matrix 2 1 :e)))
      ))
  (testing "Lookup in the matrix"
    (let [matrix (-> (edge-matrix)
                     (inc)
                     (inc)
                     (update 0 1 :b))]
      (is (= :b (lookup matrix 0 1)))
      (is (= nil (lookup matrix 0 0)))
      (is (= nil (lookup matrix 12 12)) "Returns nil if co-ords out of bounds"))))
