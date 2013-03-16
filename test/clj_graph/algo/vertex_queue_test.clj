(ns clj-graph.algo.vertex-queue-test
  (:use clojure.test)
  (:use clj-graph.algo.vertex-queue))

(deftest first-in-first-out-queue

  (testing "enqueuing"

    ;; Appending to the queue
    (is (= 1 (count (enqueue FIFO [:a]))))
    (is (= 2 (count (enqueue FIFO [:a :b]))))
    (is (= 3 (count (-> FIFO 
                        (enqueue [:a :b]) 
                        (enqueue [:c])))))

    ;; Collection type before and after is preserved
    (let [type-before (type FIFO)
          type-after (type (enqueue FIFO [:a]))]
      (is (= type-before type-after)) "The type should be preserved")

  (testing "dequeueing"

    ;; The empty queue case
    (is (= [FIFO nil] (dequeue FIFO)) "Dequeuing the empty queue gives the empty queue and nil.")

    ;; dequeuing on FIFO pulls off the first in
    (let [q (-> FIFO
                (enqueue [:a])
                (enqueue [:b]))]
      (is (= [(enqueue FIFO [:a]) :b] (dequeue q)) "Dequeuing returns the oldest element.")))

    ;; Collection type before and after is preserved
    (let [type-before (type FIFO)
          [queue-after _] (dequeue (enqueue FIFO [:a])) 
          type-after (type queue-after)]
      (is (= type-before type-after)) "The type should be preserved")))

(deftest first-in-last-out-queue
  (testing "enqueuing"
    
    ;; Appending to the queue
    (is (= 1 (count (enqueue FILO [:a]))))
    (is (= 2 (count (enqueue FILO [:a :b]))))
    (is (= 3 (count (-> FILO 
                        (enqueue [:a :b]) 
                        (enqueue [:c])))))

    ;; Collection type before and after is preserved
    (let [type-before (type FILO)
          type-after (type (enqueue FILO [:a]))]
      (is (= type-before type-after)) "The type should be preserved"))
  (testing "dequeuing"
    
    ;; The empty queue case
    (is (= [FILO nil] (dequeue FILO)) "Dequeuing the empty queue gives the empty queue and nil.")

    ;; dequeuing on FILO pulls off the last in
    (let [q (-> FILO
                (enqueue [:a])
                (enqueue [:b]))]
      (is (= [(enqueue FILO [:b]) :a] (dequeue q)) "Dequeuing returns the oldest element.")) 

    ;; Collection type before and after is preserved
    (let [type-before (type FILO)
          [queue-after _] (dequeue (enqueue FILO [:a])) 
          type-after (type queue-after)]
      (is (= type-before type-after)) "The type should be preserved")))
