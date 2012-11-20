(ns rect.core-test
  (:use clojure.test
        rect.core))

(def X 'X)
(def _ '_)
(def rect
  [
   [X X X X X X]
   [X X X X X X]
   [X _ X X X X]
   [X X X X X X]
   [X X X X _ X]
   ])

(defn next-largest-child [[x y]]
  (sort [(dec (max x y)) (min x y)]))

(deftest largest-child-tests
  (is (= (next-largest-child [4 6])
         [4 5]))
  (is (= (next-largest-child [4 5])
         [4 4]))
  (is (= (next-largest-child [4 2])
         [2 3])))

(defn get-child-rect [parent-rect
                      [top-left-x top-left-y]
                      [child-height child-width]]
  (map (fn [row] (->> row (drop top-left-x) (take child-height)))
       (->> parent-rect
            (drop top-left-y)
            (take child-width))))

(deftest get-child-rect-tests
  ;; start from 0
  (is (= (get-child-rect rect [1 2] [4 3])
         [[_ X X X]
          [X X X X]
          [X X X _]
          ])))

'(defn gen-full-rect [width height]
  (repeat height (repeat width X)))

'(deftest gen-full-rect-tests
  (is (= (gen-full-rect 3 4)
         [[X X X]
          [X X X]
          [X X X]
          [X X X]])))

(defn full? [rect]
  (apply = (cons true
                 (map (fn full-row? [row] (apply = (cons X row)))
                      rect))))

(deftest full-tests
  (is (= (full? [[_ X X X]
                 [X X X X]
                 [X X X _]
                 ])
         false))
  (is (= (full?
          [[X X X]
           [X X X]
           [X X X]
           [X X X]])
         true)))

(defn max-top-left [[parent-x parent-y] [width height]]
  [(- parent-x width)
   (- parent-y height)])

(deftest max-top-left-test
  (is (= (max-top-left [6 4] [5 3])
         [1 1])))

(defn check-child [parent-rect child-size]
  (let [parent-size [(count (first parent-rect)) (count parent-rect)]]
    (first (for [x [0 (first (max-top-left parent-size child-size))]
                 y [0 (second (max-top-left parent-size child-size))]
                 :when (full? (get-child-rect parent-rect
                                              [x y]
                                              child-size))]
             {:top-left [x y]
              :child-size child-size}))))

(deftest check-child-tests
  (is (= (check-child rect [2 2])
         {:top-left [0 0], :child-size [2 2]}))
  (is (= (check-child rect [2 3])
         {:top-left [4 0], :child-size [2 3]}))
  (is (= (check-child rect [1 1])
         {:top-left [0 0], :child-size [1 1]})))

(defn get-size [rect]
  [(count (first rect)) (count rect)])

(deftest get-size-tests
  (is (= (get-size rect)
         [6 5])))

(defn find-largest-full-child [parent-rect]
  (loop [child-size (get-size parent-rect)]
    (or (check-child parent-rect child-size)
        (check-child parent-rect (reverse child-size))
        (recur (next-largest-child child-size))
        )))

(deftest find-largest-full-child-test
  (is (= (find-largest-full-child rect)
         '{:top-left [2 0], :child-size (4 4)})))