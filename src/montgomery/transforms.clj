(ns montgomery.transforms
  (:require [montgomery.utils :as utils]))

(defn returns->values
  ([returns] (reductions * 1.0 (map inc returns)))
  ([seed [& returns]] (reductions * seed (map inc returns))))

(defn values->running-maximums
  [values]
  (loop [index 1.0
         result []]
    (if (<= index (count values))
      (recur (inc index)
             (conj result (apply max (take index values))))
      result)))

(defn values->drawdowns
  [values]
  (loop [i 1.0
         result []]
    (if (<= i (count values))
      (recur (inc i)
             (conj result (dec (/ (nth values (dec i))
                                  (apply max (take i values))))))
      result)))

(defn returns->drawdowns
  [returns]
  (-> returns
      returns->values
      values->drawdowns))

(defn values->new-highs-and-drawdowns
  [values]
  (loop [result [{:new-high (first values)}]
         this (second values)
         prev-max (first values)
         values (rest values)]
    (if values
      (recur (conj result {(if (>= this prev-max) :new-high :drawdown) this})
             (second values)
             (max (first values) prev-max)
             (next values))
      result)))

(defn coll->histogram
  [coll n-bins]
  (let [maximum (apply max coll)
        minimum (apply min coll)
        bin-width (/ (- maximum minimum) n-bins)]
    (loop [i 1.0
           result []]
      (if (<= i n-bins)
        (recur (inc i)
               (let [top (* i bin-width)
                     bottom (* (dec i) bin-width)]
                 (conj result {:top top
                               :bottom bottom
                               :count (count (utils/take-between bottom top coll))})))
        result))))
