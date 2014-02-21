(ns montgomery.transforms
  (:require [montgomery.utils :as utils]))

(defn returns->values
  "([returns] [seed [& returns]])
  ----------------------------------------------------------------
  Convert a series of returns into asset values.  If no initial
  value is supplied, the 'seed value' will default to 100.00."
  ([returns] (reductions * 100.0 (map inc returns)))
  ([seed [& returns]] (reductions * seed (map inc returns))))

(defn values->running-maximums
  "([values])
  ----------------------------------------------------------------
  Calculates the running maximum from a series of asset values.
  Assumes the series is ordered from oldest to newest."
  [values]
  (loop [index 1.0
         result []]
    (if (<= index (count values))
      (recur (inc index)
             (conj result (apply max (take index values))))
      result)))

(defn values->drawdowns
  "([values])
  ----------------------------------------------------------------
  Calculates the percent underwater, or drawdown, defined as the
  difference between the current asset value less the maximum of
  all preceding asset values divided by the maximum of all
  preceding asset values."
  [values]
  (loop [i 1.0
         result []]
    (if (<= i (count values))
      (recur (inc i)
             (conj result (dec (/ (nth values (dec i))
                                  (apply max (take i values))))))
      result)))

(defn returns->drawdowns
  "([returns])
  ----------------------------------------------------------------
  Pipes a series of returns through returns->values and then
  through values->drawdowns."
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

(defn- take-while-underwater-and-categorize
  "([values])
  ----------------------------------------------------------------
  The goal of this function is first to determine if there is
  a drawdown starting with (second values).  If there is not a
  drawdown present, then we know (first values) is a :new-high.
  If there is, take from values where less than the prev high
  water mark, (first values); these are termed 'underwater-values.'
  From these 'underwater-values' find the minimum; this is our
  local minimum.  Values preceding and including this point are
  considered :drawdown. Values after are deemed :recovery."
  [values]
  (let [underwater-values (take-while (fn [x] (< x (first values))) (rest values))]
    (if (seq underwater-values)
      (let [local-min-index (.indexOf underwater-values (apply min underwater-values))]
        (loop [result []
               i 0]
          (if (< i (count underwater-values))
            (recur (conj result {(if (<= i local-min-index) :drawdown :recovery)
                                 (nth underwater-values i)})
                   (inc i))
            result)))
      [{:new-high (first values)}])))

(defn values->new-highs-recoveries-and-drawdowns
  [values]
  (loop [result [{:new-high (first values)}]
         this (second values)
         prev-max (first values)
         my-values values]
    (if (seq my-values)
      (let [categorized-values (take-while-underwater-and-categorize my-values)
            n (count categorized-values)]
        (recur (into result categorized-values)
               (nth my-values (min (dec (count my-values)) (inc n)))
               (max prev-max
                    (apply max (flatten (map (fn [x] (vals x))
                                             categorized-values))))
               (nthrest my-values (inc n))))
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
