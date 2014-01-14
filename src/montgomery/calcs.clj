(ns montgomery.calcs
  (:require [montgomery.utils :as utils]
            [clojure.math.numeric-tower :as math]))

(defn arithmetic-avg
  [coll]
  (float (/ (utils/sum coll)
            (count coll))))

(defn geometric-avg
  [coll]
  (float (math/expt (utils/product coll)
                    (utils/one-over (count coll)))))

(defn squared-deviations
  [coll]
  (let [mean (arithmetic-avg coll)]
    (map (fn [x] (utils/square (- x mean)))
         coll))))

(defn variance
  [coll]
  (/ (utils/sum (squared-deviations coll))
     (utile/one-over (count coll))))

(defn sample-variance
  [coll]
  (/ (utils/sum (squared-deviations coll))
     (utile/one-over (dec (count coll)))))

(defn standard-deviation
  [coll]
  math/sqrt ()

(defn sample-standard-deviation
  [coll]
  (math/sqrt (sample-variance coll)))


