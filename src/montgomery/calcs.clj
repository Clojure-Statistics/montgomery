(ns montgomery.calcs
  (:require [montgomery.utils :as utils]
            [clojure.math.numeric-tower :as math]))

(defn arithmetic-mean
  [coll]
  (float (/ (utils/sum coll)
            (count coll))))

(defn geometric-mean
  [coll]
  (float (math/expt (utils/product coll)
                    (utils/one-over (count coll)))))

(defn squared-deviations
  [coll]
  (let [mean (arithmetic-mean coll)]
    (map (fn [x] (utils/square (- x mean)))
         coll)))

(defn variance
  [coll]
  (/ (utils/sum (squared-deviations coll))
     (count coll)))

(defn sample-variance
  [coll]
  (/ (utils/sum (squared-deviations coll))
     (dec (count coll))))

(defn standard-deviation
  [coll]
  (math/sqrt (variance coll)))

(defn sample-standard-deviation
  [coll]
  (math/sqrt (sample-variance coll)))

(defn average-gain
  [returns]
  (arithmetic-avg (filter pos? returns)))

(defn average-loss
  [returns]
  (arithmetic-avg (filter neg? returns)))

(defn avg-gain-over-avg-loss
  [returns]
  (/ (average-gain returns)
     (average-loss returns)))

