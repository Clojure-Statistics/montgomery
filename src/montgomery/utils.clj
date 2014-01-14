(ns montgomery.utils
  (:require [clojure.math.numeric-tower :as math]))

(defn- sum
  [coll]
  (reduce + coll))

(defn- product
  [coll]
  (reduce * coll))

(defn- square
  [x]
  (math/expt x 2))

(defn- one-over
  [x]
  (/ 1 x))


