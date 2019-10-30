(ns lemondronor.cooleradar-test
  (:require [cljs.test :refer (deftest is)]
            [lemondronor.cooleradar :as cooleradar]))

(def epsilon 0.0000001)

(defn a= [a b]
  (< (Math/abs (- a b)) epsilon))

(defn normalize-angle [a]
  (mod (+ a (* 2 Math/PI)) (* 2 Math/PI)))

(defn angles= [a b]
  (a= (normalize-angle a) (normalize-angle b)))


(deftest bearing->angle
  (is (angles= (cooleradar/bearing->angle 0) (/ Math/PI 2)))
  (is (angles= (cooleradar/bearing->angle (/ Math/PI 2)) 0))
  (is (angles= (cooleradar/bearing->angle Math/PI) (* 2 (/ 3 4) Math/PI))))


(def pos1 {:lat 34.133856404730224 :lon -118.19234294423293})
(def pos2 {:lat 34.1576265 :lon -118.29006930000001})

(deftest distance
  (is (a= (cooleradar/distance pos1 pos2) 9.3763996)))

(deftest bearing
  (is (a= (cooleradar/bearing pos1 pos2) (cooleradar/to-radians 286.40522))))


(deftest update-radar
  (let [radar0 {:lat 1 :lon 2 :rpm 1}
        now (.getTime (js/Date.))
        radar1 (cooleradar/update-radar radar0 now)
        radar2 (cooleradar/update-radar radar1 (+ now 1000))
        radar3 (cooleradar/update-radar radar2 (+ now 2000))]
    (is (= radar1
           {:lat 1
            :lon 2
            :rpm 1
            :prev-bearing 0
            :bearing 0
            :prev-update-time now}))
    (is (= [(:lat radar2) (:lon radar2)] [1 2]))
    (is (= (:prev-bearing radar2) 0))
    (is (= (:prev-update-time radar2) (+ now 1000)))
    (is (angles= (:bearing radar2) (cooleradar/to-radians (* 1 (/ 360 60)))))
    (is (angles= (:bearing radar3) (cooleradar/to-radians (* 2 (/ 360 60)))))))
