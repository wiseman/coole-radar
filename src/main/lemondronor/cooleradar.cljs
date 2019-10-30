(ns lemondronor.cooleradar
  (:require
   [cljs.pprint :as pprint]
   ["blessed" :as blessed]
   ["blessed-contrib" :as bcontrib]
   ["request" :as request]))


(defn to-radians [a]
  (* a (/ Math/PI 180)))

(defn distance [pos1 pos2]
  (let [r 6372.8 ;; Radius of the Earth in kilometers.
        lat1 (to-radians (:lat pos1))
        lat2 (to-radians (:lat pos2))
        lon1 (to-radians (:lon pos1))
        lon2 (to-radians (:lon pos2))
        d-lat (- lat2 lat1)
        d-lon (- lon2 lon1)
        a (+ (* (Math/sin (/ d-lat 2))
                (Math/sin (/ d-lat 2)))
             (* (Math/cos lat1)
                (Math/cos lat2)
                (Math/sin (/ d-lon 2))
                (Math/sin (/ d-lon 2))))
        c (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a))))
        d (* r c)]
    d))

(defn bearing [pos1 pos2]
  (let [lat1 (to-radians (:lat pos1))
        lat2 (to-radians (:lat pos2))
        lon1 (to-radians (:lon pos1))
        lon2 (to-radians (:lon pos2))
        d-lon (- lon2 lon1)
        y (* (Math/sin d-lon) (Math/cos lat2))
        x (- (* (Math/cos lat1) (Math/sin lat2))
             (* (Math/sin lat1) (Math/cos lat2) (Math/cos d-lon)))
        brg (Math/atan2 y x)]
    (mod (+ brg (* 2 Math/PI)) (* 2 Math/PI))))

(def radar-range-km 30)


(defn ->canvas-coords [canvas x y]
  [(- (* 2 x) 12) (* 4 y)])


(defn draw-radar [canvas n]
  (let [ctx (.-ctx canvas)
        w (.-width canvas)
        h (.-height canvas)
        x (* (/ w 2) (Math/cos n))
        y (* (/ h 2) (Math/sin n))]
    (set! (.-fillStyle ctx) "black")
    (let [[cw ch] (->canvas-coords canvas w h)
          [cx cy] (->canvas-coords canvas x y)]
      (.clearRect ctx 0 0 cw ch)
      (set! (.-strokeStyle ctx) "green")
      (.save ctx)
      (.translate ctx (/ cw 2) (/ ch 2))
      (.beginPath ctx)
      (.moveTo ctx 0 0)
      (.lineTo ctx cx cy)
      (.stroke ctx)
      (.restore ctx))))


(defn pos->canvas-coords [pos origin radar-range-km canvas]
  (let [d (distance origin pos)
        theta (- (/ Math/PI 2) (bearing origin pos))
        w (.-width canvas)
        h (.-height canvas)
        [cw ch] (->canvas-coords canvas w h)
        x (* d (Math/cos theta))
        y (* d (Math/sin theta))
        x-multiplier (/ cw (* 2 radar-range-km))
        y-multiplier (/ ch (* 2 radar-range-km))
        cx (+ (/ cw 2) (* x-multiplier x))
        cy (- ch (+ (/ ch 2) (* y-multiplier y)))]
    [cx cy]))

(defn plot-aircraft [aircraft radar canvas]
  (let [ctx (.-ctx canvas)
        [cx cy] (pos->canvas-coords aircraft radar radar-range-km canvas)
        brng (* (/ 180 Math/PI) (bearing radar aircraft))
        d (distance radar aircraft)
        icon (if (= (:type aircraft) :fixed)
               "✈"
               "x")
        alt (:alt aircraft)]
    (when (and (> cx 0) (> cy 0))
      (set! (.-fillStyle ctx) "green")
      (.fillText ctx icon cx cy)
      (.fillText ctx (:label aircraft) cx (+ 4 cy))
      ;;(.fillText ctx (str (.toFixed cx "0") " " (.toFixed cy "0")) cx (+ 8 cy))
      ;;(.fillText ctx (str (.toFixed brng "0") " " (.toFixed d "0") ":" (:dist aircraft)) cx (+ 12 cy))
      ;; (when alt
      ;;   (.fillText ctx (str alt) cx (+ 8 cy)))
      )))


(def planes_ (atom []))

(defn update-planes [radar]
  (let [lat (:lat radar)
        lon (:lon radar)
        url (str "https://vrs.heavymeta.org/VirtualRadar/AircraftList.json?feed=1&lat=" lat "&lng=" lon)
        options (clj->js {:url url
                          :gzip true})]
    (.get request
          options
          (fn [error response body]
            (if error
              (println "WOO ERROR" error response body)
              (if (= (.-statusCode response) 200)
                (let [planes (->> (get (js->clj (.parse js/JSON body)) "acList")
                                  (map (fn [ac]
                                         (if (contains? ac "Lat")
                                           {:lat (get ac "Lat")
                                            :lon (get ac "Long")
                                            :alt (get ac "Alt")
                                            :type (condp = (get ac "Species")
                                                    1 :fixed
                                                    4 :heli
                                                    :fixed)
                                            :dist (get ac "Dst")
                                            :bearing (get ac "Brng")
                                            :label (or (get ac "Reg")
                                                       (get ac "Icao"))}
                                           nil)))
                                  (filter identity)
                                  (filter #(let [d (:dist %)]
                                             (and d (< d radar-range-km)))))]
                  (reset! planes_ planes))
                (println "WOO BAD RESPONSE" error response body)))))))


(defn plot-frame [canvas color msg]
  (let [ctx (.-ctx canvas)
        w (- (* 2 (.-width canvas)) 12)
        h (* 4 (.-height canvas))]
    (.scale ctx 1 1)
    (set! (.-strokeStyle ctx) color)
    (.moveTo ctx 0 0)
    (.beginPath ctx)
    (.lineTo ctx 0 0)
    (.lineTo ctx 0 (- h 1))
    (.lineTo ctx (- w 1) (- h 1))
    (.lineTo ctx (- w 1) 0)
    (.lineTo ctx 0 0)
    (.stroke ctx)
    (.fillText ctx msg 0 0)
    (.fillText ctx (str (.-_scale canvas) " ") (- w 1) 0)))


(def radars
  {"yosemite" {:lat 34.133856404730224 :lon -118.19234294423293}
   "521circle7" {:lat 34.1576265 :lon -118.29006930000001}})


(defn main [& args]
  (let [screen (blessed/screen)
        canvas (bcontrib/canvas (clj->js {:width "100%"
                                          :height "100%"
                                          :top 0
                                          :left 0}))
        ;;radar {:lat 34.1576265 :lon -118.29006930000001}
        radar {:lat 34.133856404730224 :lon -118.19234294423293}
        ;;radar {:lat 34.1338 :lon -118.19248}
        plane {:lat 34.07382 :lon -118.47336 :label "WOO"}]
    (letfn [(update [n]
              (draw-radar canvas n)
              (doseq [plane @planes_]
                (plot-aircraft plane radar canvas))
              (.render screen)
              (js/setTimeout #(update (+ n 0.02)) 30))]
      (js/setInterval #(update-planes radar) 1000)
      (.append screen canvas)
      (.key screen
            #js ["escape" "q" "C-c"]
            #(.exit js/process 0))
      (update 0)
      (.render screen))))
