(ns lemondronor.cooleradar
  (:require
   [cemerick.url :as c-url]
   [cljs.core.async :refer [chan <! >! put! go close!]]
   [cljs.pprint :as pprint]
   ["blessed" :as blessed]
   ["blessed-contrib" :as bcontrib]
   ["fs" :as fs]
   ["request" :as request]))


(defn spit [path data]
  (fs/appendFileSync path (str data "\n")))


(defn to-radians [a]
  (* a (/ Math/PI 180)))

(defn to-degrees [a]
  (* a (/ 180 Math/PI)))


;; Given two positions (:lat and :lon), calculate the distance (in km)
;; between them using Haversine.
;;
;; Note: Update my stackoverflow answer which has the wrong formula.

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


;; Calculate the bearing (not angle) between two positions.

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


;; Canvas has a weird coordinate system. See
;; https://github.com/yaronn/blessed-contrib/blob/4b690bc04fb06ab278cb0f84065ca7001bb26668/lib/widget/canvas.js#L36
;; and https://github.com/yaronn/blessed-contrib/issues/184

(defn ->canvas-coords [canvas x y]
  [(- (* 2 x) 12) (* 4 y)])


(defn bearing->angle [brg]
  (- (/ Math/PI 2) brg))


(defn normalize-bearing [brg]
  (mod (+ brg (* Math/PI 2)) (* Math/PI 2)))


(def initial-radar-range-km 30)


(defn update-radar [radar now]
  (let [bearing (or (:bearing radar) 0)
        prev-update-time (:prev-update-time radar)
        new-bearing (if prev-update-time
                      (normalize-bearing
                       (+ bearing
                          (* (or (:rpm radar) 5)
                             (/ (- now prev-update-time) (* 1000 60))
                             (* 2 Math/PI))))
                      0)]
    (assoc radar
           :prev-bearing bearing
           :bearing new-bearing
           :prev-update-time now)))


(defn render-radar [app]
  (let [canvas (:radar-canvas app)
        radar (:radar app)
        ctx (.-ctx canvas)
        w (.-width canvas)
        h (.-height canvas)
        theta (+ Math/PI (- (bearing->angle (:bearing radar))))
        x (* (/ w 2) (Math/cos theta))
        y (* (/ h 2) (Math/sin theta))]
    (set! (.-fillStyle ctx) "black")
    (let [[cw ch] (->canvas-coords canvas w h)
          [cx cy] (->canvas-coords canvas x y)]
      (.clearRect ctx 0 0 cw ch)
      (set! (.-strokeStyle ctx) "green")
      (set! (.-fillStyle ctx) "green")
      (.fillText ctx (str "RNG:" (.toFixed (:radar-range-km app) "1") " KM") 0 0)
      (.fillText ctx (str "RPM:" (.toFixed (get-in app [:radar :rpm]) "1")) 0 4)
      (.save ctx)
      (.translate ctx (/ cw 2) (/ ch 2))
      (.beginPath ctx)
      (.moveTo ctx 0 0)
      (.lineTo ctx cx cy)
      (.stroke ctx)
      (.restore ctx))))


(defn pos->canvas-coords [pos origin radar-range-km canvas]
  (let [d (distance origin pos)
        theta (bearing->angle (bearing origin pos))
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


(defn vrs-parse-aircraft [json-str app]
  (let [radar (:radar app)]
    (->> (get (js->clj (.parse js/JSON json-str)) "acList")
         (map (fn [ac]
                (if (contains? ac "Lat")
                  {:lat (get ac "Lat")
                   :lon (get ac "Long")
                   :alt (get ac "Alt")
                   :speed (get ac "Spd")
                   :type (condp = (get ac "Species")
                           1 :fixed
                           4 :heli
                           :fixed)
                   :reg (get ac "Reg")
                   :icao (get ac "Icao")}
                  nil)))
         (filter identity)
         (map (fn [ac]
                (assoc ac
                       :bearing (bearing ac radar)
                       :distance (distance ac radar))))
         (filter #(let [d (:distance %)]
                    (and d (< d (:radar-range-km app))))))))


(defn vrs-get-aircraft& [base-url app]
  (let [radar (:radar app)
        lat (:lat radar)
        lon (:lon radar)
        url (-> (c-url/url base-url "AircraftList.json")
                (assoc :query {:lat lat :lng lon}))
        options (clj->js {:url (str url) :gzip true})
        ch (chan)]
    (go
      (.get request
            options
            (fn [error response body]
              (if error
                (println "WOO ERROR" error response body)
                (if (= (.-statusCode response) 200)
                  (let [planes (vrs-parse-aircraft body app)]
                    (put! ch planes))
                  (println "Bad response from VRS:" (.-statusCode response) body))))))
    ch))


(def radars
  {"yosemite" {:lat 34.133856404730224 :lon -118.19234294423293 :rpm 5}
   "521circle7" {:lat 34.1576265 :lon -118.29006930000001 :rpm 5}})

(defn get-radar [spec]
  (radars spec))


(defn age->color [age]
  ;;'#a12f01'
  (let [g (Math/round (* 255 (/ (- 20000 age) 20000)))
        g-str (-> g
                  Math/round
                  (.toString 16)
                  (.padStart 2 "0"))
        color (str "#00" g-str "00")]
    [0 g 0]))


(defn render-aircraft [aircraft app]
  (let [canvas (:radar-canvas app)
        radar (:radar app)
        ctx (.-ctx canvas)
        [cx cy] (pos->canvas-coords aircraft radar (:radar-range-km app) canvas)
        [w h] [(.-width canvas) (.-height canvas)]
        [cw ch] (->canvas-coords canvas w h)
        brng (* (/ 180 Math/PI) (bearing radar aircraft))
        d (distance radar aircraft)
        icon (if (= (:type aircraft) :fixed)
               "✈"
               "x")
        alt (:alt aircraft)]
    (when (and (> cx 0) (> cy 0) (< cx cw) (< cy ch))
      (set! (.-fillStyle ctx) (clj->js (age->color (:illuminated-age aircraft))))
      (.fillText ctx icon cx cy)
      (.fillText ctx (or (:reg aircraft) (:icao aircraft)) cx (+ 4 cy)))))


(defn render-aircrafts [app]
  (doseq [aircraft (:hits app)]
    (render-aircraft aircraft app)))


(def airports [{:label "BUR" :lat 34.1983 :lon -118.3574 :icon "🛬"}
               {:label "LAX" :lat 33.9416 :lon -118.4085 :icon "🛬"}])


(defn plot-airport [airport app]
  (let [radar (:radar app)
        canvas (:radar-canvas app)
        ctx (.-ctx canvas)
        [cx cy] (pos->canvas-coords airport radar (:radar-range-km app) canvas)
        brng (* (/ 180 Math/PI) (bearing radar airport))
        d (distance radar airport)
        icon "🛬"]
    (when (and (> cx 0) (> cy 0))
      (set! (.-fillStyle ctx) "green")
      (.fillText ctx icon cx cy)
      (.fillText ctx (:label airport) cx (+ 4 cy)))))


(defn render-airports [app]
  (doseq [airport airports]
    (plot-airport airport app)))


(defn update-hits [hits radar truth now]
  (let [radar-bearing1 (normalize-bearing (:prev-bearing radar))
        radar-bearing2 (normalize-bearing (:bearing radar))
        illuminated (->> truth
                         (filter #(<= radar-bearing1 (normalize-bearing (:bearing %)) radar-bearing2))
                         (map #(assoc % :illuminated-time now)))]
    (->> hits
         (concat illuminated)
         (map #(assoc % :illuminated-age (- now (:illuminated-time %))))
         (filter #(< (:illuminated-age %) (* 30 1000)))
         (sort-by :illuminated-age)
         reverse)))


(defn render-info-table [app]
  (let [table (:info-table app)
        hits-by-icao (group-by :icao (:hits app))
        hit-icaos (keys hits-by-icao)
        truth-by-icao (group-by :icao (:aircraft-truth app))
        data (->> hit-icaos
                  (map (fn [icao]
                         (spit "debug.out" (str icao " " (truth-by-icao icao)))
                         (let [d (or (first (truth-by-icao icao))
                                     (last (hits-by-icao icao)))]
                           (spit "foo.out" d)
                           [(:icao d)
                            (:reg d)
                            (.padStart (.toFixed (/ (:alt d) 100) 0) 3 " ")
                            (.padStart (.toFixed (:speed d) 0) 3 " ")
                            (.padStart (.toFixed (:distance d) 0) 3 " ")
                            (:distance d)])))
                  (sort-by last)
                  (map butlast))]
    (when (seq (:hits app))
      (.setData table (clj->js {:headers ["ICAO" "REG" "ALT" "SPD" "DST"] :data data})))))


(defn debug-println [x msg]
  (spit "debug.out" (str msg x))
  x)


(defn update-app [app now]
  (-> app
      (update :radar update-radar now)
      (as-> app (update app :hits update-hits (:radar app) (:aircraft-truth app) now))))


(defn render-app [app]
  (render-radar app)
  (render-info-table app)
  (render-airports app)
  (render-aircrafts app))


(defn add-controls [app_]
  (let [screen (:screen @app_)
        update-in-app! (fn [keys & args]
                         (apply swap! app_ update-in keys args))]
    (.key screen
          #js ["escape" "q" "C-c"]
          #(.exit js/process 0))
    (.key screen
          #js ["+" "="]
          #(update-in-app! [:radar-range-km] * (/ 1 1.1)))
    (.key screen
          #js ["-" "_"]
          #(update-in-app! [:radar-range-km] * 1.1))
    (.key screen
          #js ["0"]
          #(update-in-app! [:radar-range-km] (fn [_] initial-radar-range-km)))
    (.key screen
          #js ["'"]
          #(update-in-app! [:radar :rpm] * 1.1))
    (.key screen
          #js [";"]
          #(update-in-app! [:radar :rpm] * (/ 1 1.1)))))


(defn main [& args]
  (let [screen (blessed/screen)
        grid (bcontrib/grid. #js {"rows" 12 "cols" 12 "screen" screen})
        radar-canvas (.set grid 0 0 6 6 bcontrib/canvas {})
        table (.set grid 6 0 6 6 bcontrib/table (clj->js {:columnWidth [7 7 4 4 4]
                                                          :interactive true}))
        radar (get-radar "521circle7")
        vrs-url (cond-> (first args)
                  (not (.endsWith "/")) (str "/"))
        app_ (atom {:screen screen
                    :radar-canvas radar-canvas
                    :info-table table
                    :radar radar
                    :radar-range-km initial-radar-range-km
                    :aircraft-truth []
                    :hits []})]
    (letfn [(tick []
              (let [now (.getTime (js/Date.))]
                (swap! app_ update-app now)
                (render-app @app_)
                (.render screen)
                (js/setTimeout tick 30)))]
      (js/setInterval
       #(go
          (let [aircraft-truth (<! (vrs-get-aircraft& vrs-url @app_))]
            (swap! app_ assoc :aircraft-truth aircraft-truth)))
       1000)
      (add-controls app_)
      (tick)
      (.render screen))))
