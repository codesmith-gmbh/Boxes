(ns app.client
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [helix.hooks :refer [use-effect]])
  (:require ["react-dom" :as rdom]
            [cljs-http.client :as http]
            [cljs.core.async :as async :refer [<!]]
            [helix.hooks :as hooks]
            [helix.dom :as d]
            [helix.core :refer [defnc $]]
            [app.projections :as p]
            [app.math :as math]))

(defn fetch-greeting! [set-state]
  (go (let [_        (<! (async/timeout 3000))
            response (<! (http/get "/greet"))
            greeting (-> response :body :greetings)]
        (set-state greeting))))

(defn svg [{:keys [vertices edges]}]
  (d/svg {:height         500 :width 500
          :style          {:border "1px solid black"}
          :viewBox        "0 0 500 500" :xmlns "http://www.w3.org/2000/svg"
          :stroke-linecap "round"
          :stroke         "black" :stroke-width 2}
         (into []
               (map-indexed
                 (fn [idx [p1 p2]]
                   (let [[x1 y1] (get vertices p1)
                         [x2 y2] (get vertices p2)]
                     (d/line {:key idx :x1 (+ (* 20 x1) 100) :y1 (+ 300 (* -20 y1)) :x2 (+ 100 (* 20 x2)) :y2 (+ 300 (* -20 y2))}))))
               edges)))

(defn value [event]
  (.-value (.-target event)))

(defnc numeric-slider [{:keys [width state set-state min max label]}]
  (let [on-change #(let [number (js/parseInt (value %1))
                         number (if (js/isNaN number) min number)
                         number (if (< number min) min number)
                         number (if (> number max) max number)]
                     (set-state number))]
    (d/div {:style {:width width :padding "5"}}
           (d/input {:style     {:width (- width 100)} :type "range" :min min :max max :value state
                     :on-change on-change})
           (d/input {:style     {:width 40}
                     :type      "text" :value state
                     :on-change on-change})
           (d/span {:style {:width 50 :padding-left "5"}}
                   label))))


(defnc rendering [{:keys [height width projection object x-translate y-translate magnify]}]
  (let [{:keys [vertices edges]} (projection object)]
    (d/svg {:height         height
            :width          width
            :style          {:border "1px solid black"}
            :viewBox        "0 0 500 500" :xmlns "http://www.w3.org/2000/svg"
            :stroke-linecap "round"
            :stroke         "black" :stroke-width 2}
           (into []
                 (map-indexed
                   (fn [idx [p1 p2]]
                     (let [[x1 y1] (get vertices p1)
                           [x2 y2] (get vertices p2)]
                       (d/line {:key idx
                                :x1  (+ (* magnify x1) x-translate)
                                :y1  (+ (* -1 magnify y1) y-translate)
                                :x2  (+ (* magnify x2) x-translate)
                                :y2  (+ (* -1 magnify y2) y-translate)}))))
                 edges))))

(defnc app [{:keys [size]}]
  (let [[x-translate set-x-translate] (hooks/use-state 100)
        [y-translate set-y-translate] (hooks/use-state 300)
        [angle-x set-angle-x] (hooks/use-state 30)
        [angle-y set-angle-y] (hooks/use-state 90)
        [angle-z set-angle-z] (hooks/use-state -30)]
    (d/div
      ($ numeric-slider {:width     size
                         :state     x-translate
                         :set-state set-x-translate
                         :min       0
                         :max       size
                         :label     "x-transl"})
      ($ numeric-slider {:width     size
                         :state     y-translate
                         :set-state set-y-translate
                         :min       0
                         :max       size
                         :label     "y-transl"})
      ($ rendering {:height      size
                    :width       size
                    :object      p/house
                    :projection  (p/project-isometric
                                   (p/degree-to-rad angle-x)
                                   (p/degree-to-rad angle-y)
                                   (p/degree-to-rad angle-z))
                    :x-translate x-translate
                    :y-translate y-translate
                    :magnify     20})
      ($ numeric-slider {:width     size
                         :state     angle-x
                         :set-state set-angle-x
                         :min       -90
                         :max       90
                         :label     "angle x"})
      ($ numeric-slider {:width     size
                         :state     angle-y
                         :set-state set-angle-y
                         :min       -90
                         :max       90
                         :label     "angle y"})
      ($ numeric-slider {:width     size
                         :state     angle-z
                         :set-state set-angle-z
                         :min       -90
                         :max       90
                         :label     "angle z"}))))

(defn mount! []
  (rdom/render
    ($ app {:size 500})
    (js/document.getElementById "app")))

(defn init []
  (mount!)
  (js/console.log "Initialized."))

(defn refresh []
  (mount!)
  (js/console.log "Refreshed."))