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

(defnc cube []
  (svg (p/project-cavalier p/house)))

(defn value [event]
  (.-value (.-target event)))

(defnc app []
  (let [[angle set-angle] (hooks/use-state 45)]
    (d/div
      (svg ((p/project-military (p/degree-to-rad (- angle))) p/house))
      (d/div {:style {:width 500}}
             (d/input {:style     {:width 300} :type "range" :min 0 :max 90 :value angle
                       :on-change #(set-angle (value %1))})
             (d/input {:type "text" :value angle
                       :on-change #(let [number (js/parseInt (value %1))
                                         number (if (js/isNaN number) 0 number)
                                         number (if (< number 0) 0 number)
                                         number (if (> number 90) 90 number)]
                                     (set-angle number))})))))

(defn mount! []
  (rdom/render
    ($ app)
    (js/document.getElementById "app")))

(defn init []
  (mount!)
  (js/console.log "Initialized."))

(defn refresh []
  (mount!)
  (js/console.log "Refreshed."))