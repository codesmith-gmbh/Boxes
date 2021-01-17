(ns app.projections
  (:require [app.math :as math]))

(def house
  {:vertices {:p1  [0 0 0]
              :p2  [0 3 0]
              :p3  [3.5 5 0]
              :p4  [7 3 0]
              :p5  [7 0 0]

              :p6  [0 3 9]
              :p7  [0 0 9]
              :p8  [7 3 9]
              :p9  [7 0 9]
              :p10 [3.5 5 9]

              :p11 [3 0 9.1]
              :p12 [5 0 9.1]
              :p13 [5 8 9.1]
              :p14 [3 8 9.1]
              :p15 [3 0 10.2]
              :p16 [5 0 10.2]
              :p17 [5 8 10.2]
              :p18 [3 8 10.2]}
   :edges    [
              [:p1 :p2]
              [:p2 :p3]
              [:p3 :p4]
              [:p4 :p5]
              [:p5 :p1]

              [:p6 :p7]
              [:p7 :p9]
              [:p9 :p8]
              [:p8 :p10]
              [:p10 :p6]

              [:p1 :p7]
              [:p2 :p6]
              [:p3 :p10]
              [:p4 :p8]
              [:p5 :p9]

              [:p11 :p12]
              [:p12 :p13]
              [:p13 :p14]
              [:p14 :p11]
              [:p15 :p16]
              [:p16 :p17]
              [:p17 :p18]
              [:p18 :p15]

              [:p11 :p15]
              [:p12 :p16]
              [:p13 :p17]
              [:p14 :p18]
              ]})

(defn project [f object]
  (update object :vertices
          #(into {}
                 (map (fn [[k v]]
                        [k (f v)]))
                 %1)))

(defn project-front [object]
  (project (fn [[x y z]] [x y])
           object))

(defn project-side [object]
  (project (fn [[x y z]] [z y])
           object))

(defn project-top [object]
  (project (fn [[x y z]] [z x])
           object))

(defn mat-mul-3x2-3 [m1 m2]
  (let [[x11 x12 x13 x21 x22 x23] m1
        [y1 y2 y3] m2]
    [(+ (* x11 y1) (* x12 y2) (* x13 y3))
     (+ (* x21 y1) (* x22 y2) (* x23 y3))]))

(defn project-matrice [m]
  (partial project (fn [vertex]
                     (mat-mul-3x2-3 m vertex))))

(defn degree-to-rad [angle]
  (* (/ math/pi 180) angle))

(defn oblique-transform [angle]
  [1 0 (math/cos angle)
   0 1 (math/sin angle)])

(defn project-oblique [angle]
  (project-matrice (oblique-transform angle)))

(def cavalier-transform
  (oblique-transform (/ math/pi 8)))

(defn project-cavalier [object]
  (project (fn [vertex]
             (mat-mul-3x2-3 cavalier-transform vertex))
           object))

(defn project-military [angle]
  (let [c (math/cos angle)
        s (math/sin angle)]
    (partial project
             (fn [[x y z]]
               [(- (* c x) (* s z))
                (+ (* s x) (* c z) y)]))))

(defn isometric-matrice [angle-x angle-y angle-z]
  [(math/cos angle-x) (math/cos angle-y) (math/cos angle-z)
   (math/sin angle-x) (math/sin angle-y) (math/sin angle-z)])

(defn project-isometric [angle-x angle-y angle-z]
  (project-matrice (isometric-matrice angle-x angle-y angle-z)))

(comment

  (/ (math/sqrt 2) 2)

  (math/cos (/ math/pi 4))

  )