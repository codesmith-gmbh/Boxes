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

(defn vector-from-to
  ([from to]
   (let [[x1 y1 z1] from
         [x2 y2 z2] to]
     [(- x2 x1) (- y2 y1) (- z2 z1)]))
  ([from]
   (partial vector-from-to from)))

(defn vector-length [v]
  (math/sqrt (transduce (map math/sqr) (completing +) v)))

(defn perspective-rigid-transformation [[xe ye ze :as eye] center]
  (let [[dx dy dz :as dp] (vector-from-to eye center)
        r1 (vector-length [dx dz])
        s1 (/ dx r1)
        c1 (/ dz r1)
        [_ rdy rdz :as rdp] [(- (* c1 dx) (* s1 dz)) dy (+ (* s1 dx) (* c1 dz))]
        r2 (vector-length rdp)
        c2 (/ rdz r2)
        s2 (/ rdy r2)]
    (print dp rdp)
    (print (vector-length dp) (vector-length rdp))
    (print s1 c1 s2 c2)
    (fn [[x y z]]
      (let [x-xe (- x xe)
            y-ye (- y ye)
            z-ze (- z ze)]
        [(- (* c1 x-xe) (* s1 z-ze))
         (+ (- (* s1 s2 x-xe))
            (* c2 y-ye)
            (- (* c1 s2 z-ze)))
         (+ (* s1 c2 x-xe)
            (* s2 y-ye)
            (* c1 c2 z-ze))]))))

(defn project-rigid [object]
  ;; constants from the course
  (let [f (perspective-rigid-transformation [11.0 2.0 -15.0] [3.5 3.0 5.0])
        g (fn [v]
            (let [[x y z] (f v)]
              [x y]))]
    (project g object)))

(defn project-perspective [object]
  (let [f (perspective-rigid-transformation [11.0 2.0 -15.0] [3.5 3.0 5.0])
        g (fn [v]
            (let [[x y z] (f v)]
              [(* 20 (/ x z)) (* 20 (/ y z))]))]
    (project g object)))

(defn dynamic-perspective [xe ye ze]
  (let [f (perspective-rigid-transformation [xe ye ze] [3.5 3.0 5.0])
        g (fn [v]
            (let [[x y z] (f v)]
              [(* 20 (/ x z)) (* 20 (/ y z))]))]
    (fn [object]
      (project g object))))

(comment

  (vector-length [3 4])

  (transduce (map math/sqr) (completing +) 0 [3 4])

  (/ (math/sqrt 2) 2)

  (math/cos (/ math/pi 4))

  )