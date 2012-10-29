(ns air-liners.core
  (:refer-clojure :exclude [rand])
  (:use [quil.core :exclude [vertex sin]]))
  (def % partial)

;;
;; Constants and State
;;

(def tau 6.2831853071795864)
(def $frame-rate 60)
(def $liner-count 7)
(def $export-path "export/air-liners.obj")

(def $base-steps (inc 128))
(def $base-height 150)
(def $z-step 3)
(def $r 150)
(def $min-r 20)
(def $h 17)
(def $w (/ tau 15))
(def $steps 400)
(def $length (* $frame-rate 10))

(def !air-liners (atom []))
(def !z (atom 0)) ; z-position in output object mesh
(def !vc (atom 1)) ; how many vertices we've serialized to the obj file
(def !should-quit? (atom false))

(defn age []
  (- 1 (/ (frame-count) $length)))

;;
;; General Utilities
;;

(defn step-through
  ([min max steps]
   (step-through min max steps :closed))
  ([min max steps mode]
   (cond
    (zero? steps) []
    (= steps 1) [min]
    (= mode :open) (let [step (-> (- max min) (/ steps))]
                     (for [i (range steps)]
                       (+ min (* i step))))
     :otherwise (let [step (-> (- max min) (/ (dec steps)))]
                  (concat (for [i (range (dec steps))]
                            (+ min (* i step)))
                          [max])))))

(defn sin
  [x]
  (if (zero? (mod x tau))
    0.0
    (quil.core/sin x)))

(defn polar->cart
  [r t]
  [(-> (cos t) (* r)), (-> (sin t) (* r))])

(defn rand
  ([] (clojure.core/rand))
  ([n] (clojure.core/rand n))
  ([min max] (-> (clojure.core/rand (- max min)) (+ min))))

;;
;; Mesh Output
;;

(defn obj-vertex
  [x y z]
  (println "v" x y z)
  (swap! !vc inc))

(defn obj-quad
  [v0 v1 v2 v3]
  (println "f" v0 v1 v2 v3))

(defn vertex
  [x y z]
  (quil.core/vertex x y)
  (obj-vertex x z y))

;;
;; Data Types
;;

(defn air-liner
  ([]
   (air-liner (rand tau)
              (let [vel-mag-ceil (/ tau (* 5 $frame-rate))
                    vel-mag-floor (/ tau (* 19 $frame-rate))]
                (* (rand vel-mag-floor vel-mag-ceil)
                   1 #_(if (zero? (rand-int 2)) -1 1)))))
  ([pos vel]
   {:pos pos, :vel vel}))

(defn intervals
  [liners]
  (let [sorted (sort (for [{pos :pos} liners] (mod pos tau)))
        intvls (partition 2 1 (concat sorted (take 1 sorted)))]
    (->> (sort-by second intvls)
      (remove (fn [[u v]] (= u v))))))

(defn radius-fn
  [liners]
  (let [intvls (intervals liners)
        cos-fns (for [[u v] intvls]
                  (fn [x]
                    (let [w (if (> u v)
                              (- (+ v tau) u)
                              (- v u))
                          x' (-> (if (and (> u v) (< x u)) (+ x tau) x)
                               (- u), (/ w), (* tau))]
                      (-> (cos x')
                        (- 1), (* $h w)))))
        sorted-fns (map vector (map second intvls) cos-fns)]
    (fn [x]
      (let [cos-fn (-> (filter (fn [[end _]] (> end x)) sorted-fns)
                     first, second
                     (or (-> sorted-fns first second)))]
        (cos-fn x)))))

;;
;; Sketch Stuff
;; here be the animation thread
;;

(defn setup []
  (smooth)
  (frame-rate $frame-rate)
  (spit $export-path "")
  (reset! !air-liners [])
  (reset! !vc 1)
  (reset! !z 0)
  (reset! !should-quit? false)
  (dotimes [_ $liner-count]
    (swap! !air-liners conj (air-liner))))

(defn tick! []
  (reset! !air-liners
          (for [{:keys [pos vel] :as al} @!air-liners]
            (assoc al :pos (+ pos vel)))))

(defn draw []
  (tick!)
  (translate (/ (width) 2) (/ (height) 2))
  (background 255)
  (fill 0)
  (stroke-weight 0)
  (stroke 0 0)
  (begin-shape)
  (let [z (-> @!z double)
        rfn (radius-fn @!air-liners)
        obj (with-out-str
              (doseq [t (step-through 0 tau $steps)
                      :let [[x y] (polar->cart (-> (rfn t)
                                                 (+ $r)
                                                 (max $min-r))
                                               t)]]
                (vertex x y z))
              (let [frame-verts (range (- @!vc $steps) @!vc)]
                ;; side faces
                (doseq [[v_i v_j] (partition 2 1 (concat frame-verts
                                                         (take 1 frame-verts)))]
                  (obj-quad v_i v_j (- v_j $steps) (- v_i $steps)))
                ;; cap face
                (when (or (= (frame-count) 1) @!should-quit?)
                  (print "f ")
                  (doseq [v_i frame-verts]
                    (print v_i " "))
                  (println))))]
    (spit $export-path obj :append true))
  (end-shape)
  (swap! !z + $z-step)
  (when @!should-quit?
    (exit)))

(defn key-pressed []
  (when (== (key-code) 81)
    (reset! !should-quit? true)))

(defn -main
  "I don't do a whole lot."
  []
  (defsketch air-liners
    :title "Air Liners"
    :setup setup
    :draw draw
    :size [646 400]
    :key-pressed key-pressed))
