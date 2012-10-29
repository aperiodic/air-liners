(ns air-liners.core
  (:refer-clojure :exclude [rand])
  (:use [quil.core :exclude [vertex sin]]))
  (def % partial)

;;
;; Constants and State
;;

(def tau 6.2831853071795864)
(def $frame-rate 60)
(def $liner-count 1)
(def $export-path "export/air-liners.obj")

(def $base-steps (inc 128))
(def $base-height 150)
(def $z-step 3)
(def $r 150)
(def $h 50)
(def $w (/ tau 15))
(def $liner-steps 40)
(def $liner-verts (* 2 $liner-steps))
(def $frame-verts (* $liner-count $liner-verts))

(def !air-liners (atom []))
(def !z (atom 0)) ; z-position in output object mesh
(def !vc (atom 1)) ; how many vertices we've serialized to the obj file
(def !should-quit? (atom false))

;;
;; General Utilities
;;

(defn step-through
  [min max steps]
  (cond
    (zero? steps) []
    (= steps 1) [min]
    :otherwise (let [step (-> (- max min) (/ (dec steps)))]
                 (concat (for [i (range (dec steps))]
                           (+ min (* i step)))
                         [max]))))

(defn sin
  [x]
  (if (= x tau)
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
              (let [vel-mag-ceil (/ tau (* 4 $frame-rate))
                    vel-mag-floor (/ tau (* 19 $frame-rate))]
                (* (rand vel-mag-floor vel-mag-ceil)
                   1 #_(if (zero? (rand-int 2)) -1 1)))))
  ([pos vel]
   {:pos pos, :vel vel, :width $w}))

(defn solid-intervals
  [liners]
  (let [sorted (sort-by :pos (for [liner liners]
                               (update-in liner [:pos] mod tau)))
        merged (reduce (fn [intvls {p :pos, w :width}]
                         (let [v' (+ p w)]
                           (if-let [[u v c] (last intvls)]
                             (if (<= p v)
                               (concat (butlast intvls) [[u v' (inc c)]])
                               (concat intvls [[p v' 1]]))
                             ; else (no intervals yet)
                             [[p v' 1]])))
                       ()
                       sorted)
        modded (for [[u v c] merged] [(mod u tau) (mod v tau) c])
        [u_n v_n c] (last modded)]
    (if (< v_n u_n) ; last interval straddles zero, may overlap w/first
      (let [[u_0 v_0] (first modded)]
        (if (>= v_n u_0) ; overlap
          (concat (->> modded butlast (drop 1)) [[u_n v_0 (inc c)]])
          modded))
      modded)))

(defn counts->steps
  [intervals]
  (for [[u v c] intervals]
    [u v (* c $liner-steps)]))

(defn split-straddler
  [intervals]
  (let [[u v steps] (last intervals)]
    (if (> u v) ; straddles zero
      (let [width (- (+ v tau) u)
            v-steps (int (* (/ v width) steps))
            u-steps (- steps v-steps)]
        (cond
          (zero? v-steps) (concat (butlast intervals) [[u tau steps]])
          (zero? u-steps) (concat [[0 v steps]] (butlast intervals))
          :otherwise
            (concat [[0 v v-steps true]]
                    (butlast intervals)
                    [[u tau u-steps true]])))
      ; else (does not straddle zero)
      intervals)))

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

(defn tick []
  (reset! !air-liners
          (for [{:keys [pos vel] :as al} @!air-liners]
            (assoc al :pos (+ pos vel)))))

(defn solid-ring []
  (let [layer-points (* 2 $base-steps)
        curr-z @!z]
    (doseq [z [curr-z (+ curr-z $base-height)]
            [i t] (map vector (range) (step-through 0 tau $base-steps))]
      ;; inner point
      (let [[x y] (polar->cart $r t)]
        (obj-vertex x (double z) y))
      ;; outer point
      (let [[x y] (polar->cart (+ $r $h) t)]
        (obj-vertex x (double z) y))
      (when (> i 0)
        ;; faces
        (let [outer-left (- (dec @!vc) 2)]
          ;; top/bottom face
          (obj-quad outer-left (+ outer-left 2)
                    (inc outer-left) (dec outer-left))
          ;; side faces
          (when (= z $base-height)
            (let [lower-outer-left (- outer-left layer-points)]
              ;; outer face
              (obj-quad lower-outer-left (+ lower-outer-left 2)
                        (+ outer-left 2) outer-left)
              ;; inner face
              (obj-quad (dec outer-left) (inc outer-left)
                        (inc lower-outer-left) (dec lower-outer-left))))))))
    (swap! !z + $base-height))

(defn draw []
  (tick)
  (translate (/ (width) 2) (/ (height) 2))
  (background 255)
  (fill 0)
  (stroke-weight 0)
  (stroke 0 0)
  (when (= (frame-count) 1)
    ;; solid ring at base
    (spit $export-path (with-out-str (solid-ring)) :append true))
  (let [z (-> @!z double)
        liner-intervals (-> @!air-liners
                          solid-intervals counts->steps split-straddler)]
    (doseq [[u v steps split?] liner-intervals
            :let [verts (* 2 steps)]]
      (begin-shape)
      (let [inner-points (for [t (step-through u v steps)] (polar->cart $r t))
            outer-points (for [t (-> (step-through u v steps) reverse)]
                           (polar->cart (+ $r $h) t))
            obj (with-out-str
                  (doseq [[x y] (concat inner-points outer-points)]
                    (vertex x y z))
                  ;; wall faces
                  (when (> (frame-count) 1)
                    (let [a_0 (- @!vc verts)
                          a_last (dec @!vc)
                          face-pairs (if-not (and split? (= v tau))
                                       (partition 2 1 (range a_0 (+ a_0 verts)))
                                       (concat
                                         (partition 2 1 (range a_0 (+ a_0 steps)))
                                         (partition 2 1 (range (+ a_0 steps) (+ a_0 verts)))))]
                      (when-not (and split? (= u 0))
                        (obj-quad a_0 a_last (- a_last $frame-verts) (- a_0 $frame-verts)))
                      (doseq [[a_i a_j] face-pairs]
                        (obj-quad a_i a_j (- a_j $frame-verts) (- a_i $frame-verts)))))
                  ;; cap face
                  (when (or (= (frame-count) 1) @!should-quit?)
                    (print "f ")
                    (doseq [a_i (range (- @!vc verts) @!vc)]
                      (print a_i " "))
                    (println)))]
        (spit $export-path obj :append true))
      (end-shape))
    (when (some #(> (count %) 3) liner-intervals) ; interval was split

      )
    )
  ;(stroke-weight 2)
  ;(stroke 255 0 0)
  ;(doseq [[u v] (-> @!air-liners solid-intervals counts->steps split-straddler)]
  ;  (line (polar->cart $r u) [0 0])
  ;  (line (polar->cart $r v) [0 0]))
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
