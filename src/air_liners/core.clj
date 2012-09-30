(ns air-liners.core
  (:refer-clojure :exclude [rand])
  (:use [quil core]))
  (def % partial)

;;
;; General Utilities
;;

(defn step-through
  [min max steps]
  (let [step (-> (- max min) (/ (dec steps)))]
    (for [i (range steps)]
      (+ min (* i step)))))

(defn polar->cart
  [r t]
  [(-> (cos t) (* r)), (-> (sin t) (* r))])

(defn rand
  ([] (clojure.core/rand))
  ([n] (clojure.core/rand n))
  ([min max] (-> (clojure.core/rand (- max min)) (+ min))))

;;
;; Constants and State
;;

(def tau 6.2831853071795864)
(def $frame-rate 60)
(def $export-path "export/air-liners.obj")

(def al-count 5)
(def !air-liners (atom []))
(def !vc (atom 1)) ; how many vertices we've serialized to the obj file
(def !should-quit? (atom false))

;;
;; Data Types
;;

(defn air-liner
  ([]
   (air-liner (rand tau)
              (let [vel-mag-ceil (/ tau (* 5 $frame-rate))
                    vel-mag-floor (/ tau (* 15 $frame-rate))]
                (* (rand vel-mag-floor vel-mag-ceil)
                   1 #_(if (zero? (rand-int 2)) -1 1)))
              (rand (/ tau 40) (/ tau 10))))
  ([pos vel width]
   {:pos pos, :vel vel}))


;;
;; Sketch Stuff
;; here be the animation thread
;;

(defn setup []
  (smooth)
  (frame-rate 60)
  (spit $export-path "")
  (dotimes [_ al-count]
    (swap! !air-liners conj (air-liner))))

(defn tick []
  (reset! !air-liners
          (for [{:keys [pos vel] :as al} @!air-liners]
            (assoc al :pos (+ pos vel)))))

(defn draw []
  (tick)
  (translate (/ (width) 2) (/ (height) 2))
  (background 255)
  (fill 0)
  (stroke-weight 0)
  (let [frame-obj-lines (atom [])
        z (-> (frame-count) (* 3) double)
        al-steps 20
        al-verts (* 2 20)
        frame-verts (* al-verts al-count)
        r 150
        w (/ tau 30)
        h 25]
    (doseq [{pos :pos} @!air-liners]
      (begin-shape)
      (let [inner-points (for [t (step-through pos (+ pos w) al-steps)]
                           (polar->cart r t))
            outer-points (for [t (reverse (step-through pos (+ pos w) al-steps))]
                           (polar->cart (+ r h) t))
            obj (with-out-str
                  (doseq [[x y] inner-points]
                    (vertex x y)
                    (swap! !vc inc)
                    (println "v" x z y))
                  (doseq [[x y] outer-points]
                    (vertex x y)
                    (swap! !vc inc)
                    (println "v" x z y))
                  (when (or (= (frame-count) 1)
                            @!should-quit?)
                    (let [a_0 (- @!vc al-verts)
                          a_last (+ a_0 (dec al-verts))]
                      (print "f ")
                      (doseq [a_i (map (% + a_0) (range al-verts))]
                        (print a_i " "))
                      (println)))
                  (when (> (frame-count) 1)
                    (let [a_0 (- @!vc al-verts)
                          a_last (+ a_0 (dec al-verts))]
                      (println "f" a_0 a_last (- a_last frame-verts) (- a_0 frame-verts))
                      (doseq [[a_i a_j] (partition 2 1 (map (% + a_0) (range al-verts)))]
                        (println "f" a_i a_j (- a_j frame-verts) (- a_i frame-verts)))))
                  (end-shape))]
        (spit $export-path obj :append true)
        (when @!should-quit?
          (exit))))))

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
