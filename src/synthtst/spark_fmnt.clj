;    Copyright (C) 2016, 2018  Joseph Fosco. All Rights Reserved
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns synthtst.spark-fmnt
  (:require
   [overtone.live :refer :all]
   )
  )

(defn weighted-choice
  "Makes a random selection based on a vector of weights.
   Returns the index into the vector of the selection
   Will throw an IndexOutOfBounds exception if the vector is empty

   weight-vector - vector of the form [x1 x2 x3 x4 ....]
                   where each entry is the relative weight of that entry"
  [weight-vector]
  (let [rand-num (rand (reduce + weight-vector))]
    (loop [i 0 sum 0]
      (if (< rand-num (+ (weight-vector i) sum))
        i
        (recur (inc i) (+ (weight-vector i) sum)))))
  )

(def max-release-weights [0.0025 0.005 0.01 0.05 0.1 0.5 1 2 3])

(def release-weights [10 5 4 3 3 2 2 2 2 1 1 1 1 1 1 1])
;;(def release-weights [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1])
;;(def release-weights [10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])

(defonce spark-main-g (group "spark-main"))
(defonce spark-early-g (group "spark early" :head spark-main-g))
(defonce spark-later-g (group "spark later" :after spark-early-g))

(defonce spark3-bus (audio-bus 1 "spark-bus"))

;;; using a bus for base pitch

(defonce fmnt-main-g (group "fmnt-main" :tail spark-early-g))
(defonce fmnt-early-g (group "fmnt early" :head fmnt-main-g))
(defonce fmnt-later-g (group "fmnt later" :after fmnt-early-g))

(defonce pitch-bus (control-bus "pitch-bus"))

;; (defonce impulse-bus (control-bus))

(def base-attack 0.01)
(def base-release 0.05)

(defsynth spark3-synth
  []
  (out [0 1]
       ;; (compander (in spark3-bus) (in spark3-bus) 0.5 1.0 0.5 0.01 0.05)
       (limiter (in spark3-bus) 0.9 0.01)
       )
  )

(defsynth pitch-osc
  [out-bus 0 freq 0.25 min-pitch 100 max-pitch 2000]
  (out:kr out-bus (lin-exp (lf-noise1:kr freq) -1 1 min-pitch max-pitch ))
  )

;; (defsynth pulses
;;   [out-bus 2 attack base-attack release base-release]
;;   (out:kr out-bus
;;           (impulse (range-lin (lf-noise0:kr 0.5)
;;                               (/ 1 (/ (+ 0.5 attack release) 2))
;;                               (/ 1 (/ (+ 3 attack release) 2))
;;                               )
;;                    )
;;           )
;;   )

(defsynth spark3-sound
  [gate 1 attack 0.001 release 0.001 gate 0 action FREE]
  (let [eg (env-gen (perc attack release) gate 1 0 1 action)
        cutoff-min 800
        cutoff-max 10000
        ]
    (out spark3-bus
         (-> eg
             (*
              (limiter
               (rhpf
                (white-noise)
                (latch:kr (+ (/ (* (- cutoff-max cutoff-min)
                                   (- (lf-noise1:kr 20) -1))
                                2) cutoff-min)
                          (lf-noise1:kr 50))
                (/ (+ 1 (lf-noise1:kr 12)) 2) ;; scale between 0 an 1
                )
               1.0
               0.01
               ))
             )
         )
    )
  )

(defsynth fmnt4a [freq-bus 0 cfreq 880
                 min-cfreq -75 max-cfreq 75 freq-cfreq 100
                 min-bw 10 max-bw 500 freq-bw 50
                 attack base-attack release base-release
                 vol 1 action FREE gate 0]

  (out spark3-bus
       (let [;;env-ff (toggle-ff (in:kr impulse-bus))
             ;;env-gate (gate env-ff (delay1:kr (in:kr impulse-bus)))
             envelope-generator (env-gen (perc attack release)
                                         gate 1 0 1 action)
             pitch (latch (+ (in:kr freq-bus)
                             (range-lin (lf-noise0:kr 1) -50 50))
                          gate
                          )
             ]
         (* (formant pitch
                     (+ pitch (range-lin (lf-noise1:kr freq-cfreq)
                                         min-cfreq
                                         max-cfreq))
                     (range-lin (lf-noise1:kr freq-bw) min-bw max-bw))
            envelope-generator
            vol
            )
         ))
  )

(def f4a (fmnt4a [:tail fmnt-later-g] :freq-bus pitch-bus))
(do
  (let [release 1.001]
    (ctl f4a :release release)
    (ctl impulse-cntl :release release)
    ))
(do
  (let [attack 0.01]
    (ctl f4a :attack attack)
    (ctl impulse-cntl :attack attack)
    ))
(stop)

;;------------------------------------------------------------------

(def spark3-synth-inst (spark3-synth [:tail spark-later-g]))


(def pitch-cntl (pitch-osc [:tail fmnt-early-g] :out-bus pitch-bus :freq 2.0))
(ctl pitch-cntl :max-pitch 60)
(ctl pitch-cntl :min-pitch 60)

;; (def impulse-cntl (pulses [:tail fmnt-early-g]
;;                           :out-bus impulse-bus
;;                           :release 0.05))

(def spk (spark3-sound [:tail spark-early-g] :release 0.1 :gate 1 :action FREE))
(ctl spk :gate 0)
(ctl spk :gate 1)
(stop)

(defn play-synth
  [synth rel-weights play-count times-to-play attack]
  (let  [continue-playing (< play-count times-to-play)
         ;; attack (rand 0.01)
         array-step (/ times-to-play (dec (count max-release-weights)))
         max-release-min-base-ndx (int (/ play-count array-step))
         max-release-min-ndx (- max-release-min-base-ndx
                                (rand-int (inc max-release-min-base-ndx)))
         max-release (* (max-release-weights max-release-min-ndx)
                        (inc (weighted-choice rel-weights)))
         release (+ 0.001 (rand max-release))
         action (if continue-playing NO-ACTION FREE)
         release-time (+ (now) (int (* (+ attack release) 1000)) 100)
         next-time (+ release-time 250 (rand-int 800))
         ]
    (when (not continue-playing) (println "STOPPING !!!!!!!!!"))
;;    (println continue-playing play-count times-to-play)
    (ctl synth
         :gate 1
         :attack attack
         :release release
         :action action
         )
    (apply-at release-time
              #'ctl synth [:gate 0]
              )

    (when continue-playing
      (let [rel-weights-ndx (rand-int (count rel-weights))
            inc-amt (inc (rand-int 3))
            new-rel-weights (assoc rel-weights
                                   rel-weights-ndx
                                   (+ (rel-weights rel-weights-ndx)
                                      inc-amt
                                      )
                                   )]
        (println new-rel-weights)
        (apply-at next-time
                  #'play-synth
                  [synth
                   new-rel-weights
                   (inc play-count)
                   times-to-play
                   attack]
                  )))
    )
  )

(defn many-synths
  [num-synths synth times-to-play group & extra-synth-params]
  (let [base-synth-params (list
                           :gate 0
                           :action NO-ACTION)
        synth-params (if extra-synth-params
                       (conj (flatten (conj base-synth-params
                                            extra-synth-params))
                             [:tail group])
                       (conj base-synth-params [:tail group])
                       )
        attack (or (:attack (apply hash-map extra-synth-params))
                   (rand 0.01))
        all-synths (for [s (range num-synths)]
                     (apply synth synth-params))
        ]
    (println synth-params)
    (for [synth all-synths] (apply-at (+ (now) (+ 500 (rand 2000)))
                                      #'play-synth
                                      [synth
                                       release-weights
                                       0
                                       times-to-play
                                       attack
                                       ]
                                  ))
    )
 )

(many-synths 15 spark3-sound 50 spark-early-g)
(many-synths 15 fmnt4a 20 fmnt-later-g :freq-bus pitch-bus :vol 0.2 :attack 2.0)
(stop)

(def fs (fmnt4a [:tail fmnt-later-g]
                :gate 1
                :action FREE
                :freq-bus pitch-bus
                :attack 2.0
                :vol 0.2))
(ctl fs :gate 1 :attack 0.01 :release 0.05 :action NO-ACTION)
(ctl fs :gate 0 :attack 0.01 :release 0.05 :action NO-ACTION)
