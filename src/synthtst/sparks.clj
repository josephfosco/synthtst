;    Copyright (C) 2018  Joseph Fosco. All Rights Reserved
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

(ns synthtst.sparks
  (:require
   [overtone.live :refer :all]
   )
  )


(defonce spark-main-g (group "spark-main"))
(defonce spark-early-g (group "spark early" :head spark-main-g))
(defonce spark-later-g (group "spark later" :after spark-early-g))

(defonce spark3-bus (audio-bus 1 "spark-bus"))

(defsynth spark3-synth
  []
  (out [0 1]
       ;; (compander (in spark3-bus) (in spark3-bus) 0.5 1.0 0.5 0.01 0.05)
       (limiter (in spark3-bus) 0.9 0.01)
       )
  )

(def spark3-synth-inst (spark3-synth [:tail spark-later-g]))


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
                (latch:ar (+ (/ (* (- cutoff-max cutoff-min)
                                   (- (lf-noise1:kr 20) -1))
                                (- 1 -1)) cutoff-min)
                          (lf-noise1:kr 150))
                (/ (+ 1 (lf-noise1:kr 12)) 2) ;; scale between 0 an 1
                )
               1.0
               0.01
               ))
             )
         )
    )
  )

(def spk (spark3-sound [:tail spark-early-g] :release 0.1 :gate 1 :action FREE))
(ctl spk :gate 0)
(ctl spk :gate 1)

(defn play-synth
  [synth play-again]
  ;; (println "play-spark ****")
  (when (not play-again) (println "STOPPING !!!!!!!!!"))
  (let  [attack 0.001
         release (+ 0.001 (rand 0.039))
         action (if play-again NO-ACTION FREE)
         release-time (+ (now) (int (* (+ attack release) 1000)) 100)
         continue-playing (if (< (rand) 0.9) true false)
         next-time (+ release-time 250 (rand-int 800))
         ]
    (ctl synth
         :gate 1
         :attack attack
         :release release
         :action action
         )
    (apply-at release-time
              #'ctl synth [:gate 0]
              )

    (when play-again
      (apply-at next-time
                #'play-synth
                synth
                continue-playing []
                )
      )
    )
  )

(defn many-synths
  [num-synths synth]
  (let [all-synths (for [s (range num-synths)]
                     (synth [:tail spark-early-g] :gate 0 :action NO-ACTION))
        ]
    (for [s all-synths] (apply-at (+ (now) (+ 500 (rand 2000)))
                                  #'play-synth
                                  s true []
                                  ))
    )
 )

(many-synths 30 spark3-sound)
(stop)
