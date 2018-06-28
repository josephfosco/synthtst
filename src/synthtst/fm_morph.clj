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

(ns synthtst.fm-morph
  (:require
   [overtone.live :refer :all]
   )
  )

(defonce fm-main-g (group "fm-main"))
(defonce fm-early-g (group "fm early" :head fm-main-g))
(defonce fm-later-g (group "fm later" :after fm-early-g))

(defonce fm-audio-bus (audio-bus 1 "fm-audio-bus"))
(defonce fm-ctl-bus (control-bus 1 "fm-ctl-bus"))

(defsynth fm-bus-synth
  []
  (out [0 1]
       (limiter (in fm-audio-bus) 0.9 0.01)
       )
  )

(def fm-main-out (fm-bus-synth))

;; (defsynth pitch-osc
;;   [out-bus 0 freq 0.25 min-pitch 100 max-pitch 2000]
;;   (out:kr out-bus (lin-exp (lf-noise1:kr freq) -1 1 min-pitch max-pitch ))
;;   )

(defsynth fm-oper
  [freq 440 vol 1]
  (out fm-audio-bus
       (* (sin-osc :freq freq)
          vol
          )
         )
  )


(def fm-oper-inst (fm-oper [:tail fm-later-g] :freq 110))
(stop)

(def base-freq 110)
(def num-oper 8)
(dotimes [i num-oper]
  (fm-oper [:tail fm-later-g] :freq (* base-freq i) :vol (* (/ 1 num-oper)
                                                            (/ 1 (inc i))))
  )

(defsynth fm-synth
  [freq1 440 freq2 440 fm-amt 10 vol 1]
  (out fm-audio-bus
       (*
        (sin-osc :freq (+ freq1
                          (* (sin-osc :freq freq2) fm-amt)))
        vol
        )
       )
  )


(def fm-synth-inst (fm-synth [:tail fm-later-g] :freq1 110 :freq2 1))
(stop)
(ctl fm-synth-inst :freq2 30)
(ctl fm-synth-inst :freq1 220)
(ctl fm-synth-inst :fm-amt 100)






(def pitch-cntl (pitch-osc [:tail fmnt-early-g] :out-bus pitch-bus :freq 2.0))
(ctl pitch-cntl :max-pitch 400)
(ctl pitch-cntl :min-pitch 100)


(def spk (spark3-sound [:tail spark-early-g] :release 0.1 :gate 1 :action FREE))
(ctl spk :gate 0)
(ctl spk :gate 1)
(stop)

(defn play-synth
  [synth rel-weights play-count times-to-play atck]
  (let  [continue-playing (< play-count times-to-play)
         ratio-done (/ play-count times-to-play)
         attack (if atck
                  atck
                  (* (rand) (/ (inc (weighted-choice attack-weights))
                                 (cond
                                   (< ratio-done 0.2) 10000
                                   (< ratio-done 0.4) 1000
                                   (< ratio-done 0.6) 100
                                   (< ratio-done 0.8) 10
                                   (<= ratio-done 1) 2
                                   )
                                 ;; 0.001
                                 ;; (if (< (/ play-count times-to-play) 0.5)
                                 ;;   (max 1000 0.001)
                                 ;;   (if (> (rand) 0.5) 2 10))
                                 )))
         array-step (/ times-to-play (dec (count max-release-weights)))
         max-release-min-base-ndx (int (/ play-count array-step))
         max-release-min-ndx (- max-release-min-base-ndx
                                (rand-int (inc max-release-min-base-ndx)))
         max-release (* (max-release-weights max-release-min-ndx)
                        (inc (weighted-choice rel-weights)))
         release (+ 0.001 (rand max-release))
         action (if continue-playing NO-ACTION FREE)
         release-time (+ (now) (int (* (+ attack release) 1000)) 100)
         next-time (+ release-time 250 (rand-int 2000))
         ]
    (when (not continue-playing) (println "STOPPING !!!!!!!!!"))
    ;; (println continue-playing play-count times-to-play)

    (ctl synth
         :gate 1
         :attack attack
         :release release
         :action action
         )
    (apply-at release-time
              #'ctl synth [:gate 0]
              )
    (println attack release)
    (when continue-playing
      (let [rel-weights-ndx (rand-int (count rel-weights))
            inc-amt (inc (rand-int 3))
            new-rel-weights (assoc rel-weights
                                   rel-weights-ndx
                                   (+ (rel-weights rel-weights-ndx)
                                      inc-amt
                                      )
                                   )]
        ;; (println new-rel-weights)
        (apply-at next-time
                  #'play-synth
                  [synth
                   new-rel-weights
                   (inc play-count)
                   times-to-play
                   atck]
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
        attack (:attack (apply hash-map extra-synth-params))
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

(many-synths 25 spark3-sound 25 spark-early-g)
(many-synths 25 fmnt4a 25 fmnt-later-g :freq-bus pitch-bus :vol 0.2)
(stop)


;;------------------------------------------------------------------

;; BUS TEST - defining and reading multiple busses at once

(defonce ctl-bus (control-bus "ctl-bus"))

(defsynth ctl-syn
  [freq 440 vol 0.2]
  (out:kr ctl-bus [freq vol])
  )

(def c-syn (ctl-syn))
(ctl c-syn :freq 220)
(ctl c-syn :vol 0.2)

(defsynth bus-tst
  [cbus ctl-bus]
  (out [0 1]
       (let [[c-freq c-vol] (in:kr cbus 2)]
         ;; (* (sin-osc :freq c-freq) c-vol)
         (* (sin-osc :freq c-freq) c-vol)
         )
       )
  )

(def bt (bus-tst))
(stop)
