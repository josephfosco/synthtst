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

(ns synthtst.fmnt-synth
  (:require
   [overtone.live :refer :all]
   )
  )

(definst fmnt [freq 440 bw 200.0
               attack 0.1 decay 0.2 sustain 0.6 release 0.7 vol 0.3]
  (let [cfreq (* freq 1.37)
        env-impulse (impulse 0.5)
        env-ff (toggle-ff env-impulse)
        env-gate (gate env-ff env-impulse)
        env-generator (env-gen (env-adsr attack decay sustain release) env-gate 1 0 1)
        ]
    (* env-generator
       (formant freq
                (+ cfreq (* (- env-generator 0.5) 1000))
                (+ bw (* env-generator 500)))
       vol
       )
    )
  )

(def f1 (fmnt))
(def f1 (fmnt :attack 0.01 :decay 0.1 :sustain 0.0))
(stop)

(definst fmnt2 [freq 440 cfreq 880 bw 200.0 gate-val 0
                attack 0.05 release 0.3 vol 0.3]
  (let [env-generator (env-gen (perc attack release) gate-val 1 0 1 NO-ACTION)
        ]
    (* env-generator
       ;;(formant freq cfreq (* bw (- env-generator 0.5)))
       (formant freq cfreq bw)
       vol
       )
    )
  )

(def f2 (fmnt2))
(ctl fmnt2 :gate-val 1)
(ctl fmnt2 :gate-val 0)
(stop)

(definst fmnt3 [freq 440 cfreq 880 bw 200.0 gate-val 0
                attack 0.1 sustain 0.6 release 0.7  vol 0.3]
  (let [env-generator (env-gen (env-asr attack sustain release) gate-val 1 0 1 FREE)
        ]
    (* env-generator
       ;;(formant freq cfreq (* bw (- env-generator 0.5)))
       (formant (+ 1060 (* 1000 (lf-noise0:kr 1)))
                (+ 800 (* 1000 (lf-noise1:kr 20)))
                (+ 510 (* 500 (lf-noise0:kr 10))))
       vol
       )
    )
  )

(def f3 (fmnt3))
(ctl fmnt3 :gate-val 1)
(ctl fmnt3 :gate-val 0)
(stop)

;;; using a bus for base pitch

(defonce fmnt-main-g (group "fmnt-main"))
(defonce fmnt-early-g (group "fmnt early" :head fmnt-main-g))
(defonce fmnt-later-g (group "fmnt later" :after fmnt-early-g))

(defonce pitch-bus (control-bus))

(defsynth pitch-osc
  [out-bus 0 freq 0.25 min-pitch 100 max-pitch 2000]
  (out:kr out-bus (lin-exp (lf-noise1:kr freq) -1 1 min-pitch max-pitch ))
  )

(def pitch-cntl (pitch-osc :out-bus pitch-bus :freq 2.0))
(ctl pitch-cntl :max-pitch 1000)

;; This synth does not work though I thought it would,
;; but it makes some interesting sounds (esp when used
;; with the release=2.0)
;; for future investigation
(definst fmnt4 [freq-bus 0 cfreq 880 bw 200.0
                min-cfreq -75 max-cfreq 75 freq-cfreq 100
                min-bw 10 max-bw 500 freq-bw 50
                attack 0.01 release 0.05
                vol 1]

  (let [env-impulse (impulse (range-lin (lf-noise0:kr 0.5)
                                        (+ release 3.0)
                                        (+ release 5.0)))
        env-ff (toggle-ff env-impulse)
        env-gate (gate env-ff env-impulse)
        envelope-generator (env-gen (perc attack release) env-gate 1 0 1)
        pitch (+ (in:kr freq-bus) (range-lin (lf-noise0:kr 1) -50 50))
        ]
    (* (formant pitch
                (+ pitch (range-lin (lf-noise1:kr freq-cfreq)
                                    min-cfreq
                                    max-cfreq))
                (range-lin (lf-noise1:kr freq-bw) min-bw max-bw))
       envelope-generator
       vol
       )
    )
  )

(def f4 (fmnt4))
(ctl f4 :release 2.0)
(stop)

(defonce impulse-bus (control-bus))

(def base-attack 0.01)
(def base-release 0.05)

(defsynth pulses
  [out-bus 2 attack base-attack release base-release]
  (out:kr out-bus
          (impulse (range-lin (lf-noise0:kr 0.5)
                              (/ 1 (/ (+ 0.5 attack release) 2))
                              (/ 1 (/ (+ 3 attack release) 2))
                              )
                   )
          )
  )

(def impulse-cntl (pulses [:tail fmnt-early-g]
                          :out-bus impulse-bus
                          :release 0.05))

(defsynth fmnt4a [freq-bus 0 cfreq 880
                 min-cfreq -75 max-cfreq 75 freq-cfreq 100
                 min-bw 10 max-bw 500 freq-bw 50
                 attack base-attack release base-release
                 vol 1]

  (out [0 1]
       (let [env-ff (toggle-ff (in:kr impulse-bus))
             env-gate (gate env-ff (delay1:kr (in:kr impulse-bus)))
             envelope-generator (env-gen (perc attack release)
                                         env-gate 1 0 1 NO-ACTION)
             pitch (latch (+ (in:kr freq-bus)
                             (range-lin (lf-noise0:kr 1) -50 50))
                          env-gate
                          )
             ]
         (* (formant pitch
                     (+ pitch (range-lin (lf-noise1:kr freq-cfreq)
                                         min-cfreq
                                         max-cfreq))
                     (range-lin (lf-noise1:kr freq-bw) min-bw max-bw))
            envelope-generator
            (* 0.2 vol)
            )
         ))
  )

(def f4a (fmnt4a [:tail fmnt-later-g] :freq-bus pitch-bus))
(do
  (let [release 2.00]
    (ctl f4a :release release)
    (ctl impulse-cntl :release release)
    ))
(do
  (let [attack 5.00]
    (ctl f4a :attack attack)
    (ctl impulse-cntl :attack attack)
    ))
(stop)

(definst fmnt5 [freq-bus 0 cfreq 880 bw 200.0
                min-cfreq -75 max-cfreq 75 freq-cfreq 100
                min-bw 10 max-bw 500 freq-bw 50
                attack 0.0 release 0.001
                vol 1]

  (let [env-impulse (impulse (range-lin (lf-noise0:kr 0.5) 3.0 5.0))
        env-ff (toggle-ff env-impulse)
        env-gate (gate env-ff env-impulse)
        envelope-generator (env-gen (perc attack release) env-gate 1 0 1)
        pitch (+ (gate (in:kr freq-bus) (pulse-divider env-impulse 2 2))
                 (range-lin (lf-noise0:kr 0.25) -50 50))
        ]
    (* (formant pitch
                pitch
                300
                )
       envelope-generator
       vol
       )
    )
  )

(def f5 (fmnt5))
(ctl f5 :attack 0.0)
(ctl f5 :release 2.0)
(stop)

(definst fmnt6 [freq-bus 0 cfreq 880 bw 200.0
                max-impulse 3.0 min-impulse 5.0
                max-cfreq 2000 freq-cfreq 11
                min-bw 10 max-bw 1000 freq-bw 10
                attack 0.0 release 0.001
                vol 1]
  (let [env-impulse (impulse (range-lin (lf-noise0:kr 0.5) max-impulse min-impulse))
        env-ff (toggle-ff env-impulse)
        env-gate (gate env-ff env-impulse)
        envelope-generator (env-gen (perc attack release) env-gate 1 0 1)
        pitch-base (gate (in:kr freq-bus) (pulse-divider env-impulse 2 2))
        pitch-offset (range-lin (lf-noise0:kr 0.25)
                                (* (/ pitch-base 4) -1)
                                (/ pitch-base 4))
        pitch (+ pitch-base pitch-offset)
        ]
    (* (formant pitch
                (+ pitch (range-lin (lf-noise1:kr freq-cfreq)
                                    (* (/ pitch-base 2) -1)
                                    max-cfreq))
                (range-lin (lf-noise1:kr freq-bw) min-bw max-bw)
                )
       envelope-generator
       vol
       )
    )
  )

(def f6 (fmnt6))
(stop)

(def synths (atom ()))

(defn make-synth
  "creates fmnt5 synths and adds it to synths"
  [synth]
  (reset! synths (conj @synths (synth))
          )
  )

(defn make-synths
  [synth & {:keys [t cnt] :or {t (now) cnt 0}}]
  (let [next-t (+ t 200)]
    (make-synth synth)
    (if (< cnt 7) (apply-at next-t
                            #'make-synths
                            [synth :t next-t :cnt (inc cnt)]))
    )
  )

(make-synths fmnt5)
(stop)

(defn set-synth-val
  [parm val]
  (dorun (map ctl @synths (repeat parm) (repeat val)))
  )

(defn change-synth-val
  [t t-inc parm p-val p-inc p-max]
  (set-synth-val parm p-val)
  (let [next-t (+ t (* t-inc 1000))]
    (if (or (and (pos? p-inc) (< p-val p-max))
            (and (neg? p-inc) (> p-val p-max)))
      (do
        (println "p-val " p-val "p-max " p-max (> p-val p-max))
        (apply-at next-t
                  #'change-synth-val
                  [next-t t-inc parm (+ p-val p-inc) p-inc p-max])
        )
      )
    )
  )

(def synth2 (fmnt2))

(defn play
  [p]
  (let [pitch (+ 28 (rand-int 100))
        freq (midi->hz p)
        cfreq (if (< pitch 80) (+ freq 500) 400)
        bw 800
        ]
    (println p)
    (ctl synth2 :freq freq :cfreq freq :gate-val 1 :bw bw)
    (apply-at (+ (now) 50) ctl [synth2 :gate-val 0])
    )
  )

(play 67)

(defn playl
  [t pitch]
  (play pitch)
  (let [next-t (+ t 500)
        next-p (if (< pitch 100) (+ pitch 1) 28)]
    (apply-at next-t #'playl [next-t next-p]
    )
    ))

(playl 0 45)
(stop)


(definst fmnt-rand [freq 440 bw 200.0
                    attack 0.1 decay 0.2 sustain 0.6 release 0.7 vol 0.3
                    impulse-rate 0.5 rand-range 400]
  (let [cfreq (* freq 1.37)
        env-impulse (impulse (* 2 impulse-rate))
        env-ff (toggle-ff env-impulse)
        env-gate (gate env-ff env-impulse)
        env-generator (env-gen (env-adsr attack decay sustain release) env-gate 1 0 1)
        freq-bus (control-bus)
        ]
    (out freq-bus (+ (* rand-range (latch (lf-noise0:kr) env-impulse)) freq))
    (* env-generator
       (formant (in:kr freq-bus)
                (+ (* (in freq-bus) 1.37) (* (- env-generator 0.5) 1000))
                (+ bw (* env-generator 500)))
       vol
       )
    )
  )

(def f1 (fmnt-rand :impulse-rate 2))
(def f1 (fmnt-rand
         :freq 100
         :attack 0.05
         :decay 0.1
         :sustain 0.6
         :release 0.1
         :impulse-rate 25
         :rand-range 5000))
(ctl f1 :impulse-rate 1)
(stop)
