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

(defonce main-audio-bus (audio-bus 1 "fm-audio-bus"))

(defsynth main-out-synth
  []
  (out [0 1]
       (limiter (in main-audio-bus) 0.9 0.01)
       )
  )

;; (def fm-main-out (main-out-synth [:tail fm-later-g]))

;;-----------------------------------------------------------------

(defsynth fm-out-bus
  [freq 880 vol 1]
  (out main-audio-bus
       (* (sin-osc :freq freq)
          vol
          )
         )
  )


(def fm-mod-out-inst (fm-out-bus [:tail fm-early-g] :freq 110))
(stop)

(dotimes [i num-oper]
  (fm-out-bus [:tail fm-later-g] :freq (* base-freq i) :vol (* (/ 1 num-oper)
                                                            (/ 1 (inc i))))
  )

(defsynth fm-synth
  [freq1 440 freq2 440 fm-amt 50 vol 1]
  (let [envelope (env-gen (perc 3.0 3.0 1) 1 1 0 1 FREE)]
      (out fm-audio-bus
           (*
            (sin-osc :freq (+ freq1
                              (* (sin-osc :freq freq2) fm-amt envelope)))
            (* envelope vol)
            )
           ))
  )


(def fm-synth-inst (fm-synth [:tail fm-later-g] :freq1 110 :freq2 1760))
(stop)
(ctl fm-synth-inst :freq2 30)
(ctl fm-synth-inst :freq1 220)
(ctl fm-synth-inst :fm-amt 100)

;;-----------------------------------------------------------------

(def fm-main-out (main-out-synth [:tail fm-later-g]))

(def gl-base-freq 110)

(defonce fm-mod-bus1 (audio-bus 1 "fm-mod-bus1"))
(defonce feedback-bus1 (audio-bus 1 "feedback-bus1"))
(defonce fm-mod-bus2 (audio-bus 1 "fm-mod-bus2"))
(defonce feedback-bus2 (audio-bus 1 "feedback-bus2"))

(defsynth feedback-synth1
  [inbus 3 outbus 3]
  (let [input (in-feedback:ar inbus)]
    (out:ar outbus input)
    ))

(def feedback1 (feedback-synth1 [:head fm-early-g]
                                :inbus fm-mod-bus1
                                :outbus feedback-bus1))
(def feedback2 (feedback-synth1 [:head fm-early-g]
                                :inbus fm-mod-bus2
                                :outbus feedback-bus2)
  )
(stop)

(defsynth fm-oper1
  [
   base-freq 110
   freq-ratio 1
   in-mod-bus 3
   out-mod-bus 1
   out-mod-bus-lvl 0.5
   vol 1
   action NO-ACTION
   gate 0
   ]
  (let [envelope (env-gen (perc 3.0 3.0) gate 1 0 1 action)
        out-osc (* (sin-osc :freq (+ (* base-freq freq-ratio)
                                     (in:ar in-mod-bus)))
                   (* envelope 1)
                   )
        ]
    (out out-mod-bus (* out-osc out-mod-bus-lvl))
    (out main-audio-bus (* out-osc vol))
    ))


(def oper1a (fm-oper1 [:tail fm-early-g]
                     :base-freq gl-base-freq
                     :freq-ratio 1
                     :in-mod-bus feedback-bus2
                     :out-mod-bus fm-mod-bus1
                     :out-mod-bus-lvl 1
                     :vol 1
                     ))
(def oper2a (fm-oper1 [:tail fm-early-g]
                     :base-freq gl-base-freq
                     :freq-ratio 2.0
                     :in-mod-bus feedback-bus1
                     :out-mod-bus fm-mod-bus2
                     :out-mod-bus-lvl 500
                     :vol 0
                     ))
(do
  (ctl oper1a :gate 1 :action FREE)
  (ctl oper2a :gate 1 :action FREE)
  )

(ctl oper1 :gate 1 :action FREE)

(stop)

(def testo (sin-osc))

;;---------------------------------------------------------
(defonce fm-main-g (group "fm-main"))
(defonce fm-early-g (group "fm early" :head fm-main-g))
(defonce fm-later-g (group "fm later" :after fm-early-g))

(defonce main-audio-bus (audio-bus 1 "fm-audio-bus"))

(defsynth main-out-synth
  []
  (out [0 1]
       (limiter (in main-audio-bus) 0.9 0.01)
       )
  )

(def fm-main-out (main-out-synth [:tail fm-later-g]))

(def num-operators 2)
(def num-cntl-buses 3)

(defonce fm-mod-buses (vec (for [i (range num-operators)]
                              (audio-bus 1 (str "fm-mod-bus" i)))))
(defonce feedback-buses (vec (for [i (range num-operators)]
                                (audio-bus 1 (str "feedback-bus" i)))))

(defsynth feedback-synth
  [inbus 3 outbus 3]
  (let [input (in-feedback:ar inbus)]
    (out:ar outbus input)
    ))

;; feedback-synths move audio data from the fm-mod-busses to the
;; feedback-busses. This is done so that all operators can access the output
;; of all other operators (on the feedback-busses). Without this, an
;; individual operator would only be able to use the output of operators
;; defined after it was. This works because feedback-synth uses the
;; in-feedback ugen.
(def feedback-synths (vec (for [i (range num-operators)]
                        (feedback-synth [:head fm-early-g]
                                        :inbus (fm-mod-buses i)
                                        :outbus (feedback-buses i))
                        )))

(defonce base-freq-bus (control-bus 1 "base-freq-bus"))
(control-bus-set! base-freq-bus 110)

;; Creates a vector of num-operators vectors with each internal vector
;; having num-cntl-buses control-buses
(def cntl-buses
  (vec (for [opr (range num-operators)]
         (vec (for [c-bus (range num-cntl-buses)]
                (control-bus)))
         ))
  )

(defsynth cntl-synth
  [
   out-bus 0
   freq-ratio 1
   out-mod-lvl 0.5
   volume 1
   morph-time 1
   ]
  (let [fr (lag3:kr freq-ratio morph-time)
        o-ml (lag3:kr out-mod-lvl morph-time)
        vol (lag3:kr volume morph-time)
        ]
    (out:kr out-bus [fr o-ml vol])
    )
  )

(def cntl-parms [
                {:freq-ratio 1 :out-mod-lvl 1 :vol 1}
                {:freq-ratio 2.0 :out-mod-lvl 500 :vol 0}
                ])

(def cntl-synths
  (vec (for [i (range num-operators)]
         (let [parms (cntl-parms i)]
           (cntl-synth [:head fm-early-g]
                       ((cntl-buses i) 0)
                       (:freq-ratio parms)
                       (:out-mod-lvl parms)
                       (:vol parms)
                       ))
         ))
  )

(defsynth fm-oper
  [
   b-freq-bus 1
   freq-ratio-bus 6
   in-mod-bus 3
   out-mod-bus 2
   out-mod-lvl-bus 7
   vol 1
   action NO-ACTION
   gate 0
   ]
  (let [envelope (env-gen (perc 3.0 3.0) gate 1 0 1 action)
        out-osc (* (sin-osc :freq (+ (* (in:kr b-freq-bus)
                                        (in:kr freq-ratio-bus))
                                     (in:ar in-mod-bus)))
                   envelope
                   )
        ]
    (out out-mod-bus (* out-osc (in:kr out-mod-lvl-bus)))
    (out main-audio-bus (* out-osc (in:kr vol)))
    ))

(def oper1 (fm-oper [:tail fm-early-g]
                    :b-freq-bus base-freq-bus
                    :freq-ratio-bus ((cntl-buses 0) 0)
                    :in-mod-bus (feedback-buses 1)
                    :out-mod-bus (fm-mod-buses 0)
                    :out-mod-lvl-bus ((cntl-buses 0) 1)
                    :vol ((cntl-buses 0) 2)
                    ))
(def oper2 (fm-oper [:tail fm-early-g]
                    :b-freq-bus base-freq-bus
                    :freq-ratio-bus ((cntl-buses 1) 0)
                    :in-mod-bus (feedback-buses 0)
                    :out-mod-bus (fm-mod-buses 1)
                    :out-mod-lvl-bus ((cntl-buses 1) 1)
                    :vol ((cntl-buses 1) 2)
                    ))

(do
  (ctl oper1 :gate 1 :action FREE)
  (ctl oper2 :gate 1 :action FREE)
  )

(ctl (cntl-synths 0) :freq-ratio 1)
(ctl (cntl-synths 0) :out-mod-lvl 0)
(ctl (cntl-synths 0) :volume 1)
(ctl (cntl-synths 1) :freq-ratio 0.01)
(ctl (cntl-synths 1) :out-mod-lvl 500)
(ctl (cntl-synths 1) :volume 0)
(control-bus-get ((cntl-buses 0) 0))
(control-bus-get ((cntl-buses 0) 1))
(control-bus-get ((cntl-buses 0) 2))
(control-bus-get ((cntl-buses 1) 0))
(control-bus-get ((cntl-buses 1) 1))
(control-bus-get ((cntl-buses 1) 2))
(control-bus-set! base-freq-bus 110)
(stop)

(defsynth tsynth
  []
  (let [envelope (env-gen (perc 1.0 1.0) 1 1 0 1 FREE)]
    (out main-audio-bus
           (* (sin-osc :freq (in:kr base-freq-bus)) envelope)
           ))
  )

(def synthtest (tsynth [:tail fm-early-g]))

(defsynth tsynth2
  []
  (let [envelope (env-gen (perc 1.0 1.0) 1 1 0 1 FREE)
        osc1 (* (sin-osc :freq 440) envelope)
        osc2 (* (sin-osc :freq 110) envelope)
        ]
    (out 0 [osc1 osc2]
         ))
  )

(def synthtest2 (tsynth2 [:tail fm-early-g]))

;;---------------------------------------------------------
