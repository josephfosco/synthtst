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

(def num-operators 3)
(def num-cntl-buses 10)

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

;; These indexes must match the order the control-buses are listed in the
;; "out:kr" statement of the cntl-synth
(def out-mod-lvl0-ndx 0)
(def out-mod-lvl1-ndx 1)
(def out-mod-lvl2-ndx 2)
(def out-mod-lvl3-ndx 3)
(def out-mod-lvl4-ndx 4)
(def out-mod-lvl5-ndx 5)
(def out-mod-lvl6-ndx 6)
(def out-mod-lvl7-ndx 7)
(def freq-ratio-ndx 8)
(def volume-ndx 9)

(defsynth cntl-synth
  [
   out-bus 0
   out-mod-lvl0 0
   out-mod-lvl1 0
   out-mod-lvl2 0
   out-mod-lvl3 0
   out-mod-lvl4 0
   out-mod-lvl5 0
   out-mod-lvl6 0
   out-mod-lvl7 0
   freq-ratio 1
   volume 1
   morph-time 1
   ]
  (when out-bus
    (let [fr (lag3:kr freq-ratio morph-time)
          o-ml0 (lag3:kr out-mod-lvl0 morph-time)
          o-ml1 (lag3:kr out-mod-lvl1 morph-time)
          o-ml2 (lag3:kr out-mod-lvl2 morph-time)
          o-ml3 (lag3:kr out-mod-lvl3 morph-time)
          o-ml4 (lag3:kr out-mod-lvl4 morph-time)
          o-ml5 (lag3:kr out-mod-lvl5 morph-time)
          o-ml6 (lag3:kr out-mod-lvl6 morph-time)
          o-ml7 (lag3:kr out-mod-lvl7 morph-time)
          vol (lag3:kr volume morph-time)
          ]
      (out:kr out-bus [o-ml0 o-ml1 o-ml2 o-ml3 o-ml4 o-ml5 o-ml6 o-ml7 fr vol])
      )
    )
  )

(def cntl-parms [
                {:out-mod-lvl0 0 :out-mod-lvl1 0 :freq-ratio 1 :vol 1}
                {:out-mod-lvl0 0 :out-mod-lvl1 0 :freq-ratio 7 :vol 0}
                {:out-mod-lvl0 500 :out-mod-lvl1 0 :freq-ratio 1.42 :vol 0}
                ])

(def cntl-synths
  (vec (for [i (range num-operators)]
         (let [parms (cntl-parms i)]
           (cntl-synth [:head fm-early-g]
                       ((cntl-buses i) 0)
                       (or (:out-mod-lvl0 parms) 0)
                       (or (:out-mod-lvl1 parms) 0)
                       (or (:out-mod-lvl2 parms) 0)
                       (or (:out-mod-lvl3 parms) 0)
                       (or (:out-mod-lvl4 parms) 0)
                       (or (:out-mod-lvl5 parms) 0)
                       (or (:out-mod-lvl6 parms) 0)
                       (or (:out-mod-lvl7 parms) 0)
                       (:freq-ratio parms)
                       (:vol parms)
                       ))
         ))
  )

(defsynth fm-oper
  [
   b-freq-bus 1
   in-mod-bus 3
   out-mod-bus 2
   out-mod-lvl-bus0 7
   out-mod-lvl-bus1 7
   out-mod-lvl-bus2 7
   out-mod-lvl-bus3 7
   out-mod-lvl-bus4 7
   out-mod-lvl-bus5 7
   out-mod-lvl-bus6 7
   out-mod-lvl-bus7 7
   freq-ratio-bus 6
   vol 1
   action NO-ACTION
   gate 0
   ]
  (let [envelope (env-gen (perc 5.0 5.0) gate 1 0 1 action)
        out-osc (* (sin-osc :freq (+ (* (in:kr b-freq-bus)
                                        (in:kr freq-ratio-bus))
                                     (in:ar in-mod-bus)))
                   envelope
                   )
        ]
    (out out-mod-bus
         [(* out-osc (in:kr out-mod-lvl-bus0))
          (* out-osc (in:kr out-mod-lvl-bus1))])
    (out main-audio-bus (* out-osc (in:kr vol)))
    ))

(def fm-voice
  (for [oper-id (range num-operators)]
    (fm-oper [:tail fm-early-g]
             :b-freq-bus base-freq-bus
             :in-mod-bus (feedback-buses oper-id)
             :out-mod-bus (fm-mod-buses 0)
             :out-mod-lvl-bus0 ((cntl-buses oper-id) out-mod-lvl0-ndx)
             :out-mod-lvl-bus1 ((cntl-buses oper-id) out-mod-lvl1-ndx)
             :freq-ratio-bus ((cntl-buses oper-id) freq-ratio-ndx)
             :vol ((cntl-buses oper-id) volume-ndx)

             )
    ))

(for [oper fm-voice]
  (ctl oper :gate 1 :action FREE)
  )

(ctl (cntl-synths 0) :freq-ratio 1)
(ctl (cntl-synths 0) :out-mod-lvl 0)
(ctl (cntl-synths 0) :volume 1)
(ctl (cntl-synths 1) :freq-ratio 2)
(ctl (cntl-synths 1) :out-mod-lvl0 200)
(ctl (cntl-synths 1) :volume 0)
(ctl (cntl-synths 2) :out-mod-lvl0 0)
(control-bus-get ((cntl-buses 0) 0))
(control-bus-get ((cntl-buses 0) 1))
(control-bus-get ((cntl-buses 0) 2))
(control-bus-get ((cntl-buses 1) 0))
(control-bus-get ((cntl-buses 1) 1))
(control-bus-get ((cntl-buses 1) 2))
(control-bus-set! base-freq-bus 110)
(stop)

(defsynth tsynth
  [freq 220]
  (let [envelope (env-gen (perc 1.0 1.0) 1 1 0 1 FREE)]
    (out [0 1]
           (* (sin-osc :freq freq) envelope)
           ))
  )

(def synthtest (tsynth :freq 110))

;;---------------------------------------------------------
(def test-list '(1 2 3 4))

(defmacro test-macro
  []
  (let [arg-list (list 1 2 3 4)]
     `(the-test ~@arg-list)
     ))
(:id base-freq-bus)

(defsynth tm-synth
  [freq-bus 1]
  (let [envelope (env-gen (perc 1.0 1.0) 1 1 0 1 FREE)]
    (out [0 1]
         (* (sin-osc :freq (in:kr freq-bus)) envelope)
           ))
  )

(def ms (tm-synth :freq-bus (:id base-freq-bus)))



;; (def tfreq-buses [(control-bus 1 "freq-bus-t0") (control-bus 1 "freq-bus-t1")])
;; (control-bus-set! (tfreq-buses 0) 110)
;; (control-bus-set! (tfreq-buses 1) 220)
;; (def tnum-out-buses 2)

;; (defmacro make-tm-synth
;;   [freq-buses]
;;   `(defsynth tm-synth
;;      [out-bus-num 0
;;       ~@(flatten (for [freq-bus-num (range 2)]
;;                    `(~(symbol (str "freq-bus" freq-bus-num)) ~freq-bus-num)))
;;       ]
;;      (let [envel# (env-gen (perc 1.0 1.0) 1 1 0 1 FREE)]
;;        (out out-bus-num
;;             [~(for [bus (range tnum-out-buses)]
;;                 `(* (sin-osc :freq (in:kr (:id ~(symbol (str "freq-bus" bus)))))
;;                     envel#))
;;              ]
;;             ))
;;      )
;;   )

;; (make-tm-synth)

;; (defsynth tmt-synth
;;   [synth-num 0
;;    out-bus-num 0
;;    freq-bus-num (:id (tfreq-buses 0))
;;    num-freq-buses (count tfreq-buses)
;;    ]
;;   (let [envel (env-gen (perc 1.0 1.0) 1 1 0 1 FREE)]
;;     (out out-bus-num
;;          [(* (sin-osc :freq
;;                       (in:kr (mod (+ freq-bus-num synth-num 0)
;;                                   num-freq-buses)))
;;              envel)
;;           (* (sin-osc :freq
;;                       (in:kr (mod (+ freq-bus-num synth-num 1)
;;                              num-freq-buses)))
;;              envel)
;;           ]
;;          ))
;;   )

;; ;; out put buses
;; ;; (+ (mod (+ synth_num bus_iteration 1) num_synths) first_bus_num)

;; (def tmt-s-inst
;;   (tmt-synth
;;    :out-bus-num 0
;;    :freq-bus-num (:id (tfreq-buses 0)))
;;   )

;;-----------------------------------------------------------------
;; TRY MULTI-CHANNEL CONTROL BUS
;;-----------------------------------------------------------------


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

(def num-operators 8)
(def num-cntl-buses 10)

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
         (control-bus num-cntl-buses (str opr "-"))
         ))
  )

(defsynth mod-lvls-synth
  [
   out-bus 0
   out-mod-lvl0 0
   out-mod-lvl1 0
   out-mod-lvl2 0
   out-mod-lvl3 0
   out-mod-lvl4 0
   out-mod-lvl5 0
   out-mod-lvl6 0
   out-mod-lvl7 0
   morph-time 1
   ]
  (let [
        o-ml0 (lag3:kr out-mod-lvl0 morph-time)
        o-ml1 (lag3:kr out-mod-lvl1 morph-time)
        o-ml2 (lag3:kr out-mod-lvl2 morph-time)
        o-ml3 (lag3:kr out-mod-lvl3 morph-time)
        o-ml4 (lag3:kr out-mod-lvl4 morph-time)
        o-ml5 (lag3:kr out-mod-lvl5 morph-time)
        o-ml6 (lag3:kr out-mod-lvl6 morph-time)
        o-ml7 (lag3:kr out-mod-lvl7 morph-time)
        ]
    (out:kr out-bus [o-ml0 o-ml1 o-ml2 o-ml3 o-ml4 o-ml5 o-ml6 o-ml7])
    )
  )

(defsynth cntl-synth
  [
   out-bus-num 0
   freq-ratio 1
   volume 1
   morph-time 1
   ]
  (let [
        fr (lag3:kr freq-ratio morph-time)
        vol (lag3:kr volume morph-time)
        ]
    (out:kr out-bus-num [fr vol])
    )
  )

(def cntl-parms [
                 {:out-mod-lvl0 0   :out-mod-lvl1 0
                  :out-mod-lvl2 0
                  :freq-ratio 1 :vol 1
                  }
                 {:out-mod-lvl0 500 :out-mod-lvl1 0
                  :out-mod-lvl2 0
                  :freq-ratio 2 :vol 0
                  }
                 {:out-mod-lvl0 0   :out-mod-lvl1 400
                  :out-mod-lvl2 0
                  :freq-ratio 3 :vol 0
                  }
                 {:out-mod-lvl0 0   :out-mod-lvl1 0
                  :out-mod-lvl2 300
                  :freq-ratio 4 :vol 0
                  }
                 {:out-mod-lvl0 0   :out-mod-lvl1 0
                  :out-mod-lvl2 0
                  :freq-ratio 5 :vol 0
                  }
                 {:out-mod-lvl0 0   :out-mod-lvl1 0
                  :out-mod-lvl2 0
                  :freq-ratio 6 :vol 0
                  }
                 {:out-mod-lvl0 0   :out-mod-lvl1 0
                  :out-mod-lvl2 0
                  :freq-ratio 7 :vol 0
                  }
                 {:out-mod-lvl0 0   :out-mod-lvl1 0
                  :out-mod-lvl2 0
                  :freq-ratio 8 :vol 0
                  }
                ])

; These indexes must match the order the control-buses are listed in the
;; "out:kr" statement of the cntl-synth
(def out-mod-lvl0-ndx 0)
(def out-mod-lvl1-ndx 1)
(def out-mod-lvl2-ndx 2)
(def freq-ratio-ndx 8)
(def volume-ndx 9)

(def mod-lvl-synths
  (vec (for [i (range num-operators)]
         (let [parms (cntl-parms i)]
           (mod-lvls-synth [:head fm-early-g]
                       (cntl-buses i)
                       (or (:out-mod-lvl0 parms) 0)
                       (or (:out-mod-lvl1 parms) 0)
                       (or (:out-mod-lvl2 parms) 0)
                       (or (:out-mod-lvl3 parms) 0)
                       (or (:out-mod-lvl4 parms) 0)
                       (or (:out-mod-lvl5 parms) 0)
                       (or (:out-mod-lvl6 parms) 0)
                       (or (:out-mod-lvl7 parms) 0)
                       ))
         ))
  )

(def cntl-synths
  (vec (for [i (range num-operators)]
         (let [parms (cntl-parms i)
               cntl-bus-num (+ (:id (cntl-buses i)) freq-ratio-ndx)]
           (cntl-synth [:head fm-early-g]
                       cntl-bus-num
                       (:freq-ratio parms)
                       (:vol parms)
                       ))
         ))
  )

(defsynth fm-oper
  [
   in-mod-bus 3
   out-mod-bus 2
   cntl-bus 7
   action NO-ACTION
   gate 0
   ]
  (let [
        [mod-lvl0
         mod-lvl1
         mod-lvl2
         mod-lvl3
         mod-lvl4
         mod-lvl5
         mod-lvl6
         mod-lvl7
         freq-ratio
         vol
         ] (in:kr cntl-bus num-cntl-buses)
        envelope (env-gen (perc 5.0 5.0) gate 1 0 1 action)
        out-osc (* (sin-osc :freq (+ (* (in:kr base-freq-bus) freq-ratio)
                                     (in:ar in-mod-bus)))
                   envelope
                   )
        ]
    (out:ar out-mod-bus
            [
             (* out-osc mod-lvl0)
             (* out-osc mod-lvl1)
             (* out-osc mod-lvl2)
             ])
    (out:ar main-audio-bus (* out-osc vol))
    ))

(def fm-voice
  (for [oper-id (range num-operators)]
      (fm-oper [:tail fm-early-g]
               :in-mod-bus (feedback-buses oper-id)
               :out-mod-bus (fm-mod-buses 0)
               :cntl-bus (cntl-buses oper-id)
               )
    ))

(doseq [oper fm-voice]
  (ctl oper :gate 1 :action FREE)
  )

(ctl (cntl-synths 0) :freq-ratio 1)
(ctl (cntl-synths 0) :out-mod-lvl 0)
(ctl (cntl-synths 0) :volume 1)
(ctl (cntl-synths 1) :freq-ratio 1.42)
(ctl (cntl-synths 1) :out-mod-lvl0 500)
(ctl (cntl-synths 1) :volume 0)
(ctl (mod-lvl-synths 2) :out-mod-lvl1 0.0)
(control-bus-get ((cntl-buses 0) 0))
(control-bus-get ((cntl-buses 0) 1))
(control-bus-get ((cntl-buses 0) 2))
(control-bus-get ((cntl-buses 1) 0))
(control-bus-get ((cntl-buses 1) 1))
(control-bus-get ((cntl-buses 1) 2))
(control-bus-set! base-freq-bus 110)
(stop)
