;    Copyright (C) 2016  Joseph Fosco. All Rights Reserved
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

(ns synthtst.loop-synths
  (:require
   [overtone.live :refer :all]
   )
  )

(definst low-freq
  [freq 70 vol 2 attack 1.5 sustain 1.0 release 3.5 gate 1.0 action FREE]
  (-> (* (sin-osc:kr freq 0 0.25 1.25) vol)
      (+ (* (lf-tri (* freq 1.5)) (/ vol 6)) (* (lf-tri (* freq 1.05)) (/ vol 2)))
      (* (env-gen (asr attack sustain release) gate 1 0 1 action))

      )
  )

(def lfs (low-freq 120))
(ctl lfs :gate 0)
(stop)


(definst pulse-mod
  [freq 70 vol 1 attack 1.5 sustain 1.0 release 3.5 gate 1.0 action FREE]
  (let [pw (+ (* (sin-osc:kr 0.1) 0.25) 0.5)]
    (-> (* (pulse (* freq 1.05) pw) vol)
        (* (env-gen (asr attack sustain release) gate 1 0 1 action))
        ))
  )

(def pmo (pulse-mod 300))
(ctl pmo :gate 0)

(definst low-freq2
  [freq 70 vol 2 attack 2 sustain 1.0 release 10 gate 1.0 action FREE]
  (let [pw (+ (* (sin-osc:kr 0.1) 0.25) 0.5)
        eg(env-gen (perc attack release) gate 1 0 1 action)
        ]
    (-> (lpf
         (-> (* (sin-osc freq) (* vol 1))
             (+ (* (square (* freq 2.01)) (/ vol 6.5))
                (* (pulse (* freq 1.02) pw) (/ vol 2)))
             )
         (+ (* eg 500) 100)
         )
        (* eg)

             )
    )
  )

(def lfs2 (low-freq2 100))
(ctl lfs2 :gate 0)
(stop)

(definst low-freq3
  [freq 70 vol 1 attack 2 sustain 1.0 release 10 gate 1.0 action FREE]
  (let [eg(env-gen (perc attack release) gate 1 0 1 action)
        ]
    ;; (-> (lpf
    ;;      (-> (* (sin-osc freq) (* vol 1))
    ;;          (+ (* (square (* freq 2.01)) (/ vol 6.5))
    ;;             (* (pulse (* freq 1.02) pw) (/ vol 2)))
    ;;          )
    ;;      (+ (* eg 500) 100)
    ;;      )
    ;;     (* eg)
    ;;     )
    (->
        (-> (* (pm-osc 100 600 10) (* vol 1))
            ;; (+ (* (square (* freq 2.01)) (/ vol 6.5))
            ;;    (* (pulse (* freq 1.02) pw) (/ vol 2)))
            )
        (* eg)
        )
    )
  )

(def lfs3 (low-freq3 400))
(ctl lfs3 :gate 0)
(stop)

(definst low-freq-am
  [freq 70 vol 1 attack 0.15 sustain 1.0 release 10 gate 1.0]
  (let[eg  (env-gen (perc attack release :curve [-3 -2]) gate 1 0 1 FREE)
       attack-eg (env-gen (perc 0.3 2.0) gate 0.3 0 1 NO-ACTION)
       noise-eg (env-gen (perc 0.1 10 :curve [-4 -2]) gate 1 0 1 NO-ACTION)
       w-mod (* (+ (sin-osc:kr 0.2 1) 1) 0.5)
       mdltr (+ (* (var-saw 100 :width w-mod) (* vol 1)))
       ]
    (+ (-> (lpf
            (-> (* (square freq) vol)
                (* mdltr)
                )
            (+ (* eg 900) 100))
           (* eg)
           )
       (* (sin-osc freq) attack-eg)
       (-> (hpf
            (* (pink-noise) noise-eg 0.025)
            (+ (+ (* (+ (* noise-eg -1) 1) 1500) 20)
               (* (* (+ 1 (lf-noise1:kr 6)) 100) noise-eg)
               )
             )
           )
       )
    ))

(def lfam (low-freq-am 150))
(ctl lfam :gate 0)
(stop)

(definst low-freq-n
  [freq 70 vol 1 attack 0.2 sustain 1.0 release 10 gate 1.0]
  (let[eg  (env-gen (perc attack release) gate 1 0 1 FREE)
       attack-eg (env-gen (perc 0.05 0.1) gate 1 0 1 NO-ACTION)
       noise-eg (env-gen (perc 1.5 7 :curve [-7 4] ) gate 1 0 1 NO-ACTION)
       w-mod (* (+ (sin-osc:kr 0.2 1) 1) 0.5)
       mdltr (+ (* (var-saw 100 :width w-mod) (* vol 1)))
       ]
    (+ (-> (resonz
            (* (pink-noise) noise-eg 1)
            (+ (+ (* noise-eg 1500) 200)
               (* (* (+ 1 (lf-noise1:kr 2)) 300) noise-eg)
               )
            0.2
            )
           )
       )
    ))

(def lfn (low-freq-n 150))

(definst hp-n
  [freq 70 vol 1 attack 0.2 sustain 1.0 release 10 gate 1.0]
  (let[eg  (env-gen (perc attack release) gate 1 0 1 FREE)
       attack-eg (env-gen (perc 0.05 0.1) gate 1 0 1 NO-ACTION)
       noise-eg (env-gen (perc 1.5 7 :curve [-7 4] ) gate 1 0 1 NO-ACTION)
       w-mod (* (+ (sin-osc:kr 0.2 1) 1) 0.5)
       mdltr (+ (* (var-saw 100 :width w-mod) (* vol 1)))
       ]
    (+ (-> (hpf
            (* (white-noise) noise-eg 1)
            (+ (+ (* (+ 1 (* -1 noise-eg)) 1500) 20)
               (* (* (+ 1 (lf-noise1:kr 6)) 200) noise-eg)
               )
            )
           )
       )
    ))

(def hpn (hp-n 150))

(definst wn
  []
  (crackle)
  )

(wn)
(stop)


(def flute-pitch-bus (control-bus))

(defsynth flute-pitch
  [pitch 440]
  (out:kr flute-pitch-bus pitch)
  )

(def fpb (flute-pitch))

(defsynth flute
  [freq 440 vol 1 attack 0.15 sustain 1.0 release 0.1 gate 1.0]
     (let[eg  (env-gen (asr attack sustain release :curve [-3 1 -2]) gate 1 0 1 FREE)
          ]
       (out [0 1]
        (-> (rlpf
             (-> (* (lf-saw :freq (in:kr flute-pitch-bus)) 0.5) )
             (+ (* eg 60) (in:kr flute-pitch-bus) (* (sin-osc:kr 3.5) 40))
             0.7
             )
            (* eg)
            ))
       ))

(def fl (flute))
(ctl fpb :pitch 440)
(ctl fpb :pitch 880)
(ctl fl :gate 0)
(stop)

(defsynth gong
  [freq (midi->hz 38) vol 1 gate 1.0]
     (let[eg  (env-gen (perc 0.04 8 :curve [-3 -2]) gate 1 0 1 FREE)
          vib-eg (env-gen (perc 3 3) gate 0.5 :action NO-ACTION)
          ]
       (out [0 1]
            (* vol
               (-> (lpf
                    (-> (* (lf-tri :freq freq) (lf-tri :freq (* freq 1.5)))
                        (+ (sin-osc freq)))
                    1400
                    )
                   (+ (* 0.03
                         (hpf
                          (lpf (white-noise)
                               (+ (env-gen (perc 1.5 6 :curve [2 2]) gate 5000 0 1 NO-ACTION)
                                  (+ (* (+ (sin-osc:kr 2.5) 1.8) 300)
                                     vib-eg)
                                  ))
                          (+ 5000 (* -5000 (env-gen (perc 1.5 6 :curve [2 2]) gate 1 0 1 NO-ACTION)))
                          ))
                      )
                   (* (+ eg (* (sin-osc:kr 3) vib-eg 0.15)))
                   ))
            )))

(def gng (gong))
(stop)

(defsynth woosh
  [freq (midi->hz 38) vol 1 attack 0.04 release 8 gate 1.0]
     (let[eg  (env-gen (perc attack release :curve [-3 -2]) gate 1 0 1 FREE)
          vib-eg (env-gen (perc 5 2 :curve [1 -2]) gate 0.5 0.5 :action NO-ACTION)
          n-eg  (env-gen (perc attack release :curve [-3 -2]) gate 1 0 1 NO-ACTION)
          ]
       (out [0 1]
            (-> (* 0.05 (hpf
                         (lpf (white-noise)
                              (+ (env-gen (perc 3 5 :curve [1 -2]) gate 5000 0 1 NO-ACTION)
                                 (* (* (+ (sin-osc:kr 2.5) 1) 150)
                                    vib-eg)
                                 ))
                         (+ 5000 (* -5000 (env-gen (perc 3 5 :curve [1 -2]) gate 1 0 1 NO-ACTION)))
                         )
                   )
                ;;(* (+ eg (* 0.1 (* (sin-osc:kr 0.8) vib-eg))))
                )
            )
       ))

(def wsh (woosh))
(stop)

(defsynth ring-filter
  [freq (midi->hz 50) vol 1 gate 1.0]
  (let [env (env-gen (perc 0.01 0.01) gate 10 :action NO-ACTION)
        eg2 (env-gen (perc 0 2) gate :action FREE)
        ]
    (out [0 1]
         (-> env
          ;; (rlpf env
          ;;          220
          ;;          0.01
          ;;          )
             (* 5)
             )
         )
    )
  )

(def rfl (ring-filter))
(stop)


(defsynth string-sect
  [freq 440 vol 1 attack 0.3 sustain 1.0 release 0.3 gate 1.0]
     (let[eg  (env-gen (asr attack sustain release :curve [-3 1 -2]) gate 1 0 1 FREE)
          ]
       (out [0 1]
        (-> (lpf
             (-> (pulse :freq freq :width (+ 0.5 (* 0.4 (sin-osc:kr 3))))
                 (+ (var-saw :freq (+ freq (* freq 0.01)) :width 0))
                 (* 0.3)
                 )
             2000
             )
            (* eg)
            ))
       ))

(def stsc (string-sect))
(ctl stsc :freq 440)
(ctl stsc :freq 880)
(ctl stsc :gate 0)
(stop)

(defsynth bells
  [freq 440 vol 1 attack 0.001 release 3 gate 1.0]
  (let[eg  (env-gen (perc attack release :curve [0 -3]) gate 0.2 0 1 FREE)
       osc1 (lf-tri :freq freq)
       osc2 (lf-tri :freq (* 2.3784 freq))
          ]
       (out [0 1]
        (-> (lpf
             (-> osc1
                 (+ osc2)
                 (+ (* osc1 osc2))
                 )
             (+ 20 (* eg 10000))
             )
            (* eg)
            ))
       ))

(def bl (bells))
(stop)





;; (definst fmnt [freq 440 bw 200.0
;;                attack 0.1 decay 0.2 sustain 0.6 release 0.7 vol 0.3]
;;   (let [cfreq (* freq 1.37)
;;         env-impulse (impulse 0.5)
;;         env-ff (toggle-ff env-impulse)
;;         env-gate (gate env-ff env-impulse)
;;         env-generator (env-gen (env-adsr attack decay sustain release) env-gate 1 0 1)
;;         ]
;;     (* env-generator
;;        (formant freq (+ cfreq (* (- env-generator 0.5) 1000)) (+ bw (* env-generator 500)))
;;        vol
;;        )
;;     )
;;   )

;; (def f1 (fmnt))
;; (stop)

;; (definst fmnt2 [freq 440 cfreq 880 bw 200.0 gate-val 0
;;                 attack 0.05 release 0.3 vol 0.3]
;;   (let [env-generator (env-gen (perc attack release) gate-val 1 0 1 NO-ACTION)
;;         ]
;;     (* env-generator
;;        ;;(formant freq cfreq (* bw (- env-generator 0.5)))
;;        (formant freq cfreq bw)
;;        vol
;;        )
;;     )
;;   )

;; (definst fmnt3 [freq 440 cfreq 880 bw 200.0 gate-val 0
;;                 attack 0.1 sustain 0.6 release 0.7  vol 0.3]
;;   (let [env-generator (env-gen (env-asr attack sustain release) gate-val 1 0 1 FREE)
;;         ]
;;     (* env-generator
;;        ;;(formant freq cfreq (* bw (- env-generator 0.5)))
;;        (formant (+ 1060 (* 1000 (lf-noise0:kr 1)))
;;                 (+ 800 (* 1000 (lf-noise1:kr 20)))
;;                 (+ 510 (* 500 (lf-noise0:kr 10))))
;;        vol
;;        )
;;     )
;;   )

;; ;;; using a bus for base pitch

;; (defonce pitch-bus (control-bus))

;; (defsynth pitch-osc
;;   [out-bus 0 freq 0.25 min-pitch 100 max-pitch 2000]
;;   (out:kr out-bus (lin-exp (lf-noise1:kr freq) -1 1 min-pitch max-pitch ))
;;   )

;; (def pitch-cntl (pitch-osc :out-bus pitch-bus :freq 2.0))

;; (definst fmnt4 [freq-bus 0 cfreq 880 bw 200.0
;;                 min-cfreq -75 max-cfreq 75 freq-cfreq 100
;;                 min-bw 10 max-bw 500 freq-bw 50
;;                 attack 0.01 release 0.05
;;                 vol 0.1]

;;   (let [env-impulse (impulse (range-lin (lf-noise0:kr 0.5) 3.0 5.0))
;;         env-ff (toggle-ff env-impulse)
;;         env-gate (gate env-ff env-impulse)
;;         envelope-generator (env-gen (perc attack release) env-gate 1 0 1)
;;         pitch (+ (in:kr freq-bus) (range-lin (lf-noise0:kr 1) -50 50))
;;         ]
;;     (* (formant pitch
;;                 (+ pitch (range-lin (lf-noise1:kr freq-cfreq) min-cfreq max-cfreq))
;;                 (range-lin (lf-noise1:kr freq-bw) min-bw max-bw))
;;        envelope-generator
;;        vol
;;        )
;;     )
;;   )

;; (definst fmnt5 [freq-bus 0 cfreq 880 bw 200.0
;;                 min-cfreq -75 max-cfreq 75 freq-cfreq 100
;;                 min-bw 10 max-bw 500 freq-bw 50
;;                 attack 0.0 release 0.001
;;                 vol 0.1]

;;   (let [env-impulse (impulse (range-lin (lf-noise0:kr 0.5) 3.0 5.0))
;;         env-ff (toggle-ff env-impulse)
;;         env-gate (gate env-ff env-impulse)
;;         envelope-generator (env-gen (perc attack release) env-gate 1 0 1)
;;         pitch (+ (gate (in:kr freq-bus) (pulse-divider env-impulse 2 2))
;;                  (range-lin (lf-noise0:kr 0.25) -50 50))
;;         ]
;;     (* (formant pitch
;;                 pitch
;;                 300
;;                 )
;;        envelope-generator
;;        vol
;;        )
;;     )
;;   )

;; (definst fmnt6 [freq-bus 0 cfreq 880 bw 200.0
;;                 max-impulse 3.0 min-impulse 5.0
;;                 max-cfreq 2000 freq-cfreq 11
;;                 min-bw 10 max-bw 1000 freq-bw 10
;;                 attack 0.0 release 0.001
;;                 vol 0.1]
;;   (let [env-impulse (impulse (range-lin (lf-noise0:kr 0.5) max-impulse min-impulse))
;;         env-ff (toggle-ff env-impulse)
;;         env-gate (gate env-ff env-impulse)
;;         envelope-generator (env-gen (perc attack release) env-gate 1 0 1)
;;         pitch-base (gate (in:kr freq-bus) (pulse-divider env-impulse 2 2))
;;         pitch-offset (range-lin (lf-noise0:kr 0.25)
;;                                 (* (/ pitch-base 4) -1)
;;                                 (/ pitch-base 4))
;;         pitch (+ pitch-base pitch-offset)
;;         ]
;;     (* (formant pitch
;;                 (+ pitch (range-lin (lf-noise1:kr freq-cfreq)
;;                                     (* (/ pitch-base 2) -1)
;;                                     max-cfreq))
;;                 (range-lin (lf-noise1:kr freq-bw) min-bw max-bw)
;;                 )
;;        envelope-generator
;;        vol
;;        )
;;     )
;;   )

;; (def synths (atom ()))

;; (defn make-synth
;;   "creates fmnt5 synths and adds it to synths"
;;   [synth]
;;   (reset! synths (conj @synths (synth))
;;           )
;;   )

;; (defn make-synths
;;   [synth & {:keys [t cnt] :or {t (now) cnt 0}}]
;;   (let [next-t (+ t 200)]
;;     (make-synth synth)
;;     (if (< cnt 7) (apply-at next-t
;;                             #'make-synths
;;                             [synth :t next-t :cnt (inc cnt)]))
;;     )
;;   )

;; (defn set-synth-val
;;   [parm val]
;;   (dorun (map ctl @synths (repeat parm) (repeat val)))
;;   )

;; (defn change-synth-val
;;   [t t-inc parm p-val p-inc p-max]
;;   (set-synth-val parm p-val)
;;   (let [next-t (+ t (* t-inc 1000))]
;;     (if (or (and (pos? p-inc) (< p-val p-max))
;;             (and (neg? p-inc) (> p-val p-max)))
;;       (do
;;         (println "p-val " p-val "p-max " p-max (> p-val p-max))
;;         (apply-at next-t
;;                   #'change-synth-val
;;                   [next-t t-inc parm (+ p-val p-inc) p-inc p-max])
;;         )
;;       )
;;     )
;;   )

;; (def synth2 (fmnt2))

;; (defn play
;;   [p]
;;   (let [pitch (+ 28 (rand-int 100))
;;         freq (midi->hz p)
;;         cfreq (if (< pitch 80) (+ freq 500) 400)
;;         bw 800
;;         ]
;;     (println p)
;;     (ctl synth2 :freq freq :cfreq freq :gate-val 1 :bw bw)
;;     (apply-at (+ (now) 50) ctl [synth2 :gate-val 0])
;;     )
;;   )

;; (defn playl
;;   [t pitch]
;;   (play pitch)
;;   (let [next-t (+ t 500)
;;         next-p (if (< pitch 100) (+ pitch 1) 28)]
;;     (apply-at next-t #'playl [next-t next-p]
;;     )
;;   ))
