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
                          (lpf (pink-noise)
                               (+ (env-gen (perc 1.5 6 :curve [2 2]) gate 7000 0 1 NO-ACTION)
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
  [freq 440 vol 1 attack 0.001 release 6 gate 1.0]
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

(defsynth sparks
  [gate 1 attack 0.001 release 0.001 gate 0 action NO-ACTION]
  (let [eg (env-gen (perc attack release) gate 1 0 1 action)
        cutoff-min 800
        cutoff-max 10000
        ]
    (out [0 1]
         (-> eg
             (* (rhpf
                 (white-noise)
                 (+ (/ (* (- cutoff-max cutoff-min) (- (lf-noise1:kr 10) -1)) (- 1 -1)) cutoff-min)
                 (/ (+ 1 (lf-noise1:kr 12)) 2) ;; scale between 0 an 1
                 ))
             )
         )
    )
  )

(def spk (sparks :release 0.05 :gate 1 :action FREE))
(ctl spk :gate 0)
(ctl spk :gate 1)

(defn play-spark
  [spk play-again]
  ;; (println "play-spark ****")
  (when (not play-again) (println "STOPPING !!!!!!!!!"))
  (let  [attack 0.001
         release (+ 0.001 (* 0.001 (rand-int 49)))
         action (if play-again NO-ACTION FREE)
         release-time (+ (now) (int (* (+ attack release) 1000)) 100)
         continue-playing (if (< (rand) 0.9) true false)
         next-time (+ release-time 250 (rand-int 800))
         ]
    (ctl spk
         :gate 1
         :attack attack
         :release release
         :action action
         )
    (apply-at release-time
              #'ctl spk [:gate 0]
              )

    (when play-again
      (apply-at next-time
                #'play-spark
                spk continue-playing []
                )
      )
    )
  )

(defn many-sparks
  [num-sparks]
  (let [all-sparks (for [s (range num-sparks)]
                     (sparks :gate 0 :action NO-ACTION))
        ]
    (for [s all-sparks] (apply-at (+ (now) (+ 500 (rand 2000)))
                                  #'play-spark
                                  s true []
                                  ))
    )
 )

(many-sparks 9)
(stop)

(defsynth spark2
  [gate 1 attack 0.001 release 0.001 gate 0 action NO-ACTION]
  (let [eg (env-gen (perc attack release) gate 1 0 1 action)
        cutoff-min 800
        cutoff-max 10000
        ;; freq (+ (/ (* (- cutoff-max cutoff-min) (- (lf-noise1:kr 10) -1)) (- 1 -1)) cutoff-min)
        freq (+ (rand-int (- cutoff-max cutoff-min)) cutoff-min)
        ]
    (out [0 1]
         (-> eg
             (*
              (+ (saw (/ freq 2))
                 (rhpf
                  (white-noise)
                  freq
                  (/ (+ 1 (lf-noise1:kr 0.5)) 2) ;; scale between 0 an 1
                  )))
             )
         )
    )
  )

(def spk (spark2 :release 0.2 :gate 1 :action FREE))
(ctl spk :gate 0)
(ctl spk :gate 1)

(defsynth spark3
  [gate 1 attack 0.001 release 0.001 gate 0 action NO-ACTION]
  (let [eg (env-gen (perc attack release) gate 1 0 1 action)
        cutoff-min 800
        cutoff-max 10000
        ]
    (out [0 1]
         (-> eg
             (* (rhpf
                 (white-noise)
                 (latch:ar (+ (/ (* (- cutoff-max cutoff-min) (- (lf-noise1:kr 20) -1)) (- 1 -1)) cutoff-min)
                        (lf-noise1:ar 100))
                 ;; (/ (+ 1 (lf-noise1:kr 12)) 2) ;; scale between 0 an 1
                 1.0
                 ))
             )
         )
    )
  )

(def spk (spark3 :release 0.1 :gate 1 :action FREE))
(ctl spk :gate 0)
(ctl spk :gate 1)

(defn many-synths
  [num-synths synth]
  (let [all-synths (for [s (range num-synths)]
                     (synth :gate 0 :action NO-ACTION))
        ]
    (for [s all-synths] (apply-at (+ (now) (+ 500 (rand 2000)))
                                  #'play-spark
                                  s true []
                                  ))
    )
 )

(many-synths 9 spark3)
(stop)
