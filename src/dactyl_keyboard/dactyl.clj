(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

(defn rotate-around-x [angle position]
  (mmul
    [[1 0 0]
     [0 (Math/cos angle) (- (Math/sin angle))]
     [0 (Math/sin angle) (Math/cos angle)]]
    position))

(defn rotate-around-y [angle position]
  (mmul
    [[(Math/cos angle) 0 (Math/sin angle)]
     [0 1 0]
     [(- (Math/sin angle)) 0 (Math/cos angle)]]
    position))


(defn rotate-around-z [angle position]
  (mmul
    [[(Math/cos angle) (- (Math/sin angle)) 0]
     [(Math/sin angle) (Math/cos angle) 0]
     [0 0 1]]
    position))

(defn rotate-shape-on-z [angle shape]
  (rotate (deg2rad angle) [0 0 1] shape))

(defn debug [shape]
  (color [0.5 0.5 0.5 0.5] shape))

(def WHI [255/255 255/255 255/255 1])
(def RED [255/255 0/255 0/255 1])
(def ORA [220/255 128/255 0/255 1])
(def YEL [220/255 255/255 0/255 1])
(def GRE [0/255 255/255 0/255 1])
(def CYA [0/255 255/255 255/255 1])
(def BLU [0/255 128/255 255/255 1])
(def NBL [0/255 0/255 255/255 1])
(def PUR [127/255 0/255 255/255 1])
(def PIN [255/255 0/255 255/255 1])
(def MAG [255/255 0/255 127/255 1])
(def BRO [102/255 51/255 0/255 1])
(def BLA [0/255 0/255 0/255 1])

; (def KEYCAP [220/255 163/255 163/255 1])
(def KEYCAP [239/255 222/255 205/255 0.95])

(def case-text-0 "Your")
(def case-text-1 "Text")
(def case-text-2 "Here")


;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 3)
(def ncols 5)
(def last_row_with_2_keys false)

;select only one of the following
(def use_flex_pcb_holder false)                             ; optional for flexible PCB, ameobas don't really benefit from this
(def use_hotswap_holder true)                               ; kailh hotswap holder
(def use_solderless false)                                  ; solderless switch plate, RESIN PRINTER RECOMMENDED!
(def wire-diameter 1.75)                                    ; outer diameter of silicone covered 22awg ~1.75mm 26awg ~1.47mm)

(def use_hotswap_led_cutout false)
(def use_hotswap_friction_holes false)
(def use_switch-dogbone-cutout false)

(def north_facing true)
(def extra-curve-bottom-row false)                          ; enable magic number curve of bottom two keys
(def tilt-outer-columns 7)                                  ; angle to tilt outer columns in degrees, adjust spacing where this is used if increased
(def recess-bottom-plate false)
(def adjustable-wrist-rest-holder-plate false)

(def use_screw_inserts true)
(def use_usb_holder true)
(def bottom_plate_text true)
(def case_text true)

(def rendered-caps true)                                    ; slows down model viewing but much nicer looking for more accurate clearances

(defn column-curvature [column]
  (cond (= column 0) (deg2rad 20)                           ;;index outer
        (= column 1) (deg2rad 20)                           ;;index
        (= column 2) (deg2rad 17)                           ;;middle
        (= column 3) (deg2rad 17)                           ;;ring
        (= column 4) (deg2rad 22)                           ;;pinky
        (>= column 5) (deg2rad 22)                          ;;pinky outer
        :else 0))

(def row-curvature (deg2rad 1))                             ; curvature of the rows
(defn centerrow [column]
  (cond (= column 0) 2.0                                    ;;index outer
        (= column 1) 2.0                                    ;;index
        (= column 2) 2.1                                    ;;middle
        (= column 3) 2.1                                    ;;ring
        (= column 4) 1.8                                    ;;pinky
        (>= column 5) 1.8                                   ;;pinky outer
        :else 0))

(def tenting-angle (deg2rad 26))                            ; controls left-right tilt / tenting (higher number is more tenting) default=35
(def centercol 3)                                           ; Zero indexed, TODO: this should be 2.5 for a 6 column boar

(defn column-offset [column] (cond
                               (= column 0) [-4 -4 1]       ;;index outer
                               (= column 1) [-4 -4 2]       ;;index Note: changing x, affects thumb x as well
                               (= column 2) [-1 0 -1.5]     ;;middle
                               (= column 3) [3.5 -4.5 2]    ;;ring
                               (= column 4) [4 -13.5 11]    ;;pinky
                               (>= column 5) [2 -13 11]     ;;pinky outer
                               :else [0 0 0]))

(def keyboard-z-offset 8)                                   ; controls overall height default=23.5

(def extra-width 2)                                         ; extra horizontal space between the base of keys
(defn extra-height [column]                                 ; extra vertical space between the base of keys
  (cond (= column 0) 1.9                                    ;;index outer
        (= column 1) 1.9                                    ;;index
        (= column 2) 1.7                                    ;;middle
        (= column 3) 1.7                                    ;;ring
        (= column 4) 2.0                                    ;;pinky
        (>= column 5) 2.0                                   ;;pinky outer
        :else 0))

(def wall-z-offset -7)                                      ; length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 1)
(def wall-thickness 1)                                      ; wall thickness parameter

(def thumb-pos [(+ 5.5 4) 1 9])
(def thumb-rot [0 10 0])

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (if last_row_with_2_keys
                 (dec lastrow)
                 lastrow))
(def lastcol (dec ncols))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 13.8)
(def keyswitch-width 13.9)
(def plate-thickness 5)

(def retention-tab-thickness 1.5)
(def retention-tab-hole-thickness (- plate-thickness retention-tab-thickness))
(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))

(def holder-x mount-width)
(def holder-thickness (/ (- holder-x keyswitch-width) 2))
(def holder-y (+ keyswitch-height (* holder-thickness 2)))
(def swap-z 3)
(def web-thickness (if use_hotswap_holder (+ plate-thickness swap-z) plate-thickness))
(def keyswitch-below-plate (- 8 web-thickness))             ; approx space needed below keyswitch, ameoba is 6mm
(def square-led-size 6)

(def switch-teeth-cutout
  (let [
        ; cherry, gateron, kailh switches all have a pair of tiny "teeth" that stick out
        ; on the two side walls, this gives those teeth somewhere to press into
        teeth-x 4.5
        teeth-y 0.75
        teeth-z 1.75
        teeth-x-offset 0
        teeth-y-offset (+ (/ keyswitch-height 2) (/ teeth-y 2.01))
        teeth-z-offset (- plate-thickness 1.95)
        ]
    (->> (cube teeth-x teeth-y teeth-z)
         (translate [teeth-x-offset teeth-y-offset teeth-z-offset])
         )
    )
  )

(def hotswap-x2 (* (/ holder-x 3) 1.85))
(def hotswap-y1 4.3)                                        ;first y-size of kailh hotswap holder
(def hotswap-y2 6.2)                                        ;second y-size of kailh hotswap holder
(def hotswap-z (+ swap-z 0.5))                              ;thickness of kailn hotswap holder + some margin of printing error (0.5mm)
(def hotswap-cutout-z-offset -2.6)
(def hotswap-cutout-2-x-offset (- (- (/ holder-x 4) 0.70)))
(def hotswap-cutout-1-y-offset 4.95)
(def hotswap-cutout-2-y-offset 4)
(def hotswap-case-cutout-x-extra 2.75)
(defn hotswap-case-cutout [mirror-internals]
  (let [shape (union
                (translate [0
                            hotswap-cutout-1-y-offset
                            hotswap-cutout-z-offset]
                           (cube (+ keyswitch-width hotswap-case-cutout-x-extra)
                                 hotswap-y1
                                 hotswap-z))
                (translate [hotswap-cutout-2-x-offset
                            hotswap-cutout-2-y-offset
                            hotswap-cutout-z-offset]
                           (cube hotswap-x2 hotswap-y2 hotswap-z))
                )
        rotated
        (if north_facing
          (->> shape
               (mirror [1 0 0])
               (mirror [0 1 0])
               )
          shape
          )
        mirrored
        (->> (if mirror-internals
               (->> rotated (mirror [1 0 0]))
               rotated))
        ]
    mirrored
    )
  )

(defn hotswap-case-cutout-less-left [mirror-internals]
  (let [shape (union
                (translate [0
                            hotswap-cutout-1-y-offset
                            hotswap-cutout-z-offset]
                           (cube (- (+ keyswitch-width hotswap-case-cutout-x-extra) (if mirror-internals 0.15 0))
                                 hotswap-y1
                                 hotswap-z))
                (translate [(if mirror-internals (- hotswap-cutout-2-x-offset -0.25) hotswap-cutout-2-x-offset)
                            hotswap-cutout-2-y-offset
                            hotswap-cutout-z-offset]
                           (cube (- hotswap-x2 (if mirror-internals 1.25 1)) hotswap-y2 hotswap-z))
                )
        rotated
        (if north_facing
          (->> shape
               (mirror [1 0 0])
               (mirror [0 1 0])
               )
          shape
          )
        mirrored
        (->> (if mirror-internals
               (->> rotated (mirror [1 0 0]))
               rotated))
        ]
    mirrored
    )
  )

(def hotswap-holder
  ;irregularly shaped hot swap holder
  ;    ____________
  ;  |  _|_______|    |  hotswap offset from out edge of holder with room to solder
  ; y1 |_|_O__  \ _  y2  hotswap pin
  ;    |      \O_|_|  |  hotswap pin
  ;    |  o  O  o  |     fully supported friction holes
  ;    |    ___    |
  ;    |    |_|    |  space for LED under SMD or transparent switches
  ;
  ; can be described as having two sizes in the y dimension depending on the x coordinate
  (let [
        swap-x holder-x
        swap-y holder-y

        swap-offset-x 0
        swap-offset-y (/ (- holder-y swap-y) 2)
        swap-offset-z (- (/ swap-z 2))                      ; the bottom of the hole.
        swap-holder (->> (cube swap-x swap-y swap-z)
                         (translate [swap-offset-x
                                     swap-offset-y
                                     swap-offset-z]))
        hotswap-x holder-x                                  ;cutout full width of holder instead of only 14.5mm
        hotswap-x3 (/ holder-x 4)
        hotswap-y3 (/ hotswap-y1 2)

        hotswap-cutout-1-x-offset 0.01
        hotswap-cutout-2-x-offset (- (/ holder-x 4.5))
        hotswap-cutout-3-x-offset (- (/ holder-x 2) (/ hotswap-x3 2.01))
        hotswap-cutout-4-x-offset (- (/ hotswap-x3 2.01) (/ holder-x 2))

        hotswap-cutout-3-y-offset 7.4

        hotswap-cutout-led-x-offset 0
        hotswap-cutout-led-y-offset -6

        hotswap-cutout-1 (->> (cube hotswap-x hotswap-y1 hotswap-z)
                              (translate [hotswap-cutout-1-x-offset
                                          hotswap-cutout-1-y-offset
                                          hotswap-cutout-z-offset]))
        hotswap-cutout-2 (->> (cube hotswap-x2 hotswap-y2 hotswap-z)
                              (translate [hotswap-cutout-2-x-offset
                                          hotswap-cutout-2-y-offset
                                          hotswap-cutout-z-offset]))
        hotswap-cutout-3 (->> (cube hotswap-x3 hotswap-y3 hotswap-z)
                              (translate [hotswap-cutout-3-x-offset
                                          hotswap-cutout-3-y-offset
                                          hotswap-cutout-z-offset]))
        hotswap-cutout-4 (->> (cube hotswap-x3 hotswap-y3 hotswap-z)
                              (translate [hotswap-cutout-4-x-offset
                                          hotswap-cutout-3-y-offset
                                          hotswap-cutout-z-offset]))
        hotswap-led-cutout (->> (cube square-led-size square-led-size 10)
                                (translate [hotswap-cutout-led-x-offset
                                            hotswap-cutout-led-y-offset
                                            hotswap-cutout-z-offset]))
        hotswap-cutout (union hotswap-cutout-1
                              hotswap-cutout-2
                              hotswap-cutout-3
                              hotswap-cutout-4)
        ; for the main axis
        main-axis-hole (->> (cylinder (/ 4.1 2) 10)
                            (with-fn 30))
        plus-hole (->> (cylinder (/ 3.3 2) 10)
                       (with-fn 30)
                       (translate [-3.81 2.54 0]))
        minus-hole (->> (cylinder (/ 3.3 2) 10)
                        (with-fn 30)
                        (translate [2.54 5.08 0]))
        friction-hole (->> (cylinder (/ 1.95 2) 10)
                           (with-fn 30))
        friction-hole-right (translate [5 0 0] friction-hole)
        friction-hole-left (translate [-5 0 0] friction-hole)
        hotswap-shape
        (difference (union swap-holder
                           ; (debug hotswap-cutout-3)
                           )
                    main-axis-hole
                    plus-hole
                    minus-hole
                    (when use_hotswap_friction_holes
                      (union friction-hole-left
                             friction-hole-right)
                      )
                    hotswap-cutout
                    (when use_hotswap_led_cutout
                      hotswap-led-cutout)
                    )
        ]
    (if north_facing
      (->> hotswap-shape
           (mirror [1 0 0])
           (mirror [0 1 0])
           )
      hotswap-shape
      )
    )
  )

(def solderless-plate
  (let [
        solderless-x holder-x
        solderless-y holder-y                               ; should be less than or equal to holder-y
        solderless-z 4                                      ;
        solderless-cutout-z (* 1.01 solderless-z)
        solderless-offset-x 0
        solderless-offset-y (/ (- holder-y solderless-y) 2)
        solderless-offset-z (- (/ solderless-z 2))          ; the bottom of the hole.
        switch_socket_base (cube solderless-x
                                 solderless-y
                                 solderless-z)
        wire-channel-diameter (+ 0.3 wire-diameter)         ; elegoo saturn prints 1.75mm tubes ~1.62mm
        wire-channel-offset (- (/ solderless-z 2) (/ wire-channel-diameter 3))
        ;led-cutout-x-offset 0
        ;led-cutout-y-offset -6
        led-cutout (translate [0 -6 0]
                              (cube square-led-size
                                    square-led-size
                                    solderless-cutout-z))
        main-axis-hole (->> (cylinder (/ 4.1 2) solderless-cutout-z)
                            (with-fn 30))
        plus-hole (->> (cylinder (/ 1.55 2) solderless-cutout-z)
                       (with-fn 30)
                       (scale [1 0.85 1])
                       (translate [-3.81 2.54 0]))
        minus-hole (->> (cylinder (/ 1.55 2) solderless-cutout-z)
                        (with-fn 30)
                        (scale [1 0.85 1])
                        (translate [2.54 5.08 0]))
        friction-hole (->> (cylinder (/ 1.95 2) solderless-cutout-z)
                           (with-fn 30))
        friction-hole-right (translate [5 0 0] friction-hole)
        friction-hole-left (translate [-5 0 0] friction-hole)

        diode-wire-dia 0.75
        diode-row-hole (->> (cylinder (/ diode-wire-dia 2) solderless-cutout-z)
                            (with-fn 30)
                            (translate [3.65 3.0 0]))
        diode-pin (translate [-3.15 3.0 (/ solderless-z 2)]
                             (cube 2 diode-wire-dia 2))
        diode-wire (translate [2.75 3.0 (/ solderless-z 2)]
                              (cube 2 diode-wire-dia 2))
        diode-body (translate [-0.2 3.0 (/ solderless-z 2)]
                              (cube 4 1.95 3))

        row-wire-radius (/ wire-channel-diameter 2)
        row-wire-channel-end-radius 3.25
        row-wire-channel-end (->> (circle row-wire-radius)
                                  (with-fn 50)
                                  (translate [row-wire-channel-end-radius 0 0])
                                  (extrude-rotate {:angle 90})
                                  (rotate (deg2rad 90) [1 0 0])
                                  (translate [(+ 7 (- row-wire-channel-end-radius))
                                              5.08
                                              (+ wire-channel-offset (- row-wire-channel-end-radius))])
                                  )
        row-wire-channel-ends (translate [8 5.08 -1.15]
                                         (union (cube 3 wire-channel-diameter solderless-z)
                                                (translate [(/ 3 -2) 0 0]
                                                           (->> (cylinder (/ wire-channel-diameter 2) solderless-z)
                                                                (with-fn 50)))))
        row-wire-channel-cube-end (union (->> (cube wire-channel-diameter
                                                    wire-channel-diameter
                                                    wire-channel-diameter)
                                              (translate [6 5.08 (+ 0 wire-channel-offset)])
                                              )
                                         (->> (cylinder (/ wire-channel-diameter 2)
                                                        wire-channel-diameter)
                                              (with-fn 50)
                                              (translate [5 5.08 (+ (/ wire-channel-diameter 2) wire-channel-offset)])
                                              )
                                         )
        row-wire-channel-curve-radius 45
        row-wire-channel (union
                           (->> (circle row-wire-radius)
                                (with-fn 50)
                                (translate [row-wire-channel-curve-radius 0 0])
                                (extrude-rotate {:angle 90})
                                (rotate (deg2rad 90) [1 0 0])
                                (rotate (deg2rad -45) [0 1 0])
                                (translate [0
                                            5.08
                                            (+ 0.25 wire-channel-offset (- row-wire-channel-curve-radius))])
                                )
                           row-wire-channel-end
                           row-wire-channel-ends
                           row-wire-channel-cube-end
                           (->> (union row-wire-channel-end
                                       row-wire-channel-ends
                                       row-wire-channel-cube-end
                                       )
                                (mirror [1 0 0])
                                )
                           )
        col-wire-radius (+ 0.025 (/ wire-channel-diameter 2))
        ;col-wire-ends-radius (+ 0.1 (/ wire-channel-diameter 2))
        ;col-wire-ends-zoffset 0.0725                        ; should be diff of two magic numbers above
        col-wire-channel-curve-radius 15
        col-wire-channel (->> (circle col-wire-radius)
                              (with-fn 50)
                              (translate [col-wire-channel-curve-radius 0 0])
                              (extrude-rotate {:angle 90})
                              (rotate (deg2rad 135) [0 0 1])
                              (translate [(+ 3.10 col-wire-channel-curve-radius)
                                          0
                                          (- 0.1 wire-channel-offset)])
                              )

        solderless-shape
        (translate [solderless-offset-x
                    solderless-offset-y
                    solderless-offset-z]
                   (difference (union switch_socket_base
                                      ;(debug row-wire-channel-cube-end) ; may have to disable below to appear
                                      )
                               main-axis-hole
                               plus-hole
                               minus-hole
                               friction-hole-left
                               friction-hole-right
                               diode-row-hole
                               row-wire-channel
                               col-wire-channel
                               diode-pin
                               diode-body
                               diode-wire
                               led-cutout
                               ))
        ]
    (if north_facing
      (->> solderless-shape
           (mirror [1 0 0])
           (mirror [0 1 0])
           )
      solderless-shape
      )
    )
  )

(def switch-dogbone-cutout                                  ;switches corner cut outs
  (let [cutout-radius 0.75
        cutout (->> (cylinder cutout-radius 99)
                    (with-fn 15))
        cutout-x (- (/ keyswitch-width 2) (/ cutout-radius 2))
        cutout-y (- (/ keyswitch-height 2) (/ cutout-radius 2))
        ]
    (union
      (translate [cutout-x cutout-y 0] cutout)
      (translate [(- cutout-x) cutout-y 0] cutout)
      (translate [cutout-x (- cutout-y) 0] cutout)
      )
    )
  )

;(def amoeba-x 1)                                            ; mm width TODO wtf?
(def amoeba-y 16)                                           ; mm high
(def keyswitch-below-clearance (/ keyswitch-below-plate -2))

(def switch-bottom
  (translate [0 0 keyswitch-below-clearance]
             (cube amoeba-y
                   amoeba-y
                   keyswitch-below-plate)))

(def flex-pcb-holder
  (let [pcb-holder-x (* 0.99 amoeba-y)                      ; keyswitch-width
        pcb-holder-y 5
        pcb-holder-z 3                                      ;keyswitch-below-plate
        pcb-holder-z-offset (- (* 2 keyswitch-below-clearance) (/ pcb-holder-z 2))
        minus-hole (->> (cylinder (/ 4 2) 99)
                        (with-fn 15)
                        (translate [2.54 5.08 0]))
        ]
    (union
      (difference
        (translate [0
                    (/ keyswitch-height 2)
                    pcb-holder-z-offset]
                   (difference (cube pcb-holder-x pcb-holder-y pcb-holder-z)
                               ;cut triangle out of pcb clip
                               (->> (cube (* 1.01 pcb-holder-x) pcb-holder-y pcb-holder-z)
                                    (translate [0 0 (/ pcb-holder-z -1.25)])
                                    (rotate (deg2rad -45) [1 0 0])
                                    )
                               )
                   )
        minus-hole
        )
      (translate [0
                  (+ (/ keyswitch-height 2) (/ pcb-holder-y 3))
                  keyswitch-below-clearance]
                 (color YEL (cube pcb-holder-x
                                  (/ pcb-holder-y 3)
                                  (* 3 keyswitch-below-plate)))
                 )
      )))

(defn single-plate [mirror-internals]
  (let [top-wall (->> (cube mount-height 1.5 plate-thickness)
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube 1.5 mount-width plate-thickness)
                       (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                   0
                                   (/ plate-thickness 2)]))
        plate-half (difference (union top-wall left-wall)
                               switch-teeth-cutout
                               (when use_switch-dogbone-cutout
                                 switch-dogbone-cutout)
                               )
        plate (union plate-half
                     (->> plate-half
                          (mirror [1 0 0])
                          (mirror [0 1 0]))
                     (when use_hotswap_holder hotswap-holder)
                     (when use_solderless solderless-plate)
                     )
        ]
    (->> (if mirror-internals
           (->> plate (mirror [1 0 0]))
           plate
           )
         )
    )
  )

(def single-plate-blank
  (union
    (translate [0 0 (/ plate-thickness 2)]
               (cube mount-width
                     mount-height
                     (+ plate-thickness 0.001)
                     )
               )
    (if use_hotswap_holder (translate [0 0 (- (/ hotswap-z 2))]
                                      (cube mount-width
                                            mount-height
                                            hotswap-z)))
    (if use_solderless (hull solderless-plate))
    )
  )

(defn single-plate-cut [mirror-internals]
  (difference
    single-plate-blank
    (single-plate mirror-internals)
    )
  )

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)                                       ; Fits DSA
(def sa-height 12.5)

(def sa-key-height-from-plate 7.39)
(def sa-cap-bottom-height (+ sa-key-height-from-plate plate-thickness))
(def sa-cap-bottom-height-pressed (+ 3 plate-thickness))

;(def sa-double-length 37.5)
(defn sa-cap [keysize]
  (let [bl2 (case keysize 1 (/ sa-length 2)
                          1.5 (/ sa-length 2)
                          2 sa-length)
        bw2 (case keysize 1 (/ sa-length 2)
                          1.5 (/ 27.94 2)
                          2 (/ sa-length 2))
        m 8.25
        keycap-xy (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
        keycap-top (case keysize 1 (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                 1.52 (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                 2 (polygon [[6 16] [6 -16] [-6 -16] [-6 16]]))
        key-cap (hull (->> keycap-xy
                           (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                           (translate [0 0 0.05]))
                      (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                           (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                           (translate [0 0 (/ sa-height 2)]))
                      (->> keycap-top
                           (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                           (translate [0 0 sa-height])))
        key-cap-display (if rendered-caps
                          (translate [0 0 0]
                                     (import "../things/SA-R3.stl"))
                          key-cap)
        ]
    (union
      (->> key-cap-display
           (translate [0 0 sa-cap-bottom-height])
           (color KEYCAP))
      ; (debug (->> key-cap
      ;      (translate [0 0 sa-cap-bottom-height-pressed])))
      )
    )
  )

(defn sa-cap-cutout [keysize]
  (let [cutout-x 0.40
        cutout-y 1.95
        cutout-z-offset (- sa-cap-bottom-height-pressed 2.99)
        bl2 (case keysize
              1 (+ (/ sa-length 2) cutout-y)
              1.5 (+ (/ sa-length 2) cutout-y)
              2 (+ sa-length cutout-y))
        bw2 (case keysize
              1 (+ (/ sa-length 2) cutout-x)
              1.5 (+ (/ 27.94 2) cutout-x)
              2 (+ (/ sa-length 2) cutout-x))
        keycap-cutout-xy (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
        key-cap-cutout (hull (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 0.05]))
                             (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 sa-height])))
        ]
    (->> key-cap-cutout
         (translate [0 0 cutout-z-offset]))
    )
  )

; increase x-cutout and left shift
(defn sa-cap-cutout-left-side [keysize]
  (let [cutout-x 1.40                                       ;was 0.4
        cutout-y 1.95
        cutout-z-offset (- sa-cap-bottom-height-pressed 2.99)
        bl2 (case keysize
              1 (+ (/ sa-length 2) cutout-y)
              1.5 (+ (/ sa-length 2) cutout-y)
              2 (+ sa-length cutout-y))
        bw2 (case keysize
              1 (+ (/ sa-length 2) cutout-x)
              1.5 (+ (/ 27.94 2) cutout-x)
              2 (+ (/ sa-length 2) cutout-x))
        keycap-cutout-xy (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
        key-cap-cutout (hull (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 0.05]))
                             (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 sa-height])))
        ]
    (union
      (->> key-cap-cutout
           (translate [-1 0 cutout-z-offset]) (color [0.5 0.5 0.5 0.5]))
      )
    )
  )

(defn sa-cap-cutout-right-side [keysize]
  (let [cutout-x 1.40                                       ;was 0.4
        cutout-y 1.95
        cutout-z-offset (- sa-cap-bottom-height-pressed 2.99)
        bl2 (case keysize
              1 (+ (/ sa-length 2) cutout-y)
              1.5 (+ (/ sa-length 2) cutout-y)
              2 (+ sa-length cutout-y))
        bw2 (case keysize
              1 (+ (/ sa-length 2) cutout-x)
              1.5 (+ (/ 27.94 2) cutout-x)
              2 (+ (/ sa-length 2) cutout-x))
        keycap-cutout-xy (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
        key-cap-cutout (hull (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 0.05]))
                             (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 sa-height])))
        ]
    (union
      (->> key-cap-cutout
           (translate [1 0 cutout-z-offset]) (color [0.5 0.5 0.5 0.5]))
      )
    )
  )

;reduces y-cutout
; creates mount-width cutout below cap-cutout
; adds diagonal edge from mount-width to cap-cutout
(defn sa-cap-cutout-mod-for-lastrow-2 [keysize]
  (let [cutout-x 0.4
        cutout-y 0.4
        cutout-z-offset (- sa-cap-bottom-height-pressed 2.99)
        bl2 (case keysize
              1 (+ (/ sa-length 2) cutout-y)
              1.5 (+ (/ sa-length 2) cutout-y)
              2 (+ sa-length cutout-y))
        bw2 (case keysize
              1 (+ (/ sa-length 2) cutout-x)
              1.5 (+ (/ 27.94 2) cutout-x)
              2 (+ (/ sa-length 2) cutout-x))
        mw2 (/ mount-width 2)
        mh2 (/ mount-height 2)
        switch-mount-xy (polygon [[mw2 mh2] [mw2 (- mh2)] [(- mw2) (- mh2)] [(- mw2) mh2]])
        keycap-cutout-xy (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
        key-cap-cutout (hull (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 0.05]))
                             (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 sa-height])))
        dsa-keycap-lowest-bottom-height-from-plate 2
        switch-diagonal (hull (->> switch-mount-xy
                                   (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                   (translate [0 0 0.05]))
                              (->> keycap-cutout-xy
                                   (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                   (translate [0 0 dsa-keycap-lowest-bottom-height-from-plate])))
        ]
    (union
      (->> key-cap-cutout
           (translate [0 0 (+ cutout-z-offset dsa-keycap-lowest-bottom-height-from-plate)]) (color [0.5 0.5 0.5 0.5]))
      (->> switch-diagonal (translate [0 0 cutout-z-offset]) (color PIN))
      )
    )
  )

; increased x-cutout for sides (left|right)
; reduced y-cutout
; mount-width cutout below cap-cutout
; configurable height between cap and mount cutout
; adds diagonal edge from mount-width to cap-cutout
; additional cutout block with configurable z-height and y-offset
(defn sa-cap-cutout-mod-for-cornerrow-2 [keysize, outer]
  (let [cutout-x 0.4
        cutout-x-additional (if outer 3 0)
        cutout-y 0.4                                        ;was 1.95
        ;orig-cutout-y 1.95
        cutout-z-offset (- sa-cap-bottom-height-pressed 2.99)
        bl2 (case keysize
              1 (+ (/ sa-length 2) cutout-y)
              1.5 (+ (/ sa-length 2) cutout-y)
              2 (+ sa-length cutout-y))
        bw2 (case keysize
              1 (+ (/ sa-length 2) cutout-x cutout-x-additional)
              1.5 (+ (/ 27.94 2) cutout-x)
              2 (+ (/ sa-length 2) cutout-x))
        keycap-cutout-xy (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
        key-cap-cutout (hull (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 0.05]))
                             (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 sa-height])))
        mw2 bw2
        mh2 (+ (/ sa-length 2) 0)                           ;18.25 + 1.95
        orig-keycap-cutout-xy (polygon [[mw2 mh2] [mw2 (- mh2)] [(- mw2) (- mh2)] [(- mw2) mh2]])
        dsa-keycap-lowest-bottom-height-from-plate 1
        diagonal-part (hull (->> orig-keycap-cutout-xy
                                 (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                 (translate [0 (- 1 cutout-y) 0.05])
                                 )
                            (->> keycap-cutout-xy
                                 (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                 (translate [0 0 dsa-keycap-lowest-bottom-height-from-plate])))
        additional-cutout-block-z 3
        ]
    (union
      (->> key-cap-cutout
           (translate [(- cutout-x-additional) 0 (+ cutout-z-offset dsa-keycap-lowest-bottom-height-from-plate)]) (color [0.5 0.5 0.5 0.5]))
      (->> diagonal-part (translate [(- cutout-x-additional) 0 cutout-z-offset]) (color PIN))
      (->> orig-keycap-cutout-xy
           (extrude-linear {:height additional-cutout-block-z :twist 0 :convexity 0})
           (translate [(- cutout-x-additional) 1.6 (+ cutout-z-offset (/ additional-cutout-block-z 2))]))
      )
    )
  )

(defn sa-cap-cutout-mod-generic
  "Generates configurable sa cap cutout shape for given keysize.
  Options are:
    reduced-y-cutout -- reduce overall y cutout size
    create-mount-width-bottom-cutout -- true when mount-width sized cutout should be created below actual sa cap cutout
    create-sa-cap-bottom-cutout -- true when bottom cutout should have size of sa-cap cutout
    mount-width-cutout-height -- height to create mount-width shape below sa cap cutout
    mount-width-2-cap-cutout-diagonal-edges -- true to create diagonal edges between mount-width and sa-cap shapes
    additional-x-cutout-left -- overall x-offset to be created and left shifted
    additional-x-cutout-right -- overall x-offset to be created and right shifted
    additional-cutout-block -- true to create an additional cutout block
    additional-cutout-block-y-offset -- y-offset for additional cutout block
    additional-cutout-block-z-height -- z-height for additional cutout block
    "
  [keysize & {:keys [reduced-y-cutout
                     create-mount-width-bottom-cutout
                     create-sa-cap-bottom-cutout
                     bottom-cutout-y-offset
                     key-cap-lowest-bottom-z-height
                     mount-width-2-cap-cutout-diagonal-edges
                     additional-x-cutout-left
                     additional-x-cutout-right
                     additional-cutout-block
                     additional-cutout-block-y-offset
                     additional-cutout-block-z-height
                     ] :or {reduced-y-cutout                        0
                            create-mount-width-bottom-cutout        false
                            create-sa-cap-bottom-cutout             false
                            bottom-cutout-y-offset                  0
                            key-cap-lowest-bottom-z-height          0
                            mount-width-2-cap-cutout-diagonal-edges false
                            additional-x-cutout-left                0
                            additional-x-cutout-right               0
                            additional-cutout-block                 false
                            additional-cutout-block-y-offset        1
                            additional-cutout-block-z-height        3}
              }
   ]
  (let [cutout-x (+ 0.4 additional-x-cutout-left additional-x-cutout-right)
        cutout-y (- 1.95 reduced-y-cutout)
        cutout-z-offset (- sa-cap-bottom-height-pressed 2.99)
        bl2 (case keysize
              1 (+ (/ sa-length 2) cutout-y)
              1.5 (+ (/ sa-length 2) cutout-y)
              2 (+ sa-length cutout-y))
        bw2 (case keysize
              1 (+ (/ sa-length 2) cutout-x)
              1.5 (+ (/ 27.94 2) cutout-x)
              2 (+ (/ sa-length 2) cutout-x))
        keycap-cutout-xy (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
        key-cap-cutout (hull (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 0.05]))
                             (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 sa-height])))
        mw2 (if create-mount-width-bottom-cutout
              (/ mount-width 2)
              (if create-sa-cap-bottom-cutout
                bw2
                bw2
                )
              )
        mh2 (if create-mount-width-bottom-cutout
              (/ mount-height 2)
              (if create-sa-cap-bottom-cutout
                (+ (/ sa-length 2) 0)                       ;18.25 + 1.95))
                (+ (/ sa-length 2) 0)
                )
              )

        bottom-cutout-xy (polygon [[mw2 mh2] [mw2 (- mh2)] [(- mw2) (- mh2)] [(- mw2) mh2]])
        diagonal-part (hull (->> bottom-cutout-xy
                                 (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                 (translate [0 bottom-cutout-y-offset 0.05])
                                 )
                            (->> keycap-cutout-xy
                                 (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                 (translate [0 0 key-cap-lowest-bottom-z-height])))
        cutout-x-additional-translate (- (+ 0 additional-x-cutout-right) additional-x-cutout-left)
        ]
    (union
      (->> key-cap-cutout
           (translate [cutout-x-additional-translate 0 (+ cutout-z-offset key-cap-lowest-bottom-z-height)]) (color [0.5 0.5 0.5 0.5]))
      (when mount-width-2-cap-cutout-diagonal-edges
        (->> diagonal-part (translate [cutout-x-additional-translate 0 cutout-z-offset]) (color PIN))
        )

      (when additional-cutout-block
        (->> bottom-cutout-xy
             (extrude-linear {:height additional-cutout-block-z-height :twist 0 :convexity 0})
             (translate [cutout-x-additional-translate (+ 0.6 additional-cutout-block-y-offset) (+ cutout-z-offset (/ additional-cutout-block-z-height 2))])))
      )
    )
  )
(declare key-place)

;TODO fix clearing area so that part where two key places face each other is clean
;TODO fix right wall side clearings
(defn cap-cutout-handler-5x6-2-keys-last-row [keysize column row shape]
  (let [
        column-0-1-rows-0-1 (->> (sa-cap-cutout-mod-generic keysize
                                                            :reduced-y-cutout 0.2795
                                                            :additional-x-cutout-left 1) (key-place column row))
        column-2-rows-0-to-1 (->> (sa-cap-cutout-mod-generic keysize
                                                             :reduced-y-cutout 0.6192) (key-place column row))
        column-4-rows-0-to-2 (->> (sa-cap-cutout-mod-generic keysize
                                                             :reduced-y-cutout 0.0645) (key-place column row))
        column-5-rows-0-to-2 (->> (sa-cap-cutout-mod-generic keysize
                                                             :reduced-y-cutout 0.0645
                                                             :additional-x-cutout-right 4) (key-place column row))
        cornerrow-left-outer (->> (sa-cap-cutout-mod-generic keysize
                                                             :reduced-y-cutout 1.55
                                                             :create-sa-cap-bottom-cutout true
                                                             :key-cap-lowest-bottom-z-height 1
                                                             :bottom-cutout-y-offset (- 1 0.4)
                                                             :mount-width-2-cap-cutout-diagonal-edges true
                                                             :additional-x-cutout-left 1
                                                             :additional-cutout-block true
                                                             :additional-cutout-block-y-offset 1.07
                                                             :additional-cutout-block-z-height 3
                                                             ) (key-place column row))
        cornerrow-reduced-y (->> (sa-cap-cutout-mod-generic keysize
                                                            :reduced-y-cutout 1.55
                                                            :create-sa-cap-bottom-cutout true
                                                            :key-cap-lowest-bottom-z-height 1
                                                            :bottom-cutout-y-offset (- 1 0.4)
                                                            :mount-width-2-cap-cutout-diagonal-edges true
                                                            :additional-cutout-block true
                                                            :additional-cutout-block-y-offset 1.06
                                                            :additional-cutout-block-z-height 3
                                                            ) (key-place column row))
        cornerrow-col4 (->> (sa-cap-cutout-mod-generic keysize
                                                       :reduced-y-cutout 0.0645
                                                       :additional-cutout-block true
                                                       :additional-cutout-block-y-offset -3.87
                                                       :additional-cutout-block-z-height 7
                                                       ) (key-place column row))
        cornerrow-col5 (->> (sa-cap-cutout-mod-generic keysize
                                                       :reduced-y-cutout 0.0645
                                                       :additional-x-cutout-right 4
                                                       :additional-cutout-block true
                                                       :additional-cutout-block-y-offset -3.9
                                                       :additional-cutout-block-z-height 7
                                                       ) (key-place column row))
        row-2-col-2 (->> (sa-cap-cutout-mod-generic keysize
                                                    :reduced-y-cutout 0.62
                                                    :additional-cutout-block true
                                                    :additional-cutout-block-y-offset -1.25) (key-place column row))
        cornerrow-col-2-reduced-y (->> (sa-cap-cutout-mod-generic keysize
                                                                  :reduced-y-cutout 1.55
                                                                  :create-sa-cap-bottom-cutout true
                                                                  :key-cap-lowest-bottom-z-height 1
                                                                  :bottom-cutout-y-offset (- 1 0.4)
                                                                  :mount-width-2-cap-cutout-diagonal-edges true
                                                                  :additional-cutout-block true
                                                                  :additional-cutout-block-y-offset 0.73
                                                                  :additional-cutout-block-z-height 4
                                                                  ) (key-place column row))
        row-2-left (->> (sa-cap-cutout-mod-generic keysize
                                                   :reduced-y-cutout 0.27
                                                   :additional-x-cutout-left 1
                                                   :additional-cutout-block false
                                                   :additional-cutout-block-y-offset -2.3) (key-place column row))
        lastrow (->> (sa-cap-cutout-mod-generic 1
                                                :reduced-y-cutout 1.55
                                                :create-mount-width-bottom-cutout true
                                                :key-cap-lowest-bottom-z-height 2
                                                :mount-width-2-cap-cutout-diagonal-edges true) (key-place column row))

        ]
    (case column
      0 (case row
          0 column-0-1-rows-0-1
          1 column-0-1-rows-0-1
          2 row-2-left
          3 cornerrow-left-outer
          (->> shape
               (key-place column row)))
      1 (case row
          0 column-0-1-rows-0-1
          1 column-0-1-rows-0-1
          2 row-2-left
          3 cornerrow-reduced-y
          (->> shape
               (key-place column row)))
      2 (case row
          0 column-2-rows-0-to-1
          1 column-2-rows-0-to-1
          2 row-2-col-2
          3 cornerrow-col-2-reduced-y
          4 lastrow                                         ;TODO: check in prusa
          (->> shape
               (key-place column row)))
      3 (case row
          0 column-2-rows-0-to-1
          1 column-2-rows-0-to-1
          2 row-2-col-2
          3 cornerrow-col-2-reduced-y
          4 lastrow
          (->> shape
               (key-place column row)))
      4 (case row
          0 column-4-rows-0-to-2
          1 column-4-rows-0-to-2
          2 column-4-rows-0-to-2
          3 cornerrow-col4
          (->> shape
               (key-place column row)))
      5 (case row
          0 column-5-rows-0-to-2
          1 column-5-rows-0-to-2
          2 column-5-rows-0-to-2
          3 cornerrow-col5
          (->> shape
               (key-place column row)))

      (->> shape
           (key-place column row))
      ))
  )

(defn test-sa-cap-cutout-mod-for-cornerrow-2 [keysize]
  (let [cutout-x 0.4
        cutout-y 0.4                                        ;was 1.95
        ;orig-cutout-y 1.95
        cutout-z-offset (- sa-cap-bottom-height-pressed 2.99)
        bl2 (case keysize
              1 (+ (/ sa-length 2) cutout-y)
              1.5 (+ (/ sa-length 2) cutout-y)
              2 (+ sa-length cutout-y))
        bw2 (case keysize
              1 (+ (/ sa-length 2) cutout-x)
              1.5 (+ (/ 27.94 2) cutout-x)
              2 (+ (/ sa-length 2) cutout-x))
        keycap-cutout-xy (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
        key-cap-cutout (hull (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 0.05]))
                             (->> keycap-cutout-xy
                                  (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                  (translate [0 0 sa-height])))
        mw2 bw2
        mh2 (+ (/ sa-length 2) 0)                           ;18.25 + 1.95
        ;mh2 (+ (/ sa-length 2) orig-cutout-y) ;18.25 + 1.95
        ;mh2 (/ mount-height 2)
        orig-keycap-cutout-xy (polygon [[mw2 mh2] [mw2 (- mh2)] [(- mw2) (- mh2)] [(- mw2) mh2]])
        dsa-keycap-lowest-bottom-height-from-plate 1
        diagonal-part (hull (->> orig-keycap-cutout-xy
                                 (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                 (translate [0 (- 1 cutout-y) 0.05])
                                 ;(translate [0 (+ 1 (- orig-cutout-y cutout-y)) 0.05])
                                 )
                            (->> keycap-cutout-xy
                                 (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                 (translate [0 0 dsa-keycap-lowest-bottom-height-from-plate])))
        ]
    (union
      (->> key-cap-cutout
           (translate [0 0 (+ cutout-z-offset dsa-keycap-lowest-bottom-height-from-plate)]) (color [0.5 0.5 0.5 0.5]))
      (->> diagonal-part (translate [0 0 cutout-z-offset]) (color PIN))
      ;(->> orig-keycap-cutout-xy (translate [ 0 0 4]) (color BLU))
      (->>
        (intersection (cube (+ 19 cutout-x) 20 (* dsa-keycap-lowest-bottom-height-from-plate 3)) key-cap-cutout) ; y was 2.3
        (translate [0 1 cutout-z-offset]) (color RED))      ;(translate [ 0 (+ 1.1 (/ mount-height 2)) cutout-z-offset]) (color RED))
      )                                                     ;TODO make diagonal edge on y0 side
    )
  )

(defn cap-cutout-handler-3x5-no-2-keys-last-row [keysize column row shape]
  (let [
        column-0-rows-0-1 (->> (sa-cap-cutout-mod-generic keysize
                                                          :reduced-y-cutout 0.285
                                                          :additional-x-cutout-left 1) (key-place column row))
        column-1-rows-0-1 (->> (sa-cap-cutout-mod-generic keysize
                                                          :reduced-y-cutout 0.2795
                                                          :additional-x-cutout-left 1) (key-place column row))
        column-2-3-row-0 (->> (sa-cap-cutout-mod-generic keysize
                                                         :reduced-y-cutout 0.6192
                                                         :additional-cutout-block true
                                                         :additional-cutout-block-y-offset 1.3
                                                         ) (key-place column row))
        column-3-row-1 (->> (sa-cap-cutout-mod-generic keysize
                                                       :reduced-y-cutout 0.6192) (key-place column row))
        column-4-rows-0-to-2 (->> (sa-cap-cutout-mod-generic keysize
                                                             :reduced-y-cutout 0.0645) (key-place column row))
        column-5-rows-0-to-2 (->> (sa-cap-cutout-mod-generic keysize
                                                             :reduced-y-cutout 0.0645
                                                             :additional-x-cutout-right 4) (key-place column row))
        cornerrow-left-outer (->> (sa-cap-cutout-mod-generic keysize
                                                             :reduced-y-cutout 1.55
                                                             :create-sa-cap-bottom-cutout true
                                                             :key-cap-lowest-bottom-z-height 1
                                                             :bottom-cutout-y-offset (- 1 0.4)
                                                             :mount-width-2-cap-cutout-diagonal-edges true
                                                             :additional-x-cutout-left 1
                                                             :additional-cutout-block true
                                                             :additional-cutout-block-y-offset 1.3
                                                             :additional-cutout-block-z-height 3
                                                             ) (key-place column row))
        cornerrow-reduced-y (->> (sa-cap-cutout-mod-generic keysize
                                                            :reduced-y-cutout 1.55
                                                            :create-sa-cap-bottom-cutout true
                                                            :key-cap-lowest-bottom-z-height 1
                                                            :bottom-cutout-y-offset (- 1 0.4)
                                                            :mount-width-2-cap-cutout-diagonal-edges true
                                                            :additional-x-cutout-left 1
                                                            :additional-cutout-block true
                                                            :additional-cutout-block-y-offset 1.06
                                                            :additional-cutout-block-z-height 3
                                                            ) (key-place column row))
        cornerrow-col3 (->> (sa-cap-cutout-mod-generic keysize
                                                       :reduced-y-cutout 1.55
                                                       :mount-width-2-cap-cutout-diagonal-edges true
                                                       :additional-cutout-block true
                                                       ) (key-place column row))
        cornerrow-col5 (->> (sa-cap-cutout-mod-generic keysize
                                                       :reduced-y-cutout 0.0645
                                                       :additional-x-cutout-right 4
                                                       :additional-cutout-block true
                                                       :additional-cutout-block-y-offset -3.9
                                                       :additional-cutout-block-z-height 7
                                                       ) (key-place column row))
        row-2-col-2 (->> (sa-cap-cutout-mod-generic keysize
                                                    :reduced-y-cutout 0.62
                                                    :additional-cutout-block true
                                                    :additional-cutout-block-y-offset -1.25) (key-place column row))
        cornerrow-col-2-reduced-y (->> (sa-cap-cutout-mod-generic keysize
                                                                  :reduced-y-cutout 1.55
                                                                  :create-sa-cap-bottom-cutout true
                                                                  :key-cap-lowest-bottom-z-height 1
                                                                  :bottom-cutout-y-offset (- 1 0.4)
                                                                  :mount-width-2-cap-cutout-diagonal-edges true
                                                                  :additional-cutout-block true
                                                                  :additional-cutout-block-y-offset 0.73
                                                                  :additional-cutout-block-z-height 4
                                                                  ) (key-place column row))
        row-2-left (->> (sa-cap-cutout-mod-generic keysize
                                                   :reduced-y-cutout 0.27
                                                   :additional-x-cutout-left 1
                                                   :additional-cutout-block false
                                                   :additional-cutout-block-y-offset -2.3) (key-place column row))
        lastrow (->> (sa-cap-cutout-mod-generic 1
                                                :reduced-y-cutout 1.55
                                                :create-mount-width-bottom-cutout true
                                                :key-cap-lowest-bottom-z-height 2
                                                :mount-width-2-cap-cutout-diagonal-edges true) (key-place column row))

        ]
    (case column
      0 (case row
          0 column-0-rows-0-1
          1 column-0-rows-0-1
          2 cornerrow-left-outer
          (->> shape
               (key-place column row)))
      1 (case row
          0 column-1-rows-0-1
          1 row-2-left
          2 cornerrow-reduced-y
          (->> shape
               (key-place column row)))
      2 (case row
          0 column-2-3-row-0
          1 row-2-col-2
          2 cornerrow-col-2-reduced-y
          (->> shape
               (key-place column row)))
      3 (case row
          0 column-2-3-row-0
          1 column-3-row-1
          2 cornerrow-col3
          (->> shape
               (key-place column row)))
      4 (case row
          0 column-5-rows-0-to-2
          1 column-5-rows-0-to-2
          2 cornerrow-col5
          (->> shape
               (key-place column row)))

      (->> shape
           (key-place column row))
      ))
  )

(defn cap-cutout-handler [keysize column row shape]
  (cond
    (and last_row_with_2_keys (and (== ncols 6) (== nrows 5))) (cap-cutout-handler-5x6-2-keys-last-row keysize column row shape)
    (and (not last_row_with_2_keys) (and (== ncols 5) (== nrows  3))) (cap-cutout-handler-3x5-no-2-keys-last-row keysize column row shape)
    :else nil
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 ncols))
(def rows (range 0 nrows))

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [; begin wonky code to handle tilted outer columns
        extra-row-tilt (if (> tilt-outer-columns 0)
                         (case column
                           0 tilt-outer-columns
                           5 (- tilt-outer-columns)
                           0
                           )
                         0
                         )
        extra-width-for-tilt (if (> tilt-outer-columns 0)
                               (case column
                                 0 0.55
                                 5 0.8
                                 0
                                 )
                               0
                               )
        extra-height-for-tilt (if (> tilt-outer-columns 0)
                                (case column
                                  0 0.5
                                  5 0.5
                                  0
                                  )
                                0
                                )
        extra-z-for-tilt (if (> tilt-outer-columns 0)
                           (case column
                             0 2.0
                             5 0.75
                             0
                             )
                           0
                           )
        extra-width (+ extra-width extra-width-for-tilt)
        ; end wonky code to handle tilted outer columns

        ; being wonky bottom row extra rotation code
        extra-rotation (if (and extra-curve-bottom-row
                                (= row lastrow))
                         -0.33
                         0)
        extra-rotation-offset (if (and extra-curve-bottom-row
                                       (= row lastrow))
                                0.07
                                0)
        extra-rotation-zheight (if (and extra-curve-bottom-row
                                        (= row lastrow))
                                 5.5
                                 0)
        ; end wonky bottom row extra rotation code

        column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                            (Math/sin (/ row-curvature 2)))
                         sa-cap-bottom-height)
        height-space (+ (extra-height column) extra-height-for-tilt)
        row-radius (+ (/ (/ (+ mount-height height-space) 2)
                         (Math/sin (/ (column-curvature column) 2)))
                      sa-cap-bottom-height)
        column-angle (* row-curvature (- centercol column))
        placed-shape (->> shape
                          (translate-fn [0 0 extra-z-for-tilt])
                          (rotate-y-fn (deg2rad extra-row-tilt))
                          (rotate-x-fn extra-rotation)
                          (translate-fn [0 0 extra-rotation-zheight])
                          (translate-fn [0 0 (- row-radius)])
                          (rotate-x-fn (* (+ extra-rotation-offset (column-curvature column))
                                          (- (centerrow column) (if (< nrows 5)
                                                                  (+ row 1)
                                                                  row))))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))]

    (->> placed-shape
         (rotate-y-fn tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      column row shape))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))

(defn key-places [shape]
  (apply union
         (for [column columns
               row rows
               :when (or (not last_row_with_2_keys) (or (.contains [2 3] column)
                                                        (not= row lastrow)))
               ]
           (if (.equals (sa-cap-cutout 1) shape)
             (cap-cutout-handler 1 column row shape)
             (if (.equals (hotswap-case-cutout true) shape) ; left
               (if (or (and (= column 0) (and (.contains [0 1 2 3] row))) (and (= column 4) (and (.contains [0 1 2] row))))
                 (->> (hotswap-case-cutout-less-left true)
                      (key-place column row))
                 (->> shape
                      (key-place column row)))
               (if (.equals (hotswap-case-cutout false) shape) ; right
                 (if (or (and (.contains [0 1] column) (and (.contains [0 1 2] row))) (and (= column 4) (and (.contains [1 2] row))))
                   (->> (hotswap-case-cutout-less-left false)
                        (key-place column row))
                   (->> shape
                        (key-place column row)))
                 (->> shape
                      (key-place column row)))
               )
             )
           )))
(def key-space-below
  (key-places switch-bottom))
(def caps
  (key-places (sa-cap 1)))
(def caps-cutout
  (key-places (sa-cap-cutout 1)))

(defn flex-pcb-holder-places [shape]
  (apply union
         (for [column columns] (->> shape (key-place column 0)))
         (for [column columns]
           (->> (->> shape
                     (mirror [1 0 0])
                     (mirror [0 1 0]))
                (key-place column
                           cornerrow))
           )))
(def flex-pcb-holders
  (flex-pcb-holder-places flex-pcb-holder))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

; posts are located at the inside corners of the key plates.
; the 'web' is the fill between key plates.
;

(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tm (translate [0 (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-bm (translate [0 (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))


; plate posts for connecting columns together without wasting material
; or blocking sides of hotswap sockets
(def plate-post-size 1.2)
(def plate-post-thickness (- web-thickness 2))
(def plate-post (->> (cube plate-post-size plate-post-size plate-post-thickness)
                     (translate [0 0 (+ plate-post-thickness (/ plate-post-thickness -1.5)
                                        )])))
(def plate-post-adj (/ plate-post-size 2))
(def plate-post-tr (translate [(- (/ mount-width 2) plate-post-adj) (- (/ mount-height 2) plate-post-adj) 0] plate-post))
(def plate-post-tm (translate [0 (- (/ mount-height 2) plate-post-adj) 0] plate-post))
(def plate-post-tl (translate [(+ (/ mount-width -2) plate-post-adj) (- (/ mount-height 2) plate-post-adj) 0] plate-post))
(def plate-post-bl (translate [(+ (/ mount-width -2) plate-post-adj) (+ (/ mount-height -2) plate-post-adj) 0] plate-post))
(def plate-post-bm (translate [0 (+ (/ mount-height -2) plate-post-adj) 0] plate-post))
(def plate-post-br (translate [(- (/ mount-width 2) plate-post-adj) (+ (/ mount-height -2) plate-post-adj) 0] plate-post))

; fat web post for very steep angles between thumb and finger clusters
; this ensures the walls stay somewhat thicker
(def fat-post-size 1.2)
(def fat-web-post (->> (cube fat-post-size fat-post-size web-thickness)
                       (translate [0 0 (+ (/ web-thickness -2)
                                          plate-thickness)])))

(def fat-post-adj (/ fat-post-size 2))
(def fat-web-post-tr (translate [(- (/ mount-width 2) fat-post-adj) (- (/ mount-height 2) fat-post-adj) 0] fat-web-post))
(def fat-web-post-tm (translate [0 (- (/ mount-height 2) fat-post-adj) 0] fat-web-post))
(def fat-web-post-tl (translate [(+ (/ mount-width -2) fat-post-adj) (- (/ mount-height 2) fat-post-adj) 0] fat-web-post))
(def fat-web-post-bl (translate [(+ (/ mount-width -2) fat-post-adj) (+ (/ mount-height -2) fat-post-adj) 0] fat-web-post))
(def fat-web-post-bm (translate [0 (+ (/ mount-height -2) fat-post-adj) 0] fat-web-post))
(def fat-web-post-br (translate [(- (/ mount-width 2) fat-post-adj) (+ (/ mount-height -2) fat-post-adj) 0] fat-web-post))
; wide posts for 1.5u keys in the main cluster

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(defn piramid-hulls [top & shapes]
  (apply union
         (map (partial apply hull top)
              (partition 2 1 shapes))))

(def connectors
  (union
    ;; Row connections
    (for [column (range 0 (dec ncols))
          row (range 0 (if last_row_with_2_keys
                         lastrow
                         nrows))]
      (if use_hotswap_holder
        (triangle-hulls
          (key-place (inc column) row plate-post-tl)
          (key-place column row plate-post-tr)
          (key-place (inc column) row plate-post-bl)
          (key-place column row plate-post-br))
        (triangle-hulls
          (key-place (inc column) row fat-web-post-tl)
          (key-place column row fat-web-post-tr)
          (key-place (inc column) row fat-web-post-bl)
          (key-place column row fat-web-post-br))
        )
      )

    ;; Column connections
    (for [column columns
          row (range 0 cornerrow)]
      (triangle-hulls
        (key-place column row web-post-bl)
        (key-place column row web-post-br)
        (key-place column (inc row) web-post-tl)
        (key-place column (inc row) web-post-tr)))

    ;; Diagonal connections
    (for [column (range 0 (dec ncols))
          row (range 0 cornerrow)]
      (if use_hotswap_holder
        (triangle-hulls
          (key-place column row plate-post-br)
          (key-place column (inc row) plate-post-tr)
          (key-place (inc column) row plate-post-bl)
          (key-place (inc column) (inc row) plate-post-tl))
        (triangle-hulls
          (key-place column row fat-web-post-br)
          (key-place column (inc row) fat-web-post-tr)
          (key-place (inc column) row fat-web-post-bl)
          (key-place (inc column) (inc row) fat-web-post-tl))
        )
      )

    ; top two to the main keyboard, starting on the left
    (when last_row_with_2_keys
      (->> (if use_hotswap_holder
             (triangle-hulls
               (key-place 2 lastrow plate-post-br)
               (key-place 3 lastrow plate-post-bl)
               (key-place 2 lastrow plate-post-tr)
               (key-place 3 lastrow plate-post-tl)
               (key-place 3 cornerrow plate-post-bl)
               (key-place 3 lastrow fat-web-post-tl)
               (key-place 3 cornerrow fat-web-post-bl)
               (key-place 3 lastrow fat-web-post-tr)
               (key-place 3 cornerrow fat-web-post-br)
               (key-place 3 lastrow plate-post-tr)
               (key-place 3 cornerrow plate-post-br)
               (key-place 4 cornerrow plate-post-bl))
             (triangle-hulls
               (key-place 2 lastrow fat-web-post-br)
               (key-place 3 lastrow fat-web-post-bl)
               (key-place 2 lastrow fat-web-post-tr)
               (key-place 3 lastrow fat-web-post-tl)
               (key-place 3 cornerrow fat-web-post-bl)
               (key-place 3 lastrow fat-web-post-tr)
               (key-place 3 cornerrow fat-web-post-br)
               (key-place 4 cornerrow fat-web-post-bl)))
           (color BLA)))

    (when last_row_with_2_keys
      (->> (if use_hotswap_holder
             (triangle-hulls
               (key-place 1 cornerrow plate-post-br)
               (key-place 2 lastrow plate-post-tl)
               (key-place 2 cornerrow plate-post-bl)
               (key-place 2 lastrow fat-web-post-tl)
               (key-place 2 cornerrow fat-web-post-bl)
               (key-place 2 lastrow fat-web-post-tr)
               (key-place 2 cornerrow fat-web-post-br)
               (key-place 2 lastrow plate-post-tr)
               (key-place 2 cornerrow plate-post-br)
               (key-place 3 cornerrow plate-post-bl))
             (triangle-hulls
               (key-place 1 cornerrow fat-web-post-br)
               (key-place 2 lastrow fat-web-post-tl)
               (key-place 2 cornerrow fat-web-post-bl)
               (key-place 2 lastrow fat-web-post-tr)
               (key-place 2 cornerrow fat-web-post-br)
               (key-place 3 cornerrow fat-web-post-bl)))
           (color GRE)))

    (when last_row_with_2_keys
      (->> (if use_hotswap_holder
             (triangle-hulls
               (key-place 3 lastrow plate-post-tr)
               (key-place 3 lastrow plate-post-br)
               (key-place 3 lastrow plate-post-tr)
               (key-place 4 cornerrow plate-post-bl))
             (triangle-hulls
               (key-place 3 lastrow fat-web-post-tr)
               (key-place 3 lastrow fat-web-post-br)
               (key-place 3 lastrow fat-web-post-tr)
               (key-place 4 cornerrow fat-web-post-bl)))
           (color CYA)))

    (->> (if use_hotswap_holder
           (triangle-hulls
             (key-place 1 cornerrow plate-post-br)
             (key-place 2 lastrow plate-post-tl)
             (key-place 2 lastrow plate-post-bl))

           (triangle-hulls
             (key-place 1 cornerrow fat-web-post-br)
             (key-place 2 lastrow fat-web-post-tl)
             (key-place 2 lastrow fat-web-post-bl)))
         (color MAG))
    )
  )

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(def thumborigin
  (map + (key-position 1 cornerrow [(/ mount-width 2)
                                    (- (/ mount-height 2))
                                    0])
       thumb-pos))

"need to account for plate thickness which is baked into thumb-_-place rotation & move values
when plate-thickness was 2
need to adjust for difference for thumb-z only"
(def thumb-design-z 2)
(def thumb-z-adjustment (- (if (> plate-thickness thumb-design-z)
                             (- thumb-design-z plate-thickness)
                             (if (< plate-thickness thumb-design-z)
                               (- thumb-design-z plate-thickness)
                               0))
                           1.1))
(def thumb-x-rotation-adjustment -12)                       ; globally adjust front/back tilt of thumb keys
(defn thumb-place [rot move shape]
  (->>
    (->> shape
         (translate [0 0 thumb-z-adjustment])               ;adapt thumb positions for increased plate
         (rotate (deg2rad thumb-x-rotation-adjustment) [1 0 0]) ;adjust angle of all thumbs to be less angled down towards user since key is taller

         (rotate (deg2rad (nth rot 0)) [1 0 0])
         (rotate (deg2rad (nth rot 1)) [0 1 0])
         (rotate (deg2rad (nth rot 2)) [0 0 1])
         (translate thumborigin)
         (translate move))

    (rotate (deg2rad (nth thumb-rot 0)) [1 0 0])
    (rotate (deg2rad (nth thumb-rot 1)) [0 1 0])
    (rotate (deg2rad (nth thumb-rot 2)) [0 0 1])))

; convexer
(defn thumb-r-place [shape] (thumb-place [14 -35 10] [-14.5 -10 5] shape)) ; right
(defn thumb-m-place [shape] (thumb-place [8 -21.5 20] [-33 -15.2 -6] shape)) ; middle
(defn thumb-l-place [shape] (thumb-place [6 -5 25] [-53 -23.5 -11.5] shape)) ; left

(defn thumb-layout [shape]
  (union
    (thumb-r-place shape)
    (thumb-m-place shape)
    (thumb-l-place shape)))

(def thumbcaps (thumb-layout (if rendered-caps
                               (->> (import "../things/SA-R1.stl")
                                    (translate [0 0 sa-cap-bottom-height])
                                    (rotate-shape-on-z 90)
                                    (color KEYCAP)
                                    )
                               (sa-cap 1))))
(def thumbcaps-cutout
  (let [
        thumb-cutout-reduced (sa-cap-cutout-mod-generic 1
                                                        :reduced-y-cutout 1.55
                                                        :create-mount-width-bottom-cutout true
                                                        :mount-width-2-cap-cutout-diagonal-edges true
                                                        :key-cap-lowest-bottom-z-height 1.5)
        ]
    (thumb-layout (rotate (deg2rad -90) [0 0 1] thumb-cutout-reduced)))
  )
(def thumb-space-below (thumb-layout switch-bottom))
(defn thumb-key-cutouts [mirror-internals]
  (thumb-layout (single-plate-cut mirror-internals)))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(def wall-border-z-offset -0.75)                            ; length of the first downward-sloping part of the wall (negative)
(def wall-border-xy-offset 1.1)
(def wall-border-thickness 1)                               ; wall thickness parameter

(defn wall-locate0 [dx dy border] [(* dx (if border wall-border-thickness wall-thickness))
                                   (* dy (if border wall-border-thickness wall-thickness))
                                   0])
(defn wall-locate1 [dx dy border] [(* dx (if border wall-border-thickness wall-thickness))
                                   (* dy (if border wall-border-thickness wall-thickness))
                                   0])
(defn wall-locate2 [dx dy border] [(* dx (if border wall-border-xy-offset wall-xy-offset))
                                   (* dy (if border wall-border-xy-offset wall-xy-offset))
                                   (if border wall-border-z-offset wall-z-offset)])
(defn wall-locate3 [dx dy border] [(* dx (+ (if border wall-border-xy-offset wall-xy-offset) (if border wall-border-thickness wall-thickness)))
                                   (* dy (+ (if border wall-border-xy-offset wall-xy-offset) (if border wall-border-thickness wall-thickness)))
                                   (* 2 (if border wall-border-z-offset wall-z-offset))])

(def thumb-connectors
  (union
    ; top two
    (->> (if use_hotswap_holder
           (triangle-hulls
             (thumb-m-place plate-post-tr)
             (thumb-m-place plate-post-br)
             (thumb-r-place plate-post-tl)
             (thumb-r-place plate-post-bl))
           (triangle-hulls
             (thumb-m-place web-post-tr)
             (thumb-m-place web-post-br)
             (thumb-r-place web-post-tl)
             (thumb-r-place web-post-bl))
           ) (color RED))
    (->> (if use_hotswap_holder
           (triangle-hulls
             (thumb-m-place plate-post-tl)
             (thumb-l-place plate-post-tr)
             (thumb-m-place plate-post-bl)
             (thumb-l-place plate-post-br)
             (thumb-m-place plate-post-bl))
           (triangle-hulls
             (thumb-m-place web-post-tl)
             (thumb-l-place web-post-tr)
             (thumb-m-place web-post-bl)
             (thumb-l-place web-post-br)
             (thumb-m-place web-post-bl))
           ) (color ORA))

    (hull                                                   ; between thumb m and top key
      (key-place 0 cornerrow (translate (wall-locate1 -1.5 0 false) web-post-bl))
      (thumb-m-place web-post-tr)
      (thumb-m-place web-post-tl))
    (->> (piramid-hulls                                     ; top ridge thumb side
           (key-place 0 cornerrow (translate (wall-locate1 -1.5 0 false) fat-web-post-bl))
           (key-place 0 cornerrow (translate (wall-locate2 -1.5 0 false) fat-web-post-bl))
           (key-place 0 cornerrow web-post-bl)
           ;(thumb-r-place web-post-tr)
           (thumb-r-place web-post-tl)
           (thumb-m-place fat-web-post-tr)
           (thumb-m-place fat-web-post-tl)
           (thumb-l-place fat-web-post-tr)
           (key-place 0 cornerrow (translate (wall-locate2 -1.5 0 false) fat-web-post-bl))
           ) (color BLA))
    (->> (triangle-hulls
           (key-place 0 cornerrow fat-web-post-br)
           (key-place 0 cornerrow fat-web-post-bl)
           (thumb-r-place web-post-tl)
           (key-place 1 cornerrow web-post-bl)
           (key-place 1 cornerrow web-post-br)) (color BLU))
    (->> (triangle-hulls
           (thumb-r-place fat-web-post-tl)
           (thumb-r-place (translate (wall-locate1 1.5 1.5 false) fat-web-post-tr))
           (key-place 1 cornerrow web-post-br)
           ; (key-place 2 lastrow web-post-tl)
           ) (color NBL))
    (->> (triangle-hulls
           (key-place 2 lastrow web-post-tl)
           ; (thumb-r-place fat-web-post-tr)
           ; (key-place 2 lastrow web-post-bl)
           (thumb-r-place fat-web-post-br)) (color PUR))


    (when (not last_row_with_2_keys)
      (union
        (->> (triangle-hulls
               (thumb-r-place fat-web-post-tr)
               (thumb-r-place (translate (wall-locate1 1.5 1.5 false) fat-web-post-tr))
               (key-place 1 lastrow web-post-br)

               (thumb-r-place (translate (wall-locate3 1.5 1.5 false) fat-web-post-tr))
               (key-place 1 lastrow web-post-br)
               (key-place 2 lastrow web-post-bl)

               (thumb-r-place (translate (wall-locate3 1.5 1.5 false) fat-web-post-tr))
               (key-place 2 lastrow web-post-bl)
               (key-place 2 lastrow web-post-br)

               (thumb-r-place (translate (wall-locate3 1.5 1.5 false) fat-web-post-tr))
               (key-place 2 lastrow web-post-br)
               (key-place 3 lastrow web-post-bl)

               (thumb-r-place (translate (wall-locate3 1.5 1.5 false) fat-web-post-tr))
               (key-place 3 lastrow (translate (wall-locate3 0 -1.5 false) web-post-bl))
               (key-place 3 lastrow web-post-bl)
               ) (color PUR))
        (->> (hull
               (key-place 3 lastrow web-post-bl)
               (key-place 3 lastrow (translate (wall-locate1 0 -1.5 false) web-post-bl))
               (key-place 3 lastrow (translate (wall-locate2 -1.5 -1.5 false) fat-web-post-bl))
               (thumb-r-place (translate (wall-locate3 1.5 -1.5 false) fat-web-post-br))
               (thumb-r-place (translate (wall-locate3 1.5 1.5 false) fat-web-post-tr))
               ) (color GRE))
        )
      )


    (when last_row_with_2_keys
      (union
        (->> (triangle-hulls
               (thumb-r-place web-post-br)
               (key-place 2 lastrow web-post-bl)
               (if use_hotswap_holder (key-place 3 lastrow plate-post-bl)
                                      (key-place 3 lastrow web-post-bl))
               (key-place 2 lastrow web-post-br)) (color GRE))
        (->> (triangle-hulls
               (thumb-r-place web-post-tr)
               (thumb-r-place web-post-br)
               (key-place 2 lastrow plate-post-bl)
               ) (color RED))
        (->> (triangle-hulls
               (thumb-r-place web-post-tr)
               (key-place 2 lastrow plate-post-bl)
               (key-place 2 lastrow plate-post-tl)
               ;(key-place 1 cornerrow web-post-br)
               ) (color MAG))
        (->> (triangle-hulls
               (thumb-r-place web-post-tr)
               (key-place 2 lastrow plate-post-tl)
               (key-place 1 cornerrow web-post-br)
               ) (color ORA)))
      )
    )
  )

; dx1, dy1, dx2, dy2 = direction of the wall. '1' for front, '-1' for back, '0' for 'not in this direction'.
; place1, place2 = function that places an object at a location, typically refers to the center of a key position.
; post1, post2 = the shape that should be rendered
(defn wall-brace [place1 dx1 dy1 post1
                  place2 dx2 dy2 post2
                  border]
  "If you want to change the wall, use this.
   place1 means the location at the keyboard, marked by key-place or thumb-xx-place
   dx1 means the movement from place1 in x coordinate, multiplied by wall-xy-locate.
   dy1 means the movement from place1 in y coordinate, multiplied by wall-xy-locate.
   post1 means the position this wall attached to place1.
         xxxxx-br means bottom right of the place1.
         xxxxx-bl means bottom left of the place1.
         xxxxx-tr means top right of the place1.
         xxxxx-tl means top left of the place1.
   place2 means the location at the keyboard, marked by key-place or thumb-xx-place
   dx2 means the movement from place2 in x coordinate, multiplied by wall-xy-locate.
   dy2 means the movement from place2 in y coordinate, multiplied by wall-xy-locate.
   post2 means the position this wall attached to place2.
         xxxxx-br means bottom right of the place2.
         xxxxx-bl means bottom left of the place2.
         xxxxx-tr means top right of the place2.
         xxxxx-tl means top left of the place2.
   How does it work?
   Given the following wall
       a ==\\ b
            \\
           c \\ d
             | |
             | |
             | |
             | |
           e | | f
   In this function a: usually the wall of a switch hole.
                    b: the result of hull and translation from wall-locate1
                    c: the result of hull and translation from wall-locate2
                    d: the result of hull and translation from wall-locate3
                    e: the result of bottom-hull translation from wall-locate2
                    f: the result of bottom-hull translation from wall-locate3"
  (union
    (->> (hull
           (place1 post1)
           (place1 (translate (wall-locate1 dx1 dy1 border) post1))
           ; (place1 (translate (wall-locate2 dx1 dy1 border) post1))
           (place1 (translate (wall-locate2 dx1 dy1 border) post1))
           (place2 post2)
           (place2 (translate (wall-locate1 dx2 dy2 border) post2))
           ; (place2 (translate (wall-locate2 dx2 dy2 border) post2))
           (place2 (translate (wall-locate2 dx2 dy2 border) post2))
           )
         (color YEL))
    (if (not border)
      (->> (bottom-hull
             (place1 (translate (wall-locate2 dx1 dy1 border) post1))
             ; (place1 (translate (wall-locate2 dx1 dy1 border) post1))
             (place2 (translate (wall-locate2 dx2 dy2 border) post2))
             ; (place2 (translate (wall-locate2 dx2 dy2 border) post2))
             )
           (color ORA))
      )
    ))

(defn wall-brace-deeper [place1 dx1 dy1 post1
                         place2 dx2 dy2 post2
                         border]
  "try to extend back wall further back for certain sections"
  (union
    (->> (hull
           (place1 post1)
           (place1 (translate (wall-locate1 dx1 dy1 border) post1))
           ; (place1 (translate (wall-locate3 dx1 dy1 border) post1))
           (place1 (translate (wall-locate3 dx1 dy1 border) post1))

           (place2 post2)
           (place2 (translate (wall-locate1 dx2 dy2 border) post2))
           ; (place2 (translate (wall-locate3 dx2 dy2 border) post2))
           (place2 (translate (wall-locate3 dx2 dy2 border) post2))
           )
         (color BLU))
    (if (not border)
      (->> (bottom-hull
             (place1 (translate (wall-locate3 dx1 dy1 border) post1))
             ; (place1 (translate (wall-locate3 dx1 dy1 border) post1))

             (place2 (translate (wall-locate3 dx2 dy2 border) post2))
             ; (place2 (translate (wall-locate3 dx2 dy2 border) post2))
             )
           (color YEL))
      )
    ))

(defn wall-brace-back [place1 dx1 dy1 post1
                       place2 dx2 dy2 post2
                       border]
  (union
    (->> (hull
           (place1 post1)
           (place1 (translate (wall-locate1 dx1 dy1 border) post1))
           ;(place1 (translate (wall-locate3 dx1 dy1 border) post1))
           (place1 (translate (wall-locate2 dx1 dy1 border) post1))

           (place2 post2)
           (place2 (translate (wall-locate1 dx2 dy2 border) post2))
           ; (place2 (translate (wall-locate3 dx2 dy2 border) post2))
           (place2 (translate (wall-locate2 dx2 dy2 border) post2))
           )
         (color PUR))
    ; This fixes the uneven part
    (->> (hull
           (place1 web-post-tl)
           (place1 (translate (wall-locate1 dx1 dy1 border) web-post-tl))
           (place1 (translate (wall-locate2 dx1 dy1 border) web-post-tl))
           (translate [1.18 5 -6] (place1 web-post-tl))

           (place2 web-post-tr)
           (place2 (translate (wall-locate1 dx2 dy2 border) web-post-tr))
           (place2 (translate (wall-locate3 dx2 dy2 border) web-post-tr))
           )
         (color RED))
    (if (not border)
      (->> (bottom-hull
             (place1 (translate (wall-locate2 dx1 dy1 border) post1))
             (place1 (translate (wall-locate2 dx1 dy1 border) post1))

             (place2 (translate (wall-locate3 dx2 dy2 border) post2))
             (place2 (translate (wall-locate3 dx2 dy2 border) post2))
             )
           (color MAG))
      )
    )
  )

(defn wall-brace-left [place1 dx1 dy1 post1
                       place2 dx2 dy2 post2
                       border]
  (union
    (->> (hull
           (place1 post1)
           (place1 (translate (wall-locate1 dx1 dy1 border) post1))
           (place1 (translate (wall-locate3 dx1 dy1 border) post1))
           (place1 (translate (wall-locate2 dx1 dy1 border) post1))

           (place2 post2)
           (place2 (translate (wall-locate1 dx2 dy2 border) post2))
           ; (place2 (translate (wall-locate3 dx2 dy2 border) post2))
           (place2 (translate (wall-locate2 dx2 dy2 border) post2))
           )
         (color CYA))
    (if (not border)
      (->> (bottom-hull
             (place1 (translate (wall-locate3 dx1 dy1 border) post1))
             (place1 (translate (wall-locate3 dx1 dy1 border) post1))

             (place2 (translate (wall-locate2 dx2 dy2 border) post2))
             (place2 (translate (wall-locate2 dx2 dy2 border) post2))
             )
           (color NBL))
      )
    )
  )

(defn wall-brace-less [place1 dx1 dy1 post1
                       place2 dx2 dy2 post2
                       border]
  (union
    (->> (hull
           (place1 post1)
           (place1 (translate (wall-locate1 dx1 dy1 border) post1))
           ; (place1 (translate (wall-locate2 dx1 dy1 border) post1))
           (place1 (translate (wall-locate1 dx1 dy1 border) post1))
           (place2 post2)
           (place2 (translate (wall-locate1 dx2 dy2 border) post2))
           ; (place2 (translate (wall-locate2 dx2 dy2 border) post2))
           (place2 (translate (wall-locate1 dx2 dy2 border) post2))
           )
         (color YEL))
    (if (not border)
      (->> (bottom-hull
             (place1 (translate (wall-locate1 dx1 dy1 border) post1))
             ; (place1 (translate (wall-locate2 dx1 dy1 border) post1))
             (place2 (translate (wall-locate1 dx2 dy2 border) post2))
             ; (place2 (translate (wall-locate2 dx2 dy2 border) post2))
             )
           (color ORA))
      )
    ))

(defn key-wall-brace-less [x1 y1 dx1 dy1 post1
                           x2 y2 dx2 dy2 post2
                           border]
  (wall-brace-less (partial key-place x1 y1) dx1 dy1 post1
                   (partial key-place x2 y2) dx2 dy2 post2
                   border))

(defn key-wall-brace [x1 y1 dx1 dy1 post1
                      x2 y2 dx2 dy2 post2
                      border]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2
              border))


(defn key-wall-brace-left [x1 y1 dx1 dy1 post1
                           x2 y2 dx2 dy2 post2
                           border]
  (wall-brace-left
    (partial key-place x1 y1) dx1 dy1 post1
    (partial key-place x2 y2) dx2 dy2 post2
    border))

(defn key-wall-brace-back [x1 y1 dx1 dy1 post1
                           x2 y2 dx2 dy2 post2
                           border]
  (wall-brace-back
    (partial key-place x1 y1) dx1 dy1 post1
    (partial key-place x2 y2) dx2 dy2 post2
    border))

(defn key-wall-brace-deeper [x1 y1 dx1 dy1 post1
                             x2 y2 dx2 dy2 post2
                             border]
  (wall-brace-deeper
    (partial key-place x1 y1) dx1 dy1 post1
    (partial key-place x2 y2) dx2 dy2 post2
    border))

(defn key-corner [x y loc border]
  (case loc
    :tl (union
          (key-wall-brace x y 0   1.5 fat-web-post-tl x y -1.5 0 fat-web-post-tl border)
          (key-wall-brace x y 0   2.5 fat-web-post-tl x y -1.5 0 fat-web-post-tl border)
          )
    :tr (union
          (key-wall-brace x y 1.5 1.5 fat-web-post-tr x y  1.5 0 fat-web-post-tr border)
          (key-wall-brace x y 1.5 2.5 fat-web-post-tr x y  1.5 0 fat-web-post-tr border))
    :bl (key-wall-brace x y 0  -1.5 fat-web-post-bl x y -1.5 0 fat-web-post-bl border)
    :br (key-wall-brace x y 0  -1.5 fat-web-post-br x y  1.5 0 fat-web-post-br border)))

(defn right-wall [border]
  (let [row-range (if last_row_with_2_keys lastrow nrows)]
    (union
      (key-corner lastcol 0 :tr border)
      (for [y (range 0 row-range)] (case y 0 (key-wall-brace-less lastcol y 1.5 2.5 web-post-tr lastcol y 1.5 0 web-post-br border)
                                           (key-wall-brace-less lastcol y 1.5 0 web-post-tr lastcol y 1.5 0 web-post-br border)))
      (for [y (range 1 row-range)] (key-wall-brace-less lastcol (dec y) 1.5 0 web-post-br lastcol y 1.5 0 web-post-tr border))
      (key-corner lastcol cornerrow :br border)
      ))
  )

(defn back-wall [border]
  (union
    (for [c (range 0 ncols)]
      (union
        (cond (== c 0) (key-wall-brace c 0 0 1.5 web-post-tl c 0 0 1.5 web-post-tr border)
              (== c 1) (key-wall-brace c 0 0 1.5 web-post-tl c 0 0 1.5 web-post-tr border)
              (== c lastcol) (key-wall-brace c 0 0 1.5 web-post-tl c 0 1.5 1.5 web-post-tr border)
              :else (key-wall-brace c 0 0 1.5 web-post-tl c 0 0 1.5 web-post-tr border)
              )
        (cond (== c 0) (key-wall-brace c 0 0 2.5 web-post-tl c 0 0 2.5 web-post-tr border)
              (== c 1) (key-wall-brace c 0 0 2.5 web-post-tl c 0 0 2.5 web-post-tr border)
              (== c lastcol) (key-wall-brace c 0 0 2.5 web-post-tl c 0 1.5 2.5 web-post-tr border)
              :else (key-wall-brace c 0 0 2.5 web-post-tl c 0 0 2.5 web-post-tr border)
              ))
      )
    (for [c (range 1 ncols)]
      (union
        (case c 1 (key-wall-brace c 0 0 1.5 web-post-tl (dec c) 0 0 1.5 web-post-tr border)
                2 (key-wall-brace c 0 0 1.5 fat-web-post-tl (dec c) 0 0 1.5 fat-web-post-tr border)
                (->> (key-wall-brace c 0 0 1.5 fat-web-post-tl (dec c) 0 0 1.5 fat-web-post-tr border) (color PUR))
                )
        (case c 1 (key-wall-brace c 0 0 2.5 web-post-tl (dec c) 0 0 2.5 web-post-tr border)
                2 (key-wall-brace c 0 0 2.5 fat-web-post-tl (dec c) 0 0 2.5 fat-web-post-tr border)
                (->> (key-wall-brace c 0 0 2.5 fat-web-post-tl (dec c) 0 0 2.5 fat-web-post-tr border) (color PUR))
                ))
      )
    )
  )

(defn left-wall [border]
  (let [row-range (if last_row_with_2_keys lastrow nrows)]
    (union
      ; left-back-corner
      (->> (key-corner 0 0 :tl border)
           (color GRE))
      ; (key-wall-brace  0 0  -1 0 web-post-tl 0 1 -1 0 web-post-bl border)

      (for [y (range 0 row-range)] (key-wall-brace 0 y -1.5 0 web-post-tl 0 y -1.5 0 web-post-bl border))
      (for [y (range 1 row-range)] (key-wall-brace 0 (dec y) -1.5 0 web-post-bl 0 y -1.5 0 web-post-tl border))

      ; thumb connector
      (->> (wall-brace (partial key-place 0 cornerrow) -1.5 0 web-post-bl thumb-l-place 0 1.5 fat-web-post-tr border) (color WHI))
      ))
  )

(defn front-wall [border]
  (union
    (key-wall-brace 3 lastrow 0 -1.5 web-post-bl 3 lastrow 0.5 -1.5 web-post-br border)
    (key-wall-brace 3 lastrow 0 -1.5 fat-web-post-br 4 cornerrow -1 -1.5 fat-web-post-bl border)
    (for [x (range 4 ncols)] (key-wall-brace x cornerrow 0 -1.5 fat-web-post-bl x cornerrow 0 -1.5 fat-web-post-br border)) ; TODO fix extra wall
    (for [x (range 5 ncols)] (key-wall-brace x cornerrow -1 -1.5 fat-web-post-bl (dec x) cornerrow 0 -1.5 fat-web-post-br border))
    (when last_row_with_2_keys
      (->> (wall-brace thumb-r-place 0 -1.5 fat-web-post-br (partial key-place 3 lastrow) 0 -1.5 web-post-bl border)
           (color RED)))
    )
  )

(defn thumb-wall [border]
  (union
    ; thumb walls
    (when (not last_row_with_2_keys)
      (union                                                ;manual wall-brace-deeper
        (hull
          (thumb-r-place fat-web-post-tr)
          (thumb-r-place (translate (wall-locate1 1.5 1.5 border) fat-web-post-tr))
          (thumb-r-place (translate (wall-locate3 1.5 1.5 border) fat-web-post-tr))

          (thumb-r-place fat-web-post-br)
          (thumb-r-place (translate (wall-locate1 1.5 -1.5 border) fat-web-post-br))
          (thumb-r-place (translate (wall-locate3 1.5 -1.5 border) fat-web-post-br))
          )
        (if (not border)
          (bottom-hull
            (thumb-r-place (translate (wall-locate3 1.5 -1.5 border) fat-web-post-br))
            (thumb-r-place (translate (wall-locate3 1.5 0 border) fat-web-post-tr))
            (key-place 3 lastrow (translate (wall-locate2 -1 -1.5 border) fat-web-post-bl))
            ))))

    (->> (wall-brace-deeper thumb-r-place 1.5 -1.5 fat-web-post-br thumb-r-place 0 -1.5 fat-web-post-bl border) (color ORA))
    (->> (wall-brace thumb-r-place 1.5 -1.5 fat-web-post-br thumb-r-place 0 -1.5 fat-web-post-bl border) (color ORA))
    (->> (wall-brace-deeper thumb-m-place 0 -1.3 fat-web-post-br thumb-m-place 0 -1.3 fat-web-post-bl border) (color YEL))
    (->> (wall-brace thumb-m-place 0 -1.5 fat-web-post-br thumb-m-place 0 -1.5 fat-web-post-bl border) (color YEL))
    (->> (wall-brace-deeper thumb-l-place 0 -1.3 fat-web-post-br thumb-l-place 0 -1.3 fat-web-post-bl border) (color GRE))
    (->> (wall-brace thumb-l-place 0 -1.5 fat-web-post-br thumb-l-place 0 -1.5 fat-web-post-bl border) (color GRE))
    (->> (wall-brace-deeper thumb-l-place 0 1.5 fat-web-post-tr thumb-l-place 0 1.5 fat-web-post-tl border) (color CYA))
    (->> (wall-brace thumb-l-place 0 1.5 fat-web-post-tr thumb-l-place 0 1.5 fat-web-post-tl border) (color CYA))
    (->> (wall-brace-deeper thumb-l-place -1.3 0 fat-web-post-tl thumb-l-place -1.3 0 fat-web-post-bl border) (color BLU))
    ; thumb corners
    (->> (wall-brace-deeper thumb-l-place -1.3 0 fat-web-post-bl thumb-l-place 0 -1.3 fat-web-post-bl border) (color NBL))
    (->> (wall-brace-deeper thumb-l-place -1.3 0 fat-web-post-tl thumb-l-place 0 1.5 fat-web-post-tl border) (color PUR))
    ; thumb tweeners
    (->> (wall-brace-deeper thumb-r-place 0 -1.5 fat-web-post-bl thumb-m-place 0 -1.3 fat-web-post-br border) (color PIN))
    (->> (wall-brace thumb-r-place 0 -1.5 fat-web-post-bl thumb-m-place 0 -1.5 fat-web-post-br border) (color PIN))
    (->> (wall-brace-deeper thumb-m-place 0 -1.3 fat-web-post-bl thumb-l-place 0 -1.3 fat-web-post-br border) (color MAG))
    (->> (wall-brace thumb-m-place 0 -1.5 fat-web-post-bl thumb-l-place 0 -1.5 fat-web-post-br border) (color MAG))
    ; (->> (wall-brace-deeper thumb-m-place  0  1 fat-web-post-tl thumb-l-place  0  1 fat-web-post-tr border) (color BRO))
    (->> (wall-brace thumb-l-place -1.5 0 fat-web-post-bl thumb-l-place -1.5 0 fat-web-post-tl border) (color BLA))
    )
  )

(def case-walls
  (union
    (right-wall false)
    (back-wall false)
    (left-wall false)
    (front-wall false)
    (thumb-wall false)
    )
  )

(def case-top-border
  (union
    (right-wall true)
    (back-wall true)
    (left-wall true)
    (front-wall true)
    (thumb-wall true)
    )
  )

;;;;;;;;;;;;;;;;;;;
;; Screw Inserts ;;
;;;;;;;;;;;;;;;;;;;

; Screw insert definition & position
(defn screw-insert-shape [bottom-radius top-radius height]
  (->> (binding [*fn* 30]
         (cylinder [bottom-radius top-radius] height)))
  )

(defn screw-insert [column row bottom-radius top-radius height offset]
  (let [position (key-position column row [0 0 0])]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate (map + offset [(first position) (second position) (/ height 2)])))))


(def screw-insert-bottom-offset 0)
(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union
    (->> (screw-insert 2 0 bottom-radius top-radius (/ height 1.3) [0 6 screw-insert-bottom-offset]) (color RED)) ; top middle
    (->> (screw-insert 0 2 bottom-radius top-radius height [ -7 12 screw-insert-bottom-offset]) (color PIN)) ; left
    (if recess-bottom-plate
      (->> (screw-insert 0 2 bottom-radius top-radius height [-8 8 screw-insert-bottom-offset]) (color NBL))) ; left-thumb
    (->> (screw-insert 0 lastrow bottom-radius top-radius height [-17 -30 screw-insert-bottom-offset]) (color BRO)) ; thumb
    (->> (screw-insert (- lastcol 1) 0 bottom-radius top-radius (/ height 1.75) [12.5 -9 screw-insert-bottom-offset]) (color PUR)) ; top right
    (->> (screw-insert 2 (+ lastrow 1) bottom-radius top-radius (/ height 1.6) [6 10 screw-insert-bottom-offset]) (color BLA)) ; bottom middle
    ))

(def screw-insert-height 16.5)                              ; Hole Depth Y: 4.4
(def screw-insert-radius (/ 4.0 2))                         ; Hole Diameter H: 4.0-4.1

(def screw-insert-holes (screw-insert-all-shapes
                          screw-insert-radius
                          screw-insert-radius
                          (* screw-insert-height 1.5)
                          ))

(def screw-insert-wall-thickness 4)
(def screw-insert-outers (screw-insert-all-shapes
                           (+ screw-insert-radius screw-insert-wall-thickness)
                           (+ screw-insert-radius screw-insert-wall-thickness)
                           screw-insert-height
                           ))

(defn top-screw-insert-all-shapes [bottom-radius top-radius height]
  (union
    (->> (screw-insert 3 0 bottom-radius top-radius height [-4 4 2]) (color RED)) ; top middle
    (->> (screw-insert 0 1 bottom-radius top-radius height [-5.5 10.75 14]) (color PIN)) ; left
    (->> (screw-insert 0 3 bottom-radius top-radius height [-10.5 22 15]) (color NBL)) ; left-thumb
    (->> (screw-insert 0 lastrow bottom-radius top-radius height [-16.5 0 15]) (color BRO)) ; thumb
    (->> (screw-insert lastcol 0 bottom-radius top-radius height [4 -9.5 0.3]) (color PUR)) ; top right TODO fix position/overall keyboard height?
    ; (->> (screw-insert lastcol       0 bottom-radius top-radius height [ -2  -54.5   0]) (color GRE)) ; bottom right
    (->> (screw-insert 0 lastrow bottom-radius top-radius height [9 -2.5 18]) (color CYA)) ; bottom thumb
    (->> (screw-insert 3 lastrow bottom-radius top-radius height [-5 -5 16]) (color BLA)) ; bottom middle
    ))

(def top-screw-length 16)                                   ; M2/M3 screw thread length
(def top-screw-insert-height 10)                            ; M2/M3 screw insert length 3.5, use higher value to cut through angled things

; (def top-screw-insert-radius (/ 3.0 2)) ; M2 screw insert diameter
; (def top-screw-radius (/ 2.1 2))        ; M2 screw diameter
; (def top-screw-head-radius (/ 3.55 2))  ; M2 screw head diameter (3.4 plus some clearance)

(def top-screw-insert-radius (/ 3.3 2))                     ; M3 screw insert diameter
(def top-screw-radius (/ 2.6 2))                            ; M3 screw diameter
(def top-screw-head-radius (/ 4.55 2))                      ; M3 screw head diameter (4.4 plus some clearance)

(def top-screw-clear-length (- top-screw-length top-screw-insert-height))
(def top-screw-block-height 4)
(def top-screw-block-wall-thickness 4)

(def top-screw (top-screw-insert-all-shapes
                 top-screw-radius
                 top-screw-radius
                 top-screw-length
                 ))

(def top-screw-insert-holes
  (union
    ; actual threaded insert hole
    (translate [0 0 top-screw-clear-length]
               (top-screw-insert-all-shapes
                 top-screw-insert-radius
                 top-screw-insert-radius
                 top-screw-insert-height
                 ))

    ; clearance and possible drainage hole through top of case
    (translate [0 0 2]
               top-screw)

    ; screw head clearance
    (translate [0 0 (- (* 1.5 top-screw-length))]
               (top-screw-insert-all-shapes
                 top-screw-head-radius
                 top-screw-head-radius
                 (* 1.5 top-screw-length)
                 ))
    ))

(def top-screw-block-outers
  (difference
    (top-screw-insert-all-shapes
      (+ top-screw-insert-radius top-screw-block-wall-thickness)
      (+ top-screw-insert-radius top-screw-block-wall-thickness)
      top-screw-block-height
      )
    top-screw
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USB Controller Holder ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def usb-holder-vertical false)
(def usb-holder-stl
  (if usb-holder-vertical
    (import "../things/usb_holder_vertical.stl")
    (import "../things/usb_holder.stl")
    )
  )
(def usb-holder-cutout-stl
  (if usb-holder-vertical
    (import "../things/usb_holder_vertical_cutout.stl")
    (import "../things/usb_holder_w_reset_cutout.stl")
    )
  )
(def usb-holder-cutout-height
  (if usb-holder-vertical
    (* 30.6 2)
    30.3  ;w reset
    ;17 ;wo reset
    )
  )

(def usb-holder-clearance 0.2)
(def usb-holder-bottom-offset 0.15)

(def usb-holder-z-rotate 2)
(def usb-holder-offset-coordinates
  (if use_hotswap_holder
    [-44 34 usb-holder-bottom-offset]
    [-22.5 50.9 usb-holder-bottom-offset]))
(defn usb-holder-place [shape]
  (->> shape
       (translate usb-holder-offset-coordinates)
       (rotate-shape-on-z usb-holder-z-rotate)
       ))

(def usb-holder (usb-holder-place usb-holder-stl))
(def usb-holder-space
  (let [cutout (translate [0 0 (/ usb-holder-bottom-offset 2)]
                          (extrude-linear {:height usb-holder-cutout-height :twist 0 :convexity 0}
                                          (offset usb-holder-clearance
                                                  (project
                                                    (scale [1.001 1 1] usb-holder-cutout-stl)
                                                    )
                                                  )
                                          )
                          )
        ]
    (usb-holder-place cutout))
  )

;;;;;;;;;;;;;;;;;;
; Case Text      ;
;;;;;;;;;;;;;;;;;;
(defn text-insert [mirror-for-left]
  (translate [(if mirror-for-left -37 6 ) (if mirror-for-left 8 5) 0] (rotate (deg2rad 180) [0 0 (if mirror-for-left 0 1)] (translate [0 0 -2] (extrude-linear {:height 4, :center true}
                                                                                                                                                               (mirror [0 (if mirror-for-left 0 1) 0]
                                                                                                                                                                       (scale [1 1 2] (union
                                                                                                                                                                                        (translate [0 0 0] (text case-text-0 :size 6))
                                                                                                                                                                                        (translate [0 -12 0] (text case-text-1 "Liberation Sans" 22))
                                                                                                                                                                                        (translate [0 -24 0] (text case-text-2 "Liberation Sans" 22))
                                                                                                                                                                                        ;(translate [6.5 -1.4 0](text '"Liberation Sans" 2 '"3"))
                                                                                                                                                                                        ;(translate [7.5 0 0](text '"Liberation Sans" 2 '""))
                                                                                                                                                                                        )))))))
  )

(defn text-insert-vert-raw [text-size]
  (translate [0 0 2] (extrude-linear {:height 4, :center true}
                                     (union
                                       (text case-text-1 :font "Liberation Sans" :size text-size)
                                       (translate [0 -7 0] (text case-text-2 :font "Liberation Sans" :size text-size)))))
  )

(defn text-insert-vert-corner [mirror-text offset-property-of offset-name ] ;TODO parameter for text sizes
  (let [text-property-of-2-lines (->>
                                   (union
                                     (text "Property" :font "Liberation Sans" :size (if mirror-text 4 5))
                                     (translate [15 -7 0] (text "of" :font "Liberation Sans" :size (if mirror-text 4 5))))
                                   (extrude-linear {:height 4, :center true})
                                   )
        left-wall-x-offset-mirrored (if mirror-text -1 0)
        left-wall-y-offset-mirrored (if mirror-text 25 0)
        back-wall-x-offset-mirrored (if mirror-text 32.5 0)
        back-wall-y-offset-mirrored (if mirror-text -3 0)
        back-wall-z-offset-mirrored (if mirror-text -1 0)
        position-back-wall (key-position 0 0 (map + (wall-locate2 1.5 0 false) [(+ (/ mount-width -2) 0 back-wall-x-offset-mirrored) (+ (/ mount-height 2) 1.5 back-wall-y-offset-mirrored) 0]))
        position-left-wall (key-position 0 1 (map + (wall-locate2 1.5 0 false) [(+ (/ mount-width -2) 2 left-wall-x-offset-mirrored) (+ (/ mount-height 2) -5 left-wall-y-offset-mirrored) 0]))
        offsets-first (if mirror-text offset-property-of offset-name)
        offsets-second (if mirror-text offset-name offset-property-of)
        text-shape-back-wall (->>
                               (if mirror-text text-property-of-2-lines (text-insert-vert-raw 5))
                               (mirror [(if mirror-text -1 0) 0 0])
                               (rotate (deg2rad 90) [1 0 0])
                               (translate (map + offsets-first [(first position-back-wall) (second position-back-wall) (+ 24 back-wall-z-offset-mirrored)]))
                               )
        text-shape-left-wall (->>
                               (if mirror-text (text-insert-vert-raw 6) text-property-of-2-lines)
                               (mirror [(if mirror-text -1 0) 0 0])
                               (rotate (deg2rad 90) [1 0 0])
                               (rotate (deg2rad 87) [0 0 1])
                               (translate (map + offsets-second [(first position-left-wall) (second position-left-wall) 24]))
                               ;(rotate (deg2rad 15) [1 0 0])
                               )
        text-shape (union
                     text-shape-back-wall
                     text-shape-left-wall
                     )
        ]
    text-shape
    )
  )

;;;;;;;;;;;;;;;;;;
;; Bottom Plate ;;
;;;;;;;;;;;;;;;;;;

(def bottom-plate-thickness 2)
(def screw-insert-fillets-z 2)

(def screw-insert-bottom-plate-bottom-radius (+ screw-insert-radius 0.9))
(def screw-insert-bottom-plate-top-radius (- screw-insert-radius 0.3))
(def screw-insert-holes-bottom-plate (screw-insert-all-shapes
                                       screw-insert-bottom-plate-top-radius
                                       screw-insert-bottom-plate-top-radius
                                       99
                                       ))

(def screw-insert-fillets-bottom-plate (screw-insert-all-shapes
                                         screw-insert-bottom-plate-bottom-radius
                                         screw-insert-bottom-plate-top-radius
                                         screw-insert-fillets-z
                                         ))


(defn screw-insert-wrist-rest [bottom-radius top-radius height]
  (for [x (range 0 9)
        y (range 0 9)]
    (translate [(* x 5) (* y 5) 0]
               (screw-insert-shape
                 bottom-radius
                 top-radius
                 height)
               )
    )
  )

(defn screw-insert-wrist-rest-four [bottom-radius top-radius height]
  (for [x (range 0 2)
        y (range 0 2)]
    (translate [(* x 20) (* y 20) 0]
               (screw-insert-shape
                 bottom-radius
                 top-radius
                 height)
               )
    )
  )

(def wrist-shape-connector-width 67)
(def wrist-shape-connector-half-width (/ wrist-shape-connector-width 2))
(def wrist-shape-connector (polygon [[wrist-shape-connector-half-width 10]
                                     [30 -20]
                                     [-30 -20]
                                     [(- wrist-shape-connector-half-width) 10]]))
(def wrist-shape
  (union
    (translate [0 -45 0] (cube 60 55 bottom-plate-thickness))
    (translate [0 0 (- (/ bottom-plate-thickness -2) 0.05)]
               (hull (->> wrist-shape-connector
                          (extrude-linear {:height 0.1 :twist 0 :convexity 0}))
                     (->> wrist-shape-connector
                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                          (translate [0 0 bottom-plate-thickness]))))
    )
  )

; begin heavily modified crystalhand wrist rest code
(def wrist-rest-x-angle 16)
(def wrist-rest-y-angle-adj -8)                             ; additional tenting angle for wrist rest
(def wrist-rest-z-height-adj 10)                            ; additional z height for wrist rest

;magic numbers to tweak how well the gel wrist rest is held
(def wrist-rest-recess-depth 4)
(def wrist-rest-recess-x-scale 4.25)
(def wrist-rest-recess-y-scale 4.33)

(def wrirst-rest-base-zheight (* 2.01 wrist-rest-recess-depth))
(def wrist-rest-right-base
  (let [
        wrist-rest-cut-bottom (translate [0 0 -150]
                                         (cube 300 300 300))
        zheight-cut (* 1.01 wrirst-rest-base-zheight)
        shape-curve-cut (scale [1.1, 1, 1]
                               (->> (cylinder 7 zheight-cut)
                                    (with-fn 250)
                                    (translate [0 -13.4 0]))
                               (->> (cube 18 10 zheight-cut)
                                    (translate [0 -12.4 0])))
        shape (scale [wrist-rest-recess-x-scale
                      wrist-rest-recess-y-scale
                      1]
                     (union
                       (difference
                         (scale [1.3, 1, 1]
                                (->> (cylinder 10 wrirst-rest-base-zheight)
                                     (with-fn 250)
                                     (translate [0 0 0])
                                     (color BLU)))
                         shape-curve-cut
                         )
                       (->> (cylinder 6.8 wrirst-rest-base-zheight)
                            (with-fn 250)
                            (translate [-6.15 -0.98 0])
                            (color YEL))
                       (->> (cylinder 6.8 wrirst-rest-base-zheight)
                            (with-fn 250)
                            (translate [6.15 -0.98 0])
                            (color ORA))
                       (->> (cylinder 5.9 wrirst-rest-base-zheight)
                            (with-fn 250)
                            (translate [-6.35 -2 0])
                            (color PUR))
                       (scale [1.01, 1, 1]
                              (->> (cylinder 5.9 wrirst-rest-base-zheight)
                                   (with-fn 250)
                                   (translate [6.35 -2. 0])
                                   (color GRE)))
                       )
                     )
        ]
    (difference (->> shape
                     (rotate (deg2rad 180) [0 0 1])
                     )
                wrist-rest-cut-bottom
                )
    )
  )

(defn wrist-rest-angler [shape]
  (let [wrist-rest-y-angle (* tenting-angle 45)
        angled-shape (->> shape
                          (rotate (/ (* pi wrist-rest-x-angle) 180) [1 0 0])
                          (rotate (/ (* pi wrist-rest-y-angle) 180) [0 1 0])
                          (rotate (/ (* pi wrist-rest-y-angle-adj) 180) [0 1 0])
                          (translate [0 0 (+ wrist-rest-z-height-adj
                                             wrirst-rest-base-zheight)])
                          )
        ]
    angled-shape
    )
  )

(def wrist-rest-right
  (let [
        outline (scale [1.08 1.08 1]
                       wrist-rest-right-base
                       )
        recess-cut (translate [0 0 (- (/ wrirst-rest-base-zheight 2)
                                      wrist-rest-recess-depth)]
                              wrist-rest-right-base
                              )
        top (difference
              outline
              recess-cut
              )
        top-angled (wrist-rest-angler top)
        base (translate [0 0 150]
                        (extrude-linear {:height 300}
                                        (project
                                          (scale [0.999 0.999 1]
                                                 top-angled)
                                          )
                                        )
                        )
        base-cut (hull top-angled
                       (translate [0 0 300] base)
                       )
        base-trimmed (difference base
                                 base-cut
                                 )
        ]
    (union top-angled
           base-trimmed
           ; (debug thingy)
           )
    )
  )
; end heavily modified crystalhand wrist rest code

(def case-walls-bottom (cut
                         (translate [0 0 10]
                                    case-walls
                                    )
                         ))
(def case-walls-bottom-projection (project
                                    (union
                                      (extrude-linear {:height 0.01
                                                       :scale  0.995
                                                       :center true}
                                                      case-walls-bottom
                                                      )
                                      (extrude-linear {:height 0.01
                                                       :scale  1.05
                                                       :center true}
                                                      case-walls-bottom
                                                      )
                                      )
                                    ))

;;;;;;;;;;;;
;; Models ;;
;;;;;;;;;;;;

(defn model-switch-plate-cutouts [mirror-internals]
  (difference
    (union
      (key-places single-plate-blank)
      (debug case-top-border)
      (if use_flex_pcb_holder flex-pcb-holders)
      connectors
      (thumb-layout single-plate-blank)
      thumb-connectors
      )
    )
  )

(def model-wrist-rest-right-holes
  (if adjustable-wrist-rest-holder-plate
    (difference wrist-rest-right
                (translate [-10 -5 0]
                           (screw-insert-wrist-rest-four screw-insert-radius
                                                         screw-insert-radius
                                                         999))
                (translate [-11 39 (- (/ bottom-plate-thickness 2) 0.1)] wrist-shape)
                (translate [11 39 (- (/ bottom-plate-thickness 2) 0.1)] wrist-shape)
                )
    wrist-rest-right
    )
  )

(defn model-bottom-plate [mirror-for-left]
  (let [screw-cutouts (translate [0 0 (/ bottom-plate-thickness -1.99)]
                                 screw-insert-holes-bottom-plate)
        screw-cutouts-fillets (translate [0 0 (/ bottom-plate-thickness -1.99)]
                                         screw-insert-fillets-bottom-plate)
        wrist-rest-adjust-fillets (translate [-12 -120 0]
                                             (screw-insert-wrist-rest screw-insert-bottom-plate-bottom-radius
                                                                      screw-insert-bottom-plate-top-radius
                                                                      screw-insert-fillets-z))
        wrist-rest-adjust-holes (translate [-12 -120 0]
                                           (screw-insert-wrist-rest screw-insert-bottom-plate-top-radius
                                                                    screw-insert-bottom-plate-top-radius
                                                                    (+ bottom-plate-thickness 0.1)))
        bottom-plate-blank (extrude-linear {:height bottom-plate-thickness}
                                           (union
                                             (difference
                                               (project
                                                 (extrude-linear {:height 0.01
                                                                  :scale  0 ;scale 0 creates a filled plate from the case walls
                                                                  :center true}
                                                                 case-walls-bottom
                                                                 )
                                                 )
                                               (when recess-bottom-plate
                                                 case-walls-bottom-projection
                                                 )
                                               )
                                             (when adjustable-wrist-rest-holder-plate
                                               (project (translate [8 -55 0] wrist-shape)))
                                             (when recess-bottom-plate
                                               (project
                                                 (hull (usb-holder-place usb-holder-cutout-stl)))
                                               )
                                             )
                                           )
        bottom-plate-shape (difference (union
                                         bottom-plate-blank
                                         ; (translate [8 -100 0]
                                         ;     (debug model-wrist-rest-right-holes)
                                         ; )
                                         )
                                       (when bottom_plate_text
                                         (text-insert mirror-for-left)
                                         )

                                       (when use_screw_inserts
                                         (union
                                           screw-cutouts
                                           screw-cutouts-fillets
                                           ))
                                       ;(translate [0 0 (/ bottom-plate-thickness 2.01)]
                                       ;           top-screw-insert-holes)
                                       (model-switch-plate-cutouts mirror-for-left)
                                       (if adjustable-wrist-rest-holder-plate
                                         (union
                                           (translate [0 0 (* -1.01 (/ screw-insert-fillets-z 4))]
                                                      wrist-rest-adjust-fillets)
                                           wrist-rest-adjust-holes
                                           )
                                         )
                                       )
        ]

    (if mirror-for-left (mirror [1 0 0] bottom-plate-shape) bottom-plate-shape)
    )
  )

(defn model-switch-plates-right [mirror-internals]
  (union
    (difference
      (union
        (key-places (single-plate mirror-internals))
        case-top-border
        (when use_flex_pcb_holder flex-pcb-holders)
        connectors
        (thumb-layout (single-plate mirror-internals))
        thumb-connectors
        )
      top-screw-insert-holes
      caps-cutout
      thumbcaps-cutout
      (thumb-key-cutouts mirror-internals)
      (when (not (or use_hotswap_holder use_solderless))
        (union key-space-below
               thumb-space-below))
      (when use_hotswap_holder (thumb-layout (hotswap-case-cutout mirror-internals)))
      (when use_hotswap_holder (key-places (hotswap-case-cutout mirror-internals)))
      )
    ; (debug top-screw)
    ))

(defn model-switch-plate-cutouts [mirror-internals]
  (difference
    (union
      (key-places single-plate-blank)
      (debug case-top-border)
      (when use_flex_pcb_holder flex-pcb-holders)
      connectors
      (thumb-layout single-plate-blank)
      thumb-connectors
      )
    )
  )

(defn model-case-walls-right [mirror-internals]
  ; (union
  (difference
    (union
      (when use_flex_pcb_holder flex-pcb-holders)
      (difference (union case-walls
                         screw-insert-outers
                         top-screw-block-outers
                         )
                  (model-switch-plate-cutouts mirror-internals)
                  usb-holder-space
                  screw-insert-holes
                  top-screw-insert-holes
                  )
      )

    (if recess-bottom-plate
      (union
        (translate [0 0 (- (+ 20 bottom-plate-thickness))]
                   (cube 350 350 40))
        (translate [0 0 (- (/ bottom-plate-thickness 2))]
                   (scale [1.005 1.005 1.15] (model-bottom-plate mirror-internals)))
        )
      (translate [0 0 -20] (cube 350 350 40))
      )

    caps-cutout
    thumbcaps-cutout
    (thumb-key-cutouts mirror-internals)
    (when (not (or use_hotswap_holder use_solderless))
      (union key-space-below
             thumb-space-below))
    (when use_hotswap_holder (thumb-layout (hotswap-case-cutout mirror-internals)))
    )
  ; (debug top-screw))
  )

(defn model-right [mirror-internals mirror-text]
  (difference
    (union
      (key-places (single-plate mirror-internals))
      (when use_flex_pcb_holder flex-pcb-holders)
      connectors
      (thumb-layout (single-plate mirror-internals))
      thumb-connectors
      (difference (union case-walls
                         (when use_screw_inserts
                           screw-insert-outers)
                         )
                  (when case_text
                    (text-insert-vert-corner mirror-text [0 0 0] [0 0 0]))
                  (when use_usb_holder
                    usb-holder-space)
                  (when use_screw_inserts
                    screw-insert-holes)
                  )
      )

    (if recess-bottom-plate
      (union
        (translate [0 0 (- (+ 20 bottom-plate-thickness))]
                   (cube 350 350 40))
        (translate [0 0 (- (/ bottom-plate-thickness 2))]
                   (scale [1.005 1.005 1.15] (model-bottom-plate mirror-internals)))
        )
      (translate [0 0 -20] (cube 350 350 40))
      )

    caps-cutout
    thumbcaps-cutout
    (thumb-key-cutouts mirror-internals)
    (when (not (or use_hotswap_holder use_solderless))
      (union key-space-below
             thumb-space-below))

    (when use_hotswap_holder (thumb-layout (hotswap-case-cutout mirror-internals)))
    (when use_hotswap_holder (key-places (hotswap-case-cutout mirror-internals)))
    (union
      (thumb-l-place (hotswap-case-cutout mirror-internals))
      (thumb-m-place (hotswap-case-cutout mirror-internals))
      (thumb-r-place (hotswap-case-cutout mirror-internals))
      )
    (when use_hotswap_holder (key-places (hotswap-case-cutout mirror-internals)))
    )
  )

(def model-left
  (mirror [-1 0 0] (model-right true true)))

(defn test-model-right [mirror-internals]
  (union
    (difference
      (union
        (key-places (single-plate mirror-internals))
        (if use_flex_pcb_holder flex-pcb-holders)
        (debug connectors)
        (thumb-layout (single-plate mirror-internals))
        thumb-connectors
        (difference (union case-walls
                           screw-insert-outers
                           ; top-screw-block-outers
                           )
                    usb-holder-space
                    screw-insert-holes
                    ; top-screw-insert-holes
                    )
        ; (debug top-screw)
        )

      (if recess-bottom-plate
        (union
          (translate [0 0 (- (+ 20 bottom-plate-thickness))]
                     (cube 350 350 40))
          (translate [0 0 (- (/ bottom-plate-thickness 2))]
                     (scale [1.005 1.005 1.15] (model-bottom-plate false)))
          )
        (translate [0 0 -20] (cube 350 350 40))
        )

      ;caps-cutout
      thumbcaps-cutout
      ;(thumb-key-cutouts mirror-internals) ; solves horizontal cutout
      (union
        (thumb-m-place (single-plate-cut mirror-internals))
        (thumb-l-place (single-plate-cut mirror-internals))
        (thumb-r-place (difference
                         (union
                           (translate [0 0 (/ plate-thickness 2)]
                                      (cube mount-width
                                            mount-height
                                            (+ plate-thickness 0.001)
                                            )
                                      )
                           ;(if use_hotswap (translate [0 0 (- (/ hotswap-z 2))]
                           ;                           (cube mount-width
                           ;                                 mount-height
                           ;                                 hotswap-z)))
                           (if use_solderless (hull solderless-plate))
                           )
                         (single-plate mirror-internals)
                         )
                       )
        )
      (if (not (or use_hotswap_holder use_solderless))
        (union key-space-below
               thumb-space-below))
      ;(if use_hotswap (thumb-layout (hotswap-case-cutout mirror-internals))) ; solves side cutout
      (union
        (thumb-m-place (hotswap-case-cutout mirror-internals))
        (thumb-l-place (hotswap-case-cutout mirror-internals))
        ;(thumb-r-place (hotswap-case-cutout mirror-internals))
        )
      (if use_hotswap_holder (key-places (hotswap-case-cutout mirror-internals)))
      )
    ;(debug (translate [ 0 0 10] connectors))
    ;(debug connectors)
    (debug (key-places (hotswap-case-cutout mirror-internals)))
    (debug (thumb-l-place (hotswap-case-cutout mirror-internals)))
    )
  )

;;;;;;;;;;;;;
;; Outputs ;;
;;;;;;;;;;;;;

(spit "things/single-plate.scad"
      (write-scad (single-plate false)))

;(spit "things/switch-plates-right.scad"
;      (write-scad (model-switch-plates-right false)))
;(spit "things/switch-plates-left.scad"
;      (write-scad (mirror [-1 0 0] (model-switch-plates-right true))))
; (spit "things/switch-plate-cutouts.scad"
;       (write-scad (model-switch-plate-cutouts false)))

;(spit "things/case-walls-right.scad"
;      (write-scad (model-case-walls-right false)))
;(spit "things/case-walls-left.scad"
;      (write-scad (mirror [-1 0 0] (model-case-walls-right true))))

(spit "things/right.scad"
      (write-scad (model-right false false)))

(spit "things/left.scad"
      (write-scad model-left))

(spit "things/bottom-plate-right.scad"
      (write-scad (model-bottom-plate false)))

(spit "things/bottom-plate-left.scad"
      (write-scad (model-bottom-plate true)))

; (spit "things/wrist-rest-right-base.scad"
;       (write-scad wrist-rest-right-base))
;(spit "things/wrist-rest-right.scad"
;      (if adjustable-wrist-rest-holder-plate
;        (write-scad model-wrist-rest-right-holes)
;        (write-scad wrist-rest-right)
;        )
;      )

;(spit "things/test-text-left.scad"
;      (write-scad (mirror [-1 0 0] (union
;                    (difference
;                      (union
;                        (debug (left-wall false))
;                        (debug (back-wall false))
;                        screw-insert-outers
;                        )
;                      (text-insert-vert-corner true [0 0 0] [0 0 0])
;                      usb-holder-space
;                      screw-insert-holes
;                      )
;                    ;(text-insert-vert-corner true [0 0 0] [0 0 0])
;                    ))
;                  )
;                  )
;
;(spit "things/test-text-right.scad"
;      (write-scad (union
;                    (difference
;                      (union
;                        (debug (left-wall false))
;                        (debug (back-wall false))
;                        screw-insert-outers
;                        )
;                      (text-insert-vert-corner false [0 0 0] [0 0 0])
;                      usb-holder-space
;                      screw-insert-holes
;                      )
;                    ;(text-insert-vert-corner false [0 0 0] [0 0 0])
;                    )
;                  )
;      )

;(def test-front-wall (front-wall false))
;(spit "things/test-front-wall.scad"
;      (write-scad test-front-wall))
;
;(spit "things/thumb-connectors.scad"
;      (write-scad (union
;                    ;(key-places (single-plate false))
;                    thumb-connectors
;                    (thumb-layout (single-plate false)))))
;
;(spit "things/hotswap-case-cutout.scad"
;      (write-scad (union (hotswap-case-cutout false)
;                         (single-plate false))
;                  )
;      )
;
;(spit "things/hotswap-case-cutout-less-left-false.scad"
;      (write-scad (union
;                    (hotswap-case-cutout-less-left false)
;                    (single-plate false)
;                    )))
;
;(spit "things/hotswap-case-cutout-less-left-true.scad"
;      (write-scad (union
;                    (hotswap-case-cutout-less-left true)
;                    (single-plate true)
;                    )))

;(spit "things/thumb-key-cutouts.scad"
;      (write-scad (thumb-key-cutouts false)))

;(spit "things/test-sa-cap-cutout.scad"
;      (write-scad (sa-cap-cutout 1)))
;
;(spit "things/test-sa-cap-cutout-mod-lastrow-col2.scad"
;      (write-scad (sa-cap-cutout-mod-for-lastrow-2 1)))
;
;(spit "things/test-sa-cap-cutout-mod-for-cornerrow-2-inner.scad"
;      (write-scad (sa-cap-cutout-mod-for-cornerrow-2 1 false)))
;
;(spit "things/test-sa-cap-cutout-mod-for-cornerrow-2-outer.scad"
;      (write-scad (sa-cap-cutout-mod-for-cornerrow-2 1 true)))
;
;(spit "things/test-sa-cap-cutout-left-side.scad"
;      (write-scad (sa-cap-cutout-left-side 1)))
;
;(spit "things/test-sa-cap-cutout-mod-generic-left-side.scad"
;      (write-scad
;        (sa-cap-cutout-mod-generic 1
;                                   :additional-x-cutout-left 1
;                                   :key-cap-lowest-bottom-z-height 0)))
;
;(spit "things/test-sa-cap-cutout-mod-generic-lastrow-col2.scad"
;      (write-scad (sa-cap-cutout-mod-generic 1
;                                             :reduced-y-cutout 1.55
;                                             :create-mount-width-bottom-cutout true
;                                             :key-cap-lowest-bottom-z-height 2
;                                             :mount-width-2-cap-cutout-diagonal-edges true)))
;
;(spit "things/test-sa-cap-cutout-mod-generic-cornerrow-2-inner.scad"
;      (write-scad (sa-cap-cutout-mod-generic 1
;                                             :reduced-y-cutout 1.55
;                                             :create-sa-cap-bottom-cutout true
;                                             :key-cap-lowest-bottom-z-height 1
;                                             :bottom-cutout-y-offset (- 1 0.4)
;                                             :mount-width-2-cap-cutout-diagonal-edges true
;                                             :additional-cutout-block true
;                                             :additional-cutout-block-z-height 3
;                                             )))
;
;(spit "things/test-sa-cap-cutout-mod-generic-cornerrow-2-outer.scad"
;      (write-scad (sa-cap-cutout-mod-generic 1
;                                             :reduced-y-cutout 1.55
;                                             :create-sa-cap-bottom-cutout true
;                                             :key-cap-lowest-bottom-z-height 1
;                                             :bottom-cutout-y-offset (- 1 0.4)
;                                             :mount-width-2-cap-cutout-diagonal-edges true
;                                             :additional-x-cutout-left 3
;                                             :additional-cutout-block true
;                                             :additional-cutout-block-z-height 3
;                                             )))
;
;(spit "things/test-sa-cap-cutout-mod-generic-left-and-right-side.scad"
;      (write-scad
;        (sa-cap-cutout-mod-generic 1
;                                   :additional-x-cutout-left 1
;                                   :additional-x-cutout-right 1
;                                   :key-cap-lowest-bottom-z-height 0)))

;(spit "things/test-right.scad"
;      (write-scad (test-model-right false)))
;
;(spit "things/test-left.scad"
;      (write-scad (mirror [1 0 0] (test-model-right true))))

(spit "things/test.scad"
      (write-scad
        ;PRO TIP, commend out everything but caps & thumbcaps to play with geometry of keyboard, it's MUCH faster
        (color PUR (model-case-walls-right false))

        ; (color WHI (model-right false))
        (color WHI (model-switch-plates-right false))

        ; (debug top-screw)
        caps
        ; (debug caps-cutout)
        thumbcaps
        ; (debug (import "../things/test_keycap_placement.stl"))
        ; (debug thumbcaps-cutout)
        ; (debug key-space-below)
        ; (debug thumb-space-below)
        ; (if use_hotswap_holder(debug (thumb-space-hotswap false)))
        ; (debug top-screw-block-outers)

        (color WHI usb-holder)
        (translate [0 0 (- (/ bottom-plate-thickness 2))]
                   (debug (model-bottom-plate false))
                   (translate [8 -100 (- (/ bottom-plate-thickness 2))]
                              (color PUR model-wrist-rest-right-holes)
                              )
                   )
        )
      )