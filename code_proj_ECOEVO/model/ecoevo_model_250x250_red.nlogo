globals[
  ; POPULATION PARAMETERS
  ; n-inds                            ; integer, initial number of individuals

  ;LANDSCAPE PARAMETERS
  ; disturbance-frequency            ; integer, interval between disturbances
  ; disturbance-intensity             ; continuous, maximum proportion of the population removed by a disturbance

  ; PLOTTING
  ; plot-update-frequency            ; integer, interval between updates of plots and landscape
  ; plot-animals                     ; boolean, update plots?
  ; plot-landscape                   ; boolean, update visualization of the landscape?

  ; GLOBAL CONSTANTS

  ;maintenance-cost                  ; continuous, energy costs per turn
  ;reproduction-threshold            ; continuous, energy investment required to reproduce

  ;growth-factor-logistic            ; continuous, location of maximum growth-rate in dependence of resource-density with logistic growth
  ;growth-rate-linear                ; continuous, growth rate per turn with linear growth
  ;growth-limit-linear               ; continuous, maximum resource-density with linear growth

  ;harvest-rate-curvature            ; continuous, convex (< 1) / linear (=1) / convace (> 1) relationship between resource-density (x) and harvest-rate (y)
  ;harvest-rate-factor               ; continuous, modifier of harvest-rate (higher = higher harvest-rate at same resource-density)

  ;stochasticity-LH                  ; continuous, standard deviation of normally distributed inheritance of LH
  ;stochasticity-BT                  ; continuous, standard deviation of normally distributed inheritance of BT

  BT-range                          ; continuous, upper boundary of the behavioural trait (lower boundary is always 0)
  LH-range                          ; continuous, upper boundary of the life-history trait (lower boundary is always 0)



  ; OUTPUT-RELATED
  ; create-output                    ; boolean, create output for single animals
  ; save-landscape                   ; boolean, save harvest-rate of patches every 25 time steps
  working-directory                  ; directory of the project (automatically set to the main folder of the project)
  output-id                          ; string, id of the simultion
  animals-file-name                  ; string, name of the .csv with single animals as output
  metadata-file-name                 ; string, name of the .csv with metdata (global parameters ..) as output

  LUT-growth-rate                    ; list, look-up table for growth rates to reduce computation time
  LUT-harvest-rate


  ; Lists of output variables
  list-ticks
  list-who
  list-BT
  list-LH
  list-age
  list-frepro
  list-n_offspring
  list-soma
  list-ninds
  list-generation-time
  list-times-moved
  list-r-buffer


]

extensions [ shell pathdir]

breed [animals animal]

patches-own
[
  resource-density                   ; continuous, amount of resources
  harvest-rate                       ; continuous, amount of resources which can be foraged per time step
]

animals-own
[
  gathered-resources                 ; continuous, amount of gathered resources (this value is used for plotting and is resetted at the plotting interval)
  perceived-current-mean-hr          ; continuous, average harvest-rate of neighbouring patches (queen's neighborhood)
  current-hr                         ; continuous, harvest-rate at the current patch

  BT                                 ; continuous, behavioral trait which determines the likelihood to stay at a patch despite better harvest-rates at neighboring patches
  LH                                 ; continuous, life-history-trait which determines the interval between reproductive events and the amount of resources allocated to reproduction per time step

  age                                ; integer, number of time steps alive
  soma                               ; continuous, amount of resources in the buffer (if < 0, the animal dies)

  times-moved                        ; integer, the times an animal moved to a neighboring patch (this value is used for plotting and is resetted at the plotting interval)
  generation                         ; integer, the xth generation after the initial individuals (initial individuals have the generation 0)
  age-first-reproduction             ; integer, number of time-steps until an individual reproduced
  n-offspring                        ; integer, number of reproductive events
  list-age-at-reproduction           ; list, integer values of age at reproduction
  r-buffer                           ; continuous, required investment to reproduction to reproduce
]


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         ;
;          SETUP          ;
;                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  ca
  reset-ticks

  ;;; global constants
  set LH-range 1
  set BT-range 2


  ;;; output generation (2 separate files includings metadata (parameter settings) and individual data
  if create-output
  [
    ; if the model is stored in the intended directory and the working directory has not been set, the working directory
    ; will be set to the "proj_POLS" folder
    ; otherwise a simulations folder will be create at the directory of the model
    if member? "code_proj_ECOEVO" shell:pwd [shell:cd "../../"]

    set working-directory shell:pwd

    ; if the simulations folder does not yet exist, it is created
    if not pathdir:isDirectory? word working-directory output-directory [pathdir:create word working-directory output-directory]

    let time-string remove ":" substring date-and-time 0 12
    set output-id (word Working-directory output-directory time-string random-float 1)


    initialize-output-lists

    set metadata-file-name word output-id "_metadata.csv"
    set animals-file-name word output-id "_animals.csv"


    file-open metadata-file-name
    file-print (word "ninds," "disturbancefrequency," "disturbanceseverity," "growthtype," "maintenance-cost," "reproduction-threshold," "growth-factor-logistic," "growth-rate-linear," "growth-limit-linear," "harvest-rate-factor," "harvest-rate-curvature," "stochasticity-BT," "stochasticity-LH")
    file-print (word n-inds "," disturbance-frequency "," disturbance-intensity "," growth-type "," maintenance-cost "," reproduction-threshold "," growth-factor-logistic "," growth-rate-linear "," growth-limit-linear "," harvest-rate-factor "," harvest-rate-curvature "," stochasticity-BT "," stochasticity-LH)
    file-close
  ]


  ;;; generate look-up table of growth rates
  if growth-type = "logistic"  [set LUT-growth-rate map [i -> calculate-growth-rate i ] n-values 2000 [i -> i / 100] ]

  set LUT-harvest-rate map [i -> calculate-harvest-rate i] n-values 2000 [i -> i / 100]


  ;;; initialize resource-density and harvest-rate
  setup-landscape

  ;;; initialize n-inds animals
  setup-animals

end

;''''''''''''''''''''''''';
;         PATCHES         ;
;,,,,,,,,,,,,,,,,,,,,,,,,,;

to setup-landscape
  ask patches
  [
    set resource-density (9 + random-float 2)
    set harvest-rate look-up-harvest-rate resource-density
  ]
end


;''''''''''''''''''''''''';
;         ANIMALS         ;
;,,,,,,,,,,,,,,,,,,,,,,,,,;


; generate initial population of animals
to setup-animals
  create-animals n-inds
  [
    set gathered-resources 0
    set soma 5
    set age 0
    set age-first-reproduction -999
    set n-offspring 0
    set list-age-at-reproduction []

    set xcor random-xcor
    set ycor random-ycor

    set BT random-float BT-range
    set LH random-float LH-range

    set times-moved 0
    set generation 0
    set r-buffer 0

    if create-output [save-animal-data]
  ]
end

; generate output-lists that keep track of several state variables
to initialize-output-lists
  set list-ticks ["ticks"]
  set list-who ["who"]
  set list-BT ["BT"]
  set list-LH ["LH"]
  set list-age ["age"]
  set list-frepro ["frepro"]
  set list-n_offspring ["n_offspring"]
  set list-soma ["soma"]
  set list-ninds ["ninds"]
  set list-generation-time ["generation_time"]
  set list-times-moved ["times_moved"]
  set list-r-buffer ["r_buffer"]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         ;
;           GO            ;
;                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  tick

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;     ANIMAL PROCEDURES     ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ask animals [
      forage-or-relocate
      allocate-resources
      check-reproduction
      set age age + 1
    ]

  if disturbance-frequency != 1
  [
    if (random-float 1) < (1 / disturbance-frequency)
    [
      disturb
    ]
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;    PATCH PROCEDURES      ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ask patches
  [
    grow-resources
  ]


    if (count animals = 0 and n-inds > 0) [

    if create-output [save-lists]
    stop
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;         OUTPUT            ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; SAVE CURRENT HARVEST RATE OF PATCHES (OPTIONAL)
  if ticks mod 25 = 0 and save-landscape and create-output[
    color-landscape
    file-open word output-id "_ls.csv"
    ask patches[
      file-print (word ticks "," pxcor "," pycor "," harvest-rate "," count animals)
    ]
    file-close
  ]

  ; EXPORT LISTS EVERY 1,000 STEPS
  if ticks mod 1000 = 0 and create-output[
    save-lists
    initialize-output-lists
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;          PLOTTING         ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if (ticks > 1) and (ticks mod plot-update-frequency = 0) and plot-animals
  [
    plot-LH-dist
    plot-BT-dist
    plot-BT-LH
    plot-count-animals
  ]

  if (ticks > 1) and (ticks mod plot-update-frequency = 0) and plot-landscape
  [
       color-landscape
  ]

end




;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         ;
;       SUBMODELS         ;
;                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;''''''''''''''''''''''''';
;         ANIMALS         ;
;,,,,,,,,,,,,,,,,,,,,,,,,,;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     FORAGE OR RELOCATE   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to forage-or-relocate
    set current-hr [harvest-rate] of patch-here
    let neighs neighbors
    set perceived-current-mean-hr mean [harvest-rate] of neighs

    ifelse (perceived-current-mean-hr - current-hr) <= random-gamma ((BT-range - BT) ^ 2) (1 / ((BT-range) - BT) ^ 2) ; small value added to avoid division by zero
    [
      forage
    ]
    [
      relocate
    ]
end


;;; forage-or-relocate SUBPROCEDURES ;;;

;_____________________________________________________________

; IF FORAGE

to forage
    ask patch-here
  [
    set resource-density resource-density - harvest-rate
    set harvest-rate look-up-harvest-rate resource-density
  ]
    set gathered-resources gathered-resources + current-hr
    set soma soma + current-hr
end

;_____________________________________________________________

; IF RELOCATE

to relocate
  let current-hr_ current-hr

  ;move-to one-of other patches
  move-to max-one-of neighbors with [harvest-rate >= current-hr_] [harvest-rate - current-hr_] ; EQUATION I
  set current-hr 0
  set times-moved times-moved + 1
end

;_____________________________________________________________

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    ALLOCATE RESOURCES    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to allocate-resources
  ; incur maintenance cost
  set soma soma - maintenance-cost

  if soma < 0 [
    if create-output [save-animal-data]
    die
  ]

  ; incur reproduction cost
  let reproductive-cost (soma * (LH ^ 3)) ; EQUATION II
  set soma soma - reproductive-cost
  set r-buffer r-buffer + reproductive-cost
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       REPRODUCTION       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to check-reproduction
  if r-buffer > reproduction-threshold
  [
    let parental-BT BT
    let parental-LH LH
    let parental-generation generation

    set n-offspring n-offspring + 1
    set r-buffer r-buffer - reproduction-threshold

    if age-first-reproduction = -999 [set age-first-reproduction age]
    set list-age-at-reproduction lput age list-age-at-reproduction

    hatch-animals 1
    [
      ; state variables

      set gathered-resources 0
      set times-moved 0

      set age 0
      set soma 5
      set generation parental-generation + 1
      set age-first-reproduction -999
      set n-offspring 0
      set color random 130
      set r-buffer 0
      set list-age-at-reproduction []

      ; set random location as initial patch
      set xcor random-xcor
      set ycor random-ycor

      ; inherit traits with parental trait as mean and 0.05 and 0.03 (= default) as sd respectively
      set BT random-normal parental-BT stochasticity-BT
      set LH random-normal parental-LH stochasticity-LH

      if BT < 0 [set BT 0.0000000001]
      if BT > BT-range [set BT BT-range - 0.000001]

      if LH < 0 [set LH 0]
      if LH > LH-range [set LH LH-range]

      ; save animal
      if create-output [save-animal-data]
    ]
  ]
end


;''''''''''''''''''''''''';
;         PATCHES         ;
;,,,,,,,,,,,,,,,,,,,,,,,,,;

to grow-resources

  if growth-type = "linear" [ifelse resource-density > growth-limit-linear
    [set resource-density growth-limit-linear]
    [set resource-density resource-density + growth-rate-linear] ; EQUATION III
  ]
  if growth-type = "logistic" [set resource-density resource-density + (look-up-growth-rate resource-density)]

  set harvest-rate look-up-harvest-rate resource-density
end



to color-landscape

  ask patches
  [
    let resource-color resource-density
    if resource-color > 10 [set resource-color 10]

    ifelse resource-color < 5
    [
      set pcolor 24.9 + resource-color
    ]
    [
      set pcolor 69.9 - (resource-color - 5)
    ]
  ]

end



;''''''''''''''''''''''''';
;         EVENTS          ;
;,,,,,,,,,,,,,,,,,,,,,,,,,;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        DISTURBANCE       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to disturb
  let n_deaths round (count animals * (random-float disturbance-intensity) / 100 )
  ask n-of n_deaths animals [
    if create-output [save-animal-data]
    die
  ]

end


;''''''''''''''''''''''''';
;        FORMULAS         ;
;,,,,,,,,,,,,,,,,,,,,,,,,,;


; sigmoidal change in resource density with highest growth rates at intermediate resource densities
to-report calculate-growth-rate [rd]
    let a rd - growth-factor-logistic ; EQUATION IV
    let sigma 1 / (1 + 2.72 ^ (- a)) ; EQUATION V
    report sigma * (1 - sigma) ; EQUATION VI
end

; parameterized so that the growth rate is smaller than the harvest rate (patches can almost be depleted)
to-report calculate-harvest-rate [rd]
  let hr (rd ^ harvest-rate-curvature) / harvest-rate-factor ^ harvest-rate-curvature ; EQUATION VII
  if hr > rd [set hr rd]
  report hr
end

; lookup-table for the growth rate to save computation time
to-report look-up-growth-rate [rd]
  report item floor (rd * 100) LUT-growth-rate
end

; lookup-table for the growth rate to save computation time
to-report look-up-harvest-rate [rd]
  report item floor (rd * 100) LUT-harvest-rate
end


;''''''''''''''''''''''''';
;          PLOTS          ;
;,,,,,,,,,,,,,,,,,,,,,,,,,;




; histogram of BT
to plot-BT-dist
  set-current-plot "BT-dist"
  clear-plot


  set-plot-pen-mode 1
  let min-x floor min [BT] of animals
  let max-x precision BT-range 2

  set-plot-x-range min-x - 0.001 max-x
  set-histogram-num-bars 20

  histogram [BT] of animals
end

; histogram of LH
to plot-LH-dist
  set-current-plot "LH-dist"
  clear-plot

  set-plot-pen-mode 1
  let min-x floor min [LH] of animals
  let max-x precision LH-range 2
  set-plot-x-range min-x - 0.001 max-x
  set-histogram-num-bars 20

  histogram [LH] of animals
end

; count-animals ~ ticks
to plot-count-animals
  set-current-plot "count-animals"
  set-plot-pen-mode 0
  plotxy ticks count animals
end

; sum(resource-density) ~ ticks
to plot-total-resources
  set-current-plot "total-resources"
  set-plot-pen-mode 0
  plotxy ticks sum [resource-density] of patches
end

; LH ~ BT
to plot-BT-LH
  set-current-plot "BT-LH"
  clear-plot

  set-plot-pen-mode 2
  let min-y floor min [LH] of animals
  let max-y precision LH-range 2
  let min-x floor min [BT] of animals
  let max-x precision BT-range 2


  set-plot-x-range min-x - 0.001 max-x
  set-plot-y-range min-y - 0.001 max-y

  ask animals
  [
    plotxy BT LH
  ]

end


;'''''''''''''''''''''''''';
;       OUTPUT             ;
;,,,,,,,,,,,,,,,,,,,,,,,,,,;

to save-animal-data
  set list-ticks lput ticks list-ticks
  set list-who lput who list-who
  set list-BT lput BT list-BT
  set list-LH lput LH list-LH
  set list-age lput age list-age
  set list-frepro lput age-first-reproduction list-frepro
  set list-n_offspring lput n-offspring list-n_offspring
  set list-soma lput soma list-soma
  set list-ninds lput count animals list-ninds
  set list-times-moved lput times-moved list-times-moved
  set list-r-buffer lput r-buffer list-r-buffer

  ifelse length list-age-at-reproduction > 0
  [set list-generation-time lput mean list-age-at-reproduction list-generation-time]
  [set list-generation-time lput -999 list-generation-time]
end

to save-lists
    file-open animals-file-name
    file-print (list list-ticks list-who list-BT list-LH list-age list-frepro list-n_offspring list-soma list-ninds list-generation-time list-times-moved list-r-buffer)
    file-close
end




;''''''''''''''''''''''''';
;       tests             ;
;,,,,,,,,,,,,,,,,,,,,,,,,,;

to step
  go
end
@#$#@#$#@
GRAPHICS-WINDOW
365
159
823
618
-1
-1
1.8
1
10
1
1
1
0
1
1
1
0
249
0
249
0
0
1
ticks
30.0

BUTTON
371
71
483
130
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
571
72
678
130
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
761
67
865
124
NIL
step
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1117
134
1273
167
n-inds
n-inds
0
10000
6250.0
1
1
NIL
HORIZONTAL

SLIDER
17
435
294
468
plot-update-frequency
plot-update-frequency
1
5001
5001.0
500
1
NIL
HORIZONTAL

SWITCH
19
479
140
512
plot-animals
plot-animals
1
1
-1000

SWITCH
158
479
294
512
plot-landscape
plot-landscape
1
1
-1000

TEXTBOX
20
407
170
425
PLOTTING
14
0.0
1

TEXTBOX
1118
110
1272
144
POPULATION PARAMETERS
12
0.0
1

TEXTBOX
19
16
253
50
LANDSCAPE PARAMETERS
14
0.0
1

PLOT
896
55
1096
205
LH-dist
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
896
232
1096
382
BT-dist
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
896
418
1098
586
count-animals
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

SLIDER
18
44
298
77
disturbance-frequency
disturbance-frequency
1
10001
301.0
100
1
NIL
HORIZONTAL

SWITCH
20
242
294
275
create-output
create-output
1
1
-1000

SLIDER
19
87
298
120
disturbance-intensity
disturbance-intensity
0
100
75.0
1
1
%
HORIZONTAL

INPUTBOX
18
281
295
341
output-directory
/simulations/2020-10-10/Main_Predictions/
1
0
String

CHOOSER
19
136
157
181
growth-type
growth-type
"logistic" "linear"
0

PLOT
899
617
1099
767
BT-LH
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -7500403 true "" ""

TEXTBOX
21
216
171
234
OUTPUT
14
0.0
1

SWITCH
19
349
291
382
save-landscape
save-landscape
1
1
-1000

SLIDER
1118
204
1273
237
maintenance-cost
maintenance-cost
0.01
0.25
0.15
0.01
1
NIL
HORIZONTAL

SLIDER
1120
246
1273
279
reproduction-threshold
reproduction-threshold
30
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
1121
322
1275
355
growth-factor-logistic
growth-factor-logistic
.5
5
3.0
.1
1
NIL
HORIZONTAL

SLIDER
1121
360
1274
393
growth-rate-linear
growth-rate-linear
0.01
.3
0.1
.01
1
NIL
HORIZONTAL

SLIDER
1121
398
1274
431
growth-limit-linear
growth-limit-linear
5
20
15.0
1
1
NIL
HORIZONTAL

SLIDER
1124
476
1280
509
harvest-rate-factor
harvest-rate-factor
.5
10
5.0
.5
1
NIL
HORIZONTAL

SLIDER
1122
513
1281
546
harvest-rate-curvature
harvest-rate-curvature
.5
3
1.5
.1
1
NIL
HORIZONTAL

SLIDER
1122
586
1274
619
stochasticity-BT
stochasticity-BT
0
0.1
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
1123
625
1273
658
stochasticity-LH
stochasticity-LH
0
0.1
0.03
0.01
1
NIL
HORIZONTAL

TEXTBOX
1121
184
1271
202
ENERGY BUDGET
12
0.0
1

TEXTBOX
1122
303
1272
321
RESOURCE GROWTH
12
0.0
1

TEXTBOX
1125
455
1275
473
RESOURCE UPTAKE
12
0.0
1

TEXTBOX
1125
566
1275
584
TRAIT HERITABILITY
12
0.0
1

TEXTBOX
1119
49
1281
81
ONLY USED FOR SENSITIVITY ANALYSIS
13
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Main_Predictions" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="40000"/>
    <enumeratedValueSet variable="growth-factor-logistic">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maintenance-cost">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-landscape">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-factor">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="save-landscape">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-type">
      <value value="&quot;linear&quot;"/>
      <value value="&quot;logistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-inds">
      <value value="6250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-frequency">
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
      <value value="125"/>
      <value value="150"/>
      <value value="200"/>
      <value value="300"/>
      <value value="400"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-animals">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="create-output">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-update-frequency">
      <value value="9999999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-BT">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-LH">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-intensity">
      <value value="75"/>
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-limit-linear">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-curvature">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-directory">
      <value value="&quot;Y:/Home/milles/proj_POLS/simulations/2020-10-05/Main_Predictions/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate-linear">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Landscape_fluctuation_Example" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <enumeratedValueSet variable="growth-factor-logistic">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maintenance-cost">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-landscape">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-factor">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="save-landscape">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-type">
      <value value="&quot;logistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-inds">
      <value value="6250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-frequency">
      <value value="200"/>
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-animals">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="create-output">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-update-frequency">
      <value value="9999999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-BT">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-LH">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-intensity">
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-limit-linear">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-curvature">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-directory">
      <value value="&quot;Y:/Home/milles/proj_POLS2/simulations/2020-08-20/Landscape_fluctuation_Example/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate-linear">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S1_Saturation" repetitions="3" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100000"/>
    <enumeratedValueSet variable="growth-factor-logistic">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maintenance-cost">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-landscape">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-factor">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="save-landscape">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-type">
      <value value="&quot;linear&quot;"/>
      <value value="&quot;logistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-inds">
      <value value="6250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-frequency">
      <value value="9999999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-animals">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="create-output">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-update-frequency">
      <value value="9999999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-BT">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-LH">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-limit-linear">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-curvature">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-directory">
      <value value="&quot;Y:/Home/milles/proj_POLS2/simulations/2020-08-20/S1_Saturation/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate-linear">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test_growth_factor_logistic" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="40000"/>
    <enumeratedValueSet variable="growth-factor-logistic">
      <value value="1.75"/>
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maintenance-cost">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-landscape">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-factor">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-type">
      <value value="&quot;logistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="save-landscape">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-frequency">
      <value value="100"/>
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-inds">
      <value value="6250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-animals">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-update-frequency">
      <value value="9999999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="create-output">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-BT">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-LH">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-intensity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-limit-linear">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-curvature">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-directory">
      <value value="&quot;Y:/Home/milles/proj_POLS2/simulations/2020-08-20/test/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate-linear">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test_harvest_rate_curvature" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="40000"/>
    <enumeratedValueSet variable="growth-factor-logistic">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maintenance-cost">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-landscape">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-factor">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-type">
      <value value="&quot;logistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="save-landscape">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-frequency">
      <value value="100"/>
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-inds">
      <value value="6250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-animals">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-update-frequency">
      <value value="9999999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="create-output">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-BT">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-LH">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-intensity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-limit-linear">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-curvature">
      <value value="0.8"/>
      <value value="1.5"/>
      <value value="2.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-directory">
      <value value="&quot;Y:/Home/milles/proj_POLS2/simulations/2020-08-20/test/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate-linear">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Additional_test" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="40000"/>
    <enumeratedValueSet variable="growth-factor-logistic">
      <value value="1.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maintenance-cost">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-landscape">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-factor">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="save-landscape">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-type">
      <value value="&quot;logistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-inds">
      <value value="6250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-frequency">
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
      <value value="125"/>
      <value value="150"/>
      <value value="200"/>
      <value value="300"/>
      <value value="400"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-animals">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="create-output">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-update-frequency">
      <value value="9999999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-BT">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-LH">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-intensity">
      <value value="75"/>
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-limit-linear">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-curvature">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-directory">
      <value value="&quot;Y:/Home/milles/proj_POLS2/simulations/2020-08-20/Additional_test/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate-linear">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Additional_test2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="40000"/>
    <enumeratedValueSet variable="growth-factor-logistic">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maintenance-cost">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-landscape">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-factor">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="save-landscape">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-type">
      <value value="&quot;logistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-inds">
      <value value="6250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-frequency">
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
      <value value="125"/>
      <value value="150"/>
      <value value="200"/>
      <value value="300"/>
      <value value="400"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-animals">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="create-output">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-update-frequency">
      <value value="9999999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-BT">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity-LH">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disturbance-intensity">
      <value value="75"/>
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-limit-linear">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-rate-curvature">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-directory">
      <value value="&quot;Y:/Home/milles/proj_POLS2/simulations/2020-08-20/Additional_test2/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-rate-linear">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
