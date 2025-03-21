; Main model file
; Code is kept in separate files for the sake of organization

extensions [nw csv time]

__includes [
  "source/Globals.nls"
  "source/Breeds.nls"
  "source/Setup.nls"
  "source/Go.nls"
  "source/Networks.nls"
  "source/HumatCognition.nls"
  "source/HumatReporter.nls"
  "source/GenerateHouseholds.nls"
  "source/HumatCalculations.nls"
  "source/HumatVisualization.nls"
  "source/Signal.nls"
  "source/AlterRepresentation.nls"
  "source/Inquire.nls"
  "source/Validation.nls"
  "source/ModelReporters.nls"
  "source/HumatOutput.nls"
  "source/ModelInput.nls"
  "source/Campaign.nls"
  "source/ExperimentReporters.nls"
]
@#$#@#$#@
GRAPHICS-WINDOW
210
28
1465
750
-1
-1
1.78
1
15
1
1
1
0
0
0
1
-350
350
-200
200
1
1
1
ticks
30.0

BUTTON
4
31
70
64
NIL
Setup\n
NIL
1
T
OBSERVER
NIL
P
NIL
NIL
1

OUTPUT
1485
520
1846
818
13

BUTTON
102
68
201
101
NIL
go\n
T
1
T
OBSERVER
NIL
L
NIL
NIL
1

BUTTON
3
68
100
101
NIL
go\n
NIL
1
T
OBSERVER
NIL
G
NIL
NIL
1

MONITOR
1631
18
1692
63
Opt-in
precision Opt-In-Percentage-Population 2
17
1
11

PLOT
1486
259
1847
409
Current Actions
time
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Asking questions" 1.0 0 -8630108 true "" "plot count humats with [ Ready-To-Inquire? ]"
"Lecturing others" 1.0 0 -13791810 true "" "plot count humats with [ Ready-To-Signal? ]"
"Satisfied" 1.0 0 -7500403 true "" "plot count humats with [inquiring? = 0 and signaling? = 0]"

MONITOR
1604
180
1715
225
Social
precision Social-Dilemma-Percentage-Population 2
17
1
11

TEXTBOX
6
10
156
28
Setup the simulation:
14
0.0
1

TEXTBOX
1483
237
1739
271
Strategies to make up one's mind
14
0.0
1

TEXTBOX
1484
194
1634
212
Dilemma's (%)
14
0.0
1

TEXTBOX
1592
164
1742
182
NIL
11
0.0
1

TEXTBOX
1479
91
1588
125
Opt-in (%)
14
0.0
1

MONITOR
1722
180
1800
225
Non-social
precision Non-Social-Dilemma-Percentage-Population 2
17
1
11

SWITCH
18
374
127
407
verbose
verbose
1
1
-1000

INPUTBOX
10
112
171
172
ui-n-pop
100.0
1
0
Number

BUTTON
73
31
189
64
Reset and go
if converged and Empty-Schedule? [ setup ]\ngo
T
1
T
OBSERVER
NIL
S
NIL
NIL
0

CHOOSER
31
247
181
292
ui-population-mode
ui-population-mode
"Randomized" "Biased" "Filebased"
0

PLOT
1552
61
1890
181
Opt in percentage
time
Percentage
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"percent" 1.0 0 -16777216 true "" "plot Opt-In-Percentage-Population"

BUTTON
62
544
200
577
Select Agent File
set agent-file-path user-file\nifelse is-boolean? agent-file-path and not agent-file-path [\n  Output-Print-Verbose \"File select canceled\"\n  set ui-toggle-procedural true\n]\n[\n  Output-Print-Regular sentence \"Reading from file \" agent-file-path\n  set ui-toggle-procedural false\n]\n
NIL
1
T
OBSERVER
NIL
O
NIL
NIL
1

CHOOSER
26
464
200
509
ui-visualization-mode
ui-visualization-mode
"standard" "dilemma-status" "current-satisfaction" "current-dissonance" "times-signaled" "times-inquired" "a-evaluation" "b-evaluation"
3

BUTTON
0
313
205
346
Update View (Visualization)
ask humats [Update-Humat-Visualization]
NIL
1
T
OBSERVER
NIL
U
NIL
NIL
1

SWITCH
8
613
196
646
ui-toggle-procedural
ui-toggle-procedural
0
1
-1000

SWITCH
8
677
201
710
ui-campaign-enabled
ui-campaign-enabled
0
1
-1000

BUTTON
42
579
207
612
Select Campaign File
set campaign-file-path user-file\nifelse is-boolean? campaign-file-path and not campaign-file-path [\n  Output-Print-Verbose \"File select canceled\"\n  set campaign-file-enabled? false\n]\n[\n  Output-Print-Regular sentence \"Reading from file \" campaign-file-path\n  set ui-campaign-enabled true\n  set campaign-file-enabled? true\n]\n
NIL
1
T
OBSERVER
NIL
-
NIL
NIL
1

MONITOR
1489
421
1754
466
Average Expected Satisfaction
precision mean [ Current-Expected-Satisfaction ] of humats 2
17
1
11

MONITOR
1805
182
1880
227
Disonnant
count humats with [ Current-Dissonance > dissonance-tolerance ] / count humats
2
1
11

CHOOSER
26
189
164
234
ui-network-type
ui-network-type
"preferential" "random" "small-world"
0

@#$#@#$#@
## WHAT IS IT?

An implementation of the HUMAT model to simulate Samen Besparen campaigns as carried out by Grunneger Power.

## HOW IT WORKS

HUMATs evaluate their position with regards to a binary choice on each time step. They select the outcome that they think works best for them, with that outcome being either the one with the best evaluation, or in case of a tie, the one that gives the least dissonant cognition. Evaluation is based on the agents perceived satisfaction of needs of either alternative, where dissonant cognition occurs if some needs are satisfied but others are not. Depending on their evaluations, HUMATs may choose to communicate in the form of an Inquiry or a Signal action. The former may result in an adjustment of the HUMATs opinion based on an alter, whereas the latter attempts to influence an alter's opinion based on ego.

Alongside the HUMATs there may also exist one or more critical nodes. These are not regular HUMATs but special agents that periodically (according to a fixed schedule that may be set using a file) signal a subset of the HUMAT population. The signaling action of the critical node works using the same mechanisms as the regular HUMATs, but critical nodes are not part of a social network and randomly select (a specified number of) HUMATs to signal.

## HOW TO USE IT

The model can be used by itself by using one of the procedural methods to simulate a population (ui-population-mode selector) and pressing the Setup button. Generally speaking; the Setup button looks at the current configuration and prepares the population and resets the world. The left go button runs the simulation for a single timestep, the right go button runs the simulation until it converges.

The population can be configured procedurally, (the ui-toggle-procedural toggle controls this) or through a file. To configure the population based on a file, the `Select Agent File` must be pressed and a valid agent population file should be loaded. The number of agents to be generated procedurally can be controlled using the `ui-n-pop` button.

A critical node can be enabled toggling the `ui-campaign-enabled` toggle. A default critical node is added to the simulation when this setting is configured at Setup. A critical node may also be loaded from a file using the `Select Campaign File` button.

The visualization mode used in the view can be controlled using the `ui-visualization-mode` selector. Selecting a different mode will make it so that the next time the view is updated the agents are visualized accordingly. A view update can be executed manually by pressing the `Update View (Visualization)' button.

Toggling on the `verbose` toggle will make it so more information is output in the output window to the right of the agent view. If this setting is toggled off, the information can still be viewed by opening the `Command Center`.

## THINGS TO NOTICE

To the right of the world view are some reporters for key statistics. The `Opt-in' tracks the percentage of the agent population that is currently deciding on alternative A (representing opting in). The dilemma status percentages in the population are tracked in the next two reporters. Finally, a graph shows the number of agents in the population that are currently executing a particular action (which can also be no action).

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
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Baseline" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10"/>
    <metric>Opt-In-Percentage-Population</metric>
    <enumeratedValueSet variable="ui-population-mode">
      <value value="&quot;Biased&quot;"/>
      <value value="&quot;Randomized&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="verbose">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ui-toggle-procedural">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ui-n-pop">
      <value value="100"/>
      <value value="1000"/>
      <value value="10000"/>
      <value value="100000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ui-campaign-enabled">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ui-visualization-mode">
      <value value="&quot;current-satisfaction&quot;"/>
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
