open Ast
open Global

module StringMap = Map.Make(String)

let unitnd = { prov = dprov; data = T_unit; info = dummyinfo }
let cons1nd = { prov = dprov; data = Cons("Left"); info = dummyinfo }
let cons2nd = { prov = dprov; data = Cons("Right"); info = dummyinfo }
let bool = T_sum([cons1nd, [unitnd]; cons2nd, [unitnd]])
let boolnd = { prov = dprov; data = bool; info = dummyinfo }
let boolplusbool = T_sum([cons1nd, [boolnd]; cons2nd, [boolnd]])
let boolplusboolnd = { prov = dprov; data = boolplusbool; info = dummyinfo }
let alicepnd = { prov = dprov; data = "!Alice"; info = dummyinfo }
let bobpnd = { prov = dprov; data = "!Bob"; info = dummyinfo }
let charliepnd   = { prov = dprov; data = "!Charlie"; info = dummyinfo }
let davepnd   = { prov = dprov; data = "!Dave"; info = dummyinfo }
let evelynpnd   = { prov = dprov; data = "!Evelyn"; info = dummyinfo }
let fredpnd   = { prov = dprov; data = "!Fred"; info = dummyinfo }
let alicevnd = { prov = dprov; data = V_princ(alicepnd); info = dummyinfo }
let bobvnd = { prov = dprov; data = V_princ(bobpnd); info = dummyinfo }
let charlievnd = { prov = dprov; data = V_princ(charliepnd); info = dummyinfo }
let davevnd = { prov = dprov; data = V_princ(davepnd); info = dummyinfo }
let evelynvnd = { prov = dprov; data = V_princ(evelynpnd); info = dummyinfo }
let fredvnd = { prov = dprov; data = V_princ(fredpnd); info = dummyinfo }
let abunionvnd = { prov = dprov; data = V_ps_union(alicevnd, bobvnd); info = dummyinfo }
let abcunionvnd = { prov = dprov; data = V_ps_union(abunionvnd, charlievnd); info = dummyinfo }
let abcdunionvnd = { prov = dprov; data = V_ps_union(abcunionvnd, davevnd); info = dummyinfo }
let abcdeunionvnd = { prov = dprov; data = V_ps_union(abcdunionvnd, evelynvnd); info = dummyinfo }
let abcdefunionvnd = { prov = dprov; data = V_ps_union(abcdeunionvnd, fredvnd); info = dummyinfo }

let abclitvnd = { prov = dprov; data = V_ps_lit([alicevnd; bobvnd; charlievnd]); info = dummyinfo }
let abcdlitvnd = { prov = dprov; data = V_ps_lit([alicevnd; bobvnd; charlievnd; davevnd]); info = dummyinfo }
let abcdelitvnd = { prov = dprov; data = V_ps_lit([alicevnd; bobvnd; charlievnd; davevnd; evelynvnd]); info = dummyinfo }
let abcdeflitvnd = { prov = dprov; data = V_ps_lit([alicevnd; bobvnd; charlievnd; davevnd; evelynvnd; fredvnd]); info = dummyinfo }
let ablitvnd = { prov = dprov; data = V_ps_lit([alicevnd; bobvnd]); info = dummyinfo }
let natnd = { prov = dprov; data = T_nat; info = dummyinfo }
let ffldnd = { prov = dprov; data = Field("#f"); info = dummyinfo }
let gfldnd = { prov = dprov; data = Field("#g"); info = dummyinfo }
let afldnd = { prov = dprov; data = Field("#a"); info = dummyinfo }
let bfldnd = { prov = dprov; data = Field("#b"); info = dummyinfo }
let cfldnd = { prov = dprov; data = Field("#c"); info = dummyinfo }
let dfldnd = { prov = dprov; data = Field("#d"); info = dummyinfo }
let fgnatnd = { prov = dprov; data = T_row([ffldnd, natnd; gfldnd, natnd]); info = dummyinfo }
let abcdnatnd = { prov = dprov; data = T_row([afldnd, natnd; bfldnd, natnd; cfldnd, natnd; dfldnd, natnd]); info = dummyinfo }
let fnatgabcdnatnd = { prov = dprov; data = T_row([ffldnd, natnd; gfldnd, abcdnatnd]); info = dummyinfo }

let alice = "!Alice"
let bob = "!Bob"
let charlie = "!Charlie"
let amap = StringMap.add alice 0 (StringMap.empty)
let abmap = StringMap.add bob 1 (amap)
let abcmap = StringMap.add charlie 2 (abmap)
