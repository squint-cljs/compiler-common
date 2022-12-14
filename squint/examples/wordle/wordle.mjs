import { atom, reset_BANG_, range, conj, vec, prn, nth, swap_BANG_, vector, mapv, subvec, inc, str, apply, map_indexed, deref, re_matches } from 'clavascript/core.js'
var board_state = atom([]);
var counter = atom(0);
var attempt = atom(0);
var word_of_the_day = atom("hello");
var write_letter = function (cell, letter) {
return cell["textContent"] = letter;
;
};
var make_cell = function () {
let cell1 = document.createElement("div");
cell1["className"] = "cell";
return cell1;
};
var make_board = function (n) {
let board2 = document.createElement("div");
board2["className"] = "board";
let seq__37 = range(n);
let chunk__48 = null;
let count__59 = 0;
let i__610 = 0;
while(true){
if ((i__610 < count__59)) {
let _11 = _nth(chunk__48, i__610);
let cell12 = make_cell();
swap_BANG_(board_state, conj, cell12);
board2.appendChild(cell12);
null;
let G__13 = seq__37;
let G__14 = chunk__48;
let G__15 = count__59;
let G__16 = unchecked_inc(i__610);
seq__37 = G__13;
chunk__48 = G__14;
count__59 = G__15;
i__610 = G__16;
continue;
} else {
let temp__29223__auto__17 = ((seq__37["length"] > 0)) ? (seq__37) : (null);
if (temp__29223__auto__17) {
let seq__318 = temp__29223__auto__17;
if (false) {
let c__29616__auto__19 = chunk_first(seq__318);
let G__20 = chunk_rest(seq__318);
let G__21 = c__29616__auto__19;
let G__22 = count(c__29616__auto__19);
let G__23 = 0;
seq__37 = G__20;
chunk__48 = G__21;
count__59 = G__22;
i__610 = G__23;
continue;
} else {
let _24 = (seq__318[0]);
let cell25 = make_cell();
swap_BANG_(board_state, conj, cell25);
board2.appendChild(cell25);
null;
let G__26 = seq__318.slice(1);
let G__27 = null;
let G__28 = 0;
let G__29 = 0;
seq__37 = G__26;
chunk__48 = G__27;
count__59 = G__28;
i__610 = G__29;
continue;
}}};break;
}
;
return board2;
};
var get_letter = function (cell) {
return cell["textContent"];
};
var color_cell = function (idx, cell) {
let color30 = function (el, color) {
return el["style"]["backgroundColor"] = color;
;
};
let letter31 = get_letter(cell);
if ((letter31 === nth(deref(word_of_the_day), idx))) {
return color30(cell, "green");} else {
if (contains_QMARK_(set(deref(word_of_the_day)), letter31)) {
return color30(cell, "aqua");} else {
if ("else") {
return color30(cell, "#333333");} else {
return null;}}}
};
var check_solution = function (cells) {
let seq__3236 = map_indexed(vector, cells);
let chunk__3337 = null;
let count__3438 = 0;
let i__3539 = 0;
while(true){
if ((i__3539 < count__3438)) {
let vec__4043 = _nth(chunk__3337, i__3539);
let idx44 = nth(vec__4043, 0, null);
let cell45 = nth(vec__4043, 1, null);
color_cell(idx44, cell45);
null;
let G__46 = seq__3236;
let G__47 = chunk__3337;
let G__48 = count__3438;
let G__49 = unchecked_inc(i__3539);
seq__3236 = G__46;
chunk__3337 = G__47;
count__3438 = G__48;
i__3539 = G__49;
continue;
} else {
let temp__29223__auto__50 = ((seq__3236["length"] > 0)) ? (seq__3236) : (null);
if (temp__29223__auto__50) {
let seq__3251 = temp__29223__auto__50;
if (false) {
let c__29616__auto__52 = chunk_first(seq__3251);
let G__53 = chunk_rest(seq__3251);
let G__54 = c__29616__auto__52;
let G__55 = count(c__29616__auto__52);
let G__56 = 0;
seq__3236 = G__53;
chunk__3337 = G__54;
count__3438 = G__55;
i__3539 = G__56;
continue;
} else {
let vec__5760 = (seq__3251[0]);
let idx61 = nth(vec__5760, 0, null);
let cell62 = nth(vec__5760, 1, null);
color_cell(idx61, cell62);
null;
let G__63 = seq__3251.slice(1);
let G__64 = null;
let G__65 = 0;
let G__66 = 0;
seq__3236 = G__63;
chunk__3337 = G__64;
count__3438 = G__65;
i__3539 = G__66;
continue;
}}};break;
}
;
prn(str(mapv(get_letter, cells)));
prn(str(vec(deref(word_of_the_day))));
return (apply(str, mapv(get_letter, cells)) === str(deref(word_of_the_day)));
};
var user_input = function (key) {
let start67 = (5 * deref(attempt));
let end68 = (5 * (deref(attempt) + 1));
if (re_matches(/[a-z]/, key)&&(deref(counter) < end68)) {
write_letter(nth(deref(board_state), deref(counter)), key);
return swap_BANG_(counter, inc);} else {
if ((key === "backspace")&&(deref(counter) > start67)) {
swap_BANG_(counter, dec);
return write_letter(nth(deref(board_state), deref(counter)), "");} else {
if ((key === "enter")&&(deref(counter) === end68)) {
if (check_solution(subvec(deref(board_state), start67, end68))) {
alert("You won")};
return swap_BANG_(attempt, inc);} else {
return null;}}}
};
if ((typeof listener !== 'undefined')) {
null} else {
var listener = atom(null);
};
var unmount = function () {
document.removeEventListener("keydown", deref(listener));
let app69 = document.getElementById("app");
return app69["innerHTML"] = "";
;
};
var mount = function () {
let app70 = document.getElementById("app");
let board71 = make_board(30);
let input_listener72 = function (e) {
return user_input(e["key"].toLowerCase());
};
app70.appendChild(board71);
reset_BANG_(listener, input_listener72);
return document.addEventListener("keydown", input_listener72);
};
mount();
null;

export { make_cell, counter, word_of_the_day, board_state, listener, user_input, check_solution, color_cell, attempt, unmount, mount, make_board, write_letter, get_letter }
