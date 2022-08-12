(* A basic timer in OCaml. *)

open Span
open Unix

exception UnImplemented

type t = {
  name : string;
  time : int;
  span : int;
  delay : int;
  is_active : bool;
}

let start_time x = 60 * x
let is_active t = t.is_active

let update_time t =
  let sp =
    convert_to_seconds (Unix.localtime (Unix.time ())) - t.span
  in
  let nt = t.time - sp + t.delay in
  { name = t.name; time = nt; span = sp; delay = -5; is_active = false }

let init_timer_1 =
  let sp = init_span_1 in
  {
    name = "White";
    time = start_time 90;
    span = get_value sp;
    delay = 4;
    is_active = true;
  }

let init_timer_2 =
  let sp = init_span_2 in
  {
    name = "Black";
    time = start_time 90;
    span = get_value sp;
    delay = 4;
    is_active = false;
  }

type timer = {
  player_1 : t;
  player_2 : t;
}

let start_timer =
  let x1 = init_timer_1 in
  let x2 = init_timer_2 in
  { player_1 = x1; player_2 = x2 }

let player_one timer = timer.player_1
let player_two timer = timer.player_2

let string_of_t t =
  let s = string_of_int (t.time / 60) in
  "Time Remaining for " ^ t.name ^ ": " ^ s ^ " minutes"

let string_of_timer timer =
  let t1 = timer.player_1 in
  let t2 = timer.player_2 in
  string_of_t t1 ^ "\n" ^ string_of_t t2

let pause_timer t = t.is_active = false
let run_timer t = t.is_active = true
