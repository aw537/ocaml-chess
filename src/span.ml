(** The span of time *)

open Unix

type t = {
  time : Unix.tm;
  value : int;
}

let convert_to_seconds tm =
  let year = tm.tm_year in
  let day = (365 * year) + tm.tm_yday in
  let hour = (24 * day) + tm.tm_hour in
  let minutes = (60 * hour) + tm.tm_min in
  60 * minutes

let start_time = Unix.localtime (Unix.time ())

let init_span_1 =
  { time = start_time; value = convert_to_seconds start_time }

let init_span_2 =
  {
    time = Unix.localtime (Unix.time ());
    value = convert_to_seconds start_time;
  }

let get_value t = t.value
