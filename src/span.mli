open Unix
(** Representation of a time span. This module is a *)

type t
(** The type to represent a time span *)

val convert_to_seconds : tm -> file_perm
(** [convert_to_seconds t] converts a time into only seconds *)

val get_value : t -> file_perm
(** [get_value t] is the time from [t] *)

val init_span_1 : t
(** [init_span_1] creates the time span for white team's timer *)

val init_span_2 : t
(** [init_span_2] cretes the time span for black team's timer *)
