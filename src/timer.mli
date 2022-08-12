(** Representation of a simple timer. *)

type t
(** The type to represent a chess turn timer *)

val is_active : t -> bool
(** [is_active t] is whether [t] is running/active *)

val init_timer_1 : t
(** [init_timer_1] is the turn timer for the player using white pieces *)

val init_timer_2 : t
(** [init_timer_2] is the turn timer for the player using black pieces *)

val update_time : t -> t
(** [update_time t] updates [t] to add back a delay time with each move *)

type timer
(** The type to represent both teams' timers *)

val start_timer : timer
(** [start_timer] creates one timer that includes both white team's
    timer and black team's timer *)

val player_one : timer -> t
(** [player_one t] is the timer for white team in [t] *)

val player_two : timer -> t
(** [player_two t] is the timer for black team in [t] *)

val string_of_t : t -> string
(** [string_of_t t] returns the time remaining for one player's time *)

val string_of_timer : timer -> string
(** [string_of_timer t] returns the time remaining for both players *)
