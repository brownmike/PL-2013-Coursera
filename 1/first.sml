(* This is a comment. This is our first program. *)

val x = 34;
(* dyanmic environment: x --> 34 *)

val y = 17;
(* dynamic environment: x --> 34, y --> 17 *)

val z = (x + y) + (y + 2);
(* dyanmic environment: x --> 34, y --> 17, z --> 70 *)

val q = z + 1;
(* dynamic environment: x --> 34, y --> 17, z --> 70, q --> 71 *)

val abs_of_z = if z < 0 then 0 - z else z;
(* static env: abs_of_z : int *)
(* dyanmic environment: ..., abs_of_z --> 70 *)
