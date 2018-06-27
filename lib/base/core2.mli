(** Extension of Core. Open this module after opening [Core] to
    extend Core with additional functionality.
*)
open Core_kernel

module Result : sig
  include module type of Result

  module List : sig
    type ('a, 'b) monad = ('a, 'b) t
    type 'a t = 'a list

    val map
      : 'a list
      -> f:('a -> ('b, 'err) monad)
      -> ('b t, 'err) monad

    val mapi
      : 'a t
      -> f:(int -> 'a -> ('b, 'err) monad)
      -> ('b t, 'err) monad

    val fold
      : 'a t
      -> init:'b
      -> f:('b -> 'a -> ('b, 'err) monad)
      -> ('b, 'err) monad

    val foldi
      : 'a t
      -> init:'b
      -> f:(int -> 'b -> 'a -> ('b, 'err) monad)
      -> ('b, 'err) monad
  end

end

module Or_error : sig
  include module type of Or_error
  val tag_loc : Source_code_position.t -> 'a Or_error.t -> 'a Or_error.t
end

val errorh : ?strict:unit -> Source_code_position.t -> string -> 'a -> ('a -> Sexp.t) -> 'b Or_error.t
