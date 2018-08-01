(** GraphQL schema functor *)

(* IO signature *)
module type IO = sig
  type +'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* IO Stream *)
module type IO_stream = sig
  type 'a t
end

(* GraphQL schema functor *)
module Make (Io : IO) (Io_stream : IO_stream) :
  Graphql_intf.Schema with type 'a io = 'a Io.t
                      and type 'a io_stream = 'a Io_stream.t
