(** GraphQL schema functor *)

(* IO signature *)
module type IO = sig
  type +'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* IO Stream *)
module type IO_Stream = sig
  type +'a io
  type 'a t

  val map_s : ('a -> 'b io) -> 'a t -> 'b t
end

(* GraphQL schema functor *)
module Make (Io : IO) (Io_stream : IO_Stream with type 'a io = 'a Io.t) :
  Graphql_intf.Schema with type 'a io = 'a Io.t
                      and type 'a io_stream = 'a Io_stream.t
