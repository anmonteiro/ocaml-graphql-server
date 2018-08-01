(** GraphQL schema with Lwt support *)
module Schema : sig
  include Graphql_intf.Schema with type 'a io = 'a Lwt.t
                              and type 'a io_stream = 'a Lwt_stream.t
  end

  module Server : sig
    val start : ?port:int -> ctx:(Cohttp.Request.t -> 'ctx) -> 'ctx Schema.schema -> unit Lwt.t
  end
