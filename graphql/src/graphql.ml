module Io = struct
  type +'a t = 'a

  let bind x f = f x
  let return x = x
end

module Io_stream = struct
  type +'a io = 'a Io.t
  type 'a t = 'a

  let map_s f x = f x
end

module Schema = Graphql_schema.Make (Io) (Io_stream)

