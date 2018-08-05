module Io = struct
  type +'a t = 'a

  let bind x f = f x
  let return x = x
end

module Stream = struct
  type +'a io = 'a Io.t
  type 'a t = 'a list

  let map x f = List.map f x
end

module Schema = Graphql_schema.Make (Io) (Stream)
