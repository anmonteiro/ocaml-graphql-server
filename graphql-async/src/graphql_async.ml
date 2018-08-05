module Schema = Graphql_schema.Make(struct
  include Async_kernel.Deferred

  let bind x f = bind x ~f
end) (struct
  type +'a io = 'a Async_kernel.Deferred.t
  include Async_kernel.Stream

  let map x f = map' ~f x
end)
