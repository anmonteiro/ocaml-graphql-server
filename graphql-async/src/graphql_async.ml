module Schema = Graphql_schema.Make(struct
  include Async_kernel.Deferred

  let bind x f = bind x ~f
end) (struct
  type +'a io = 'a Async_kernel.Deferred.t
  type 'a t = 'a Async_kernel.Pipe.Reader.t

  let map x f = Async_kernel.Pipe.map' x ~f:(fun q ->
    let open Core_kernel in
    let open Async_kernel.Deferred.Infix in
    Queue.fold ~f:(fun newq itm ->
      (f itm) >>= fun itm ->
        newq >>| fun newq ->
        Queue.enqueue newq itm; newq)
    ~init:(Async_kernel.Deferred.return (Queue.create ())) q)
end)
