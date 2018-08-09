open Graphql
open Async_kernel
open Async_unix

let yojson = (module struct
  type t = Yojson.Basic.json

  let pp formatter t =
    Format.pp_print_text formatter (Yojson.Basic.pretty_to_string t)

  let equal = (=)
end : Alcotest.TESTABLE with type t = Yojson.Basic.json)

let test_query schema ctx query expected =
  Thread_safe.block_on_async_exn begin fun () ->
    match Graphql_parser.parse query with
    | Error err -> failwith err
    | Ok doc ->
      Graphql_async.Schema.execute schema ctx doc >>= (function
      | Ok (`Response data) -> Async_kernel.return data
      | Ok (`Stream stream) ->
          Async_kernel.Pipe.to_list stream >>| fun lst ->
            `List (
              List.fold_right (fun x acc -> match x with
                | Ok data -> data :: acc
                | _ -> acc) lst [])
      | Error err -> Async_kernel.return err)
      >>| fun result ->
        Alcotest.check yojson "invalid execution result" expected result
  end

let take n l =
  let rec loop n l = match n, l with
    | 0, _ | _, [] -> []
    | _, x::xs -> x :: loop (n - 1) xs
  in
  loop n l

let schema = Graphql_async.Schema.(schema [
      field "direct_string"
        ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun () () -> "foo")
      ;
      io_field "io_int"
        ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun () () -> Deferred.return (Ok 42))
    ]
  ~subscriptions:[
    subscription_field "int_stream"
      ~typ:(non_null int)
      ~args:Arg.[
        arg "first" ~typ:(non_null int)
      ]
      ~resolve:(fun () first ->
        Async_kernel.Deferred.Result.return (Async_kernel.Pipe.of_list (take first [1; 2; 3])))
  ]
)

let suite = [
  ("execution", `Quick, fun () ->
    test_query schema () "{ direct_string io_int  }" (`Assoc [
      "data", `Assoc [
        "direct_string", `String "foo";
        "io_int", `Int 42
      ]
    ])
  );
  ("subscription", `Quick, fun () ->
    test_query schema () "subscription { int_stream(first: 2) }" (`List [
      `Assoc [
        "data", `Assoc [
          "int_stream", `Int 1
        ]
      ];
      `Assoc [
        "data", `Assoc [
          "int_stream", `Int 2
        ]
      ]
    ])
  )
]

let () = Alcotest.run "graphql-server" ["async", suite]
