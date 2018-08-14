open Graphql

module Io = Lwt

module Stream = struct
  type +'a io = 'a Lwt.t
  type 'a t = 'a Lwt_stream.t * (unit -> unit)

  let map (stream, destroy) f =
    Lwt_stream.map_s f stream, destroy
end

module Schema = Graphql_schema.Make (Io) (Stream)

(*
let client = ref []

module Server = struct
  module C = Cohttp_lwt_unix
  open Lwt

  let rec consume_stream stream cb =
    Lwt.catch (fun () ->
      Lwt_stream.next stream >>= fun x ->
        let Ok x | Error x = x in
        cb(x);
        consume_stream stream cb)
    (function
      | Lwt_stream.Closed | Lwt_stream.Empty -> Lwt.return_unit
   | _ -> Lwt.return_unit)

  let static_file_response ?(encoding=`None) path =
    match Assets.read path with
    | Some body -> C.Server.respond_string ~status:`OK ~body ()
    | None -> C.Server.respond_string ~status:`Not_found ~body:"" ()

  let json_err = function
    | Ok _ as ok -> ok
    | Error err -> Error (`String err)

  let execute_query ctx schema variables operation_name query =
    let open Lwt_result in
    Lwt.return @@ json_err @@ Graphql_parser.parse query >>= fun doc ->
    Schema.execute schema ctx ~variables ?operation_name doc

  let execute_request ctx schema req conn body =
    Cohttp_lwt.Body.to_string body >>= fun body' ->
    Lwt_io.printf "Body: %s\n" body';
    let json = Yojson.Basic.from_string body' in
    let query = Yojson.Basic.(json |> Util.member "query" |> Util.to_string) in
    let variables = try Yojson.Basic.Util.(json |> member "variables" |> to_assoc) with _ -> [] in
    let operation_name =
      try Some Yojson.Basic.Util.(json |> member "operationName" |> to_string)
      with _ -> None
    in
    Lwt_io.printf "Query: %s\n" query;
    let result = execute_query ctx schema (variables :> (string * Graphql_parser.const_value) list) operation_name query in
    result >>= function
    | Ok (`Response data) ->
        let body = Yojson.Basic.to_string data in
        C.Server.respond_string ~status:`OK ~body ()
    | Ok (`Stream stream) -> C.Server.respond_string ~status:`OK ~body:"You just wait" ()
    | Error err ->
        let body = Yojson.Basic.to_string err in
        C.Server.respond_string ~status:`Internal_server_error ~body ()

  let upgrade_connection ctx schema req (conn, _) body =
    let open Websocket_cohttp_lwt in
    Cohttp_lwt.Body.drain_body body >>= fun () ->
      Websocket_cohttp_lwt.upgrade_connection req conn
          (fun frame ->
            let module Json = Yojson.Basic.Util in
            let push_to_websocket = List.hd !client in
            let json = Yojson.Basic.from_string frame.Frame.content in
            Printf.eprintf "frameyframframe?: %s \"%s\"\n%!"
            (Frame.Opcode.to_string frame.Frame.opcode)
          frame.Frame.content;
            if frame.Frame.opcode = Frame.Opcode.Text then
              match json |> Json.member "type" |> Json.to_string with
              | "connection_init" ->
                  let payload = Yojson.Basic.to_string (`Assoc ["type", `String "connection_ack"]) in
                  Printf.eprintf "payload: %s\n%!" payload;
                  let frame = Frame.create ~opcode:Frame.Opcode.Text ~content:payload () in
                  push_to_websocket (Some frame);
                  Printf.eprintf "finished pushing %!";
              (* | "connection_terminate" -> *)
              | "start" ->
                  let id = Json.(json |> member "id" |> to_string) in
                  let json = Json.member "payload" json in
                  let query = Json.(json |> member "query" |> to_string) in
                  let variables = try Json.(json |> member "variables" |> to_assoc) with _ -> [] in
                  let operation_name =
                    try Some Yojson.Basic.Util.(json |> member "operationName" |> to_string)
                    with _ -> None
                  in
                  let result =
                    execute_query ctx schema (variables :> (string * Graphql_parser.const_value) list) operation_name query
                  in
                  result >>= (function
                    | Ok (`Stream (stream, destroy)) ->
                        let push_to_client = List.hd !client in
                        Lwt.finalize (fun () ->
                            consume_stream stream
                          (fun x ->
                            let payload =
                              `Assoc ["type", `String "data";
                                      "payload", x;
                                      "id", `String id] in
                            let frame = Frame.create ~opcode:Frame.Opcode.Text ~content:(Yojson.Basic.to_string payload) ()
                            in
                            push_to_client (Some frame)))
                        (fun () ->
                          let payload = `Assoc ["type", `String "complete"] in
                          let frame = Frame.create ~opcode:Frame.Opcode.Text ~content:(Yojson.Basic.to_string payload) ()
                          in
                            push_to_client (Some frame);
                            Lwt.return_unit)) |> ignore
                    | _ -> Printf.eprintf "radzuga algo correu mal\n%!"

              (* | "stop" -> *)
              | _ -> Printf.eprintf "something else \n";
            else
              Printf.eprintf "got frame?: %s \"%s\"\n%!"
            (Frame.Opcode.to_string frame.Frame.opcode)
            frame.Frame.content)
          >>= fun (resp, body, push_to_websocket) ->
            Lwt_io.eprintlf "DUDDEEEEEEE..." >>= fun () ->
              client := [push_to_websocket];
            let headers = Cohttp.Header.add resp.headers "sec-websocket-protocol" "graphql-ws" in
            Lwt.return ({resp with headers }, body)

  let mk_callback mk_context schema conn (req : Cohttp.Request.t) body =
    Lwt_io.eprintlf "Req: %s %s" (Sexplib.Sexp.to_string (Cohttp.Code.sexp_of_meth req.meth)) req.resource
    >>= fun () ->
    let req_path = Cohttp.Request.uri req |> Uri.path in
    let path_parts = Str.(split (regexp "/") req_path) in
      match req.meth, path_parts with
      | `GET,  ["graphql"]       -> static_file_response "index.html"
      | `GET,  ["graphql"; path] -> static_file_response path
      | `POST, ["graphql"]       -> execute_request (mk_context req) schema req conn body
      | _, ["subscriptions"] -> upgrade_connection (mk_context req) schema req conn body
      | _ -> C.Server.respond_string ~status:`Not_found ~body:"" ()

  let start ?(port=8080) ~ctx schema =
    let callback = mk_callback ctx schema in
    C.Server.create ~mode:(`TCP (`Port port)) (C.Server.make ~callback ())
end
*)
