open Core_kernel.Std
open Opium.Std

module TodoStorage = Caml.Map.Make(Int)

Random.self_init()

let stored_todos = ref TodoStorage.empty

let add_cors_headers (headers: Cohttp.Header.t): Cohttp.Header.t =
  Cohttp.Header.add_list headers [
    ("access-control-allow-origin", "*");
    ("access-control-allow-headers", "Accept, Content-Type");
    ("access-control-allow-methods", "GET, HEAD, POST, DELETE, OPTIONS, PUT, PATCH")
  ]

let allow_cors =
  let filter handler req =
    handler req >>| fun response -> 
    response 
    |> Response.headers
    |> add_cors_headers
    |> Field.fset Response.Fields.headers response
  in 
    Rock.Middleware.create ~name:(Info.of_string "allow cors") ~filter

type todo = {
  title: string;
  completed: bool;
}

let json_of_todos todos = 
  `A todos

let json_of_todo { title ; completed }: Ezjsonm.value =
  let open Ezjsonm in
  dict [ "title", (string title)
       ; "completed", (bool completed) ]

let print_param = put "/hello/:name" begin fun req ->
  `String ("Hello " ^ param req "name") |> respond'
end

let get_todos = get "/todos" begin fun _ ->
  let todos = TodoStorage.fold (fun _ value acc -> value :: acc) !stored_todos [];
  in
    `Json (todos |> json_of_todos) |> respond'
end

let post_todos = post "/todos" begin fun request ->
  App.json_of_body_exn request 
  >>| fun json -> stored_todos := TodoStorage.add (Random.int 100000) (Ezjsonm.value json) !stored_todos; `Json json
  |> respond
end

let delete_todos = delete "/todos" begin fun _ ->
  stored_todos := TodoStorage.empty;
  respond' (`String "OK")
end

let accept_options = App.options "/:anything" begin fun _ -> 
  respond' (`String "OK")
end

let _ =
  App.empty
  |> middleware allow_cors
  |> accept_options
  |> get_todos
  |> delete_todos
  |> post_todos
  |> App.run_command
