open Core_kernel.Std
open Opium.Std

Random.self_init()

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
  id: int;
  url: string;
  title: string;
  completed: bool;
}

module TodoStorage = Caml.Map.Make(Int)
let stored_todos = ref TodoStorage.empty

let get_or_else t path default =
  let open Ezjsonm in
    if (mem t path)
    then (find t path)
    else default

let todo_of_json json id =
  let open Ezjsonm in
  {
    id = id;
    url = "http://54.72.243.203:3000/todos/" ^ string_of_int id;
    title = get_string (find json ["title"]);
    completed = get_bool (get_or_else json ["completed"] (bool false))
  }

let updated_todo todo json =
  let open Ezjsonm in
  {
    id = todo.id;
    url = "http://54.72.243.203:3000/todos/" ^ string_of_int todo.id;
    title = get_string (get_or_else json ["title"] (string todo.title));
    completed = get_bool (get_or_else json ["completed"] (bool todo.completed));
  }

let todo_of_json json = todo_of_json (Ezjsonm.value json)

let json_of_todo { title ; url ; completed ; _ }: Ezjsonm.t =
  let open Ezjsonm in
  dict [ "title", (string title)
       ; "url", (string url)
       ; "completed", (bool completed) ]

let json_of_todos (todos: todo list): Ezjsonm.t =
  `A (List.map todos ~f:(fun json -> Ezjsonm.value (json_of_todo json)))

let get_todos = get "/todos" begin fun _ ->
  let todos = TodoStorage.fold (fun _ value acc -> value :: acc) !stored_todos [];
  in
    `Json (json_of_todos todos)
    |> respond'
end

let get_todo = get "/todos/:id" begin fun req ->
  let todo = TodoStorage.find (int_of_string (param req "id")) !stored_todos;
  in
    `Json (json_of_todo todo)
    |> respond'
end

let patch_todo = App.patch "/todos/:id" begin fun request ->
  App.json_of_body_exn request >>| fun json ->
    let todo = TodoStorage.find (int_of_string (param request "id")) !stored_todos in
    let updated = updated_todo todo (Ezjsonm.value json) in
      stored_todos := TodoStorage.add todo.id updated !stored_todos;
      `Json (json_of_todo updated) |> respond
end

let post_todos = post "/todos" begin fun request ->
  App.json_of_body_exn request 
  >>| fun json ->
    let id = (Random.int 100000) in
    let todo = todo_of_json json id in
      stored_todos := TodoStorage.add id todo !stored_todos;
      `Json (json_of_todo todo)
  |> respond
end

let delete_todos = delete "/todos" begin fun _ ->
  stored_todos := TodoStorage.empty;
  respond' (`String "OK")
end

let accept_options = App.options "**" begin fun _ ->
  respond' (`String "OK")
end

let _ =
  App.empty
  |> middleware allow_cors
  |> accept_options
  |> get_todos
  |> get_todo
  |> patch_todo
  |> delete_todos
  |> post_todos
  |> App.run_command
