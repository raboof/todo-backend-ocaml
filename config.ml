open Mirage

let httpsrv =
  let server = conduit_direct (direct_stackv4_with_default_ipv4 default_console tap0) in
  (** let mode = `TCP (`Port 8080) in **)
  http_server server

let main =
  (*let libraries = [ "opium" ] in*)
  let libraries = [ "core_kernel"; "opium" ] in
  foreign ~libraries "Todobackend.Main" (console @-> http @-> job)

let () =
  add_to_ocamlfind_libraries ["re.str"];
  add_to_opam_packages ["re"];

  register "todobackend" [main $ default_console $ httpsrv]
