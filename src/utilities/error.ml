let exit_flag = ref true

let exit_if_error () = exit_flag := true

let resume_if_error () = exit_flag := false

exception Error of Position.t list * string

let print_error positions msg =
  Printf.sprintf "%s%s"
    (String.concat "\n"
       (List.map (fun p -> Position.string_of_pos p ^": ") positions))
    msg

let error_alert positions msg =
  if !exit_flag then (
    output_string stderr (print_error positions msg);
    exit 1
  )
  else raise (Error (positions, msg))

let global_error kind msg =
  error_alert [] (Printf.sprintf "Global Error (%s):\n  %s\n"  kind msg)

let error kind pos msg =
  error_alert [pos] (Printf.sprintf "Error (%s):\n  %s\n" kind msg)

let error2 kind pos1 pos2 msg =
  error_alert [pos1; pos2] (Printf.sprintf "Error (%s):\n  %s\n" kind msg)

let warning kind msg = 
  let mem_flag = !exit_flag in
  exit_flag := false;
  (try 
    error_alert [] (Printf.sprintf "Warning (%s):\n  %s\n"  kind msg)
  with Error (positions, msg) -> 
    output_string stderr (print_error positions msg));
  exit_flag := mem_flag
