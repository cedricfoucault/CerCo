let options = ref []

let register o = 
  options := o @ !options

let usage_msg = 
  "Usage: " 
  ^ (Filename.basename Sys.executable_name) 
  ^ " [options] file..."

let results () = 
  let extra_arguments = ref [] in
  Arg.parse (Arg.align !options)
    (fun s -> extra_arguments := s :: !extra_arguments) 
    usage_msg;
  !extra_arguments
