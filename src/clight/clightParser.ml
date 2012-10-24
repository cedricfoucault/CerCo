
let process ?is_lustre_file ?remove_lustre_externals file = 
  let temp_dir = Filename.dirname file in
  let tmp_file1 = Filename.temp_file ~temp_dir "cparser1" ".c"
  and tmp_file2 = Filename.temp_file ~temp_dir "cparser2" ".i" 
  and prepro_opts = [] in

  (* Add CerCo's primitives *)
  let _ =
    try
      let oc = open_out tmp_file1 in
      if is_lustre_file = Some true || remove_lustre_externals = Some true then
	output_string oc "#include<string.h>";
      output_string oc Primitive.prototypes ;
      close_out oc
    with Sys_error _ -> failwith "Error adding primitive prototypes." in
  let rc = Sys.command
    (Printf.sprintf "cat %s >> %s"
       (Filename.quote file) (Filename.quote tmp_file1)) in
  if rc <> 0 then (
    (*
    Misc.SysExt.safe_remove tmp_file1;
    *)
    failwith "Error adding primitive prototypes."
  );

  (* Preprocessing *)
  Cparser.Builtins.set Cparser.GCC.builtins;
  let rc = Sys.command
    (Printf.sprintf "gcc -E -U__GNUC__ %s %s > %s"
       (String.concat " " (List.map Filename.quote prepro_opts))
       (Filename.quote tmp_file1) (Filename.quote tmp_file2)) in
  if rc <> 0 then (
    (*
    Misc.SysExt.safe_remove tmp_file1;
    Misc.SysExt.safe_remove tmp_file2;
    *)
    failwith "Error calling gcc."
  );

  (* C to Cil *)
  let r = Cparser.Parse.preprocessed_file "CSf" file tmp_file2 in
  match r with
    | None -> failwith "Error during C parsing."
    | Some p ->
      (* Cil to Clight *)
      (match ClightFromC.convertProgram p with
        | None -> failwith "Error during C to Clight pass."
        | Some(pp) ->
	  Misc.SysExt.safe_remove tmp_file1;
	  Misc.SysExt.safe_remove tmp_file2;
	  if remove_lustre_externals = Some true then ClightLustre.simplify pp
	  else pp)
