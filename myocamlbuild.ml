open Ocamlbuild_plugin
open Myocamlbuild_config

let run_and_read      = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

let find_packages () =
  blank_sep_strings &
    Lexing.from_string &
      run_and_read "ocamlfind list | cut -d' ' -f1"

let find_syntaxes () = ["camlp4o"; "camlp4r"]

let ocamlfind x = S[A"ocamlfind"; x]


let _ = dispatch begin function
   | Before_options ->
       Options.ocamlc     := ocamlfind & A"ocamlc";
       Options.ocamlopt   := ocamlfind & A"ocamlopt";
       Options.ocamldep   := ocamlfind & A"ocamldep";
       Options.ocamldoc   := A"admin/myocamldoc.exe";
       Options.ocamlmktop := ocamlfind & A"ocamlmktop";

   | After_rules ->
       flag ["ocaml"; "link"] & A"-linkpkg";
       List.iter begin fun pkg ->
         flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
       end (find_packages ());
       List.iter begin fun s ->
         flag ["ocaml"; "compile";  "syntax_"^s] & S[A"-syntax"; A s];
         flag ["ocaml"; "ocamldep"; "syntax_"^s] & S[A"-syntax"; A s];
         flag ["ocaml"; "doc";      "syntax_"^s] & S[A"-syntax"; A s];
       end (find_syntaxes ());
       flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
       flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);

       rule "admin/myocamldoc"
	 ~prod:"admin/myocamldoc.exe"
	 ~deps:["admin/myocamldoc"]
	 begin fun _ _ ->
	   Seq [ cp "admin/myocamldoc" "admin/myocamldoc.exe";
		 chmod (A"+rx") "admin/myocamldoc.exe" ]
	 end;

       dep  ["doc"] ["admin/myocamldoc.exe"]; 
       dep  ["doc"; "docdir"; "extension:html"; "ocaml"] [ "doc/src/main" ]; 
       flag ["doc"; "docdir"; "extension:html"; "ocaml"] 
	 (S[A "-intro"; A "doc/src/main" ]); 
       flag ["doc"] (S[A "-keep-code"]); 
       flag ["doc"] (S[A ("-I "^parser_lib)]);

       ocaml_lib ~extern:true ~dir:parser_lib "cparser";

   | _ -> ()
end
