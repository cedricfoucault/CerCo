(* Pasted from Pottier's PP compiler *)

open Printf

type punctuation =
    unit -> string

type 'a printer =
    unit -> 'a -> string

(* ------------------------------------------------------------------------- *)

(* Newlines and indentation. *)

let maxindent =
  120

let whitespace =
  String.make maxindent ' '

let indentation =
  ref 0

let nl () =
  "\n" ^ String.sub whitespace 0 !indentation

let indent ofs producer () x =
  let old_indentation = !indentation in
  let new_indentation = old_indentation + ofs in
  if new_indentation <= maxindent then
    indentation := new_indentation;
  let result = sprintf "%t%a" nl producer x in
  indentation := old_indentation;
  result

(* ------------------------------------------------------------------------- *)

(* Lists. *)

let rec list elem () xs =
  match xs with
  | [] ->
      ""
  | x :: xs ->
      sprintf "%a%a" elem x (list elem) xs

let rec preclist delim elem () xs =
  match xs with
  | [] ->
      ""
  | x :: xs ->
      sprintf "%t%a%a" delim elem x (preclist delim elem) xs

let rec termlist delim elem () xs =
  match xs with
  | [] ->
      ""
  | x :: xs ->
      sprintf "%a%t%a" elem x delim (termlist delim elem) xs

let seplist sep elem () xs =
  match xs with
  | [] ->
      ""
  | x :: xs ->
      sprintf "%a%a" elem x (preclist sep elem) xs

let annlist announcement list () xs =
  match xs with
  | [] ->
      ""
  | _ :: _ ->
      sprintf "%t%a" announcement list xs

(* ------------------------------------------------------------------------- *)

(* Punctuation. *)

let space () =
  sprintf " "

let comma () =
  sprintf ", "

let semicolon () =
  sprintf "; "

let var () =
  sprintf "var "

let seminl () =
  sprintf "%t%t" semicolon nl

let nlspace k () =
  sprintf "%t%s" nl (String.make k ' ')

let nlnl () =
  sprintf "%t%t" nl nl

(* ------------------------------------------------------------------------- *)

(* [atmost n delimiter stop] normally prints a [delimiter], except that,
   every [n] calls, it prints a [stop] in addition. *)

let atmost n (delimiter : punctuation) (stop : punctuation) : punctuation =
  let i =
    ref 0
  in
  function () ->
    incr i;
    delimiter() ^
    if !i = n then begin
      i := 0;
      stop()
    end
    else
      ""

(* ------------------------------------------------------------------------- *)

(* Tables. *)

let width column =
  List.fold_left (fun width x ->
    max width (String.length x)
  ) 0 column

let pad width x =
  let y = String.make width ' ' in
  String.blit x 0 y 0 (String.length x);
  y

let pad column =
  List.map (pad (width column)) column

let rec zipcat column1 column2 =
  List.fold_right2 (fun x1 x2 column ->
    (x1 ^ x2) :: column
  ) column1 column2 []

let catenate columns =
  match columns with
  | [] ->
      []
  | column :: columns ->
      List.fold_left (fun table column ->
	zipcat table (pad column)
      ) (pad column) columns

let transposerev lines =
  match lines with
  | [] ->
      []
  | line :: lines ->
      List.fold_left (fun columns line ->
	List.fold_right2 (fun x column columns ->
	  (x :: column) :: columns
        ) line columns []
      ) (List.map (fun x -> [ x ]) line) lines

(* ------------------------------------------------------------------------- *)

(* Conditional. *)

let showif flag printer x =
  if flag then begin
    Printf.fprintf stdout "%s%!" (sprintf "%a" printer x);
    x
  end
  else
    x

