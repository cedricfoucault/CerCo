(* Pasted from Pottier's PP compiler *)

let map f = function
  | None ->
      None
  | Some x ->
      Some (f x)

let iter f = function
  | None ->
      ()
  | Some x ->
      f x

let fold f o accu =
  match o with
  | None ->
      accu
  | Some x ->
      f x accu

let print printer () = function
  | None ->
      ""
  | Some x ->
      printer () x

