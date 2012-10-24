(* Pasted from Pottier's PP compiler *)

let (+) =
  Int32.add

let (-) =
  Int32.sub

let ( * ) =
  Int32.mul

let (/) =
  Int32.div

let (mod) =
  Int32.rem

let max_int =
  Int32.max_int

let min_int =
  Int32.min_int

let (land) =
  Int32.logand

let (lor) =
  Int32.logor

let (lxor) =
  Int32.logxor

let (lsl) =
  Int32.shift_left

let (asr) =
  Int32.shift_right

let (lsr) =
  Int32.shift_right_logical

let (~-) =
  Int32.neg

let negative c =
  c < 0

let nonpositive c =
  c <= 0

let (<=) i1 i2 =
  nonpositive (Int32.compare i1 i2)

let (<) i1 i2 =
  negative (Int32.compare i1 i2)

let (>=) i1 i2 =
  i2 <= i1

let (>) i1 i2 =
  i2 < i1

let fits16 i =
  let two15 = 1l lsl 15 in
  i >= -two15 && i < two15

let rec is_power_of_two i =
  if i <= 0l then
    false
  else
    match i with
    | 1l ->
        true
    | _ ->
        i mod 2l = 0l && is_power_of_two (i / 2l)

let rec log2 i =
  assert (i > 0l);
  match i with
  | 1l ->
      0l
  | _ ->
      1l + log2 (i / 2l)

let rec exp2 i =
  if i = 0l then
    1l
  else
    let p = exp2 (i / 2l) in
    let p = p * p in
    if i mod 2l = 0l then
      p
    else
      p * 2l

let max i1 i2 =
  if i1 < i2 then i2 else i1

module Array = struct

  let make i v =
    Array.make (Int32.to_int i) v

  let init i f =
    Array.init (Int32.to_int i) (fun j -> f (Int32.of_int j))

  let get a i =
    Array.get a (Int32.to_int i)

  let set a i v =
    Array.set a (Int32.to_int i) v

  let length a =
    Int32.of_int (Array.length a)

end
