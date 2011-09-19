
(** This file gives a memory model that can be used by the interpreter
    of various languages throughout the compilation process and
    following the memory model of the CompCert compiler. *)

(** In the module, every size is expressed in bytes. *)


let error_prefix = "Memory"
let error s = Error.global_error error_prefix s


let string_of_quantity = function
  | AST.QInt i -> "int" ^ (string_of_int i)
  | AST.QOffset -> "offset"
  | AST.QPtr -> "ptr"


let size_of_data = function
(*
  | AST.Data_reserve n -> n
*)
  | AST.Data_int8 _ -> 1
  | AST.Data_int16 _ -> 2
  | AST.Data_int32 _ -> 4
  | AST.Data_float32 _ -> 4
  | AST.Data_float64 _ -> 8


let rec all_offsets size = match size with
  | AST.SQ _ -> [[]]
  | AST.SProd sizes ->
    let fi i offsets = (size, i) :: offsets in
    let f i size = List.map (fi i) (all_offsets size) in
    List.flatten (MiscPottier.mapi f sizes)
  | AST.SSum _ -> [[(size, 0)]]
  | AST.SArray (n, size') ->
    let all_offsets = all_offsets size' in
    let f i = List.map (fun offsets -> (size, i) :: offsets) all_offsets in
    let rec aux i =
      if i >= n then []
      else (f i) @ (aux (i+1)) in
    aux 0


(** This is the signature of the parameter module of the functor. *)

module type DATA_SIZE =
sig
  val int_size  : int
  val ptr_size  : int
  val alignment : int option
end


(** This is the signature of the module that provides functions and types to
    manipulate memories. *)

module type S =
sig

  val int_size  : int
  val ptr_size  : int
  val alignment : int option

  val size_of_quantity : AST.quantity -> int

  module Value : Value.S

  (* Memory. A memory contains values and function definitions. Since the memory
     module will be used by the interpreters of the various languages of the
     compilation chain, the type of memory is polymorphic with the type of
     function definitions. *)

  type 'fun_def memory

  (* Memory manipulation *)

  val empty : 'fun_def memory

  (** [alloc mem size] allocates a block of [size] bytes in the memory [mem]. It
      returns the new memory and the address of the beginning of the newly
      allocated area. *)
  val alloc : 'fun_def memory -> int -> 'fun_def memory * Value.address

  (* Memory free *)

  val free : 'fun_def memory -> Value.address -> 'fun_def memory

  (* Memory load and store *)

  (** [load mem size addr] reads [size] bytes from address [addr] in memory
      [mem] and returns the value found. *)
  val load : 'fun_def memory -> int -> Value.address -> Value.t
  val loadq : 'fun_def memory -> AST.quantity -> Value.address -> Value.t

  (** [store mem size addr v] writes the [size] first bytes (little endian
      representation) of value [v] at address [addr] in memory [mem]. *)
  val store  : 'fun_def memory -> int -> Value.address -> Value.t ->
               'fun_def memory
  val storeq : 'fun_def memory -> AST.quantity -> Value.address -> Value.t ->
               'fun_def memory

  (* Globals management *)

  (** [add_var mem x offsets init_datas] stores the datas [init_datas] of
      offsets [offsets] in a new block of memory [mem], and associates the
      global variable [x] with the address of the block. *)
  val add_var :
    'fun_def memory -> AST.ident -> AST.abstract_size -> AST.data list option ->
    'fun_def memory

  (** [add_fun_def mem f def] stores the function definition [def] in a new
      block of memory [mem], and associates the function name [f] with the
      address of the block. *)
  val add_fun_def : 'fun_def memory -> AST.ident -> 'fun_def -> 'fun_def memory

  val mem_global : 'fun_def memory -> AST.ident -> bool

  (** [find_global mem x] returns the address associated with the global symbol
      [x] in memory [mem]. [x] may be a global variable or the name of a
      function. *)
  val find_global : 'fun_def memory -> AST.ident -> Value.address

  (** [find_fun_def mem addr] returns the function definition found at address
      [addr] in memory [mem]. Raises an error if no function definition is
      found. *)
  val find_fun_def : 'fun_def memory -> Value.address -> 'fun_def


  (** [align off size] returns the aligned offsets (starting at [off]) of datas
      of size [size]. *)
  val align : int (* starting offset *) -> AST.abstract_size (* sizes *) ->
              (int list (* resulting offsets *) * int (* full size *))

  val concrete_offsets_size : AST.abstract_size -> int list * int

  val concrete_offsets : AST.abstract_size -> int list

  val concrete_size : AST.abstract_size -> int

  val concrete_offset : AST.abstract_offset -> int

(*
  val size_of_datas : AST.data list -> int

  (** [offsets_of_datas datas] returns the aligned offsets for the datas
      [datas], starting at offset 0. *)
  val offsets_of_datas : AST.data list -> (AST.data * int (* offset *)) list

  val alloc_datas : 'fun_def memory -> AST.data list ->
                    ('fun_def memory * Value.address)
*)

  val to_string : 'fun_def memory -> string
  val print : 'fun_def memory -> unit

end


(** The functor of a memory module. *)

module Make (D : DATA_SIZE) =
struct

  module Value = Value.Make (D)
  module Block = Value.Block
  module Offset = Value.Offset

  let address_of_block_offset b off =
    Value.of_mem_address (Value.make_mem_address b off)

  let int_size = D.int_size
  let ptr_size = D.ptr_size
  let alignment = D.alignment

  let size_of_quantity = function
    | AST.QInt i -> i
    | AST.QOffset -> int_size
    | AST.QPtr -> ptr_size


  module OffsetMap = Map.Make (Offset)
  type offsetMap = Value.chunk OffsetMap.t
  type offset = Offset.t

  (* Empty cells are interpreted as an undefined byte value. *)

  type contents =
      { low   : offset ; (* inclusive *)
	high  : offset ; (* inclusive *)
	cells : offsetMap }

  let update_cells contents cells = { contents with cells = cells }
  let add_cells contents off v =
    update_cells contents (OffsetMap.add off v contents.cells)
  let remove_cells contents off =
    update_cells contents (OffsetMap.remove off contents.cells)

  (* Alignment *)

  let is_multiple n m = m mod n = 0

  (** [align_off off size] returns the offset greater or equal to [off] that is
      aligned for storing a value of size [size]. *)
  let align_off off size = match D.alignment with
    | None -> off
    | Some alignment when (size <= alignment) && (is_multiple size alignment) ->
      let size = Offset.of_int size in
      let rem = Offset.modulou off size in
      if Offset.eq rem Offset.zero then off
      else Offset.add off (Offset.sub size rem)
    | Some alignment ->
      let size = Offset.of_int alignment in
      let rem = Offset.modulou off size in
      if Offset.eq rem Offset.zero then off
      else Offset.add off (Offset.sub size rem)

  let is_aligned off size = Offset.eq off (align_off off size)

  (** [pad off] returns the offset that is obtained by adding some padding from
      [off] and such that the result is aligned. *)
  let pad off = match D.alignment with
    | None -> off
    | Some alignment -> align_off off alignment

  (** [pad_size off size] returns the offset that is obtained by adding [size]
      to the offset [off] and then adding some extra padding such that the
      result is aligned. *)
  let pad_size off size =
    Offset.to_int (pad (Offset.add off (Offset.of_int size)))


  (* Contents in memory. The type of function definitions varies from a language
     to another; thus, it is left generic. *)

  type 'fun_def content =
    | Contents of contents
    | Fun_def of 'fun_def


  (* The mapping from blocks to contents. *)

  module BlockMap = Map.Make (Block)
  type 'fun_def blockMap = 'fun_def content BlockMap.t
  type block = Block.t

  (* The mapping from global identifiers to blocks (negative for function
     definitions and positive for global variables). *)

  module GlobalMap = Map.Make (String)
  type globalMap = Value.address GlobalMap.t

  (* The memory.
     It is a mapping from blocks to contents, a mapping from global identifiers
     (variables and functions) to pointers, a mapping from (negative) blocks to
     function definition, the next free positive block and the next free
     negative block. *)

  type 'fun_def memory =
      { blocks           : 'fun_def blockMap ;
	addr_of_global   : globalMap ;
	next_block       : block ;
	next_fun_block   : block }

  (* Pretty printing *)

  let to_string mem =
    let i = ref 0 in
    let string_of_cell off v s =
      let s' = if !i mod 4 = 0 then (i := 0 ; "\n ") else "" in
      i := !i+1 ;
      let sv =
	if Value.is_undef_byte v then ""
	else Printf.sprintf "[%s]: %s"
	  (Offset.to_string off) (Value.string_of_chunk v) in
      Printf.sprintf "%s%s %s" s s' sv in
    let string_of_cells cells = OffsetMap.fold string_of_cell cells "" in
    let string_of_block b content s =
      (Printf.sprintf "%s\nBlock %s: " s (Block.to_string b)) ^
      (match content with
	| Contents contents ->
	  i := 0 ;
	  Printf.sprintf "(%s -> %s)%s"
	    (Offset.to_string contents.low)
	    (Offset.to_string contents.high)
	    (string_of_cells contents.cells)
	| Fun_def _ -> "function definition") in 
    Printf.sprintf "%s\n" (BlockMap.fold string_of_block mem.blocks "")

    let print mem = Printf.printf "%s%!" (to_string mem)


  (* Memory manipulation *)

  let empty =
    { blocks = BlockMap.empty ;
      addr_of_global = GlobalMap.empty ;
      next_block = Block.of_int 1 ;
      next_fun_block = Block.of_int (-1) }

  (* Memory allocation *)

  (** [alloc2 mem low high] allocates in memory [mem] a new block whose readable
      and writable offsets are the interval [low] (inclusive) [high]
      (inclusive). *)
  let alloc2 mem low high =
    let b = mem.next_block in
    let contents = { low = low ; high = high ; cells = OffsetMap.empty } in
    let blocks = BlockMap.add b (Contents contents) mem.blocks in
    let next_block = Block.succ mem.next_block in
    let mem' = { mem with blocks = blocks ; next_block = next_block } in
    (mem', address_of_block_offset b low)

  (** [alloc mem size] allocates a block of [size] bytes in the memory [mem]. It
      returns the new memory and the address of the beginning of the newly
      allocated area. *)
  let alloc mem size =
    if size = 0 then (mem, Value.null)
    else alloc2 mem Offset.zero (Offset.of_int (size-1))


  (* The 'safe'-prefixed functions below raise an error when the argument is not
     of the expected form. *)

  let safe_to_address msg vs = 
    if Value.is_mem_address vs then Value.to_mem_address vs
    else error msg

  let safe_find not_found find a map =
    try find a map
    with Not_found -> not_found ()

  let safe_find_err msg = safe_find (fun () -> error msg)

  let safe_find_block msg b mem = safe_find_err msg BlockMap.find b mem.blocks

  let safe_find_contents msg b mem = match safe_find_block msg b mem with
    | Contents contents -> contents
    | Fun_def _ -> error msg

  let safe_find_offset msg off contents =
    if (Offset.leu contents.low off) && (Offset.leu off contents.high) then
      safe_find (fun () -> Value.undef_byte) OffsetMap.find off contents.cells
    else error msg

  let memory_find msg mem b off =
    safe_find_offset msg off (safe_find_contents msg b mem)


  (* Memory free *)

  let free mem vs =
    let addr = safe_to_address "free: invalid memory address." vs in
    let (b, _) = Value.decompose_mem_address addr in
    { mem with blocks = BlockMap.remove b mem.blocks }


  (* Memory load *)

  (** [load_bytes msg mem b off size] reads [size] bytes from the block [b] and
      offset [off] in memory [mem] and returns the value found. If an error
      occurs, [msg] will be printed. *)
  let load_bytes msg mem b off size =
    let shift_off n = Offset.add off (Offset.of_int n) in
    let rec aux n =
      if n >= size then []
      else (memory_find msg mem b (shift_off n)) :: (aux (n+1)) in
    Value.merge (aux 0)

  (** [load mem size addr] reads [size] bytes from address [addr] in memory
      [mem] and returns the value found. *)
  let load mem size vs =
    let msg = "load: invalid memory access." in
    let addr = safe_to_address msg vs in
    let (b, off) = Value.decompose_mem_address addr in
    if not (is_aligned off size) then
      error "Alignment constraint violated when loading value."
    else load_bytes msg mem b off size

  let loadq mem q vs = load mem (size_of_quantity q) vs


  (* Memory store *)

  (** [store_chunks msg mem size b off chunks] writes the [size] first chunks of
      list [chunks] at the offset [off] of the block [b] in the memory [mem]. *)
  let store_chunks msg mem size b off chunks =
    let shift_off n = Offset.add off (Offset.of_int n) in
    let f i contents chunk =
      let off' = shift_off i in
      if (Offset.leu contents.low off') &&
	 (Offset.leu off' contents.high) then
	if Value.is_undef_byte chunk then contents
	else add_cells contents off' chunk
      else error msg in
    match safe_find_block msg b mem with
      | Contents contents ->
	let contents = MiscPottier.foldi_until size f contents chunks in
	let blocks = BlockMap.add b (Contents contents) mem.blocks in
	{ mem with blocks = blocks }
      | _ -> error msg

  (** [store mem size addr v] writes the [size] first bytes (little endian
      representation) of value [v] at address [addr] in memory [mem]. *)
  let store mem size vs v =
    let msg = "store: invalid memory access." in
    let addr = safe_to_address msg vs in
    let (b, off) = Value.decompose_mem_address addr in
    if not (is_aligned off size) then
      error "Alignment constraint violated when storing value."
    else store_chunks msg mem size b off (Value.break v)

  let storeq mem q vs v = store mem (size_of_quantity q) vs v


  (* Data manipulation *)

  let value_of_data = function
(*
    | AST.Data_reserve _ -> Value.undef
*)
    | AST.Data_int8 i | AST.Data_int16 i | AST.Data_int32 i -> Value.of_int i
    | AST.Data_float32 f | AST.Data_float64 f -> error "float not supported."

  type concrete_size =
    | I of Offset.t
    | C of concrete_size list

  let rec first_offset = function
    | I off -> off
    | C [] -> raise (Failure "Memory.first_offset")
    | C (csize :: _) -> first_offset csize

  let first_offsets = function
    | I off -> [off]
    | C sizes -> List.map first_offset sizes

  let rec all_offsets = function
    | I off -> [off]
    | C sizes -> List.flatten (List.map all_offsets sizes)

  let rec full_align off = function

    | AST.SQ q ->
      let size = size_of_quantity q in
      let start_off = align_off off size in
      let diff = Offset.to_int (Offset.sub start_off off) in
      let full_size = size + diff in
      (I start_off, full_size)

    | AST.SProd sizes ->
      let f (l, off) size =
	let (csize, added_size) = full_align off size in
	(l @ [csize], Offset.add off (Offset.of_int added_size)) in
      let start_off = pad off in
      let (l, end_off) = List.fold_left f ([], start_off) sizes in
      let end_off = pad end_off in
      let full_size = Offset.to_int (Offset.sub end_off off) in
      (C l, full_size)

    | AST.SSum sizes ->
      let start_off = pad off in
      let sizes =
	List.map (fun size -> snd (full_align start_off size)) sizes in
      let max = Offset.of_int (MiscPottier.max_list sizes) in
      let end_off = pad (Offset.add start_off max) in
      let full_size = Offset.to_int (Offset.sub end_off off) in
      (I start_off, full_size)

    | AST.SArray (n, size) ->
      let sizes = MiscPottier.make size n in
      full_align off (AST.SProd sizes)

  let align off size =
    let (offsets, full_size) = full_align (Offset.of_int off) size in
    (List.map Offset.to_int (first_offsets offsets), full_size)

  let concrete_offsets_size = align 0

  let concrete_offsets size = fst (concrete_offsets_size size)

  let concrete_size size = snd (concrete_offsets_size size)

  let concrete_offset (size, depth) =
    let offsets = concrete_offsets size in
    List.nth offsets depth


  (* Globals manipulation *)

  let store_datas_opt mem addr offsets = function
    | None -> mem
    | Some datas ->
      let f mem (offset, data) =
	let addr = Value.add_address addr offset in
	store mem (size_of_data data) addr (value_of_data data) in
      let offsets = all_offsets offsets in
      if List.length offsets <> List.length datas then
	error "wrong sizes for global initializations (union type?)."
      else
	let offset_datas = List.combine offsets datas in
	List.fold_left f mem offset_datas

  (** [add_var mem x size init_datas] stores the datas [init_datas] of offsets
      [size] in a new block of memory [mem], and associates the global variable
      [x] with the address of the block. *)
  let add_var mem v_id size datas_opt =
    let (offsets, size) = full_align Offset.zero size in
    let (mem, addr) = alloc mem size in
    let mem = store_datas_opt mem addr offsets datas_opt in
    let addr_of_global = GlobalMap.add v_id addr mem.addr_of_global in
    { mem with addr_of_global = addr_of_global }

  (** [add_fun_def mem f def] stores the function definition [def] in a new
      block of memory [mem], and associates the function name [f] with the
      address of the block. *)
  let add_fun_def mem f_id f_def =
    let b = mem.next_fun_block in
    let next_fun_block = Block.pred mem.next_fun_block in
    let vs = address_of_block_offset b Offset.zero in
    let addr_of_global = GlobalMap.add f_id vs mem.addr_of_global in
    let blocks = BlockMap.add b (Fun_def f_def) mem.blocks in
    { mem with blocks = blocks ;
               addr_of_global = addr_of_global ;
               next_fun_block = next_fun_block }

  let mem_global mem id = GlobalMap.mem id mem.addr_of_global

  (** [find_global mem x] returns the address associated with the global symbol
      [x] in memory [mem]. [x] may be a global variable or the name of a
      function. *)
  let find_global mem gid =
    if GlobalMap.mem gid mem.addr_of_global then
      GlobalMap.find gid mem.addr_of_global
    else error ("Unknown global \"" ^ gid ^ "\"")

  (** [find_fun_def mem addr] returns the function definition found at address
      [addr] in memory [mem]. Raises an error if no function definition is
      found. *)
  let find_fun_def mem vs =
    let msg = "Invalid access to a function definition." in
    let (b, _) = Value.decompose_mem_address (safe_to_address msg vs) in
    match safe_find_block msg b mem with
      | Contents _ -> error msg
      | Fun_def def -> def

end
