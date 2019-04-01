let pair   a b        = (a, b)
let cons   x l        = x :: l

let swap f x y        = f y x
let const  x y        = x
let id     x          = x
let (|-) f g x        = g (f x)

let (//) l p          = List.filter p l
let (/@) l f          = List.map    f l

let error   e         = raise (Invalid_argument e)
let f_error e         = fun _ -> error e
let check assertion e = if not assertion then error e

(* List handling *)
let last l = List.hd (List.rev l)
let init l = List.tl (List.rev l)
let rec range a b =
  if      a > b then []
  else if a = b then [a]
  else               a :: range (a + 1) b
let rec mapi i f = function
  | x::l -> f i x :: mapi (i+1) f l
  | _    -> []
let rec zip_with f l1 l2 = match l1, l2 with
  | x::l1, y::l2 -> f x y :: zip_with f l1 l2
  | _            -> []
let zip l1 l2 = zip_with pair l1 l2
let rec map2 f l1 l2 = match l1, l2 with
  | x::l1, y::l2-> f x y  :: map2 f l1 l2
  | _           -> []

(* Pretty printing functions *)
let pad_right strings =
  let width = List.fold_left (fun w s -> max w (String.length s)) 0 strings in
  strings /@ (fun s -> s ^ String.make (width - String.length s) ' ')

let grid =
  let rec transpose : ('a list list -> 'a option list list) =
    let is_some     = function Some _ -> true | None    -> false  in
    let option_head = function []     -> None | x :: _  -> Some x in
    let option_tail = function []     -> []   | _ :: xs -> xs     in
    fun l -> let heads = l /@ option_head in
             if List.exists is_some heads
             then heads :: transpose (l /@ option_tail)
             else []
  in
  let align =
    let pad n = function
      | None   -> ""
      | Some s -> s ^ String.make (n - String.length s) ' ' in
    let length = function
      | Some s -> String.length s
      | None   -> 0                                         in
    let maximum  l = List.fold_left max 0 l                 in
    fun l -> let max_length = maximum (l /@ length) in
             l /@ (pad max_length)
  in
  let some = function None -> error "grid" | Some s -> s
  in
  fun l -> (transpose l /@ align |> transpose)
           /@ (List.map some)
           /@ (List.fold_left (^) "")

let prototype return_type function_name args =
  let indent = match args // (<>) [] with
    | []      -> []
    | x :: xs -> ((return_type ^ " " ^ function_name ^ "(") :: x)
                 :: xs /@ cons ""                       in
  String.concat ",\n" (grid indent) ^ ")"
