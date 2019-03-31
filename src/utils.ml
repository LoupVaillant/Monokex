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

let last l = List.hd (List.rev l)
let init l = List.tl (List.rev l)

(* Pretty printing functions *)
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
