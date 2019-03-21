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


