let (|-) f g x = g (f x)
let const x y  = x
let swap f x y = f y x
let cons x l   = x :: l
