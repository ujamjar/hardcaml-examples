module Make(B : HardCaml.Comb.S) = struct

  open B

  let fa x y z = 
    let s = (x ^: y) ^: z in
    let c = (x &: y) |: (x &: z) |: (y &: z) in
    c, s

  let ha x y = 
    let s = x ^: y in
    let c = x &: y in
    c, s

  let fs x y z = 
    let s = (x ^: y) ^: z in
    let b = ( (~: x) &: (y |: z) ) |: (x &: y &: z) in
    b, s

end

