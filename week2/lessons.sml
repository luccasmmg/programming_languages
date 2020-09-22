datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

fun f x =
    case x of
        Pizza => 3
      | Str s => 8
      | TwoInts(i1, i2) => i1 + i2
                                   

datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

fun eval x =
    case x of
        Constant i => i
     |  Negate exp => ~ ( eval exp)
     |  Add(exp, exp') => ( eval exp) + ( eval exp')
     |  Multiply(exp, exp') => ( eval exp) * ( eval exp')

fun max_constant e =
      case e of
          Constant i => i
       |  Negate e2 => max_constant e2
       | Add(e1, e2) => Int.max(max_constant e1, max_constant e2)
       | Multiply(e1, e2) => Int.max(max_constant e1, max_constant e2)

fun zip3 list_triple =
    case list_triple of
        ([], [], []) => []
      | (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3)::zip3(tl1, tl2, tl3)
      | _ => raise ListLengthMismatch


val test_exp = Add (Add (Constant 1, Negate (Constant 4)), Add (Constant 2, Constant 20))
val twenty = max_constant test_exp
