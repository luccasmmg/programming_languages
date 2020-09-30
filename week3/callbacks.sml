val cbs : (int -> unit) list ref = ref []

fun onKeyEvent f = cbs := f :: (!cbs)

fun onEvent i =
    let fun loop fs =
            case fs of
                [] => ()
              | f::fs' => (f i; loop fs')
    in loop (!cbs) end
