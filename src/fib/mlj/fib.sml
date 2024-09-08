structure fib :>
sig
  val main : string option array option -> unit
end 
=
struct

fun fib x = 
   if x < 2 then 1 else fib( x - 1 ) + fib( x - 2)

fun thunk () =
   (fib 30)

fun doit num f = 
   let fun loop num res = if num > 0 then (loop (num - 1) (res + f ())) else res
   in
      loop num 0
   end

fun main(env : string option array option) =
   (doit 300 thunk;
    print (Int.toString(fib 30));
    print "\n";
    ())
end;
