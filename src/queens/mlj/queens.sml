(* From "A Compiler for Lazy ML" by L. Augustsson *)
structure queens :> 
sig
  val main : string option array option -> unit
end 
=
struct

fun succ x = x + 1

fun length [] = 0
  | length (_::l) = succ (length l)

fun map f l =
  let fun map_rec [] = []
        | map_rec (x::l) = f x :: map_rec l
   in map_rec l end 

fun print_gen l = l
fun print_bool b = b

fun filter p [] = []
  | filter p (a::r) = if p a then a :: filter p r else filter p r

fun count From To = if From > To then [] else (From :: count (succ From) To)

fun concmap f [] = []
  | concmap f (x :: l) = (f x) @ (concmap f l)

fun nsoln nq =
  let fun safe d x [] = true
        | safe d x (q::l) =
          print_bool ((not (x = q)) andalso (not (x = q+d)) andalso
                      (not (x = q-d)) andalso safe (d+1) x l)
   in let fun ok [] = true
            | ok (x::l) = safe 1 x l
   in let val pos_l = count 1 nq
   in let fun testcol b = (filter ok (map (fn q => q::b) pos_l))
   in let fun gen 0 = [[]]
            | gen n = concmap testcol (print_gen (gen (n - 1)))
      
   in length (gen nq) end end end end end

(* Augustsson's version *)

fun nsoln_a nq =
  let fun ok [] = true
        | ok (x::l) = 
           let fun safe x d [] = true
                 | safe x d (q::l) =
                    ((not (x = q)) andalso (not (x = q+d)) andalso
                     (not (x = q-d)) andalso (safe x (d+1) l))
            in safe x 1 l end
  in let fun gen 0 = [[]]
           | gen n =
              concmap (fn b => (filter ok (map (fn q => q::b) (count 1 nq))))
                      (gen (n - 1))
  in length (gen nq) end end


fun testit () =
  ((nsoln 10) - (nsoln_a 10))

fun doit num f = 
   let fun loop num res = if num > 0 then (loop (num - 1) (res + f ())) else res
   in
      loop num 0
   end

fun main(env : string option array option) =
  (print (Int.toString( doit 40 testit )); ())
end;
