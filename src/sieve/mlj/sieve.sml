(* Eratosthene's sieve *)
structure sieve :>
sig
  val main : string option array option -> unit
end 
=
struct

(* interval min max = [min; min+1; ...; max-1; max] *)

fun interval min max =
  if min > max then [] else min :: interval (min+1) max

(* filter p L returns the list of the elements in list L
   that satisfy predicate p *)

fun filter p [] = []
  | filter p (a::r) = if p a then a :: filter p r else filter p r

(* Application: removing all numbers multiple of n from a list of integers *)

fun remove_multiples_of n =
  filter (fn m => if (m mod n) = 0 then false else true)

(* The sieve itself *)

fun sieve max =
  let fun filter_again [] = []
        | filter_again (l as n::r) =
          if n*n > max then l else n :: filter_again (remove_multiples_of n r)
  in
    filter_again (interval 2 max)
  end

fun do_list f =
  let fun do_rec [] = ()
        | do_rec (a::L) = (f a; do_rec L)
  in
    do_rec
  end

fun doit num f = 
   let fun loop num res = if num > 0 then (loop (num - 1) (res + f ())) else res
   in
      loop num 0
   end

fun thunk () =
   length (sieve 3000);

fun testit () =
  (doit 5000 thunk;
   do_list (fn (n:int) => (print (Int.toString n); print " ")) (sieve 3000);
   print "\n")


fun main(env : string option array option) =
  testit ()
end;
