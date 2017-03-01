(*
* Powersets in SML
* Trees
* Permutations wont be there but look into
* OCAML 99 probs
* flatten a list etc
*)

val ll = 88::44::33::22::55::11::nil;

(* Returns last element of a list *)
fun listlast(L) =
  if L = [] then 0
  else if tl(L) = [] then hd(L)
  else listlast(tl(L));

(* Adds E to the end of L *)
fun addslist(E, L) =
  if L = [] then [E]
  else L @ [E];

(* Reverses a list *)
fun reverse(L) =
  if L = [] then []
  else reverse(tl(L))::hd(L);
