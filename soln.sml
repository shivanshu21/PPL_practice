(*
* Powersets in SML
* Trees
*)

val ll = 88::44::33::22::55::11::99::66::77::0::nil;
val cdl = ["a", "a", "a", "b", "b", "c", "c", "c", "c", "d", "e"];

(* Sum of N numbers *)
fun sumsNnum(0) = 0
  | sumsNnum(N:int) = N + sumsNnum(N - 1);

(* Power function X^N *)
fun power(X, N) =
  if N = 0 then 1
  else if N < 0 then ~1
  else X * power(X, N - 1);


(* Length of a list *)
fun listlength([]) = 0
  | listlength(L) = 1 + listlength(tl(L));

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
  else if tl(L) = [] then L
  else reverse(tl(L)) @ [hd(L)];

fun palindromelist(L) = L = reverse(L);

(* Last N elements of a list *)
fun lastNlist_checker(N, L) =
  if N <= 0 then false
  else if listlength(L) < N then false
  else true;

fun lastNlist(N, L)=
  if not(lastNlist_checker(N, L)) then []
  else if L = [] then []
  else if listlength(L) = N then L
  else if listlength(tl(L)) = N then tl(L)
  else lastNlist(N, tl(L));


(* Penultimate element of a list *)
fun penultimatelist(L) =
  if L = [] then ~1
  else if tl(L) = [] then ~1
  else if listlength(tl(L)) = 1 then hd(L)
  else penultimatelist(tl(L));


(* Kth element from beginning *)
fun kthelem_checker(K, L) =
  if K > listlength(L) then false
  else if K <= 0 then false
  else true;

fun kthelem(K, L) =
  if L = [] then ~1
  else if not(kthelem_checker(K, L)) then ~1
  else if listlength(tl(L)) = (listlength(L) - K) then hd(L)
  else kthelem(K - 1, tl(L));

(* Flatten nested lists' structure *)
fun flatten([]) = []
  | flatten(h::t) = h @ flatten(t);

(* Is this element a member of list L *)
fun member(E, L) =
  if L = [] then false
  else if hd(L) = E then true
  else member(E, tl(L));

(* Eliminate consequtive duplicates *)
fun consdup(L) =
  if L = [] then []
  else if (tl(L) = []) then L
  else if (hd(L) = hd(tl(L))) then consdup(tl(L))
  else hd(L)::consdup(tl(L));

(* Pack consecutive duplicates in sub lists *)
fun findAll(E, L)=
  if L = [] then []
  else if E = hd(L) then E::findAll(E, tl(L))
  else findAll(E, tl(L));

fun removeAll(E, L)=
  if L = [] then []
  else if E = hd(L) then removeAll(E, tl(L))
  else hd(L)::removeAll(E, tl(L));

fun conspack(L) =
  if L = [] then []
  else findAll(hd(L), L) :: conspack(removeAll(hd(L), L));

(* Range function *)
fun range(X, Y)=
  if X > Y then []
  else [X] @ range(X + 1, Y);

(* Remove last element from a list *)
fun remlastlist(L)=
  if L = [] then []
  else if tl(L) = [] then []
  else [hd(L)] @ remlastlist(tl(L));

(* Builds an int from int list and vice versa *)
fun runs_sum(S, L)=
  if L = [] then S
  else if hd(L) > 9 then ~1
  else runs_sum(((S * 10) + hd(L)), tl(L));

fun listtoint(L)=
  if L = [] then 0
  else runs_sum(0, L);

fun inttolist_helper(N)=
  if N < 0 then []
  else if N < 10 then [N]
  else (N mod 10) :: inttolist(N div 10);

fun inttolist(N)=
  reverse(inttolist_helper(N));
