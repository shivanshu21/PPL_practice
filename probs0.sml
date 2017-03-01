(*fun penultimate(L) =
  if L = [] orelse if tl(L) = [] then 0
  else if tl(tl(L)) = [] then hd(L)
  else penultimate(tl(L));
  *)
fun kth(N, L) =
  if L = [] then 0
  else if N = 1 then hd(L)
  else kth(N - 1, tl(L));


fun reverse([]) = []
  | reverse(h::t) = reverse(t) @ [h];

fun palindrome(L) = L=reverse(L);

fun flatten([]) = []
  | flatten(h::t) = h @ flatten(t);

(*
fun eliminateConsequtive([]) = []
  | eliminateConsequtive(h::t) =
    if t = [] then h
    else if hd(t) = h then eliminateConsequtive(t);

fun eliminateDuplicates(L) =
  if L = [] then []
  else if member(hd(L), tl(L)) then eliminateDuplicates(tl(L))
  else hd(L)

*)
