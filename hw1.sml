(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*1- all_except_option *)
(*string, string list -> NONE or SOME lst*)
fun all_except_option(s, los) =
    case los of 
        []        => NONE
        | x::xs => if same_string(s,x)
                     then  SOME xs
                        else case all_except_option(s, xs) of 
                              NONE    => NONE
                            | SOME xs => SOME(x::xs)

(*2- get_substitutions1*)
(*string list list, string -> string list *)

fun get_substitutions1(losl, s) =   
    case losl of 
        []    => []
    |   x::xs => case all_except_option(s, x) of 
                    NONE   => get_substitutions1(xs, s)
                |   SOME y => y @ get_substitutions1(xs, s)

(*3- get_substitutions2*)
(*string list list, string -> string list *)

fun get_substitutions2(losl, s) = 
    let 
        fun aux(losl, s, acc) = 
            case losl of 
                []    => acc
                |   x::xs => case all_except_option(s, x) of 
                            NONE   => aux(xs, s, acc @ [])
                        |   SOME y => aux(xs, s, y @ acc)
    in 
        aux(losl, s, [])
    end

(*4- similar_names*)
(*string list list, record -> record list*)


fun similar_names(losl, {first=f, middle=m, last=l}) = 
    let 
        fun helper(los, {first=f, middle=m, last=l}) = 
            case los of 
                []    => []
            |   x::xs => {first=x, middle=m, last=l} :: helper(xs, {first=f, middle=m, last=l})
    in
    case losl of [] => []
    |            _  => {first=f, middle=m, last=l} :: helper(get_substitutions1(losl, f), {first=f, middle=m, last=l})
    end

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"})



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)