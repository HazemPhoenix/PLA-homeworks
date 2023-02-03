(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*a- all_except_option *)
(*string, string list -> NONE or SOME lst*)
fun all_except_option(s, los) =
    case los of 
        []        => NONE
        | x::xs => if same_string(s,x)
                     then  SOME xs
                        else case all_except_option(s, xs) of 
                              NONE    => NONE
                            | SOME xs => SOME(x::xs)

(*b- get_substitutions1*)
(*string list list, string -> string list *)

fun get_substitutions1(losl, s) =   
    case losl of 
        []    => []
    |   x::xs => case all_except_option(s, x) of 
                    NONE   => get_substitutions1(xs, s)
                |   SOME y => y @ get_substitutions1(xs, s)

(*c- get_substitutions2*)
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

(*d- similar_names*)
(*string list list, record -> record list*)


fun similar_names(losl, {first=f, middle=m, last=l}) = 
    let 
        fun aux(los, {first=f, middle=m, last=l}, acc) = 
            case los of 
                []    => acc
            |   x::xs => aux(xs, {first=f, middle=m, last=l}, {first=x, middle=m, last=l} :: acc)
    in
    case losl of [] => []
    |            _  => aux(get_substitutions1(losl, f),{first=f, middle=m, last=l}, [{first=f, middle=m, last=l}])
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*a- card_color *)
(*card -> string*)

fun card_color (s, r) =
    case s of 
        Spades =>   "black"
    |   Clubs  =>   "black"
    |   _      =>   "red"

(*b- card_value *)
(*card -> int*)

fun card_value (s, r) =
    case r of 
        Num i => i
    |   Ace   => 11
    |   _     => 10

(*c- remove_card *)
(* card list, card -> card list *)

fun remove_card(loc, c, e) =   
    case loc of 
          []    => raise e
        | x::xs => case x = c of 
                      true  => xs
                    | false => x :: remove_card(xs, c, e)


(*d- all_same_colors *)
(* card list -> boolean *)

fun all_same_color(loc) = 
    case loc of 
        []         => true
        | x::[]    => true
        | x::y::xy => case card_color(x) = card_color(y) of 
            true => all_same_color(y::xy)
          | _    => false
                
(*e- sum_cards *)
(*card list -> int*)

fun sum_cards(loc) = 
    let 
        fun aux(loc, acc) =
                case loc of 
                        []    => acc
                    |   x::xs => aux(xs, acc + card_value x)
    in 
        aux(loc, 0)
    end

(*f- score *)
(*card list, int -> int*)

fun score(loc, goal) =
    let 
        fun prelim_score(loc, goal) = 
            case (sum_cards(loc), goal) of
                 (sum, goal) => case sum > goal of 
                          true  => 3 * (sum - goal)
                        | false => goal - sum
    in 
        case all_same_color(loc) of 
              true  => prelim_score(loc, goal) div 2
            | false => prelim_score(loc, goal)
    end

(*g- officiate *)
(*card list, move list, int -> int*)

fun officiate(loc, lom, goal) = 
    let 
        fun process_moves(loc, lom, held) =
            case lom of 
                    []    => held
                |   x::xs => case x of 
                                    Discard card => process_moves(loc, lom, remove_card(loc, card, IllegalMove))
                                |   Draw         => case loc of 
                                                            []    => held
                                                        |   y::ys => case sum_cards(held) > goal of
                                                                    true  => y::held
                                                                |   false => process_moves(remove_card(loc, y, IllegalMove), xs, y::held)                         
    in
        score(process_moves(loc, lom, []), goal)
    end
