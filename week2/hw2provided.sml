(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (_, []) = NONE
  | all_except_option (str, x::xs) = case same_string(x, str) of
                                         true => SOME xs
                                       | false => case all_except_option(str, xs) of
                                                      NONE => NONE
                                                    | SOME y => SOME (x::y)
                                                                    
 
fun get_substitutions1 ([], _) = []
  | get_substitutions1 (x::xs, str) = case all_except_option(str, x) of
                                         NONE => get_substitutions1(xs, str)
                                       | SOME x' => x' @ get_substitutions1(xs, str)

fun get_substitutions2 (list_of_lists, str) =
    let fun aux ([], acc) = acc
          | aux (x::xs, acc) = aux(xs, acc @ (case all_except_option(str, x) of
                                                 NONE => []
                                               | SOME xs' => xs'))
    in
      aux(list_of_lists, [])
    end

fun similar_names (list_of_names, {first=first, middle=middle, last=last}) =
    let fun aux ([], name) = [name]
          | aux (x::xs, {first=first, middle=middle, last=last}) = {first=x, middle=middle, last=last} :: aux (xs, {first=first, middle=middle, last=last})
    in
      aux (get_substitutions1 (list_of_names, first), {first=first, middle=middle, last=last})
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
fun card_color((Clubs, _)) = Black
  | card_color ((Spades, _)) = Black
  | card_color _ = Red

fun card_value (_, Num i) = i
  | card_value (_, Ace) = 11
  | card_value _ = 10

fun remove_card([], _, e) = raise e
  | remove_card (x::xs, c, e) = case x=c of
                                  true => xs
                                  | false => x :: remove_card(xs, c, e)

fun all_same_color([]) = true
  | all_same_color (x::[]) = true
  | all_same_color (x::x'::xs) = case card_color(x)=card_color(x') of
                                   true => all_same_color(x'::xs)
                                   | false => false

fun sum_cards(list) =
    let fun aux ([], acc) = acc
      | aux (x::xs, acc) = aux(xs, card_value(x) + acc)
    in
      aux(list, 0)
    end

fun score(list, goal) =
    let val sum = sum_cards(list)
        val preliminary_score = if sum>goal then (sum-goal)*3 else goal-sum
    in
      case all_same_color(list) of
         true => preliminary_score div 2
       | false => preliminary_score
    end

fun officiate(card_list, moves_list, goal) =
    let fun aux(card_list, held_cards, moves_list) =
        let val score = score(held_cards, goal)
        in
          case (card_list, held_cards, moves_list, sum_cards(held_cards)>goal) of
             (_, _, [], _) => score
           | (_, _, _, true) => score
           | (x, y, z::zs, _) => case z of
                                     Discard card => aux(x, remove_card(y, card, IllegalMove), zs)
                                      | Draw => case x of
                                                   [] => score
                                                 | x::xs => aux(xs, x::y, zs)
        end
    in
      aux(card_list, [], moves_list)
    end
