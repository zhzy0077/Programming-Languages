(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, xs) = 
	case xs of 
		  [] => NONE
		| x::xs' => if same_string(str, x)
					then SOME(xs')
					else case all_except_option(str, xs') of
						  NONE => NONE
						| SOME xs'' => x::xs''
					end
				end
fun get_substitutions1(xs, s) = 
	case xs of
		  [] => []
		| head::tail => case all_except_option(s, head) of
							  NONE => get_substitutions1(tail, s)
							| SOME s' => s' @ get_substitutions1(tail, s)

fun get_substitutions2(lst, sub) = 
	let
		fun aux(xs, acc) = 
			case xs of
				[] => []
				| head::tail => case all_except_option(sub, head) of
									NONE => aux(tail, acc)
									SOME s' => aux(tail, a' @ acc)
	in
		aux(lst, [])
	end

fun similar_names(lst, name) = 
	case name of 
		{first = firstName, middle = middleName, last = lastName} => 
			let fun join (firstNameList, acc) =
					case firstNameList of
						[] => acc
						| firstName::rest => 
							join(rest, {first=firstName, middle=middleName, last=lastName}::acc)
			in
				name::join(get_substitutions2(lst, firstName), [])
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

fun card_color c = 
	case c of
		  (Clubs, _) => Black
		| (Diamonds, _) => Red
		| (Hearts, _) => Red
		| (Spades, _) => Black
		

fun card_value c = 
	case c of 
		  (_, Ace) => 11
		| (_, Jack) => 10
		| (_, Queen) => 10
		| (_, King) => 10
		| (_, Num i) => i

fun remove_card(cs, c, e) = 
	case cs of
		  [] => raise e
		| c'::cs' => if c = c'
					 then cs'
					 else c'::remove_card(cs', c, e)

fun all_same_color cs = 
	case cs of
		  [] => true
		| _::[] => true
		| c::c'::cs' => if (card_color c) = (card_color c') 
						then all_same_color c'::cs'
						else false

fun sum_cards cs = 
	let fun aux(cs, acc) = 
			case cs of
				  [] => acc
				| x::cs' => aux(cs', (card_value x) + acc)
	in
		aux(cs, 0)
	end

fun score(cs, goal) = 
	let val sum = sum_cards cs
		val preScore = if sum > goal
					   then 3 * (sum - goal)
                       else (goal - sum)
	in
		if all_same_color cs
		then preScore div 2
		else preScore
	end

fun officiate(cardList, moveList, goal) = 
	let fun process(heldCard, cardList, moveList) = 
			case moveList of
				  [] => score(heldCard, goal)
				| (Discard c)::restMove => process(remove_card(heldCard, c, IllegalMove), cardList, restMove)
				| Draw::restMove => case cardList of
									      [] => score(heldCard, goal)
									    | card::restCard => if sum_cards(card::heldCard) > goal
									    					then score(card::heldCard, goal)
									    					else process(card::heldCard, restCard, restMove)

	in
		process([], cardList, moveList)
	end
