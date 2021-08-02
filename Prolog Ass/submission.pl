% REQUIRED CODE FOR AUTOGRADER
% DO NOT CHANGE
:- module(submission, []).
:- use_module(library(lists), []).
% Begin Assignment Code

%%%%%%% Q8 %%%%%%%

% use the following terms when solving the problem, so the autograder can roperly check if the answer is correct or not.

% house color
blue.
green.
red.
white.
yellow.

% nationality
englishman.
frenchman.
irishman.
scotsman.
spaniard.

% pet
dog.
hamster.
horse.
snake.
tiger.

% sport
baseball.
squash.
rugger.
soccer.
tennis.

% drink
beer.
gin.
orange_juice.
whiskey.
wine.

% Implement and define the following two predicates, where O is the output.
houseColor((HouseColor, _, _, _, _), HouseColor).
nationality((_, Nationality, _, _, _), Nationality).
pet((_, _, Pet, _, _), Pet).
sport((_, _, _, Sport, _), Sport).
drink((_, _, _, _, Drink), Drink).

% (a) The Irishman lives in the first house on the left.
hint_a([N, _, _, _, _]) :- nationality(N, irishman).

% (b) The man who plays baseball lives in the house next to the man who keeps a tiger.
neighbor(P1, P2, [P1, P2|_]).
neighbor(P1, P2, [P2, P1|_]).
neighbor(P1, P2, [_|L]) :- neighbor(P1, P2, L).

mymember(X, [X|_]).
mymember(X, [_|Y]) :-
    mymember(X, Y).

hint_b(L) :- sport(P1, baseball), pet(P2, tiger), neighbor(P1, P2, L).

% (c) The occupant of the house, next to the house where the owner rides a horse, plays soccer.
hint_c(L) :- neighbor(P1, P2, L), pet(P1, horse), sport(P2, soccer).

% (d) The squash player drinks gin.
hint_d(L) :- mymember(P, L), drink(P, gin), sport(P, squash).

% (e) The Frenchman plays rugger.
hint_e(L) :- mymember(P, L), nationality(P, frenchman), sport(P, rugger).

% (f) The Irishman lives next to the blue house.
hint_f(L) :- nationality(P1, irishman), houseColor(P2, blue), neighbor(P1, P2, L).

% (g) The Englishman lives in the red house.
hint_g(L) :- mymember(P, L), nationality(P, englishman), houseColor(P, red).

% (h) The Spaniard is often seen taking his dog for a walk.
hint_h(L) :- mymember(P, L), pet(P, dog), nationality(P, spaniard).

% (i) Beer is brewed (and drunk in large quantities) in the green house.
hint_i(L) :- mymember(P, L), drink(P, beer), houseColor(P, green).

% (j) The Scotsman drinks whiskey and is often tipsey.
hint_j(L) :- mymember(P, L), drink(P, scotsman), nationality(P, scotsman).

% (k) The green house is immediately to the right of the white house.
order(P1, P2, [ P1 | Rest]) :- member(P2, Rest).
order(P1, P2, [ _  | Rest])  :- order(P1, P2, Rest).

hint_k(L) :- order(P1, P2, L), houseColor(P2, green), houseColor(P1, white).

% (l) The tennis player owns snakes.
hint_l(L) :- mymember(P, L), sport(P, tennis), pet(P, snake).

% (m) Soccer is played in the yellow house.
hint_m(L) :- mymember(P, L), sport(P, soccer), houseColor(P, yellow).

% (n) A lot of wine get consumed in the middle house
hint_n([_, _, P, _, _]) :- drink(P, wine).

solution(L) :-
hint_a(L),
hint_b(L),
hint_c(L),
hint_d(L),
hint_e(L),
hint_f(L),
hint_g(L),
hint_h(L),
hint_i(L),
hint_j(L),
hint_k(L),
hint_l(L),
hint_m(L),
hint_n(L).

% hamster_owner(O).
hamster_owner(O) :- pet(P, hamster), solution(L), mymember(P, L), nationality(P, O).

% orange_juice_drinker(O).
orange_juice_drinker(O) :- drink(P, orange_juice), solution(L), mymember(P, L), nationality(P, O).

%%%%%%% Q9 %%%%%%%

% NumberofSoldiers: the total number of soldiers including Josephus and his accomplice (>2)
% N: The selected number to count down
% J: Output position for Josephus (<NumberSoldiers)
% A: Output position for the accomplice (<NumberSoldiers)

% josephus(NumberSoldiers, N, J, A).
josephus(NumberofSoldiers, N, J, A) :-
    NumberofSoldiers > 1,
    make_soldiers(NumberofSoldiers, L),
	play(0, N, NumberofSoldiers, L, [J, A]).
	
play(_, _, _, [A, B], [A, B]) :- !.
play(Start, N, NumberofSoldiers, List, R1) :-
    Position is Start + N,
   	X is mod(Position, NumberofSoldiers),
    removeElement(X, List, R),
    NumberofSoldiers1 is NumberofSoldiers - 1,
	play(X, N, NumberofSoldiers1, R, R1).
   
make_soldiers(N, L) :-
    make_soldierss(N, 1, L),
    !.

make_soldierss(0, _, []).
make_soldierss(C, C1, [C]) :-
    C = C1.
make_soldierss(C, C1, [C1|L]) :-
    C > C1,
    C2 is C1 + 1,
    make_soldierss(C, C2, L).
    

removeElement(Index, L, R) :-
    removeElementt(Index, L, 0, R).

removeElementt(Index, [_|T], C, T) :-
    Index = C, !.
removeElementt(Index, [H|T], C, [H|R]) :-
    C1 is C + 1,
    removeElementt(Index, T, C1, R).
    