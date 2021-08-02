
% REQUIRED CODE FOR AUTOGRADER
% DO NOT CHANGE
:- module(submission, []).
:- use_module(library(lists), []).
% Begin Assignment Code

% , = AND
% :- = if
% ; = OR
% not = NOT

% write(C1), write(' is warmer than '), write(C2), nl.
% [Head|Tail]
% [X, Y|W]


%%%%%%% Q1 %%%%%%%

% X concatenated with Y results in Z
% myappend(X, Y, Z).
% http://www.let.rug.nl/bos/lpn//lpnpage.php?pagetype=html&pageid=lpn-htmlse24

myappend([], X, X).
myappend([X | Y], Z, [X | W]) :- myappend(Y, Z, W).

% Y is the reverse of X
% reverse(X, Y).
myreverse([], []).
myreverse([H|T], Y) :- myreverse(T, R), myappend(R, [H], Y).

% Y is the flattening of X
% myflatten(X, Y).

myflatten([],[]).
myflatten([H|T], R) :- 
    myflatten(H, R1),
    myflatten(T, R2),
    myappend(R1, R2, R).
myflatten(H, [H]).


% X is a member of Y
% mymember(X, Y).
mymember(X, [X|_]).
mymember(X, [_|Y]) :-
    mymember(X, Y).

% Z is list obtained from remove X from Y
% myremove(X, Y, Z).
myremove(X, [X|T], T).
myremove(X, [H|T], [H|R]) :-
    H \= X,
    myremove(X, T, R).

%%%%%%% Q2 %%%%%%%

% X occurs precisely two times in L
% mymember2(X, L).
mymember2(X, Y) :-
    mymember3(X, Y, 0).

mymember3(X, [X|Y], Count) :-
    !,
    Count1 is Count + 1,
    mymember3(X, Y, Count1).
mymember3(X, [_|Y], Count) :-
    mymember3(X, Y, Count).
mymember3(_, [], Count) :-
    Count is 2.

%%%%%%% Q3 %%%%%%%

% X is a contiguous sublist of Y
% substring(X, Y).

substring(X, List) :-
    substring2(X, List, X).

substring2([], _, _).
substring2([X|T], [X|T2], Original) :-
    substring2(T, T2, Original).
substring2([_|_], [_|T2], Original) :-
    substring2(Original, T2, Original).
        

%%%%%%% Q4 %%%%%%%

% O contains all the sublists of L
% sublists(L, O).

sublists([], [[]]).
sublists([H|T], R2) :-
    sublists(T, R),
    combine(H, R, R1),
    myappend(R, R1, R2).
    
combine(_, [], []).
combine(X, [H|T], [H1|R]) :-
    prepend(X, H, H1),
    combine(X, T, R).

prepend(X, H, [X|H]).
    
%%%%%%% Q5 %%%%%%%

% X and Y are permutations of each other.
% mypermutation(X, Y).
% from prof's notes
mypermutation([], []).
mypermutation([H|T], Z):-
    mypermutation(T, S),
    insert(H, S, Z).

insert(H, T, [H|T]).
insert(H, [X|T], [X|Z]):-
    insert(H, T, Z).

%%%%%%% Q6 %%%%%%%

% Note, the daughter and son predicates are assumed to be in this form.
%
% daughter(Mother, Father, Daughter)
% son(Mother, Father, Son)
%


% is Grandfather a grandfather of Child
% grandfather(Grandfather, Grandchild).
grandfather(X, Y) :-
    (son(_, X, Tmp); daughter(_, X, Tmp)),
    (son(Tmp, _, Y); daughter(Tmp, _, Y); son(_, Tmp, Y); daughter(_, Tmp, Y)).


% is Grandmother a grandmother of Child
% grandmother(Grandmother, Grandchild).
grandmother(X, Y) :-
    (son(X,  _, Tmp); daughter(X,  _, Tmp)),
    (son(Tmp, _, Y); daughter(Tmp, _, Y); son(_, Tmp, Y); daughter(_, Tmp, Y)).

% is Brother a brother of Child
% brother(Brother, Child).
brother(X, Y) :-
    X \= Y ->
    ((son(M, _, X), son(M, _, Y));
    (son(_, F, X), son(_, F, Y));
    (son(M, _, X), daughter(M, _, Y));
    (son(_, F, X), daughter(_, F, Y))).

% is Sister a sister of Child
% sister(Sister, Child).
sister(X, Y) :-
    X \= Y ->
    ((daughter(M, _, X), son(M, _, Y));
    (daughter(_, F, X), son(_, F, Y));
    (daughter(M, _, X), daughter(M, _, Y));
    (daughter(_, F, X), daughter(_, F, Y))).

% Sibling a sibling of child
% sibling(Sibling, Child).
sibling(X, Y) :-
    X \= Y ->
    ((daughter(M, _, X), son(M, _, Y));
    (daughter(_, F, X), son(_, F, Y));
    (daughter(M, _, X), daughter(M, _, Y));
    (daughter(_, F, X), daughter(_, F, Y));
    (son(M, _, X), son(M, _, Y));
    (son(_, F, X), son(_, F, Y));
    (son(M, _, X), daughter(M, _, Y));
    (son(_, F, X), daughter(_, F, Y))).

% Is A a cousin of B?
% I.e. (Is A a child of a sibling of B).
% cousin(A, B).
cousin(X, Y) :-
    X \= Y ->
    ((daughter(M, _, X), daughter(M1, _, Y), sibling(M, M1));
    (daughter(M, _, X), son(M1, _, Y), sibling(M, M1));
    (son(M, _, X), daughter(M1, _, Y), sibling(M, M1));
    (son(M, _, X), son(M1, _, Y), sibling(M, M1));
    (daughter(_, F, X), daughter(_, F1, Y), sibling(F, F1));
    (daughter(_, F, X), son(_, F1, Y), sibling(F, F1));
    (son(_, F, X), daughter(_, F1, Y), sibling(F, F1));
    (son(_, F, X), son(_, F1, Y), sibling(F, F1));
    (daughter(M, _, X), daughter(_, F, Y), sibling(M, F));
    (daughter(M, _, X), son(_, F, Y), sibling(M, F));
    (son(M, _, X), daughter(_, F, Y), sibling(M, F));
    (son(M, _, X), son(_, F, Y), sibling(M, F));
    (daughter(_, F, X), daughter(M, _, Y), sibling(F, M));
    (daughter(_, F, X), son(M, _, Y), sibling(F, M));
    (son(_, F, X), daughter(M, _, Y), sibling(F, M));
    (son(_, F, X), son(M, _, Y), sibling(F, M))).


%%%%%%% Q7 %%%%%%%

% Note, an edge is defined as 
%   edge(X, Y).
% which is a bi-directional edge from X to Y

% Does there exists a path between X and Y
% path(X, Y).

path(X, Y) :-
    X = Y, !;
	path2(X, Y, []).

path2(X, Y, L) :-
	edge(X, Y), !;
    (edge(X, W),
    not(mymember(W, L)),
    path2(W, Y, [X|L])).

% What is the length L of the shortest path from X to Y
% shortpath(X, Y, L).
% based off prof's notes
shortpath(Start, End, Cost):-
    path(Start, End),
    isearch(0, Start, End, Cost).

isearch(Bound, Start, End, Bound):-
    dfsearch(Bound, Start, End), !.    % finish if df search succeeds within bound
isearch(Bound, Start, End, Cost):-
    NextBound is Bound + 1,             % otherwise increment the bound
    isearch(NextBound, Start, End, Cost).

dfsearch(_, End, End):- !.
dfsearch(Bound, Start, End):-
    (edge(Start, Next)),                     % directed step???
    0 < Bound,                                 %  bound the depth
    NewBound is Bound - 1,           
    dfsearch(NewBound, Next, End).

%%%%%%% Q8 %%%%%%%

% use the following terms when solving the problem, so the autograder can roperly check if the answer is correct or not.
blue.
green.
red.
white.
yellow.

englishman.
frenchman.
irishman.
scotsman.
spaniard.

dog.
hamster.
horse.
snake.
tiger.

baseball.
squash.
rugger.
soccer.
tennis.

beer.
gin.
orange_juice.
whiskey.
wine.

% Implement and define the following two predicates, where O is the output.
% hamster_owner(O).
% orange_juice_drinker(O).

%%%%%%% Q9 %%%%%%%

% NumberofSoldiers: the total number of soldiers including Josephus and his accomplice (>2)
% N: The selected number to count down
% J: Output position for Josephus (<NumberSoldiers)
% A: Output position for the accomplice (<NumberSoldiers)

% josephus(NumberSoldiers, N, J, A).