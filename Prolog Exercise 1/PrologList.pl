/* list operation */
head([H|_],H).

tail([_|T],T).

my_concat(X,Y,[X|Y]).

remove_one(X,[X|T],T).
remove_one(X,[H|T],[H|R]) :- remove_one(X,T,R).

remove_all(_,[],[]).
remove_all(X,[H|T],R) :- (X==H -> remove_all(X,T,R) ; R = [H|Y], remove_all(X,T,Y) ).

nth_element(H, 1, [H|T]).
nth_element(X, N, [H|T]) :- N > 1, N1 is N-1, nth_element(X, N1, T).

my_append([],L,L).
my_append([H|T],L,[H|R]) :- my_append(T,L,R).

my_length([], 0).
my_length([H|T], L) :- my_length(T, L1), L is L1+1.

my_reverse([],[]).
my_reverse([H|T],R) :- my_reverse(T,Y),my_append(Y,[H],R).

my_member(X, [X|_]).
my_member(X, [_|T]) :- my_member(X, T).

prefix_list([],_).
prefix_list([H|Tx],[H|Ty]) :- prefix_list(Tx,Ty).

suffix_list(X,X).
suffix_list(X,[_|Ty]) :- suffix_list(X,Ty).


sublists([],[]).
sublists([H|T], [H|R]) :- sublists(T, R).
sublists([_|T], R) :- sublists(T,R).


/*

grandpa (X,R) :-
             ((son(_, X, Tmp); daughter(_, X, Tmp)),
              (son(Tmp,_ R); daughter(Tmp,_,R),
               son(_,Tmp,R); daughter(_,Tmp,R)).
  



visit(X,Y,Vlist) = edge(X, Z), (Z is not in Vlist), visit ( Z, Y, [X|Vlist])

puzzle problem

you need help functions:

right_of

on_the_left

next_to

then you need to implement the puzzle hints:

HS = [(1, _, _, _, _, _),
      (2,),
      (3,),
      (4,),
      (5,)]

which_house(HS) :- HS = ....,

member(which_house(1, _, irishman, _, _, _),HS)  %hint (a)      

member(which_house(A, _, _, _, _, baseball),HS),
member(which_house(B, _, _, tiger, _, _),HS),
next_to(A,B)

at last, how to find a solution of the puzzle.

owns_hamster(R) :- which_house(X), member(which_house(_, _, R, hamster, _, _),HS)

                   

#Soldiers = 5
SList = [1,2,3,4,5]
N=3

SList in Cycle  1 2 3 4 5
N               3 2 1 0

SList in Cycle  5 1 2 3
N               3 2 1 0
 
SList in Cycle  5 1 2
N               3 2 1             
                0

SList in Cycle  1 2
                J A


                [2,3]
                [2,1,3]
                [2,3,1]
                [1,2,3]
*/                   
