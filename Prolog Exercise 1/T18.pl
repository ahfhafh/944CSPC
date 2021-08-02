% You can run the file using (assuming you have changed directories to folder of Examples)
%       swipl ./T18.pl


% Use the following command to reload prolog files when editing
%       make.


% when running in prolog do not forget periods, which are used to seperate predicates.

% my_append/3
my_append([], Y, Y).
my_append([X | Xs], Y, [X | Rest]) :- my_append(Xs, Y, Rest).

% element_at/3
% Note: we cannot do element_at(K-1, Rest, Output)
% As, element_at(K-1, Rest, Ouput) = element_at(-(K, 1)) ~ element_at(0, Rest, Output)
% becuase -(K, 1) ~ 0 does not unify
%
%
% this predicate does not work when calling it like
%       element_at(K, [1,2,3,4], O).
% as K is not an integer
element_at(0, [Element | _], Element).
element_at(K, [_ | Rest], Output) :- KNext is K - 1, element_at(KNext, Rest, Output).

% my_length1/2
my_length1([], 0) :- !.
my_length1([_ | Rest], O) :- my_length1(Rest, O1), O is O1 + 1.

% my_length1(X, 1)
%       unify my_length1(X, 1)  ~ my_length1([], 0) fails
%       unify my_length1(X, 1)  ~ my_length1([_ | Rest], O) succeeds [X = [_ | Rest], O = 1]
%           :- my_length1(Rest, O1).
%           unify my_length1(Rest, O1) ~ my_length1([], 0) succeeds [X = [_ |  []], Rest = [], O1 = 0]
%           unify my_length1(Rest, O1) ~ my_length1([_ | Rest2], O2) succeeds [Rest = [_ | Rest2], O2 = O1.
%               ... continue expanding this case to infinity

% my_length1(X, 1)
%       unify my_length1(X, 1)  ~ my_length1([], 0) fails
%       unify my_length1(X, 1)  ~ my_length1([_ | Rest], O) succeeds [X = [_ | Rest], O = 1]
%           my_length1(Rest, O1)
%           unify my_length1(Rest, O1) ~ my_length1([], 0) succeeds [X = [_ |  []], Rest = [], O1 = 0]
%               :- !. (stop backtracking!)
