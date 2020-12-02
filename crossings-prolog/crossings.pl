%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%         276 Introduction to Prolog           %
%                                              %
%         Coursework (crossings)       %
%                                              %
%         Assessed Coursework                  %
%                                              %
%         MODEL SOLUTION (da04)                 %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% ------------  (utilities) DO NOT EDIT

forall(P,Q) :- \+ (P, \+ Q).

% The following might be useful for testing.
% You may edit it if you want to adjust
% the layout. DO NOT USE it in your submitted code.

write_list(List) :-
  forall(member(X, List),
         (write('  '), write(X), nl)
        ).

print_history(H) :-
 reverse(H, RevH),
 print_hist_list(RevH).

print_hist_list([]).
print_hist_list([State|Rest]) :-
 print_state(State),
 print_hist_list(Rest).

print_state([b,NHs,NZs]-[SHs,SZs]) :-
 format('*~w-~w~t~20| ~w-~w~n', [NHs,NZs,SHs,SZs]).
print_state([NHs,NZs]-[b,SHs,SZs]) :-
 format(' ~w-~w~t~20|*~w-~w~n', [NHs,NZs,SHs,SZs]).

% solutions for testing 


solution([[z,z],[z],[z,z],[z],[h,h],[h,z],[h,h],[z],[z,z],[h],[h,z]]).
solution([[z,z],[z],[z,z],[z],[h,h],[h,z],[h,h],[z],[z,z],[z],[z,z]]).
solution([[h,z],[h],[z,z],[z],[h,h],[h,z],[h,h],[z],[z,z],[h],[h,z]]).
solution([[h,z],[h],[z,z],[z],[h,h],[h,z],[h,h],[z],[z,z],[z],[z,z]]).


%% --------- END (utilities)

%% ------ Add your code to this file here.


/* Add code for Step 1 below this comment */
% safe(Bank)
%
% safe/1 holds when Bank is a given list of items (those left 
% behind on a bank when a journey is made) that is safe.

% We store the number of humans in Hn and the number of zombies
% in Zn. Therefore, we only care if there are as many humans
% as zombies, by comparing Hn and Zn

safe(Bank) :-
    Bank=[[],Z].
safe(Bank) :-
    Bank=[H,Z], length(H, Hn), length(Z, Zn), Hn >= Zn.


/* Add code for Step 2 below this comment */
% safe_state(State) 
%
% safe_state/1 holds when State represents a state 
% in which both of the banks are safe. 

% There are two cases to analyze, depending on where the boat
% is located. After unifying the State and obtaining variables
% for the actual Banks using pattern matching, we simply need
% to remember checking if those banks are 'safe'.

safe_state(State) :- 
    State=[b|Bank1]-Bank2, safe(Bank1), safe(Bank2), !.
safe_state(State) :- 
    State=Bank1-[b|Bank2], safe(Bank1), safe(Bank2), !.


/* Add code for Step 3 below this comment */
% goal(State)
%
% goal/1 holds when (given, ground) State is a valid goal state

% The goal state is when all humans and zombies are on the 2nd
% bank, with the boat (of course) being on the second bank as 
% well. Therefore, an unconditional statement is the best choice

goal(State) :- State=[[], []]-[b, [h, h, h], [z, z, z]].

/* Add code for Step 4 below this comment */
% pick(State1, Items, State2)
%
% pick/3 holds when a given state State1 and a single (ground) human/zombie  
% is or pair of (ground) humans/zombies in the list Items are picked 
% from State1, and State2 is the state obtained when the elements 
% in Items are removed from State1.

% We consider the base case to be when the list of elements that need
% to be 'picked' is empty (i.e. all items have been picked). 

% To make the reasoning simpler, we separate the procedure into two
% recursive clauses, based on the type of the item that is located 
% at the head of the 'picking' list: human or zombie. By unifying
% and pattern matching (possible due to the assumption that the terms
% are ground), we cut elements out of the initial state, and recurse
% into pick, where the initial state is updated accordingly.

% A nice detail is having the recursive call as the rightmost 
% condition, since it slightly increases performance and can
% short-circuit failing cases (not needing to compute a lot if
% there are not enough items in the initial state to satisfy
% our removal list).

pick(Bank, [], Bank).
pick(Bank1, [h|Ps], Bank2) :-
    Bank1=[[h|H1], Z1], length(Ps, X), X=<1, pick([H1, Z1], Ps, Bank2).
pick(Bank1, [z|Ps], Bank2) :-
    Bank1=[H1, [z|Z1]], length(Ps, X), X=<1, pick([H1, Z1], Ps, Bank2).

/* Add code for Step 5 below this comment */
% add_new(Items, AccState, State)
%
% add_new/3 holds when (given, ground) elements in the list Items 
% are added to their matching lists in state State

% We consider the base case to be when the list of additions is
% empty, which means all elements have been added. At this point,
% our output variable L will be assigned to whatever is in the 
% accumulator, our 2nd term.

% To make the procedure easier to grasp, I separated it into two
% clauses, depending on the type of item that is located at the
% head of the list (human or zombie), which updates the accumulator
% accordingly, while keeping the output term unmodified. A trick we
% used with a lot of success so far is unifying our different more
% complex 'types' with matching patterns. This is doable due to the
% starting assumptions that the given input terms are ground.

add_new([], Bank, Bank).
add_new([h|Tail], AccBank, Bank) :-
    AccBank=[H, Z], add_new(Tail, [[h|H], Z], Bank).
add_new([z|Tail], AccBank, Bank) :-
    AccBank=[H, Z], add_new(Tail, [H, [z|Z]], Bank).


/* Add code for Step 6 below this comment */
% visited(State, Sequence)
%
% visited/2 holds when a given state State is equivalent to some 
% member of a given Sequence

% This procedure is straight forward, as we need only check 
% membership of a State in a list of State, so we can use the
% 'member' library function.

visited(State, History) :-
    member(State, History).

/* Add code for Step 7 below this comment */
% crossing(State, Move, Next)
%
% crossing/3 holds when Move represents one of the possible (not 
% necessarily safe) river crossings that humans and zombies can 
% make in (given) state CurrentState and NextState is the state 
% that then results when Move is made

% Since the number of acceptable commands is small, they can just
% be listed as facts, as it's faster than computing a function.

checkMove([h]).
checkMove([z]).
checkMove([h, h]).
checkMove([z, z]).
checkMove([h, z]).

% The function is pretty straight forward. We use two conditionals, 
% first when pattern matching to tell the program where it is 
% supposed to remove entities from and where it is supposed to add
% entities. The boat is the key to finding the source and the 
% destination.
%
% Afterwards, we check the validity of our move(explained above),
% pick(Step 4) the move elements from the source and add (Step 5)
% them to the destination. The next state is computed again using
% an if-then-else type clause, which is needed to establish where
% to put the boat at the end. 
%
% Additionally, using this type of clause in this situation reduces
% code duplication, as there is no need to rewrite the entire predicate
% code for the case where the boat is on the opposite shore.

crossing(CurrentState, Move, NextState) :-
    (CurrentState=[b|Bank1]-Bank2
    -> CurrentState=[b|Bank1]-Bank2
    ; CurrentState=Bank2-[b|Bank1]
    ),
    checkMove(Move), 
    pick(Bank1, Move, NewBank1), 
    add_new(Move, Bank2, NewBank2), 
    (CurrentState=[b|Bank1]-Bank2
    -> NextState=NewBank1-[b|NewBank2]
    ; NextState=[b|NewBank2]-NewBank1
    ).
    
/* Add code for Step 8 below this comment */
% succeeds(Sequence)
%
% succeeds/1 holds for a sequence (a list of states) that starts
% with the initial state (all objects on the north bank) and 
% terminates with all objects on the south bank;
% Where each step is the result of "safe" journeys and no states are
% repeated. 

% Initial state.

initial([b,[h,h,h],[z,z,z]]-[[],[]]).

% The base case is known to have been hit when the predicate goal/1
% returns a positive answer. After reaching this state, the amount of
% sequences leading here might be infinite, by simply repeating the
% last 2 moves in reverse, and generating an infinite sequence.
%   To prevent this, we impose that Sequence be equal to [], the empty
% list, thus signaling that we are in our goal state, and there is no
% need to look any further.
%
% For the other clause, the algorithm is straight forward: we generate
% a move and a NextState using crossing, we verify that the state is 
% not the initial one, to avoid getting extra solutions by returning
% to the start at some point.
%   Afterwards, we check if we have ever recorded the NextState in our
% history, and if not, we proceed to check its safety. If all is well
% and the state is safe, we 'move' into it, by calling journey with
% NextState as the origin, appended to the history, and with the 
% tail of the previous Sequence of moves (which is virtual at this
% point, and will be reconstructed after we hit a positive answer,
% by backtracking).
%
% Solving this problem involved reconsidering implementations for some
% of the previous steps as well, such as safe_state. The problem came
% from the identical clauses generating a choicepoint. This was easily
% fixed, using a cut (!), after we have confirmed that the state is safe
% in either one of the two clauses.


journey(State, History, Sequence) :-
    goal(State), Sequence=[].
journey(State, History, [Move|Sequence]) :-
    crossing(State, Move, NextState),
    \+ initial(NextState),
    \+ visited(NextState, History),
    safe_state(NextState),
    journey(NextState, [NextState|History], Sequence). 

succeeds(Sequence) :-
    initial(I),
    journey(I, [], Sequence).

/* 
succeeds(Sequence) :- solution(Sequence).
*/