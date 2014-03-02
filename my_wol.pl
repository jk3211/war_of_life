:- ensure_loaded('war_of_life.pl').

% Helper functions for getting an element with a condition
% in the list
% 
sum_list([], 0).
sum_list([X|Xs], Sum):-
  sum_list(Xs, OldSum),
  Sum is X + OldSum.

avg_list(List,Average):-
  sum_list(List,Count),
  length(List,Length),
  Average is Count/Length.


forall(X, Y):- \+ (X, \+ Y).

max_list(List, Max):-
  member(Max, List),
  forall(member(X, List), Max >= X).

min_list(List, Min):-
  member(Min, List),
  forall(member(X, List), Min =< X).


list_min([], Min, Min).
list_min([L|Ls], Min0, Min) :-
    Min1 is min(L, Min0),
    list_min(Ls, Min1, Min).


test_strategy(N,FirstPlayerStrategy,SecondPlayerStrategy):-
  statistics(walltime, [Begin,_]),
  playGames(N,quiet,FirstPlayerStrategy,SecondPlayerStrategy,[],0,0,0),
  statistics(walltime, [End,_]),
  AvgGameTime is (End-Begin)/N,
  format('Average game time: ~d~n', AvgGameTime).

% playGames play games N times, and prints out the number of draws,
% number of wins for blue player, number of wins for red player,
% longest game, shortest game, and average game length.

% When the number of games left to run is zero, it prints out the result.
playGames(0,_,_,_,GameLengths,NumOfBlueWins,NumOfRedWins,NumOfDraws):-
  max_list(GameLengths,MaxLength),
  min_list(GameLengths,MinLength),
  avg_list(GameLengths,AvgLength),
  format('Number of draws: ~d~n', NumOfDraws),
  format('Number of wins for player 1 (blue): ~d~n', NumOfBlueWins),
  format('Number of wins for player 2 (red): ~d~n', NumOfRedWins),
  format('Longest (non-exhaustive) game: ~d~n', MaxLength),
  format('Shortest game: ~d~n', MinLength),
  format('Average game length (including exhaustives): ~d~n', AvgLength).

playGames(NumOfGames,ShowFlag,FirstPlayerStrategy,SecondPlayerStrategy,
         GameLengths,NumOfBlueWins,NumOfRedWins,NumOfDraws):-
  play(ShowFlag,FirstPlayerStrategy,SecondPlayerStrategy,GameLength,Winner),
  NewNumOfGames is NumOfGames - 1,
  NewGameLengths = [GameLength|GameLengths],
  (
    Winner == 'b'
    -> (NewRedWins is NumOfRedWins,
        NewBlueWins is NumOfBlueWins + 1,
        NewDraws is NumOfDraws)
    ; Winner == 'r'
    -> (NewRedWins is NumOfRedWins + 1,
        NewBlueWins is NumOfBlueWins,
        NewDraws is NumOfDraws)
    ; (NewRedWins is NumOfRedWins,
      NewBlueWins is NumOfBlueWins,
      NewDraws is NumOfDraws + 1)
  ),
  playGames(NewNumOfGames,ShowFlag,FirstPlayerStrategy,SecondPlayerStrategy,
           NewGameLengths,NewDraws,NewBlueWins,NewRedWins).


%random_move(Alive, OtherPlayerAlive, Move) :-
% findall([A,B,MA,MB],(member([A,B], Alive),
%                      neighbour_position(A,B,[MA,MB]),
%	              \+member([MA,MB],Alive),
%	              \+member([MA,MB],OtherPlayerAlive)),
%	 PossMoves),
% length(PossMoves,L),
% LP1 is L + 1,
% random(1, LP1, Pos),
% nth1(Pos, PossMoves, Move).

%%%% bloodlust %%%%
bloodlust_move(AliveBlues, AliveReds, Move).


bloodlust('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move):-
 bloodlust_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

bloodlust('r', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move):-
 bloodlust_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

%%%% Self Preservation %%%%
self_preservation_move(AliveBlues, AliveReds, Move).


self_preservation('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move):-
 self_preservation_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

self_preservation('r', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move):-
 self_preservation_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

%%%% Land Grab %%%%
land_grab_move(AliveBlues, AliveReds, Move).


land_grab('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move):-
 land_grab_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

land_grab('r', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move):-
 land_grab_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

%%%% Minimax %%%%
minimax_move(AliveBlues, AliveReds, Move).


minimax('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move):-
 minimax_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

minimax('r', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move):-
 minimax_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).




