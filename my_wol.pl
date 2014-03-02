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

max(A,B,A):- A >= B.
max(A,B,B):- B > A.

test_strategy(N,FirstPlayerStrategy,SecondPlayerStrategy):-
  statistics(walltime, [Begin,_]),
  playGames(N,verbose,FirstPlayerStrategy,SecondPlayerStrategy,[],0,0,0),
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
           NewGameLengths,NewBlueWins,NewRedWins,NewDraws).


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
enemy_score(Move,[AliveMe,AliveOther],Score):-
  alter_board(Move,AliveMe,AliveMe2),
  next_generation([AliveMe2,AliveOther],[_,FinalOther]),
  length(FinalOther,Score).

bloodlust_move(Alive,OtherPlayerAlive,Move):-
  findall(([A,B,MA,MB],Score),(member([A,B],Alive),
                               neighbour_position(A,B,[MA,MB]),
                               \+member([MA,MB],Alive),
                               \+member([MA,MB],OtherPlayerAlive),
                               enemy_score([A,B,MA,MB],[Alive,OtherPlayerAlive],Score)),
           Move_Score_Pair),
  findall(Score, member((_,Score),Move_Score_Pair), Score_List),
  min_list(Score_List,Min_Score),
  member((Move,Min_Score),Move_Score_Pair).
  
bloodlust('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
 bloodlust_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

bloodlust('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 bloodlust_move(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds).

%%%% Self Preservation %%%%
my_score(Move,[AliveMe,AliveOther],Score):-
  alter_board(Move,AliveMe,AliveMe2),
  next_generation([AliveMe2,AliveOther],[FinalMe,_]),
  length(FinalMe,Score).

self_preservation_move(Alive,OtherPlayerAlive,Move):-
  findall(([A,B,MA,MB],Score),(member([A,B],Alive),
                               neighbour_position(A,B,[MA,MB]),
                               \+member([MA,MB],Alive),
                               \+member([MA,MB],OtherPlayerAlive),
                               my_score([A,B,MA,MB],[Alive,OtherPlayerAlive],Score)),
          Move_Score_Pair),
  findall(Score, member((_,Score),Move_Score_Pair), Score_List),
  max_list(Score_List,Max_Score),
  member((Move,Max_Score),Move_Score_Pair).
  
self_preservation('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
 self_preservation_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

self_preservation('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 self_preservation_move(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds).

%%%% Land Grab %%%%
land_grab_score(Move,[AliveMe,AliveOther],Score):-
  alter_board(Move,AliveMe,AliveMe2),
  next_generation([AliveMe2,AliveOther],[FinalMe,FinalOther]),
  length(FinalMe,MyScore),
  length(FinalOther,OtherScore),
  Score is MyScore - OtherScore.

land_grab_move(Alive, OtherPlayerAlive, Move) :-
  findall(([A,B,MA,MB],Score),(member([A,B],Alive),
                               neighbour_position(A,B,[MA,MB]),
                               \+member([MA,MB],Alive),
                               \+member([MA,MB],OtherPlayerAlive),
                               land_grab_score([A,B,MA,MB],[Alive,OtherPlayerAlive],Score)),
          Move_Score_Pair),
  findall(Score, member((_,Score),Move_Score_Pair), Score_List),
  max_list(Score_List,Max_Score),
  member((Move,Max_Score),Move_Score_Pair).

land_grab('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
 land_grab_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

land_grab('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 land_grab_move(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds).

%%%% Minimax %%%%

minimax_score(Move,[AliveMe,AliveOther],Score):-
  alter_board(Move,AliveMe,AliveMe2),
  next_generation([AliveMe2,AliveOther],[Me3,Other3]),
  land_grab_move(Other3,Me3,OtherMove),
  alter_board(OtherMove,Other3,Other4),
  next_generation([Me3,Other4],[FinalMe,FinalOther]),
  length(FinalMe,MyScore),
  length(FinalOther,OtherScore),
  Score is MyScore - OtherScore.

minimax_move(Alive, OtherPlayerAlive, Move) :-
  findall(([A,B,MA,MB],Score),(member([A,B],Alive),
                               neighbour_position(A,B,[MA,MB]),
                               \+member([MA,MB],Alive),
                               \+member([MA,MB],OtherPlayerAlive),
                               minimax_score([A,B,MA,MB],[Alive,OtherPlayerAlive],Score)),
          Move_Score_Pair),
  findall(Score, member((_,Score),Move_Score_Pair), Score_List),
  max_list(Score_List,Max_Score),
  member((Move,Max_Score),Move_Score_Pair).

minimax('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
 minimax_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

minimax('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 minimax_move(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds).




