/*
mode ::= sub | sup
type ::= int | top | bot | arr type type | op mode type type
*/

flipmode(sub, sup).
flipmode(sup, sub).
select(sub, top).
select(sup, bot).

split(sub, arr(A, B), (arr(A, B1), arr(A, B2))) :-       % Sp-arrowR
  split(sub, B, (B1, B2)).
split(sub, arr(A, B),(arr(A1, B), arr(A2, B))) :-        % Sp-arrowL
  split(sup, A, (A1, A2)).
split(M, op(M, A, B), (A, B)).                           % Sp-and
split(M, op(M_, A, B), (op(M_,A1, B), op(M_, A2, B))) :- % Sp-orL
  split(M, A, (A1, A2)).
split(M, op(M_, A, B), (op(M_, A, B1), op(M_, A, B2))) :-% Sp-orR
  split(M, B, (B1, B2)).

% subtyping / supertyping checking
% (initally the boolean flag should be false)

ck(_, int, int, _).                                   % AD-int
ck(M, _, T, _) :- select(M, T).                       % AD-bound
ck(M, A, B, _) :- split(M, B, (B1, B2)),              % AD-and
  ck(M, A, B1, false), ck(M, A, B2, false).
ck(M, A, B, _) :- split(M, A, (A1, A2)),              % AD-andL AD-andR
  (ck(M, A1, B, false); ck(M, A2, B, false)).
ck(M, A, B, false):- flipmode(M, M_),                 % AD-dual
  ck(M_, B, A, true).
ck(M, arr(A1, A2), arr(B1, B2), _) :- flipmode(M, M_),% AD-arrow
   ck(M_, A1, B1, false), ck(M, A2, B2, false).

pretty(op(sub, A, B),R) :- pretty(A,PA),pretty(B,PB),
  format(atom(R),"(~w /\\ ~w)",[PA,PB]).
pretty(op(sup, A, B),R) :- pretty(A,PA),pretty(B,PB),
  format(atom(R),"(~w \\/ ~w)",[PA,PB]).
pretty(arr(A, B), R) :- pretty(A,PA),pretty(B,PB),
  format(atom(R),"(~w -> ~w)",[PA,PB]).
pretty(int, int).
pretty(top, top).
pretty(bot, bot).

test(sub, A, B) :- ck(sub, A, B, false), pretty(A, PA), pretty(B, PB),
                   format('~w <: ~w ok\n',[PA,PB]).
test(sup, A, B) :- ck(sup, A, B, false), pretty(A, PA), pretty(B, PB),
                   format('~w :> ~w ok\n',[PA,PB]).

% examples
t0(arr(int, int)).
t1(arr(int, op(sub, int, int))).
t2(arr(op(sup, int, T0), int)) :- t0(T0).
t3(op(sub, arr(T0, int), arr(int, int))) :- t0(T0).
:-t1(T1),test(sub, T1, T1).
:-t1(T1),test(sup, T1, T1).
:-t2(T2),t3(T3),test(sub, T2, T3).

:-test(sub, bot, int).
:-t0(T0),  test(sup,arr(int,top),T0).
:-       \+test(sup,bot,int).
:-t0(T0),\+test(sub,arr(int,top),T0).

:-t0(T0),test(sub,op(sub,T0,int),op(sub,T0,int)).
:-t0(T0),test(sup,op(sub,T0,int),op(sub,T0,int)).
:-t0(T0),test(sub,op(sup,T0,int),op(sup,T0,int)).
:-t0(T0),test(sup,op(sup,T0,int),op(sup,T0,int)).

:-t0(T0),  test(sub,op(sub,T0,int),T0).
:-t0(T0),\+test(sub,T0,op(sub,T0,int)).
:-t0(T0),\+test(sub,op(sup,T0,int),T0).
:-t0(T0),  test(sub,T0,op(sup,T0,int)).
:-halt.
