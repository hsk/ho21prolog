/*
type ::=
  | int
  | top
  | bot
  | (type => type)
  | (type /\ type)
  | (type \/ type)
mode ::= (<:) | (:>)
*/
:- op(800,xfy,[<:,:>]), op(500,xfy,[=>]).

flipmode(<:, :>).
flipmode(:>, <:).
select(<:, top).
select(:>, bot).
ordinary(M, T) :- \+split(M, T, _).

split(<:, A /\ B, (A, B)).
split(<:, A => B, (A => B1, A => B2)) :- split(<:, B, (B1, B2)).
split(<:, A => B, (A1 => B, A2 => B)) :- ordinary(<:, B), split(:>, A,(A1, A2)).
split(<:, A \/ B, (A1 \/ B, A2 \/ B)) :- split(<:, A,(A1, A2)).
split(<:, A \/ B, (A \/ B1, A \/ B2)) :- ordinary(<:, A), split(<:, B, (B1, B2)).
split(:>, A \/ B, (A, B)).
split(:>, A /\ B, (A1 /\ B, A2 /\ B)) :- split(:>, A, (A1, A2)).
split(:>, A /\ B, (A \/ B1, A \/ B2)) :- ordinary(:>, A),split(:>, B, (B1, B2)).

ck(_,int,int).                                            % D-int
ck(M,_,T):- select(M, T).                                 % D-bound
ck(M,T,_):- flipmode(M,M_),                               % D-bound (dual)
            select(M_,T).
ck(M,A1 => A2, B1 => B2) :- flipmode(M,M_),               % D-arrow
                            ck(M_,A1,B1),ck(M,A2,B2).
ck(M,A,B):- split(M, B,(B1, B2)),ck(M,A,B1),ck(M,A,B2).   % D-and
ck(M,A,B):- flipmode(M,M_),                               % D-and (dual)
            split(M_,A,(A1, A2)),ck(M,A1,B),ck(M,A2,B).
ck(M,A,B):- split(M, A,(A1, A2)),(ck(M,A1,B); ck(M,A2,B)).% D-andL D-andR
ck(M,A,B):- flipmode(M,M_),                               % D-andL AS-andR (dual)
            split(M_,B,(B1, B2)),(ck(M,A,B1); ck(M,A,B2)).

test(A <: B):-ck(<:, A, B).
test(A :> B):-ck(:>, A, B).
% examples
t0(int => int).
t1(int => (int /\ int)).
:-t1(T1),test(T1 <: T1).
:-t1(T1),test(T1 :> T1).
t2((int \/ T0) => int) :- t0(T0).
t3((T0 => int) /\ (int => int)) :- t0(T0).
:-t2(T2),t3(T3),test(T2 <: T3).
:-test(bot <: int).
:-t0(T0),test(int => top :> T0).
:-      \+test(bot :> int).
:-t0(T0),\+test(int => top <: T0).

:-t0(T0),test(T0 /\ int <: T0 /\ int).
:-t0(T0),test(T0 /\ int :> T0 /\ int).
:-t0(T0),test(T0 \/ int <: T0 \/ int).
:-t0(T0),test(T0 \/ int :> T0 \/ int).

:-t0(T0),  test(T0 /\ int <: T0).
:-t0(T0),\+test(T0 <: T0 /\ int).
:-t0(T0),\+test(T0 \/ int <: T0).
:-t0(T0),  test(T0 <: T0 \/ int).
:-halt.
