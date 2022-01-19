:- op(800,xfy,[<:]), op(500,xfy,[=>]).
/*
type ::=
  | int
  | top
  | bot
  | type => type
  | type /\ type
  | type \/ type
*/
splitI(A /\ B, (A, B)).
splitI(A => B, (A => B1, A => B2)) :- splitI(B, (B1, B2)).
splitI(A => B, (A1 => B, A2 => B)) :- splitU(A, (A1, A2)).
splitI(A \/ B, (A1 \/ B, A2 \/ B)) :- splitI(A, (A1, A2)).
splitI(A \/ B, (A \/ B1, A \/ B2)) :- splitI(B, (B1, B2)).

splitU(A \/ B, (A, B)).
splitU(A /\ B, (A1 /\ B, A2 /\ B)) :- splitU(A, (A1, A2)).
splitU(A /\ B, (A \/ B1, A \/ B2)) :- splitU(B, (B1, B2)).

int <: int.                                         % AS-int
_ <: top.                                           % AS-top
bot <: _.                                           % AS-bot
A <: B :- splitI(B, (B1, B2)), A <: B1, A <: B2.    % AS-and
A <: B :- splitI(A, (A1, A2)), (A1 <: B; A2 <: B).  % AS-andL AS-andR
A <: B :- splitU(A, (A1, A2)), A1 <: B, A2 <: B.    % AS-or
A <: B :- splitU(B, (B1, B2)), (A <: B1 ; A <: B2). % AS-orL AS-orR
A1 => A2 <: B1 => B2 :- B1 <: A1, A2 <: B2.         % AS-array

% examples
t0(int => int).
t1(int => (int /\ int)).
t2((int \/ T0) => int) :- t0(T0).
t3((T0 => int) /\ (int => int)) :- t0(T0).
:- t1(T1),         T1 <: T1.
:- t2(T2), t3(T3), T2 <: T3.
:-                 bot <: int.
:- t0(T0),      \+ int => top <: T0.
:- t0(T0),         T0 <: int => top.
:- t0(T0),         T0 /\ int <: T0 /\ int.
:- t0(T0),         T0 \/ int <: T0 \/ int.
:- t0(T0),         T0 /\ int <: T0.
:- t0(T0),      \+ T0 <: T0 /\ int.
:- t0(T0),      \+ T0 \/ int <: T0.
:- t0(T0),         T0 <: T0 \/ int.
:- halt.
