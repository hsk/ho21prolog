% type ::= int | top | type => type | type /\ type
:- op(800,xfy,[<:]), op(500,xfy,[=>]).
split(A /\ B, (A, B)).                                   % Bsp-and
split(A => B, (A => B1, A => B2)) :- split(B, (B1, B2)). % Bsp-arrow
int <: int.                                              % BS-int
_ <: top.                                                % BS-top
A <: B :- split(B, (B1, B2)), A <: B1, A <: B2.          % BS-and
A1 /\ _A2 <: B :- A1 <: B.                               % BS-andL
_A1 /\ A2 <: B :- A2 <: B.                               % BS-andR
A1 => A2 <: B1 => B2 :- B1 <: A1, A2 <: B2.              % BS-arrow
% examples
t0(int => int).
t1(int => (int /\ int)).
t3((T0 => int) /\ (int => int)) :- t0(T0).
:- int <: top.
:- t1(T1), T1 <: T1.
:- t0(T0), \+ int => top <: T0.
:- t0(T0), T0 <: int => top.
:- \+ int => top <: top => top.
:- t0(T0), T0 /\ int <: T0 /\ int.
:- t0(T0), \+ T0 <: T0 /\ int.
:- t0(T0), T0 /\ int <: T0.
:- halt.
