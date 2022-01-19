:- op(800,xfy,[<:]), op(500,xfy,[=>]).
/*
type ::= int | top | bot | type => type | type /\ type | type \/ type
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
bool <: bool.                                       % AS-bool
_ <: top.                                           % AS-top
bot <: _.                                           % AS-bot
A1 => A2 <: B1 => B2 :- B1 <: A1, A2 <: B2.         % AS-array
A <: B :- splitI(B, (B1, B2)), A <: B1, A <: B2.    % AS-and
A <: B :- splitI(A, (A1, A2)), (A1 <: B; A2 <: B).  % AS-andL AS-andR
A <: B :- splitU(A, (A1, A2)), A1 <: B, A2 <: B.    % AS-or
A <: B :- splitU(B, (B1, B2)), (A <: B1 ; A <: B2). % AS-orL AS-orR

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

e(true,bool).
e(false,bool).
e(I,int):-integer(I).
e(add(E1,E2),int):-e(E1,T1),T1<:int,e(E2,T2),T2<:int.
e(if(A,B,C),T1\/T2):- e(A,T),T<:bool,e(B,T1),e(C,T2).
e(iszero(A),bool):- e(A,T),T<:int.
:- e(if(true,1,true),R),writeln(R).
:- e(if(true,1,2),R),int<:R,R<:int.
:- \+e(if(if(true,1,true),true,true),_).
:- e(if(if(true,false,true),1,true),R),bool\/int<:R,writeln(R).
:- halt.
