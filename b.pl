/*
type ::=
  | int
  | bool
  | top
  | bot
  | (type => type)
  | (type /\ type)
  | (type \/ type)
mode ::= (<:) | (:>) */
:- op(800,xfy,[<:,:>]), op(500,xfy,[=>]).
flipmode(<:, :>).
flipmode(:>, <:).
select(<:, top).
select(:>, bot).
split(<:, A /\ B, (A, B)).
split(<:, A => B, (A => B1, A => B2)) :- split(<:, B, (B1, B2)),!.
split(<:, A => B, (A1 => B, A2 => B)) :- split(:>, A, (A1, A2)),!.
split(<:, A \/ B, (A1 \/ B, A2 \/ B)) :- split(<:, A, (A1, A2)),!.
split(<:, A \/ B, (A \/ B1, A \/ B2)) :- split(<:, B, (B1, B2)),!.
split(:>, A \/ B, (A, B)).
split(:>, A /\ B, (A1 /\ B, A2 /\ B)) :- split(:>, A, (A1, A2)),!.
split(:>, A /\ B, (A \/ B1, A \/ B2)) :- split(:>, B, (B1, B2)),!.
ck(int,_,int).                                            % D-int
ck(bool,_,bool).                                          % D-bool
ck(_,M,T):- select(M, T).                                 % D-bound
ck(T,M,_):- flipmode(M,M_),                               % D-bound (dual)
            select(M_,T).
ck(A1 => A2,M, B1 => B2) :- flipmode(M,M_),               % D-arrow
                            ck(A1,M_,B1),ck(A2,M,B2).
ck(A,M,B):- split(M, B,(B1, B2)),ck(A,M,B1),ck(A,M,B2).   % D-and
ck(A,M,B):- flipmode(M,M_),                               % D-and (dual)
            split(M_,A,(A1, A2)),ck(A1,M,B),ck(A2,M,B).
ck(A,M,B):- split(M, A,(A1, A2)),(ck(A1,M,B);ck(A2,M,B)). % D-andL or D-andR
ck(A,M,B):- flipmode(M,M_),                               % D-andL or D-andR (dual)
            split(M_,B,(B1, B2)),(ck(A,M,B1);ck(A,M,B2)).
A <: B :- ck(A, <:, B).
A :> B :- ck(A, :>, B).
% examples
t0(int => int).
t1(int => (int /\ int)).
:-t1(T1),(T1 <: T1).
:-t1(T1),(T1 :> T1).
t2((int \/ T0) => int) :- t0(T0).
t3((T0 => int) /\ (int => int)) :- t0(T0).
:-t2(T2),t3(T3),(T2 <: T3).
:-(bot <: int).
:-t0(T0),(int => top :> T0).
:-      \+(bot :> int).
:-t0(T0),\+(int => top <: T0).

:-t0(T0),(T0 /\ int <: T0 /\ int).
:-t0(T0),(T0 /\ int :> T0 /\ int).
:-t0(T0),(T0 \/ int <: T0 \/ int).
:-t0(T0),(T0 \/ int :> T0 \/ int).

:-t0(T0),  (T0 /\ int <: T0).
:-t0(T0),\+(T0 <: T0 /\ int).
:-t0(T0),\+(T0 \/ int <: T0).
:-t0(T0),  (T0 <: T0 \/ int).

e(true,bool).
e(false,bool).
e(I,int):-integer(I).
e(add(E1,E2),int):-e(E1,T1),T1<:int,e(E2,T2),T2<:int.
e(if(A,B,C),T1\/T2):- e(A,T),T<:bool,e(B,T1),e(C,T2).
e(iszero(A),bool):- e(A,T),T<:int.
:- e(if(true,1,true),T),T=int\/bool,int<:T,bool<:T,\+ T<:int,\+ T<:bool.
:- e(if(true,1,2),R),R<:int.
:- \+e(if(if(true,1,true),true,true),_).
:- e(if(if(true,false,true),1,true),R),int\/bool=R,bool\/int<:R,R<:bool\/int.
:- halt.
