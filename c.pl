/*
mode ::= /\ | \/
type ::= int | top | bot | arr type type | op mode type type
*/
:- op(800,xfy,[<:,:>]), op(500,xfy,[=>]).
flipmode(/\, \/).
flipmode(\/, /\).
select(/\, top).
select(\/, bot).
split(/\,A=>B,(A=>B1,A=>B2)) :- split(/\,B,(B1,B2)).             % Sp-arrowR
split(/\,A=>B,(A1=>B,A2=>B)) :- split(\/,A,(A1,A2)).             % Sp-arrowL
split(M,T,(A,B)):- T=..[M,A,B].                                  % Sp-and
split(M,T,(T1,T2)):- T=..[M_,A,B],T1=..[M_,A1,B],T2=..[M_,A2,B], % Sp-orL
                     split(M,A,(A1,A2)).
split(M,T,(T1,T2)):- T=..[M_,A,B],T1=..[M_,A,B1],T2=..[M_,A,B2], % Sp-orR
                     split(M,B,(B1,B2)).
ck(_,int,int,_).                                             % AD-int
ck(M,_,T,_):- select(M,T).                                   % AD-bound
ck(M,A,B,_):- split(M,B,(B1,B2)),ck(M,A,B1,+),ck(M,A,B2,+).  % AD-and
ck(M,A,B,_):- split(M,A,(A1,A2)),(ck(M,A1,B,+);ck(M,A2,B,+)).% AD-andL AD-andR
ck(M,A1=>A2,B1=>B2,_):- flipmode(M,M_),ck(M_,A1,B1,+),ck(M,A2,B2,+).% AD-arrow
ck(M,A,B,+):- flipmode(M,M_),ck(M_,B,A,-).                          % AD-dual
A <: B :- ck(/\, A, B, +),format('~w <: ~w ok\n',[A,B]).
A :> B :- ck(\/, A, B, +),format('~w :> ~w ok\n',[A,B]).

% examples
t0(int=>int).
t1(int=>(int /\ int)).
t2((int \/ T0)=>int) :- t0(T0).
t3((T0=>int) /\ (int=>int)) :- t0(T0).
:-t1(T1),T1 <: T1.
:-t1(T1),T1 :> T1.
:-t2(T2),t3(T3),T2 <: T3.

:-bot <: int.
:-t0(T0),  (int=>top) :> T0.
:-       \+ bot :> int.
:-t0(T0),\+ (int=>top) <: T0.

:-t0(T0),T0/\int <: T0/\int.
:-t0(T0),T0/\int :> T0/\int.
:-t0(T0),T0\/int <: T0\/int.
:-t0(T0),T0\/int :> T0\/int.

:-t0(T0),   T0/\int <: T0.
:-t0(T0),\+ T0 <: T0/\int.
:-t0(T0),\+ T0\/int <: T0.
:-t0(T0),   T0 <: T0\/int.
:-halt.
