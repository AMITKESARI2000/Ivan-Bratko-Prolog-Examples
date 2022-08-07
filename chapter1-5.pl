% Author: Amit Kesari
% Roll: CS19B003
% Year: 2022

/**
 * Page 4
 * Eg queries:
 * parent(pam,bob).
 * true
 *
 * parent(ann,bob).
 * false
 */

parent(pam,bob).
parent(tom,bob).
parent(tom,liz).
parent(bob,ann).
parent(bob,pat).
parent(pat,jim).

/**
 * Page 8
 * Eg queries:
 *
 * female(ann).
 * true
 *
 * male(ann).
 * false
 */
male(tom).
male(bob).
male(jim).

female(pam).
female(liz).
female(pat).
female(ann).



/**
 * Page 9
 * Eg queries:
 *
 * offspring(ann, tom).
 * false
 *
 * offspring(liz, pat).
 * false
 */
offspring(Y, X) :- parent(X, Y).


/**
 * Page 11
 * Eg queries:
 *
 * mother(ann, tom).
 * false
 *
 * grandparent(liz, pat).
 * false
 */
mother(X, Y) :-
	parent(X, Y),
	female(X).

grandparent(X, Z) :-
	parent(X, Y),
	parent(Y, Z).



/**
 * Page 12
 * Eg queries:
 *
 * sister(ann, pat).
 * true
 *
 * sister(liz, jim).
 * false
 */
different( X, Y) :-
	X = Y, !, fail;
	true.
sister( X, Y):-
    parent( Z, X),
    parent( Z, Y),
    female( X),
    different( X, Y).


/**
 * Page 17 - recursion
 * Eg queries:
 *
 * predecessor(pam, bob).
 * true
 *
 * predecessor(liz, jim).
 * false
 */
predecessor(X, Z) :-
    parent(X, Z).
predecessor(X, Z) :-
    parent(X, Y),
    predecessor(Y, Z).


/**
 * Page 20
 * Eg queries:
 *
 * fallible(socrates).
 * true
 *
 * fallible(amit).
 * false
 */

fallible( X) :-
    man( X).
man( socrates).

%
%=========================Chapter 2=============================
%

/**
 * Page 30 - anonymous var
 * Eg queries:
 *
 * hasachild(tom).
 * true
 *
 * hasachild(sam).
 * false
 *
 * somebody_has_child().
 * true
 */

hasachild( X) :- parent( X, _).
somebody_has_child :- parent( _, _).

/**
 * Page 35 - structures
 * Eg queries:
 *
 * hasachild(tom).
 * true
 *
 *
 * seq( r1, r2).
 * true
 *
 * par( r1, r2).
 * false
 */

seq( r1, r2).
par( r1, r2).
par( r1, par(r2, r3)).
par( r1, seq(par( r2, r3), r4)).

/* matching
D = D1.
M = may.
Yl = 1983.
date( D, M, 1983) = date( Dl, may, Y1).

point(X,Y):- X,Y.
seg(X,Y) :- X<Y.
triangle(X, Y, Z) :-
	X+Y > Z,
	X+Z > Y,
	Y+Z > X.
vertical( seg(point(X, Y), point(X,Yl) )).
horizontal( seg(point(X,Y), pornt(Xl,Y) )).

*/


/**
 * Page 43 - Matching
 * Eg queries:
 *
 * f(s(1), A).
 * A = two .
 *
 * f( s(s(s(s(s(s(1)))))),C).
 * C = one.
 *
 * f( D, three).
 * D = s(1).
 */

f( 1, one).
f( s(1), two).
f( s((1)), three).
f( s(s(s(X))),N) :-
	f( X, N).

/**
 * Page 43
 * Eg queries:
 *
 * translate(1,one).
 * true .
 *
 * translate(1,two).
 * false.
 */

translate(Number, Word):-
	Number = 1, Word = one;
	Number = 2, Word = two;
	Number = 3, Word = three.


/**
 * Page 44 - procedural matching
 * Eg queries:
 *
 * dark(X).
 * X = cat .
 *
 * dark(X), big(X).
 * X = bear.
 * [trace]
 *  Call: (11) dark(_2888) ? creep
 *  Call: (12) black(_2888) ? creep
 *  Exit: (12) black(cat) ? creep
 *  Exit: (11) dark(cat) ? creep
 *  Call: (11) big(cat) ? creep
 *  Fail: (11) big(cat) ? creep
 *  Redo: (11) dark(_2888) ? creep
 *  Call: (12) brown(_2888) ? creep
 *  Exit: (12) brown(bear) ? creep
 *  Exit: (11) dark(bear) ? creep
 *  Call: (11) big(bear) ? creep
 *  Exit: (11) big(bear) ? creep
 *  X = bear
 */
big( bear).
big( elephant).
small( cat).
brown( bear).
black( cat).
gray( elephant).

dark( Z) :-
	black( Z).
dark( Z) :-
	brown( Z).

/**
 * Page 52
 * Eg queries:
 *
 * canget(state( atdoor, onfloor, atwindow, hasnot)).
 * false.
 */

% Legal moves
move( state( middle, onbox, middle, hasnot),
      grasp,
      state( middle, onbox, middle, has)).
move( state( P, onfloor, P, H),
      climb,
      state( P, onbox, P, H) ).
move( state( Pl, onfloor, Pl, H),
      push( Pl, P2),
      state( P2, onfloor, P2, H) ).
move( state( Pl, onfloor, B, H),
      walk( Pl, P2),
      state( P2, onfloor, B, H) ).
% canget(State):monkeycan get bananain State
canget( state( -, -, -, has) ).
canget( Statel) :-
	move( Statel, move, State2),
	canget( State2).
% can 1: Monkey already has it
% can 2: Do some work to get it




%
%=========================Chapter 3=============================
%

/**
 * Page 68 - member list
 * Eg queries:
 *
 * member(b, [a,b,c] ).
 * true
 *
 * member(d, [a,b,c] ).
 * false
 */

member(X,[X|_]).
member(X,[_|TAIL]) :-
	member(X,TAIL).

/**
 * Page 70 - concat list and check final
 * Eg queries:
 *
 * member(b, [a,b,c], [b,a,b,c] ).
 * true
 *
 * member(d, [a,b,c], [d] ).
 * false
 */
conc([], L, L).
conc( [X|L1], L2, [X |L3] ) :-
	conc(L1, L2,L3).


/**
 * Page 72 - add into list and check final
 * Eg queries:
 *
 * add(d, [a,b,c], [d,a,b,c]).
 * true.
 *
 * add(a,[b], [a|[b]])
 * true.
 *
 * add(a,b, [a,b]).
 * false.
 */
add( X, L, [X|L] ).

/**
 * Page 72 - del list
 * Eg queries:
 *
 * del(c, [a,b,c], [a,b]).
 * true.
 *
 * del(a, [a|[a,b,c]], [a|[b,c]]).
 * true.
 *
 * del(a, [a|[a,b,c]], [a|[a,b,c, d]]).
 * false.
 */
del( X, [X|Tail], Tail).
del( X, [Y|Tail], [Y | Tail1] ) :-
	del( X, Tail, Tail1).



/**
 * Page 74 - sublist
 * Eg queries:
 *
 * sublist([c,d,e], [a,b,c,d,e,fl]).
 * true.
 *
 * sublist( S, [a,b,c] ).
 * S = [];
 * S = [ a ] ;
 * S = [ a,b ];
 * ...
 */

sublist( S, L) :-
	conc(Ll, L2, L),
	conc(S, L3, L2).


/**
 * Page 76 - permutation
 * Eg queries:
 *
 * permutation( [red,blue,green], P).
 * P = [red, blue, green] ;
 * P = [red, green, blue] ;
 * P = [blue, red, green] ;
 * P = [blue, green, red] ;
 * P = [green, red, blue] ;
 * P = [green, blue, red] ;
 * false.
 *
 * permutation( L, [a,b,c] ).
 * L = [a, b, c] ;
 * L = [a, c, b] ;
 * L = [b, a, c] ;
 * L = [c, a, b] ;
 * L = [b, c, a] ;
 * L = [c, b, a] ;
 * false.
 */

permutation( [], [] ).
permutation( [X|L], P) :-
	permutation(L, L1),
	insert( X, L1, P).

permutation2([], [] ).
permutation2(L, [X | P] ) :-
	del( X, L, L1),
	permutation2(L1, P).

/**
 * Page 76 - odd even length
 * Eg queries:
 *
 * odd_len([a,b,c]).
 * true.
 *
 * odd_len([a,b,c, d]).
 * false.
 */
odd_len([X]).
odd_len([X | L]) :- even_len(L).

even_len([]).
even_len([X|L]) :- odd_len(L).

/*
:- op( 1200, xfx, ':-').
:- op( 1200, fx , [ :-, ?-] ).
:- op( 1100, xfy, ';').
:- op( 1000, xfy, ',').
:- op( 700 , xfx, [=, is]).
:- op( 500 , yfx, [ + , -]).
:- op( 500 , fx , [ * , -, not] ).
:- op( 400 , yfx, [ *, /, div] ).
:- op( 300 , xfx, mod).
*/

X = 1 + 2.

/**
 * Page 87- gcd
 * Eg queries:
 *
 * gcd(4,12,4).
 * true.
 *
 * gcd(4,12,2).
 * false
 */
gcd( X, X, X).
gcd( X, Y, D) :-
	X < Y ,
	Y1 is Y - X ,
	gcd(X, Y1, D).
gcd( X, Y, D) :-
	X > Y ,
	gcd(Y, X, D).

/**
 * Page 88 - length
 * Eg queries:
 *
 * length([a,b,c], 3).
 * true.
 *
 * length([a,b,c], 2)
 * false
 * length([a,b,[c,d],e1,N).
 * N=4.
 */
length( [], 0).
length( [_ | Tail], N) :-
	length( Tail, N1),
	N is 1 + N1.


%
%=========================Chapter 4=============================
%

/**
 * Page 95 - family member
 * Eg queries:
 *
 * exists(person(Name, Surname, _,_)).
 * true.
 *
 * child(X), dateofbirth(X, date(_,_,1981)).
 * false.
 *
 * wife( person( Name, Surname, _, works( _, _) ) ).
 * true.
 *
 * family( Husband,Wife, Children),
 * total( [Husband,Wife | Children], Income).
 * Income=0.
 */
family( _, person( Name, Surname, _, _), [_, _, _ | _] ).
husband(X) :-
	family( X, _, _).
wife( X) :-
	family( _, X, _ ).
child( X) :-
	family( -, -, Children),
	member(X, Children).
member( X, [X | L] ).
member( X, [Y | L] ) :-
	member( X, L).
exists(Person) :-
	husband(Person);
	wife( Person);
	child( Person).

dateofbirth( person(_, _, Date, _), Date).
salary( person(_,_,_, works(_,S), S)).
salary( person(_,_,_, unemployed), 0).

total( [], 0).
total( [Person | List], Sum) :-
	salary(Person,S),
	total( List, Rest),
	Sum is S + Rest.
nthchild( N, Family, Child) :-
	children( Family, ChildList),
	nth_member(N, ChildList, Child).



/**
 * Page 101 - state automation
 * Eg queries:
 *
 * final(s3).
 * true.
 *
 * silent(s3, s4).
 * false.
 */
final( s3).
trans(sl, a, s1).
trans( sl, a, s2).
trans(s1, b, s1).
trans( s2, b, s3).
trans( s3, b, s4).
silent( s2, s4).
silent( s3, s1).
silent(sl, s3).


/**
 * Page 102 - state automation
 * Eg queries:
 *
 * accepts(sl, [a,a,a,b]).
 * true.
 *
 * accepts(S, [a,b] ).
 * S=s1;
 * S=s3.
 *
 * accepts(sl, [X1,X2,X3] ).
 * X1 = X3, X3 = a,
 * X2 = b .
 */
accepts(S, [] ) :-
	final( S).
accepts(S, [X | Rest] ):-
	trans( S, X, S1),
	accepts(S1, Rest).
accepts(S, String) :-
	silent(S, Sl),
	accepts(S1, String).



/**
 * Page 106 - FLIGHT ROUTE PLANNER-Travel planning
 * Eg queries:
 *
 * route( ljublijana, edinburgh,th, R).
 * [ljublijana-zurich:ju322:11:30, zurich-london:sr806:16:10, london-edinburgh:ba4822:18:40] ;
 *
 * permutation([milan, ljublijana , zurich], [City1 , City2, City3] ),
 * flight( london, City1, tu, FNl, Dep1, Arrl),
 * flight( City1 , City1, we, FN2, Dep2, Arr2),
 * flight( City2, City3, th, FN3, Dep3, Arr3),
 * flight( City3, london, fr, FN4, Dep4, Arr4).
 * false.
 *
 */
% Database : timetable( Placel, Place2, List_of_flights).
timetable(edinburgh, london, [ 9:40/ 10:50/ba4733/ alldays,
			       13:40/ 14:50/ ba4773/alldays,
			       19:40/ 20:50/ba4833/ [mo,tu,we,th,fr,su]]).

timetable(london, edinburgh,[ 9:40/ 10:50/ ba4732/ alldays,
			      11:40/ 12:50/ ba4752/ alldays,
			      18:40/ 19:50/ ba4822/ [mo,tu,we,th,fr]]).
timetable(london, ljublijana,[ 13:20/ 16:20/ ju201 / [fr],
			      13:20/ 16:20/ ju2l3 / [su]]).
timetable(london, zurich, [ 9:10/ 11:45/ ba6l4 / alldays,
			    14:45/ 17:20/ sr805/ alldays]).

timetable(london, milan, [ 8:30/ ll:20 / ba5l0 / alldays,
			   11:00/ 13:50/ a2459/ alldays]).
timetable(ljublijana,zurich,[ 11:30/ 12:40/ ju322/ [tu,th]]).
timetable(ljublijana, london,[ 11:10/ 12:20/ yu200/ [fr],
			       11:25/ 12:20/ yu212/ [su]]).


timetable(milan, london, [ 9:10 / 10:00/ a2458/ alldays,
			   12:20/ 13:10/ ba5l1/ alldays]).
timetable(milar, zurich,[ 9:25/ l0:15 / sr621/ alldays,
			  12:45/ 13:35/ sr623/ alldays]).
timetable(zurich, ljublijana,[ 13:30/ 14:40/ yu323/ [tu,th]]).
timetable(zurich, london,[ 9:00/ 9:40/ ba6l3 / [mo,tu,we,th,fr,sa],
			   16:10/ 16:55/ sr806/ [mo,tu,we,th,fr,su]]).
timetable(zurich, milan,[ 7:55/ 8:45/ sr620/ alldays]).

:-op( 50, xfy, :).
flight( Placel, Place2, Day, Fnum, Deptime, Arrtime) :-
	timetable( Placel , Place2, Flightlist),
	member( Deptime / Arrtime / Fnum / Daylist , Flightlist),
	flyday( Day, Daylist).
member(X, [X | L] ).
member(X, [Y | L] ) :-
	member(X, L).
flyday( Day, Daylist) :-
	member(Day, Daylist).
flyday( Day, alldays) :-
	member(Day, [mo,tu,we,th,fr,sa,su]).

route(P1, P2, Day, [P1-P2: Fnum : Deptime]) :-
	flight( P1, P2, Day, Fnum, Deptime,_).
route(P1, P2, Day, [P1-P3: Fnum1:Dep1|Route]):-
	route(P3, P2, Day, Route),
	flight( P1, P3, Day, Fnum1, Dep1, Arr1),
	deptime(Route, Dep2),
	transfer(Arr1, Dep2).
% Direct flight
% Indirect connection: Depl I Routel )

deptime([P1-P2: Fnum : Dep | _],Dep).
transfer(Hours1:Mins1, Hours2:Mins2) :-
	60 * (Hours2 - Hours1) + Mins2 - Mins1 >= 40.




/**
 * Page 111 - Solution1: The eight queensproblem
 * Eg queries:
 *
 * template(S), solution(S).
 * S = [1/4, 2/2,3/7,4/3,5/6,6/8,7/5,8/1];
 * ...
 *
 */

 /*
solution1([] ).
solution1( [X/Y | Others] ) :-
	solution1( Others),
	member1(Y, [1,2,3,4,5,6,7,8]),
	noattack1( X/Y, Others).
noattack1(_, []).
noattack1( X/Y, [X1/Y1 | Others]):-
	Y = \ = Y1,
	Y1-Y = \ = X1-X,
	Y1-Y = \ = X-X1,
	noattack1(X/Y, Others).
member1(X, [X | L] ).
member1(X, [Y|L]):-
	member(X, L).
% solution template
template([1/Y1, 2/Y2, 3/Y3 ,4/Y4, 5/Y5, 6/Y6, 7/Y7, 8/Y8]).
*/




/**
 * Page 116 - Solution3: The eight queensproblem
 * Eg queries:
 *
 * solution3(12,S).
 * S = [ 1,3,5,8,10,12,6,11,2,7,9,4]
 * S = [ 1, ...]
 * ...
 */
solution3(Ylist) :-
	sol3( Ylist,
	     [1,2,3,4,5,6,7,8],
	     [1,2,3,4,5,6,7,8],
	     [-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7],
	     [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]).
sol3([], [], Dy, Du, Dv).
sol3( [Y | Ylist], [X | Dx1], Dy, Du, Dv) :-
	del3( Y, Dy, Dy1),
	U is X-Y,
	del3( U, Du, Du1),
	V is X+Y,
	del3( V, Dv, Dv1),
	sol3(Ylist, Dx1, Dy1, Du1, Dv1).
del3( A, [A | List], List).
del3( A, [B | List], [B | List1] ) :-
	del3( A, List, List1).

gen3(N, N, [N] ).
gen3(N1, N2, [N1 | List] ):-
	N1 < N2,
	M is N1 + 1 ,
	gen3(M, N2, List).

solution3(N, S) :-
	gen3( 1, N, Dxy),
	Nu1 is 1 - N , Nu2 is N - 1 ,
	gen3(Nu1, Nu2, Du),
	Nv2 is N + N ,
	gen3(2, Nv2, Dv),
	sol3( S, Dxy, Dxy, Du, Dv).



%Author: Amit Kesari

%
%=========================Chapter 5=============================
%

/**
 * Page 121 - backtracking
 * Eg queries:
 *
 * f5(1,0).
 * true.
 *
 * f5(1,2).
 * false.
 *
 */

f5(X,0) :- X<3.
f5(X,2) :- 3 =< X, X < 6.
f5(X,4) :- 6 =< X.

/**
 * Page 121 - no backtracking
 * Eg queries:
 *
 * f121(1,0).
 * true.
 *
 * f121(1,2).
 * false.
 *
 */

f121(X,0) :- X<3,!.
f121(X,2) :- 3 =< X, X < 6,!.
f121(X,4) :- 6 =< X.



/**
 * Page 123 - no backtracking efficient
 * Eg queries:
 *
 * f123(10,4).
 * true.
 *
 * f123(10,0).
 * false.
 *
 */
f123(X,0) :- X < 3,!.
f123(X,2) :- X < 6,!.
f123(X,4).

/**
 * Page 125 - max
 * Eg queries:
 *
 * max(1,2,2).
 * true.
 *
 * max(1,2,1).
 * false.
 *
 */
% rules are mutually exclusive. Can use max( X, Y, Y) :- X < Y.
max( X, Y, X) :- X >= Y, !.
max( X, Y, Y).


/**
 * Page 126 - one member
 * Eg queries:
 *
 * member(X, [a,b,c]).
 * X = a ;
 * no.
 *
 */

member(X, [X | L] ) :- !.
member(X, [Y | L] ) :-
	member(X, L).


/**
 * Page 126 - add
 * Eg queries:
 *
 * add( a, [b,c],L).
 * L = [a,b,c]
 *
 * add( a, [b,c,X], L).
 * L = [b,c,a]
 * X = a
 *
 */

add( X, L, L) :-
	member(X, L), !.
add(X, L, [X | L] ).

/**
 * Pg 127 - Classification into categories
 * Eg queries:
 *
 * class(tom, fighter).
 * true.
 *
 * class(tom, sportsman).
 * true.
 *
 * class(tom, jim).
 * false.
 *
 */
beat( tom, jim).
beat( ann, tom).
beat( pat, jim).

class(X, fighter) :-
	beat( X, _),
	beat( _, X), !.
class(X, winner) :-
	beat( X, _), !.
class(X, sportsman) :-
	beat( _, X).



/**
 * Pg 128 - ex
 * Eg queries:
 *
 * p(X).
 * X = 1 ;
 * X = 2.
 *
 * p( X), p( Y).
 * X = Y, Y = 1 ;
 * X = 1, Y = 2 ;
 * X = 2, Y = 1 ;
 * X = Y, Y = 2.
 *
 * p( X), !, p( Y).
 * X = Y, Y = 1 ;
 * X = 1, Y = 2 ;
 *
 */
p( 1).
p( 2) :- !.
p( 3).



/**
 * Pg 129 - negation as failure
 * Eg queries:
 *
 * p(X).
 * X = 1 ;
 * X = 2.
 *
 */
likes( mary, X) :-
	snake(X), !, fail;
	animal( X).

/**
 * Pg 130 - differnt
 * Eg queries:
 *
 * different(a,b).
 * true.
 *
 * different(a,a).
 * false.
 */
different( X, Y) :-
	X = Y, !, fail;
	true.

/**
 * Pg 131 - using not
 * Eg queries:
 *
 * different(a,b).
 * true.
 *
 * different(a,a).
 * false.
 */
not(P) :-
	P, !, fail;
	true.
likes(mary, X):-
	animal(X),
	not(snake(X)).
different( X, Y) :-
	not( X = Y ) .




/**
 * Pg 132 -  8 queen using not
 */
solution([]).
solution( [X/Y | Others] ) :-
	solution( Others),
	member(Y, [1,2,3,4,5,6,7,8]),
	not(attacks(X/Y, Others)).
attacks(X/Y, Others) :-
	member(X1/Y1 | Others),
	( Y1 - Y ;
	Y1 is Y + X1 - X ;
	Y1 is Y - X1 + X ).
member(A, [A | L]).
member(A, [B | L] ) :-
	member(A, L).
% Solutiontemplate
template([ l/Y1,2/Y2,3/Y3,4/Y4,5/Y5,6/Y6,7/Y7,8/Y8] ).




/**
 * Pg 135
 * Eg queries:
 *
 * q( x), p( X).
 * X = b.
 *
 * p( X), q( X).
 * no
 */

human( mary).

r( a).
q( b).
p( X) :- not(r( X)).

