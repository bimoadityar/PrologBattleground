drop(X) :- 

use(X) :- isInv(X), retract(X), medicine(X), modify_health(1), write('You used the medicine, increasing your health!').
use(X) :- isInv(X), retract(X), weapon(X,Y), modify_weapon(X), write(X), write(' is equipped. '), Y == 0, write('But the ammo is empty, Bro.').
use(X) :- isInv(X), retract(X), weapon(X,Y), modify_weapon(X), write(X), write(' is equipped. '), Y \= 0, write('Ready to attack!').
use(X) :- isInv(X), retract(X), armor(X), modify_armor(X).
use(X) :- isInv(X), retract(X), ammo(X,_), modify_ammo(X).

attack :-

modify

/* Search List */
searchLi([],_,0) :- !. /* Basis */
searchLi([A|_], C, 1) :- C == A, !. /* FOund */
searchLi([A|B], C, X) :- C \== A, searchLi(B, C, X).

/*Is Member */
isInv(X) :- searchLi(Y,X,1).

/*Del X dari Inventory */
delInv([], _, []) :- !. /* basis */
delInv([A|B], X, B):- X == A, !. /* found */
delInv([A|B], X, [A|C]) :- X \== A, del(B,X,C).

/* modify */

modify_health(X) :-
	
	health(Y), retract(health(Y)), X is Y+X, asserta(health(X)).

modify_weapon(X) :- 
	
	weapon(A,B), retract(weapon(A,B)), asserta(Inventory[A]), asserta(weapon(X)).

modify_armor(X) :-
	
	armor(Y), retract(armor(Y)), asserta(armor(X)).

modify_ammo(X) :-
	ammo(A,B), retract(ammo(A,B)), X is B + X, asserta(ammo(X)).
