/*Tugas Besar IF2121 Logika Informatika*/
/*TIM Kata Bimo Bebas*/
/*13517004 - Bimo Adityarahman Wiraputra*/
/*13517100 - Irena Irmalasari*/
/*13517118 - Nada Afra Sabrina*/
/*13517133 - Fitria Budi Ananda*/

/*Predikat Dinamik*/
:- dynamic(health/1).
:- dynamic(weapon/2).
:- dynamic(armor/2).
:- dynamic(ammo/2).
:- dynamic(medicine/1).
:- dynamic(position/2).
:- dynamic(at/3).

world_size(10).


listheadconv(L,WS,L1,A,B) :-
    [R1|L1] is L,
    A is ((R1-1) mod (WS-2)) + 1,
    B is ((R1-1) / (WS-2)) + 1.


newmap:-
    /*Fakta Terkait Peta*/
    world_size(WS),
    asserta(at(1,_,deadzone)),
    asserta(at(WS,_,deadzone)),
    asserta(at(_,1,deadzone)),
    asserta(at(_,WS,deadzone)),
    /*Lokasi objek di peta pada awal permainan */
    /*Weapon*/
    asserta(at(3,3,weapon(glock-18,10))),
    asserta(at(5,9,weapon(desert-eagle,6))),
    asserta(at(7,2,weapon(p-2000,8))),
    /*Armor*/
    asserta(at(4,3,armor(tophat))),
    asserta(at(6,8,armor(rider-vest))),
    asserta(at(8,8,armor(jeans-jacket))),
    /*Medicine*/
    asserta(at(7,7,medicine(bandage))),
    /*Ammo*/
    asserta(at(1,3,ammo(glock-18,10))),
    asserta(at(2,9,ammo(desert-eagle,6))),
	asserta(at(5,2,ammo(p-2000,4))).

init:-
	/*Inisialisasi*/
	/*Fakta Terkait Pemain*/
	/*Pengaturan Awal Pemain*/
	world_size(WS),
	WS1 is (WS-2)*(WS-2)-1,
	random(0,WS1,R),
    A is (R mod (WS-2)) + 1,
    B is (R div (WS-2)) + 1,
    asserta(position(A,B)),
	asserta(health(100)),
	asserta(inventory([])),
	asserta(inv_cap(5)),
	/*Fakta Terkait Peta*/
	newmap.
	/*Fakta Terkait Objek*/

/*Rules*/
/*Rules kendali dasar*/
/*start -- Memulai permainan*/
start:-
	/*retractall*/
	/*Welcome Message*/
	init,
	help.

/*help -- Menampilkan command yang tersedia dan legenda pada peta*/
help:-
    nl, print('      =================================================================='),
    nl, print('     |     Here are available commands for you to survive the game      |'),
    nl, print('     |==================================================================|'),
    nl, print('     |start.            | Start the game.                               |'),
    nl, print('     |help.             | Get some help to look over commands available.|'),
    nl, print('     |quit.             | Farewell, quit the game.                      |'),
    nl, print('     |look.             | Look around you.                              |'),
    nl, print('     |map.              | Open map and see where on Earth are you.      |'),
    nl, print('     |n. e. s. w.       | Move to the North, East, South, or West.      |'),
    nl, print('     |take(Object).     | Pick up an object.                            |'),
    nl, print('     |drop(Object).     | Drop an object.                               |'),
    nl, print('     |use(Object).      | Use an object from your inventory.            |'),
    nl, print('     |attack.           | Attack the enemy that crosses your path.      |'),
    nl, print('     |status.           | Show your status.                             |'),
    nl, print('     |save(FileName).   | Save your game.                               |'),
    nl, print('     |load(FileName).   | Load your previously saved game.              |'),
    nl, print('     |==================================================================|'),
    nl, print('     |                   A useful legends of your map                   |'),
    nl, print('     |==================================================================|'),
    nl, print('     |         W        | Weapon                                        |'),
    nl, print('     |         A        | Armor                                         |'),
    nl, print('     |         M        | Medicine                                      |'),
    nl, print('     |         O        | Ammo                                          |'),
    nl, print('     |         P        | Player a.k.a YOU                              |'),
    nl, print('     |         E        | Enemy                                         |'),
    nl, print('     |         -        | Accessible                                    |'),
    nl, print('     |         X        | Inaccessible                                  |'),
    nl, print('      ==================================================================').

map:-
    printMap(1,1).
printMap(X,Y):-
    world_size(WS),
    X == WS,
    Y == WS,
    at(X,Y,deadzone), print('X'),!.
printMap(X,Y):-
    world_size(WS),
    Y == WS,
    at(X,Y,deadzone), print('X'),
    nl, !,
    N is X+1, printMap(N,1).
printMap(X,Y):-
    position(X,Y), print('P'), !,
    N is Y+1, printMap(X,N).
printMap(X,Y):-
    at(X,Y,deadzone), print('X'), !,
    N is Y+1, printMap(X,N).
printMap(X,Y):-
    print('-'),
    N is Y+1, printMap(X,N).


take(X):-
	position(A,B), at(A,B,weapon(X,W)), asserta(inventory([X|Y])),print('You took the '),print(X),!.
take(X):-
	position(A,B), at(A,B,ammo(X,W)), asserta(inventory([X|Y])), print('You took the '),print(X),!.
take(X):-
	position(A,B), at(A,B,medicine(X)), asserta(inventory([X|Y])), print('You took the '),print(X),!.
take(X):-
	position(A,B), at(A,B,armor(X,W)), asserta(inventory([X|Y])), print('You took the '),print(X), !.
take(X):-
	print('Object not found.').

n :-
	position(A,B), N is A-1, at(N,B,deadzone), retract(health(X)), asserta(health(0)), !.
n :-
	position(A,B), N is A-1, asserta(position(N,B)), retract(position(A,B)).
s :-
	position(A,B), N is A+1, at(N,B,deadzone), retract(health(X)), asserta(health(0)), !.
s :-
	position(A,B), N is A+1, asserta(position(N,B)), retract(position(A,B)).
w :-
	position(A,B), N is B-1, at(A,N,deadzone), retract(health(X)), asserta(health(0)), !.
w :-
	position(A,B), N is B-1, asserta(position(A,N)), retract(position(A,B)).
e :-
	position(A,B), N is B+1, at(A,N,deadzone), retract(health(X)), asserta(health(0)), !.
e :-
	position(A,B), N is B+1, asserta(position(A,N)), retract(position(A,B)).


look1(A,B) :-
	at(A,B,deadzone), print('X'), !.
look1(A,B) :-
	at(A,B,weapon(X)), print('W'), !.
look1(A,B) :-
	at(A,B,armor(X)), print('A'), !.
look1(A,B) :-
	at(A,B,ammo(X)), print('O'), !.
look1(A,B) :-
	at(A,B,medicine(X)), print('M'), !.
look1(A,B) :-
	position(A,B), print('P'), !.
/*look1(A,B) :-*/
	/*positionEnemy(_,A,B), print('E'), !.*/
look1(A,B) :-
	print('_').

look :-
	position(A,B),
	A0 is A-1, A1 is A+1, B0 is B-1, B1 is B+1,
	look1(A0,B0), print(' '),
	look1(A0,B), print(' '),
	look1(A0,B1), nl,
	look1(A,B0), print(' '),
	look1(A,B), print(' '),
	look1(A,B1), nl,
	look1(A1,B0), print(' '),
	look1(A1,B), print(' '),
	look1(A,B1).
