/* Constant fact */


worldWidth(15).
/* deadzoneSpeed the number moves the player makes when the deadzone progress */
deadzoneSpeed(5).

playerMaxHP(100).
playerMaxArmor(100).
enemyMaxHP(75).
enemyStartNumber(10).

/* weaponStat(Weapon name, Damage, Accuracy in %, Mag size) */
weaponStat(glock, 23, 84, 20).
weaponStat(deagle, 58, 62, 7).
weaponStat(mp7, 36, 88, 30).
weaponStat(galil, 45, 76, 35).
weaponStat(ak47, 57, 69, 30).

/* armorStat(Armor name, Armor increase) */
armorStat(kevlar, 40).
armorStat(flak, 60).
armorStat(helmet, 30).

/* medStat(Med name, Health increase) */
medStat(bandage, 20).
medStat(painkiller, 15).
medStat(smallKit, 40).
medStat(mediumKit, 70).

/* starting weapon ammo is to be randomized between 50%-100% of its full mag capacity */
startWeapon([glock,glock,glock,deagle,deagle,mp7,mp7,galil,ak47]).
startArmor([kevlar,kevlar,flak,helmet,helmet]).
startMed([bandage,bandage,bandage,painKillier,painKiller,smallKit,smallKit,mediumKit,mediumKit]).
startAmmo([glock,glock,deagle,deagle,mp7,mp7,galil,galil,ak47,ak47]).

/* 
Keep in mind that here we use a single number 0 - WW^2-1 to represent the coordinate, like so :
0    1    2   . . .   14
15   16   17  . . .   29
.    .    .           .
.    .    .           .
210  211  212 . . .   224
We can convert X -> (X div WW, X mod WW) and (Y,Z) -> Y * WW + Z with Y,Z in 0 - WW-1
*/ 

/* Dynamic fact */
/* player(Position, PlayerHP, PlayerArmor, Equiped weapon, Current ammo) */
:- dynamic(player/5).
/* weaponInventory(Weapon Name,Ammo), if we take an acquired weapon, just take the ammo */
:- dynamic(weaponInventory/2).
/* miscInventory([Armor Name], [Med Name], [Ammo Name]) */
:- dynamic(miscInventory/3).
/* enemy(Position, EnemyHP, Equiped weapon, Current ammo) */
:- dynamic(enemy/4).
/* weaponLoot(Position, Weapon name, current ammo) */
:- dynamic(weaponLoot/3).
/* armorLoot(Position, Armor name) */
:- dynamic(armorLoot/2).
/* medLoot(Position, Med name) */
:- dynamic(medLoot/2).
/* ammoLoot(Position, Ammo name) */
:- dynamic(ammoLoot/2).

/* Number of moves already done by player */
:- dynamic(moveCount/1).
/* Is coordinate X in deadzone */
:- dynamic(deadzone/1).





/* Command dalam spek */


/* --------- init ----------------------------------------------------- */
start :-
/* Bimo */




/* --------- quit ----------------------------------------------------- */
quit :-
/* 





/* --------- save & load ------------------------------------------------ */
save(Filename) :-


load(Filename) :-




/* --------- info ----------------------------------------------------- */
help :-
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

printLook1(X) :-
    enemy(X,_,_,_), print('E'), !.
printLook1(X) :-
    weaponLoot(X,_,_), print('W'), !.
printLook1(X) :-
    armorLoot(X,_), print('A'), !.
printLook1(X) :-
    medLoot(X,_), print('M'), !.
printLook1(X) :-
    ammoLoot(X,_), print('O'), !.
printLook1(X) :-
    deadzone(X), print('X'), !.
printLook1(X) :-
    print('_').

printLook(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P+WW+1,
    X == X0,
    printLook1(X),!.
printLook(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P+1,
    X == X0,
    printLook1(X), nl,
    N is X+WW-1, printLook(N),!.
printLook(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P-WW+1,
    X == X0,
    printLook1(X), nl,
    N is P-1, printLook(N), !.
printLook(X) :-
    printLook1(X), print(' '),
    N is X+1, printLook(N).

printPosition(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P-WW-1,
    X == X0,
    print(' at your northwest, '),!.
printPosition(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P-WW,
    X == X0,
    print(' at your north, '),!.
printPosition(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P-WW+1,
    X == X0,
    print(' at your northeast, '),!.
printPosition(X) :-
    player(P,_,_,_,_),
    X0 is P-1,
    X == X0,
    print(' at your west, '),!.
printPosition(X) :-
    player(P,_,_,_,_),
    X == P,
    print(' at your position, '),!.
printPosition(X) :-
    player(P,_,_,_,_),
    X0 is P+1,
    X == X0,
    print(' at your east, '),!.
printPosition(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P+WW-1,
    X == X0,
    print(' at your southwest, '),!.
printPosition(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P+WW,
    X == X0,
    print(' at your south, '),!.
printPosition(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P+WW-1,
    X == X0,
    print(' at your southeast, '),!.

look1(X,Found) :-
    enemy(X,_,_,_), Found = 1,nl,
    print('an enemy'),
    printPosition(X),!.
look1(X,Found) :-
    weaponLoot(X,W,_), Found = 1,nl,
    print('a weapon: '), print(W),
    printPosition(X),!.
look1(X,Found) :-
    armorLoot(X,A), Found = 1,nl,
    print('an armor: '), print('A'),
    printPosition(X),!.
look1(X,Found) :-
    medLoot(X,M), Found = 1,nl,
    print('a medicine: '), print(M),
    printPosition(X),!.
look1(X,Found) :-
    ammoLoot(X,O), Found = 1,nl,
    print('an ammo for '), print(O),
    printPosition(X),!.
look1(X,Found) :-
    deadzone(X), Found = 1,nl,
    print('a deadzone '),
    printPosition(X),!.
look1(X,Found) :-
    Found = 0.

/*Fitria*/
look :-
    worldWidth(WW),
    player(X,_,_,_,_),
    X0 is X-WW-1, X1 is X-WW, X2 is X-WW+1,
    X3 is X-1, X4 is X, X5 is X+1,
    X6 is X+WW-1, X7 is X+WW, X8 is X+WW+1,
    print('You are now in Botwalski. You see '),
    look1(X0,Found0), look1(X1,Found1), look1(X2,Found2),
    look1(X3,Found3), look1(X4,Found4), look1(X5,Found5),
    look1(X6,Found6), look1(X7,Found7), look1(X8,Found8),
    Found0 == 0, Found1 == 0, Found2 == 0,
    Found3 == 0, Found4 == 0, Found5 == 0,
    Found6 == 0, Found7 == 0, Found8 == 0,
    print('nothing on the ground.'), nl,
    printLook(X0), !.

look :-
    worldWidth(WW),
    player(X,_,_,_,_),
    X0 is X-WW-1, X1 is X-WW, X2 is X-WW+1,
    X3 is X-1, X4 is X, X5 is X+1,
    X6 is X+WW-1, X7 is X+WW, X8 is X+WW+1,
    print('You are now in Botwalski. You see '),
    look1(X0,Found0), look1(X1,Found1), look1(X2,Found2),
    look1(X3,Found3), look1(X4,Found4), look1(X5,Found5),
    look1(X6,Found6), look1(X7,Found7), look1(X8,Found8),
    nl, print('on the ground.'), nl,
    printLook(X0).

/*Fitria*/
map :-
    printMap(0);

printMap(X) :-
    worldWidth(WW),
    MaX is WW*WW-1,
    X == Max,
    deadzone(X), print('X').

printMap(X) :-
    worldWidth(WW),
    Xmod is X mod WW,
    Xmod == 0,
    deadzone(X),
    nl, print('X'), !,
    N is X+1, printMap(N).

printMap(X) :-
    player(X,_,_,_,_), print('P'), !,
    N is X+1, printMap(N).

printMap(X) :-
    deadzone(X), print('X'), !,
    N is X+1, printMap(N).

printMap(X) :-
    print("-"),
    N is X+1, printMap(N).

/*Fitria*/
status :-
    /*player(Position, PlayerHP, PlayerArmor, Equiped weapon, Current ammo)*/
    player(X, PlayerHP, PlayerArmor, Weapon, Ammo),
    print('Health: '), print(PlayerHP), nl,
    print('Armor : '), print(PlayerArmor), nl,
    print('Weapon: '), print(Weapon), nl,
    InvStatus.

InvStatus :-
    /* weaponInventory(Weapon Name,Ammo), if we take an acquired weapon, just take the ammo */
    /* weaponInventory(Weapon, WAmmo),*/
    /* miscInventory([Armor Name], [Med Name], [Ammo Name]) */
    miscInventory(Armor, Med, Ammo),
    Armor == [],
    Med == [],
    Ammo == [],
    print('Your inventory is empty!'), !.

InvStatus :-
    miscInventory(Armor, Med, Ammo),
    print('You have:'), nl,
    printList(Armor),
    printList(Med),
    printList(Ammo),
    nl, print('in your inventory.').

printList(L) :-
    L == [], !.

printList([H|T]) :-
    print(H), print(', '), 
    printList(T).


/* --------- move ----------------------------------------------------- */
n :-


s :-


w :-


e :-




/* --------- other action ---------------------------------------------- */
take(Object) :-


drop(Object) :-


use(Object) :-


attack :-

