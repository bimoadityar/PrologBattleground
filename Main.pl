/* Constant fact */


worldWidth(15).
/* deadzoneSpeed the number moves the player makes when the deadzone progress */
deadzoneSpeed(30).
deadzoneDamage(30).

playerMaxHP(100).
playerMaxArmor(100).
enemyMaxHP(75).
enemyStartNumber(10).
enemyFollowProb(30).
inventoryCapacity(5).

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
startWeaponList([glock,glock,glock,deagle,deagle,mp7,mp7,galil,ak47]).
startArmorList([kevlar,kevlar,flak,helmet,helmet]).
startMedList([bandage,bandage,bandage,painKillier,painKiller,smallKit,smallKit,mediumKit,mediumKit]).
startAmmoList([glock,glock,deagle,deagle,mp7,mp7,galil,galil,ak47,ak47]).

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

:- dynamic(border/1).

:- dynamic(programStart/0).

/* Added Rule */

playerDead :-
    player(_,PlayerHP,_,_,_),
    PlayerHP =< 0,
    gameOver.

gameOver :-
    print('Too bad, you are dead!'), nl,
    print('You could not take back Botwalski from the enemies!'), nl,
    quit.

printList(L) :-
    L == [], !.
    
printList([H|T]) :-
    print(H), print(', '), 
    printList(T).

min(A,B,C) :- A < B, C is A, !.
min(_,B,C) :- C is B, !.

max(A,B,C) :- A < B, C is B, !.
max(A,_,C) :- C is A, !.

near(A,A,B) :- B is A, !.
near(A,B,C) :- B > A, C is A+1, !.
near(A,_,C) :- C is A-1, !.


/* Search List */
searchList([],_) :- !, fail.
searchList([A|_],A) :- !.
searchList([_|B],C) :- searchList(B,C), !.

deleteList([],_,L) :- L = [], !.
deleteList([A|B],A,L) :- L = B, !.
deleteList([A|B],C,L) :- deleteList(B,C,L1), L =[A|L1], !.


/*Konso List*/
konsoList(L,Object,L1) :- L1 = [Object|L], !.

weaponInInv(A) :-
    findall(X, weaponInventory(X,_), L), listLength(L,A), !.


oneToTwoDim(X,A,B) :- worldWidth(WW), B is X mod WW, A is (X-B)//WW, !.
twoToOneDim(A,B,X) :- worldWidth(WW), X is A * WW + B, !.

randomProb(X) :- random(0,100,Y), Y < X, !.

generateList(A,A,X) :- X = [], !.
generateList(A,B,X) :- C is A+1, generateList(C,B,Y), X = [A|Y], !.

removeElmt([],_,L2) :- L2 = [], !.
removeElmt([X|B],X,L2) :- L2 = B, !.
removeElmt([A|B],X,L2) :- removeElmt(B,X,L3), L2 = [A|L3], !.

listLength([],X) :- X is 0, !.
listLength([_|B],X) :- listLength(B,Y), X is Y + 1, !.

/* assume listLength is at least X, base 0 */
takeNthElmt([A|B],0,L2,X) :- L2 = B, X = A, !.
takeNthElmt([A|B],D,L2,X) :- 
    E is D-1, takeNthElmt(B,E,L3,X), L2 = [A|L3], !.

/* remove X, a random element from L1 */
takeRandElmt([],L2,X) :- L2 = [], X = 0, !.
takeRandElmt(L1,L2,X) :-
    listLength(L1,A), random(0,A,B), takeNthElmt(L1,B,L2,X), !.

/* terrain */
terrain(A,B,C) :- twoToOneDim(A,B,X), border(X), print('to the '), print(C), print(' is the edge of island'), !.
terrain(A,B,C) :- twoToOneDim(A,B,X), deadzone(X), print('to the '), print(C), print(' is a deadzone'), !.
terrain(A,B,C) :- worldWidth(WW), Z is WW//2, A < Z, B < Z, print('to the '), print(C), print(' is a swamp'), !.
terrain(A,B,C) :- worldWidth(WW), Z is WW//2, A < Z, B >= Z, print('to the '), print(C), print(' is a forest'), !.
terrain(A,B,C) :- worldWidth(WW), Z is WW//2, A >= Z, B < Z, print('to the '), print(C), print(' is an open field'), !.
terrain(A,B,C) :- worldWidth(WW), Z is WW//2, A >= Z, B >= Z, print('to the '), print(C), print(' is a dessert' ), !.

terrainAll(X) :- 
    oneToTwoDim(X,A,B), N is A-1, S is A+1, W is B-1, E is B+1, print('You\'re in Boltswaski, '), terrain(N,B,north), print(', '), terrain(S,B,south), print(', '),
    terrain(A,W,west), print(', '), terrain(A,E,east), print('.'),nl.


deadzoneDamagePlayer :- 
    player(X,A,B,C,D), deadzone(X), deadzoneDamage(DD), A1 is A-DD, retract(player(_,_,_,_,_)), asserta(player(X,A1,B,C,D)), fail.
deadzoneDamagePlayer :- player(_,A,_,_,_), A =< 0, retract(player(_,_,_,_,_)), !.
deadzoneDamagePlayer :- !.

deadzoneDamageEnemy :-
    forall(enemy(X,A,B,C), (deadzone(X), deadzoneDamage(DD), A1 is A-DD, retract(enemy(X,A,B,C)), asserta(enemy(X,A1,B,C)))), fail.
deadzoneDamageEnemy :- 
    forall(enemy(X,A,B,C), (A =< 0, retract(enemy(X,A,B,C), asserta(weaponLoot(X,B,C))))), !.
deadzoneDamageEnemy :- !.

checkWinCon  :- \+player(_,_,_,_,_), print('You are dead.'), nl, retract(programStart), !.
checkWinCon :- \+ enemy(_,_,_,_), print('All enemy is slain. You win.'), nl, retract(programStart), !.
checkWinCon :- !.

checkDamageWin :- deadzoneDamagePlayer, deadzoneDamageEnemy, checkWinCon, !.
checkDamageWin :- !.


/* Command dalam spek */

/* --------- init ----------------------------------------------------- */
wipeData :-
    retractall(player(_,_,_,_,_)), retractall(weaponInventory(_,_)), retractall(miscInventory(_,_,_)), retractall(enemy(_,_,_,_)), retractall(weaponLoot(_,_,_)), retractall(armorLoot(_,_)), retractall(medLoot(_,_)), retractall(ammoLoot(_,_)), retractall(moveCount(_)), retractall(deadzone(_)).

addDeadzone(K) :-
    worldWidth(WW), WA is WW-1, K1 is WA-K, forall(between(0,WA,X), (twoToOneDim(K,X,A1),asserta(deadzone(A1)), twoToOneDim(K1,X,A2), asserta(deadzone(A2)), twoToOneDim(X,K,A3), asserta(deadzone(A3)), twoToOneDim(X,K1,A4), asserta(deadzone(A4)))).

addBorder(K) :-
    worldWidth(WW), WA is WW-1, K1 is WA-K, forall(between(0,WA,X), (twoToOneDim(K,X,A1),asserta(border(A1)), twoToOneDim(K1,X,A2), asserta(border(A2)), twoToOneDim(X,K,A3), asserta(border(A3)), twoToOneDim(X,K1,A4), asserta(border(A4)))).

startPlayer(L,L1) :-
     takeRandElmt(L,L1,P), playerMaxHP(MH), playerMaxArmor(MA), asserta(player(P,MH,MA,none,0)), asserta(miscInventory([],[],[])), asserta(moveCount(0)).

startEnemy(L,0,L1) :- L1 = L, !.
startEnemy(L,A,L1) :- 
    takeRandElmt(L,L2,X), startWeaponList(WL), takeRandElmt(WL,_,W), enemyMaxHP(EM), weaponStat(W,_,_,AS), AS1 is AS+1, random(0,AS1,AS2), asserta(enemy(X,EM,W,AS2)), B is A-1, startEnemy(L2,B,L1), !.
    
startWeapon(L,[],L1) :- L1 = L, !.
startWeapon(L,[A|B],L1) :-
    takeRandElmt(L,L2,X), weaponStat(A,_,_,AS), AS1 is AS+1, random(0,AS1,AS2), asserta(weaponLoot(X,A,AS2)), startWeapon(L2,B,L1), !.

startArmor(L,[],L1) :- L1 = L, !.
startArmor(L,[A|B],L1) :-
    takeRandElmt(L,L2,X), asserta(armorLoot(X,A)), startArmor(L2,B,L1), !.

startMed(L,[],L1) :- L1 = L, !.
startMed(L,[A|B],L1) :-
    takeRandElmt(L,L2,X), asserta(medLoot(X,A)), startMed(L2,B,L1), !.

startAmmo(L,[],L1) :- L1 = L, !.
startAmmo(L,[A|B],L1) :-
    takeRandElmt(L,L2,X), asserta(ammoLoot(X,A)), startAmmo(L2,B,L1), !.

startPosition :-
    addBorder(0), addDeadzone(1), worldWidth(WW), WB is WW*WW-1, findall(X, (between(0,WB,X), (\+deadzone(X)), \+border(X)), L), startPlayer(L,L1), enemyStartNumber(ES), startEnemy(L1,ES,L2), startWeaponList(WL), startWeapon(L2,WL,L3), startArmorList(AL), startArmor(L3,AL,L4), startMedList(ML), startMed(L4,ML,L5), startAmmoList(AML), startAmmo(L5,AML,_).

start :-
/* Bimo */
    asserta(programStart),
    nl, print('              888888b.    .d88888b. 88888888888 888       888        d8888 888      .d8888b.  888    d8P  8888888 '),
nl, print('              888  "88b  d88P" "Y88b    888     888   o   888       d88888 888     d88P  Y88b 888   d8P     888   '),
nl, print('              888  .88P  888     888    888     888  d8b  888      d88P888 888     Y88b.      888  d8P      888   '),
nl, print('              8888888K.  888     888    888     888 d888b 888     d88P 888 888      "Y888b.   888d88K       888   '),
nl, print('              888  "Y88b 888     888    888     888d88888b888    d88P  888 888         "Y88b. 8888888b      888   '),
nl, print('              888    888 888     888    888     88888P Y88888   d88P   888 888           "888 888  Y88b     888   '),
nl, print('              888   d88P Y88b. .d88P    888     8888P   Y8888  d8888888888 888     Y88b  d88P 888   Y88b    888   '),
nl, print('              8888888P"    Y88888P      888     888P     Y888 d88P     888 88888888 "Y8888P"  888    Y88b 8888888 '),
nl,
nl,
nl,

nl, print('              $$$$$$$$  $$    $$  $$$$$$$   $$$$$$$$  $$$$$$$   $$$$$$  $$$$$$$$  $$    $$   $$$$$$   $$$$$$$$'),
nl, print('              $$        $$    $$  $$    $$  $$        $$    $$    $$    $$        $$$   $$  $$    $$  $$'),
nl, print('              $$         $$  $$   $$    $$  $$        $$    $$    $$    $$        $$$$  $$  $$        $$'),   
nl, print('              $$$$$       $$$$    $$$$$$$   $$$$$     $$$$$$$     $$    $$$$$     $$ $$ $$  $$        $$$$$'),   
nl, print('              $$         $$  $$   $$        $$        $$    $$    $$    $$        $$  $$$$  $$        $$'),
nl, print('              $$        $$    $$  $$        $$        $$    $$    $$    $$        $$   $$$  $$    $$  $$'),      
nl, print('              $$$$$$$$  $$    $$  $$        $$$$$$$$  $$    $$  $$$$$$  $$$$$$$$  $$    $$   $$$$$$   $$$$$$$$'),
nl,
nl,
    randomize, wipeData, startPosition,
    help,
    nl.


/* Bim, ntar buat inisialisasi jumlah weapon di inventori, yang weaponInInv */



/* --------- quit ----------------------------------------------------- */
quit :-
    print('Farewell, it was a good game!'),
    print('Botwalski Experience © KataBimoBebas'),nl,
    print('Created by : '), nl,
    print('Bimo Adityarahman W '), nl,
    print('Irena Irmalasari '), nl,
    print('Nada Afra S '), nl,
    print('Fitria Budi A '), nl,
    halt.

/* --------- save & load ------------------------------------------------ */
/*Fitria*/

save(_) :-
    \+programStart, print('Program hasn\'t been started yet.'), nl, !.

save(Filename) :-
    telling(Old), tell(Filename),
    listing(player/5), listing(weaponInventory/2), listing(miscInventory/3),
    listing(enemy/4), listing(weaponLoot/3), listing(armorLoot/2), listing(medLoot/2), listing(ammoLoot/2),
    listing(moveCount/1), listing(deadzone/1), listing(border/1), listing(programStart/0),
    told, tell(Old),
    write('Your current game state has been saved to '), write(Filename), write(' succesfully.'), nl.

loadGame(Filename) :-
    wipeData,
    seeing(Old),
    see(Filename),
    repeat,
    read(Data),
    process(Data),
    seen,
    print('You have succesfully load '), print(Filename), print('.'),
    see(Old),
    !.

process(end_of_file) :- !.
process(Data) :- asserta(Data), fail.

/* --------- info ----------------------------------------------------- */
help :-
    \+programStart, print('Program hasn\'t been started yet.'), nl, !.

help :-
    nl, print('      ==================================================================='),
    nl, print('     |      Here are available commands for you to survive the game      |'),
    nl, print('     |===================================================================|'),
    nl, print('     |start.             | Start the game.                               |'),
    nl, print('     |help.              | Get some help to look over commands available.|'),
    nl, print('     |quit.              | Farewell, quit the game.                      |'),
    nl, print('     |look.              | Look around you.                              |'),
    nl, print('     |map.               | Open map and see where on Earth are you.      |'),
    nl, print('     |n. e. s. w.        | Move to the North, East, South, or West.      |'),
    nl, print('     |take(Object).      | Pick up an object.                            |'),
    nl, print('     |drop(Object).      | Drop an object.                               |'),
    nl, print('     |use(Object).       | Use an object from your inventory.            |'),
    nl, print('     |attack.            | Attack the enemy that crosses your path.      |'),
    nl, print('     |status.            | Show your status.                             |'),
    nl, print('     |save(FileName).    | Save your game.                               |'),
    nl, print('     |loadGame(FileName).| Load your previously saved game.              |'),
    nl, print('     |===================================================================|'),
    nl, print('     |                    A useful legends of your map                   |'),
    nl, print('     |===================================================================|'),
    nl, print('     |         W         | Weapon                                        |'),
    nl, print('     |         A         | Armor                                         |'),
    nl, print('     |         M         | Medicine                                      |'),
    nl, print('     |         O         | Ammo                                          |'),
    nl, print('     |         P         | Player a.k.a YOU                              |'),
    nl, print('     |         E         | Enemy                                         |'),
    nl, print('     |         -         | Accessible                                    |'),
    nl, print('     |         X         | Inaccessible                                  |'),
    nl, print('      ===================================================================').

printLook1(X) :-
    border(X), print('#'), !.
printLook1(X) :-
    enemy(X,_,_,_), print('E'), !.
printLook1(X) :-
    medLoot(X,_), print('M'), !.
printLook1(X) :-
    weaponLoot(X,_,_), print('W'), !.
printLook1(X) :-
    armorLoot(X,_), print('A'), !.
printLook1(X) :-
    ammoLoot(X,_), print('O'), !.
printLook1(X) :-
    player(X,_,_,_,_), print('P'), !.
printLook1(X) :-
    deadzone(X), print('X'), !.
printLook1(_) :-
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
    N is P+WW-1, printLook(N),!.
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
    print(' at your northwest '),!.
printPosition(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P-WW,
    X == X0,
    print(' at your north '),!.
printPosition(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P-WW+1,
    X == X0,
    print(' at your northeast '),!.
printPosition(X) :-
    player(P,_,_,_,_),
    X0 is P-1,
    X == X0,
    print(' at your west '),!.
printPosition(X) :-
    player(P,_,_,_,_),
    X == P,
    print(' at your position '),!.
printPosition(X) :-
    player(P,_,_,_,_),
    X0 is P+1,
    X == X0,
    print(' at your east '),!.
printPosition(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P+WW-1,
    X == X0,
    print(' at your southwest '),!.
printPosition(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P+WW,
    X == X0,
    print(' at your south '),!.
printPosition(X) :-
    worldWidth(WW), player(P,_,_,_,_),
    X0 is P+WW+1,
    X == X0,
    print(' at your southeast '),!.

look1(X,Found) :-
    border(X), Found = 1,nl,
    print('a cliff'),
    printPosition(X),!.
look1(X,Found) :-
    enemy(X,_,_,_), Found = 1,nl,
    print('an enemy'),
    printPosition(X),!.
look1(X,Found) :-
    medLoot(X,M), Found = 1,nl,
    print('a medicine: '), print(M),
    printPosition(X),!.
look1(X,Found) :-
    weaponLoot(X,W,_), Found = 1,nl,
    print('a weapon: '), print(W),
    printPosition(X),!.
look1(X,Found) :-
    armorLoot(X,A), Found = 1,nl,
    print('an armor: '), print(A),
    printPosition(X),!.
look1(X,Found) :-
    ammoLoot(X,O), Found = 1,nl,
    print('an ammo for '), print(O),
    printPosition(X),!.
look1(X,Found) :-
    deadzone(X), Found = 1,nl,
    print('a deadzone '),
    printPosition(X),!.
look1(_,Found) :-
    Found = 0.

/*Fitria*/
look :-
    \+programStart, print('Program hasn\'t been started yet.'), nl, !.


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
    X0 is X-WW-1,
    nl, printLook(X0).


/*Fitria*/
map :-
    \+programStart, print('Program hasn\'t been started yet.'), nl, !.

map :-
    printMap(0).

printMap(X) :-
    worldWidth(WW),
    MaX is WW*WW-1,
    X == MaX,
    border(X), print('#'), !.

printMap(X) :-
    worldWidth(WW),
    Xmod is X mod WW,
    Xmod == 0,
    border(X),
    nl, print('#'), !,
    N is X+1, printMap(N).

printMap(X) :-
    border(X), print('#'), !,
    N is X+1, printMap(N).


printMap(X) :-
    player(X,_,_,_,_), print('P'), !,
    N is X+1, printMap(N).

printMap(X) :-
    deadzone(X), print('X'), !,
    N is X+1, printMap(N).

printMap(X) :-
    print('-'),
    N is X+1, printMap(N).

    
/*Fitria*/
status :-
    \+programStart, print('Program hasn\'t been started yet.'), nl, !.

status :-
    /*player(Position, PlayerHP, PlayerArmor, Equiped weapon, Current ammo)*/
    player(_, PlayerHP, PlayerArmor, Weapon, Ammo),
    print('Health   : '), print(PlayerHP), nl,
    print('Armor    : '), print(PlayerArmor), nl,
    print('Weapon   : '), print(Weapon), nl,
    print('Ammo     : '), print(Ammo), nl,
    invStatus.
    
printWeaponInv :-
    \+ weaponInventory(_,_), !.
printWeaponInv :-
    forall(weaponInventory(Weapon,WeaponAmmo), (
        print(Weapon), print('(Ammo: '), print(WeaponAmmo), print(')'), print(', ')
    )), !.
    
invStatus :-
    /* weaponInventory(Weapon Name,Ammo), if we take an acquired weapon, just take the ammo */
    /* weaponInventory(Weapon, WAmmo),*/
    /* miscInventory([Armor Name], [Med Name], [Ammo Name]) */
    \+ weaponInventory(_,_),
    miscInventory(Armor, Med, Ammo),
    Armor == [],
    Med == [],
    Ammo == [],
    print('Your inventory is empty!'), !.
    

invStatus :-
    miscInventory(Armor, Med, Ammo),
    print('You have:\n\t'),
    printWeaponInv,
    printList(Armor),
    printList(Med),
    miscInventory(_,_,[]), !, 
    print('\n\tand determination in your inventory.'), !.

invStatus :-
    print('ammo of '), printList(Ammo),
    print('\n\tand determination in your inventory.'), !.

/* --------- move ----------------------------------------------------- */
enemyFollow(_) :-
    enemyFollowProb(P), \+randomProb(P), !.

enemyFollow(X) :-
    enemy(X,A,B,C), player(Y,_,_,_,_), randomProb(50), oneToTwoDim(X,X1,X2), oneToTwoDim(Y,Y1,_), near(X1,Y1,D), twoToOneDim(D,X2,E), \+enemy(E,_,_,_), retract(enemy(X,_,_,_)),  asserta(enemy(E,A,B,C)), !.

enemyFollow(X) :-
    enemy(X,A,B,C), player(Y,_,_,_,_), oneToTwoDim(X,X1,X2), oneToTwoDim(Y,_,Y2), near(X2,Y2,D),  twoToOneDim(X1,D,E), \+enemy(E,_,_,_), retract(enemy(X,_,_,_)), asserta(enemy(E,A,B,C)), !.

addMoveCount :-
    moveCount(X), X1 is X+1, retract(moveCount(_)), asserta(moveCount(X1)), deadzoneSpeed(DS), forall(enemy(Y,_,_,_), (enemyFollow(Y))), X1 mod DS =:= 0, A is X1//DS, addDeadzone(A).

addMoveCount :- !.

n :-
    \+programStart, print('Program hasn\'t been started yet.'), nl, !.

n :-
    player(X,_,_,_,_), oneToTwoDim(X,A,_), A =:= 1, print('You can\'t reach the edge of the island, There is an ocean around the island'), !. 

n :- 
    addMoveCount, player(X,A,B,C,D), worldWidth(WW), Y is X-WW, retract(player(_,_,_,_,_)), asserta(player(Y,A,B,C,D)),checkDamageWin, terrainAll(Y), !.

s :-
    \+programStart, print('Program hasn\'t been started yet.'), nl, !.

s :-
    player(X,_,_,_,_), oneToTwoDim(X,A,_), worldWidth(WW), A =:= (WW-1), print('You can\'t reach the edge of the island, There is an ocean around the island'), !. 

s :- 
    addMoveCount, player(X,A,B,C,D), worldWidth(WW), Y is X+WW, retract(player(_,_,_,_,_)), asserta(player(Y,A,B,C,D)),checkDamageWin, terrainAll(Y), !.

w :-
    \+programStart, print('Program hasn\'t been started yet.'), nl, !.

w :-
    player(X,_,_,_,_), oneToTwoDim(X,_,B), B =:= 1 , print('You can\'t reach the edge of the island, There is an ocean around the island'), !. 

w :- 
    addMoveCount, player(X,A,B,C,D), Y is X-1, retract(player(_,_,_,_,_)), asserta(player(Y,A,B,C,D)),checkDamageWin, terrainAll(Y), !.

e :-
    \+programStart, print('Program hasn\'t been started yet.'), nl, !.

e :-
    player(X,_,_,_,_), oneToTwoDim(X,_,B), worldWidth(WW), B =:= (WW-1) , print('You can\'t reach the edge of the island, There is an ocean around the island'), !. 

e :- 
    addMoveCount, player(X,A,B,C,D), Y is X+1, retract(player(_,_,_,_,_)), asserta(player(Y,A,B,C,D)),checkDamageWin, terrainAll(Y), !.




/* --------- other action ----------------------------------- */
take(_) :-
    \+programStart, print('Program hasn\'t been started yet.'), nl, !.

take(O) :- 
    player(X,_,_,O,_), weaponLoot(X, O, _), print('You already have '), print(Object), print('.'), inventoryCapacity(IC), miscInventory(L1,L2,L3), listLength(L3,LL), LL < IC, !, print('You take a full ammo of '), print(Object), print('.'), nl, konsoList(L3,O,L4), retract(miscInventory(_,_,_)), asserta(miscInventory(L1,L2,L4)), retract(weaponLoot(X,_,_)), !.

take(O) :-
    player(X,_,_,O,_), weaponLoot(X, O, _), print('Your ammo of the weapon is full. You leave it alone.'), !.

take(Object) :- 
    player(X,_,_,_,_), weaponLoot(X, Object, _), weaponInventory(Object,_), print('You already have '), print(Object), print('.'), inventoryCapacity(IC), miscInventory(L1,L2,L3), listLength(L3,LL), LL < IC, !, print('You take a full ammo of '), print(Object), print('.'), nl, konsoList(L3,Object,L4), retract(miscInventory(_,_,_)), asserta(miscInventory(L1,L2,L4)), retract(weaponLoot(X,_,_)), !.

take(Object) :-
    player(X,_,_,_,_), weaponLoot(X, Object, _), weaponInventory(Object,_), print('. Your ammo inventory is full. You leave it alone.'), !.

take(Object) :- 
    player(X,_,_,_,_), weaponLoot(X, Object, Y), weaponInInv(WI), inventoryCapacity(IC), WI < IC, asserta(weaponInventory(Object,Y)), retract(weaponLoot(X,_,_)), print('You take the '), print(Object), print(' lying on the ground.'), nl, !.

take(Object) :- 
    player(X,_,_,_,_), weaponLoot(X, Object, _), print('Your weapon inventory is full.'), inventoryCapacity(IC), miscInventory(L1,L2,L3), listLength(L3,LL), LL < IC, !, print('You take a full ammo of '), print(Object), print('.'), nl, konsoList(L3,Object,L4), retract(miscInventory(_,_,_)), asserta(miscInventory(L1,L2,L4)), retract(weaponLoot(X,_,_)), !.

take(Object) :-
    player(X,_,_,_,_), weaponLoot(X, Object, _), print('Your ammo inventory is full too. You leave it alone.'), nl, !.


take(Object) :- 
    player(X,_,_,_,_), armorLoot(X, Object), miscInventory(L1,L2,L3), listLength(L1,LL), inventoryCapacity(IC), LL < IC, !, print('You take the '), print(Object), print(' lying on the ground.'), nl, konsoList(L1,Object,L4), retract(armorLoot(X,_)), retract(miscInventory(_,_,_)), asserta(miscInventory(L4,L2,L3)), !.
take(Object) :-
    player(X,_,_,_,_), armorLoot(X, Object), print('Your armor inventory is already full. You leave it alone.'), nl, !.

take(Object) :- 
    player(X,_,_,_,_), medLoot(X, Object), miscInventory(L1,L2,L3), listLength(L2,LL), inventoryCapacity(IC), LL < IC, !, print('You take the '), print(Object), print(' lying on the ground.'), nl, konsoList(L2,Object,L4), retract(medLoot(X,_)), retract(miscInventory(_,_,_)), asserta(miscInventory(L1,L4,L3)), !.
take(Object) :-
    player(X,_,_,_,_), medLoot(X, Object), print('Your med inventory is already full. You leave it alone.'), nl, !.

take(Object) :- 
    player(X,_,_,_,_), ammoLoot(X, Object), miscInventory(L1,L2,L3), listLength(L3,LL), inventoryCapacity(IC), LL < IC, !, print('You take the '), print(Object), print(' ammo lying on the ground.'), nl, konsoList(L3,Object,L4), retract(ammoLoot(X,_)), retract(miscInventory(_,_,_)), asserta(miscInventory(L1,L2,L4)), !.
take(Object) :-
    player(X,_,_,_,_), ammoLoot(X, Object), print('Your ammo inventory is already full. You leave it alone.'), nl, !.
    
take(_) :- 
    print('There is no such object. You leave the ground alone.'), nl, !.


use(_) :-
    \+programStart, print('Program hasn\'t been started yet.'), nl, !.

use(Object) :-
    weaponInventory(Object,Y), player(A,B,C,none,_), !, print('You equip the '), print(Object), print('.'), nl, retract(player(_,_,_,_,_)), asserta(player(A,B,C,Object,Y)), retract(weaponInventory(Object,_)), !.

use(Object) :-
    weaponInventory(Object,Y), player(A,B,C,D,E), !, print('You equip the '), print(Object), print('.'), nl, retract(player(_,_,_,_,_)), asserta(player(A,B,C,Object,Y)), retract(weaponInventory(Object,_)), asserta(weaponInventory(D,E)), !.

use(Object) :-
    miscInventory(L1,L2,L3), searchList(L1, Object), !, player(A,B,C,D,E), !, print('You use the '), print(Object), print('.'), nl, deleteList(L1,Object,L4), retract(miscInventory(L1,L2,L3)), asserta(miscInventory(L4,L2,L3)), armorStat(Object,AD), C1 is C+AD, playerMaxArmor(Q), min(C1,Q,F), retract(player(_,_,_,_,_)), asserta(player(A,B,F,D,E)), !.

use(Object) :-
    miscInventory(L1,L2,L3), searchList(L2, Object), !, player(A,B,C,D,E), !, print('You use the '), print(Object), print('.'), nl, deleteList(L2,Object,L4), retract(miscInventory(L1,L2,L3)), asserta(miscInventory(L1,L4,L3)), medStat(Object,AD), B1 is B+AD, playerMaxHP(Q), min(B1,Q,F), retract(player(_,_,_,_,_)), asserta(player(A,F,C,D,E)), !.

use(Object) :-
    player(A,B,C,Object,_), miscInventory(L1,L2,L3), searchList(L3,Object), !, print('You use the '), print(Object), print(' ammo.'), nl, deleteList(L3,Object,L4), retract(miscInventory(L1,L2,L3)), asserta(miscInventory(L1,L2,L4)), weaponStat(Object,_,_,AD), retract(player(_,_,_,_,_)), asserta(player(A,B,C,Object,AD)), !.

use(Object) :-
    print('You failed to use that '), print(Object), print(' whatsoever.'), nl, !.

counterattack :- 
    player(X,_,_,_,_), enemy(X,_,_,0), print('The enemy wants to counterattacks but unfortunately his gun ran out of ammo.'), nl, !.

counterattack :- 
    player(X,_,_,_,_), enemy(X,A,B,C), print('The enemy counterattacks to you with his '), print(B), print('.'), weaponStat(B,_,P,_), retract(enemy(X,A,B,C)), C1 is C-1, asserta(enemy(X,A,B,C1)), \+randomProb(P), !, print(' He missed.'), nl, !. 

counterattack :-
    player(X,A,B,C,D), enemy(X,_,F,_), print(' He surely doesn\'t miss. You take the shot.'), nl, weaponStat(F,DG,_,_), K1 is B-DG, K2 is max(K1,0), K3 is A+B-DG, K4 is min(K3,A), retract(player(_,_,_,_,_)), asserta(player(X,K4,K2,C,D)), !. 


attack :-
    \+programStart, print('Program hasn\'t been started yet.'), nl, !.

attack :-
    addMoveCount, player(X,_,_,_,_), \+enemy(X,_,_,_), print('There is no enemy around you.'), nl, checkDamageWin, !.

attack :-
    player(_,_,_,none,_), print('You equipped no weapon.'), nl, checkDamageWin, !.

attack :-
    player(_,_,_,_,0), print('You have no ammo.'), nl, checkDamageWin, !.

attack :-
    player(X,A,B,C,D), enemy(X,_,_,_), print('You take your shot.'), weaponStat(C,_,P,_), retract(player(_,_,_,_,_)), D1 is D-1, asserta(player(X,A,B,C,D1)), \+randomProb(P), !, print(' You missed.'), nl,  counterattack, checkDamageWin, !.

attack :-
    player(X,_,_,C,_), enemy(X,E,F,G), print(' The shot hits the enemy. It looks painful.'), nl, weaponStat(C,DG,_,_), E1 is E-DG, retract(enemy(X,_,_,_)), asserta(enemy(X,E1,F,G)), checkDamageWin, counterattack, checkDamageWin, !.



drop(Object) :-
    weaponInventory(Object,Y),
    retract(weaponInventory(Object,Y)),
    weaponInInv(E), F is E-1, retractall(weaponInInv(_)), asserta(weaponInInv(F)),
    player(X,_,_,_,_),
    asserta(weaponLoot(X,Object,Y)), 
    print('You drop the '), write(Object), print('.'), !. /* weapon */
 
drop(Object) :-
    miscInventory(A,B,C),
    searchList(A, Object), !,
    player(P,_,_,_,_),
    deleteList(A, Object, D),
    retract(miscInventory(A,B,C)),
    asserta(miscInventory(D,B,C)),
    asserta(armorLoot(P,Object)),
    print('You drop the '), print(Object), print('.'), !. /*armor */
 
drop(Object) :-
    miscInventory(A,B,C),
    searchList(B,Object), !,
    player(P,_,_,_,_),
    asserta(medLoot(P,Object)),
    deleteList(B, Object, D),
    retract(miscInventory(A,B,C)),
    asserta(miscInventory(A,D,C)),
    print('You drop the '),   print(Object), print('.'), !. /* MED */
 
drop(Object) :-
    miscInventory(A,B,C),
    searchList(C, Object), !,
    player(P,_,_,_,_),
    asserta(ammoLoot(P,Object)),
    deleteList(C, Object, D),
    retract(miscInventory(A,B,C)),
    asserta(miscInventory(A,B,D)),
    print('You drop the '), print(Object), print('.'), !. /* ammo */
 
drop(_) :- print('There is no such item in your inventory.'), !.
