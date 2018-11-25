/* Constant fact */


worldWidth(15).
/* deadzoneSpeed the number moves the player makes when the deadzone progress */
deadzoneSpeed(5).

playerMaxHP(100).
playerMaxArmor(100).
enemyMaxHP(75).
enemyStartNumber(10).
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


/* Added Rule */

printList(L) :-
    L == [], !.
    
printList([H|T]) :-
    print(H), print(', '), 
    printList(T).
    

/* Search List */
searchLi([],_,0) :- !. /* Basis */
searchLi([A|_], C, 1) :- C == A, !. /* Found */
searchLi([A|B], C, X) :- C \== A, searchLi(B, C, X).


/*Konso List*/
konso(Object,[L],[Y]) :- [Y] = [Object|L].

weaponInInv(A) :-
    findall(X, weaponInventory(X,_), L), listLength(L,A), !.


oneToTwoDim(X,A,B) :- worldWidth(WW), B is X mod WW, A is (X-B)//WW, !.
twoToOneDim(A,B,X) :- worldWidth(WW), X is A * WW + B, !.

randomProb(X) :- random(0,100,Y), X < Y, !.

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


/* Command dalam spek */


/* --------- init ----------------------------------------------------- */
wipeData :-
    retractall(player(_,_,_,_,_)), retractall(weaponInventory(_,_)), retractall(miscInventory(_,_,_)), retractall(enemy(_,_,_,_)), retractall(weaponLoot(_,_,_)), retractall(armorLoot(_,_)), retractall(medLoot(_,_)), retractall(ammoLoot(_,_)), retractall(moveCount(_)), retractall(deadzone(_)).

addDeadzone(K) :-
    worldWidth(WW), WA is WW-1, K1 is WA-K, forall(between(0,WA,X), (twoToOneDim(K,X,A1),asserta(deadzone(A1)), twoToOneDim(K1,X,A2), asserta(deadzone(A2)), twoToOneDim(X,K,A3), asserta(deadzone(A3)), twoToOneDim(X,K1,A4), asserta(deadzone(A4)))).

startPlayer(L,L1) :-
     takeRandElmt(L,L1,P), playerMaxHP(MH), playerMaxArmor(MA), asserta(player(P,MH,MA,none,0)).

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
    addDeadzone(0), worldWidth(WW), WB is WW*WW-1, findall(X, (between(0,WB,X), \+deadzone(X)), L), startPlayer(L,L1), enemyStartNumber(ES), startEnemy(L1,ES,L2), startWeaponList(WL), startWeapon(L2,WL,L3), startArmorList(AL), startArmor(L3,AL,L4), startMedList(ML), startMed(L4,ML,L5), startAmmoList(AML), startAmmo(L5,AML,_).

start :-
/* Bimo */
    randomize, wipeData, startPosition.


/* Bim, ntar buat inisialisasi jumlah weapon di inventori, yang weaponInInv */



/* --------- quit ----------------------------------------------------- */
quit :- !.
/* 





/* --------- save & load ------------------------------------------------ */
/*Fitria*/
save(Filename) :-

    open(Filename, write, Stream),

    /*Initiate data to var*/
    /*Player data*/
    player(X, PlayerHP, PlayerArmor, EquipedWeapon, WeaponAmmo),
    
    write(Stream, X), write(Stream,'.'), nl(Stream),
    write(Stream, PlayerHP), write(Stream,'.'), nl(Stream),
    write(Stream, PlayerArmor), write(Stream,'.'), nl(Stream),
    write(Stream, EquipedWeapon), write(Stream,'.'), nl(Stream),
    write(Stream, WeaponAmmo), write(Stream,'.'), nl(Stream),

    /*Inventory data*/
    forall(weaponInventory(Weapon, InvWeaponAmmo),(
        write(Stream, Weapon), write(Stream,'.'), nl(Stream),
        write(Stream, InvWeaponAmmo), write(Stream,'.'), nl(Stream)
    )),

    write('Your current game state has been saved to '), write(Filename), write(' succesfully.'), nl,
    
    close(Stream).

    /*Only save player state and inventory data*/

loadGame(Filename) :- !.




/* --------- info ----------------------------------------------------- */
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
    printMap(0).
    
printMap(X) :-
    worldWidth(WW),
    MaX is WW*WW-1,
    X == MaX,
    deadzone(X), print('X'), !.
    
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
    print('-'),
    N is X+1, printMap(N).
    
/*Fitria*/
status :-
    /*player(Position, PlayerHP, PlayerArmor, Equiped weapon, Current ammo)*/
    player(X, PlayerHP, PlayerArmor, Weapon, Ammo),
    print('Health   : '), print(PlayerHP), nl,
    print('Armor    : '), print(PlayerArmor), nl,
    print('Weapon   : '), print(Weapon), nl,
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
    print('You have:'), nl,
    printWeaponInv,
    printList(Armor),
    printList(Med),
    printList(Ammo),
    nl, print('in your inventory.').
    

/* --------- move ----------------------------------------------------- */
n :- player(X,HP,Ar,Wp,Am), worldWidth(WW), Y is X-WW , deadzone(Y), S is 0, asserta(Player(X,S,Ar,Wp,Am)),
    retract(Player(X,HP,Ar,Wp,Am)),!.
n :- player(X,HP,Ar,Wp,Am), worldWidth(WW), Y is X-WW, asserta(Player(Y,HP,Ar,Wp,Am)), retract(Player(X,HP,Ar,Wp,Am)).


s :- player(X,HP,Ar,Wp,Am), worldWidth(WW), Y is X+WW , deadzone(Y), S is 0, asserta(Player(X,S,Ar,Wp,Am)),
    retract(Player(X,HP,Ar,Wp,Am)),!.
s :- player(X,HP,Ar,Wp,Am), worldWidth(WW), Y is X+WW, asserta(Player(Y,HP,Ar,Wp,Am)), retract(Player(X,HP,Ar,Wp,Am)).


w :- player(X,HP,Ar,Wp,Am), worldWidth(WW), Y is X-1 , deadzone(Y), S is 0, asserta(Player(X,S,Ar,Wp,Am)),
    retract(Player(X,HP,Ar,Wp,Am)),!.
w :- player(X,HP,Ar,Wp,Am), worldWidth(WW), Y is X-1, asserta(Player(Y,HP,Ar,Wp,Am)), retract(Player(X,HP,Ar,Wp,Am)).


e :- player(X,HP,Ar,Wp,Am), worldWidth(WW), Y is X+1 , deadzone(Y), S is 0, asserta(Player(X,S,Ar,Wp,Am)),
    retract(Player(X,HP,Ar,Wp,Am)),!.
e :- player(X,HP,Ar,Wp,Am), worldWidth(WW), Y is X+1 , asserta(Player(Y,HP,Ar,Wp,Am)), retract(Player(X,HP,Ar,Wp,Am)).

/* --------- other action ---------------------------------------------- */
take(Object) :- player(X,HP,Ar,Object,Z), weaponLoot(X, Object, Y), retract(weapponLoot(X, Object,Y)), \+ (weaponInventory(Object,_),
                B is Y+Z, asserta(Player(X,HP,Ar,Object,B)), retract(Player(X,HP,Ar,Object,Z)), print('You took the Ammo of '),
                print(Object), print(', now the current Ammo is'), print(B), !.
take(Object) :- player(X,_,_,_,_), weaponLoot(X, Object, Y), \+ (weaponInventory(Object,_), retract(weapponLoot(X, Object,Y)), InInv(Z), inventoryCapacity(C), Z < C,
                A is Z+1, asserta(InInv(A)), retract(InInv(Z)), asserta(weaponInventory(Object,Y), print('You took the '),print(Object),!.
take(Object) :- player(X,_,_,_,_), weaponLoot(X, Object, Y), weaponInventory(Object,Z),
                B is Y+Z, asserta(weaponInventory(Object,B)), retract(weaponInventory(Object,Z)), print('You took the Ammo of '),
                print(Object), print(', now the current Ammo is'), print(B), !.
take(Object) :- inventoryCapacity(C), Inv(Z), Z=:=C, print('Your inventory is full'), !. 

take(Object) :- player(X,_,_,_,_), armorLoot(X, Object), InInv(Z), A is Z+1, miscInventory([Ar],[Med],[Am]), 
                konso(Object,[Ar],[Y]), asserta(miscInventory([Y],[Med],[Am])),  retract(armorLoot(X,Object)),
                retract([Ar],[Med]),[Am]), print('You took the '), print(Object),!.
take(Object) :- player(X,_,_,_,_), medLoot(X, Object), InInv(Z), A is Z+1, miscInventory([Ar],[Med],[Am]), 
                konso(Object,[Med],[Y]), asserta(miscInventory([Ar],[Y],[Am])), retract(medLoot(X,Object)),
                retract([Ar],[Med]),[Am]), print('You took the '), print(Object),!.
take(Object) :- player(X,_,_,_,_), ammoLoot(X, Object), InInv(Z), A is Z+1, miscInventory([Ar],[Med],[Am]), 
                konso(Object,[Am],[Y]), asserta(miscInventory([Ar],[Med],[Y])), retract(armorLoot(X,Object)),
                retract([Ar],[Med]),[Am]),  print('You took the '), print(Object), !.            
take(Object) :- print('Object not found.').

/* Irena */ 
attack :- player(U,V,W,X,Y), enemy(P,Q,R,S), U\==P, write('There is no enemy around you!'), !.
attack :- weaponInInv(P), P==0, write('You have no weapon!'), !.
attack :- player(U,V,W,X,Y), Y==0, write('Hey, you have no ammo!'), !.
attack :-
    player(U,_,_,X,Y), enemy(P,Q,_,_), U=:=P, /* bisa attack */
    weaponStat(X,A,B,C),
    randomProb(B), !, /* kalo gak miss */
    retract(weaponStat(X,A,B,C)), C is C-1, asserta(weaponStat(X,A,B,C)), /*ammo kita kurang satu */
    Z is Q-A, retract(enemy(P,Q,_,_,)), asserta(enemy(P,Z,R,S)), /* kurangin HP */
    Z == 0, deadenemy, write('You attacked the enemy and successfully killed him. The enemy dropped  some items.'), /* enemy nya mati */
    weaponLoot(U,R,S),!.

attack :-
    player(U,V,W,X,Y), enemy(P,Q,R,S), U=:=P, /* bisa attack */
    weaponStat(X,A,B,C),
    randomProb(B), !, /* kalo gak miss */
    retract(weaponStat(X,A,B,C)), C is C-1, asserta(weaponStat(X,A,B,C)), /*ammo kita kurang satu */
    Z is Q-A, retract(enemy(P,Q,R,S,)), asserta(enemy(P,Z,R,S)), /* kurangin HP */
    Z \= 0, write('You attacked the enemy and the enemy fought back with a '), write(R), write('.'),
    retract(enemy(P,Z,R,S)), S is S-1, asserta(enemy(P,Z,R,S)), /* kurangin Ammo enemy */
    weaponStat(R,E,_,G),
    W \=  0, W is W-E, retract(player(U,V,W,X,Y)), asserta(player(U,V,W,X,Y)), !. /* kurangin armor */
/* enemynya melawan balik, armor kita ada */

attack :- /* AMMO ENEMY AKHIRNYA ABIS */
    player(U,V,W,X,Y), enemy(P,Q,R,S), U=:=P, /* bisa attack */
    weaponStat(X,A,B,C),
    randomProb(B), !, /* kalo gak miss */
    retract(weaponStat(X,A,B,C)), C is C-1, asserta(weaponStat(X,A,B,C)), /*ammo kita kurang satu */
    Z is Q-A, retract(enemy(P,Q,R,S,)), asserta(enemy(P,Z,R,S)), /* kurangin HP */
    Z \= 0, write('You attacked the enemy and the enemy tried to fight back with a '), write(R), write('.'),
    retract(enemy(P,Z,R,S)), S is S-1, asserta(enemy(P,Z,R,S)), /* kurangin Ammo enemy */
    S =:= 0, write(' But he has got no ammo left. Attack him!'), !.
    /* enemynya melawan balik, tp ammonya abis */

attack :-
    player(U,V,W,X,Y), enemy(P,Q,R,S), U=:=P, /* bisa attack */
    weaponStat(X,A,B,C),
    randomProb(B), !, /* kalo gak miss */
    retract(weaponStat(X,A,B,C)), C is C-1, asserta(weaponStat(X,A,B,C)), /*ammo kita kurang satu */
    Z is Q-A, retract(enemy(P,Q,R,S,)), asserta(enemy(P,Z,R,S)), /* kurangin HP */
    Z \= 0, write('You attacked the enemy and the enemy fought back with a '), write(R), write('.'),
    retract(enemy(P,Z,R,S)), T is S-1, asserta(enemy(P,Z,R,T)), /* kurangin Ammo enemy */
    weaponStat(R,E,_,G),
    W ==  0, V is V-E, retract(player(U,V,W,X,Y)), asserta(player(U,V,W,X,Y)), !. /* health kita kurang */
    /* enemynya melawan balik */

attack :-
    player(U,_,_,X,Y), enemy(P,Q,_,_), U=:=P, /* bisa attack */
    weaponStat(X,A,B,C), randomProb(B), !, write('You tried to attack but you failed. Beware, the enemy will get mad!'), retract(weaponStat(X,A,B,C)), C is C-1, asserta(weaponStat(X,A,B,C)),!. /*ammo kita kurang satu */
        /* kalo miss gimana */

use (Object) :-
    weaponInventory(Object,Y),
    Y=:=0,
    write(Object), write(' is equipped. '), write(' But the weapon is empty!'),
    retract(weaponInventory(Object,Y)),
    player(A,B,C,D,E),
    retract(player(A,B,C,D,E)),
    D is Object,
    E is Y,
    asserta(player(A,B,C,D,E)), !.

use (Object) :-
    weaponInventory(Object,Y),
    Y\=0,
    write(Object), write(' is reloaded with '), write(Y), write(' ammo. Ready for soup dinner!'),
    retract(weaponInventory(Object,Y)),
    player(A,B,C,D,E),
    retract(player(A,B,C,D,E)),
    D is Object,
    E is Y,
    asserta(player(A,B,C,D,E)), !.

use(Object) :-
    miscInventory(A,B,C),
    searchLi(A,Object,X), X=:=1,
    armorStat(Object,A),
    modify_armor(A),
    write('You used the '), write(Object), write('.'),!. /* armor */

use(Object) :-
    miscInventory(A,B,C),
    searchLi(B,Object,X), X=:=1,
    medStat(Object, Y),
    modify_health(Y),
    write('You used the medicine. '), !.

use(Object) :-
    miscInventory(A,B,C),
    searchLi(C,Object,X), X=:=1,
    weaponInventory(Object,Y), retract(weaponInventory(Object,Y)),
    weaponStat(Object,_,_,Z),
    asserta(weaponInventory(Object,Z)),
    write('Increasing your ammo, ready to go!'), !.

use(Object) :- write('There is no such item in your inventory.'), !.

drop(Object) :-
    weaponInventory(Object,Y),
    retract(weaponInventory(Object,Y)),
    player(X,A,B,C,D),
    asserta(weaponLoot(X,Object,Y)), !. /* weapon */

drop(Object) :-
    miscInventory(A,B,C),
    searchLi(A,Object,X), X=:=1,
    player(P,Q,R,S,T),
    asserta(armorLoot(P,Object)),
    delLi(A,Object,D),
    retract(miscInventory(A,B,C)),
    asserta(miscInventory(D,B,C)),
    write('You drop the '), write(Object), write('.'), !. /*armor */

drop(Object) :-
    miscInventory(A,B,C),
    searchLi(B,Object,X), X =:=1,
    player(P,Q,R,S,T),
    asserta(medLoot(P,Object)),
    delLi(B,Object,D),
    retract(miscInventory(A,B,C)),
    asserta(miscInventory(D,B,C)),
    write('You drop the '),   write(Object), write('.'), !. /* MED */

drop(Object) :-
    miscInventory(A,B,C),
    searchLi(C,Object,X), X=:=1,
    player(P,Q,R,S,T),
    asserta(ammoLoot(P,Object)),
    delLi(C,Object,D),
    retract(miscInventory(A,B,C)),
    asserta(miscInventory(D,B,C)),
    write('You drop the '), write(Object), write('.'), !. /* ammo */

drop(Object) :- write('There is no such item in your inventory.'), !.

    /* modify health */

modify_health(X) :-
    player(A,B,C,D,E), B<100, retract(player(A,B,C,D,E)), X is X + B, asserta(player(A,X,C,D,E)), !.

modify_health(X) :-
    player(A,B,C,D,E), B =:= 100, write('You are already on your prime condition!'),!.

modify_health(X) :-
    player(A,B,C,D,E), B>100, retract(player(A,B,C,D,E)), X is 100, player(A,X,C,D,E),
    asserta(player(A,X,C,D,E)), !.

    /* modify armor */
modify_armor(X) :-
    player(_,_,C,_,_), C<100, retract(player(_,_,C,_,_)), X is X+C, asserta(player(_,_,X,_,_)), write(' Increasing your armor!'), !.

modify_armor(X) :-
    player(A,B,C,D,E), C=:=100, write('You are fully protected!'), !.

modify_armor(X) :-
     player(A,B,C,D,E), C>100, retract(player(A,B,C,D,E)), X is 100, asserta(player(A,B,X,D,E)), write('You are fully protected!'), !.

/* Search List */
searchLi([],_,0) :- !. /* Basis */
searchLi([A|_], C, 1) :- C == A, !. /* FOund */
searchLi([A|B], C, X) :- C \== A, searchLi(B, C, X).

/*Del X dari List */
delLi([], _, []) :- !. /* basis */
delLi([A|B], X, B):- X == A, !. /* found */
delLi([A|B], X, [A|C]) :- X \== A, delInv(B,X,C).

/* DEAD */
deadplayer :- player(A,B,C,D,E), B =:= 0, write('END GAME, YOU ARE DEAD! THE COUNTRY IS BEYOND SAVING!'), !.
deadenemy  :- enemy(A,B,C,D,E), B=:=0, retract(enemy(A,B,C,D,E)), write('One enemy down. Innalillahi wa inna ilayhi raajiun'), !.
