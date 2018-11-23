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


look :-



map :-



status :-





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

