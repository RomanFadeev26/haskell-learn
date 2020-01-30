type Name = String
type Attack = Int
type Health = Int
type Robot = (Name, Attack, Health)

robot :: Robot -> ((Robot -> b) -> b)
robot (robotName, robotAttack, hitpoints) = \ message -> message (robotName, robotAttack, hitpoints)

killerRobot = robot ("Kill3r", 25, 200)

name :: Robot -> Name
name (n, _, _) = n

attack :: Robot -> Attack
attack (_, a, _) = a

hp :: Robot -> Health
hp (_, _,h) = h

getName :: ((Robot -> Name) -> t) -> t
getName aRobot = aRobot name

getAttack :: ((Robot -> Attack) -> t) -> t
getAttack aRobot = aRobot attack

getHP :: ((Robot -> Health) -> t) -> t
getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\ (_, a, h) -> robot (newName, a, h))

setAttack aRobot newAttack = aRobot (\ (n, _, h) -> robot (n, newAttack, h))

setHP aRobot newHP = aRobot (\ (n, a, _) -> robot (n, a, newHP))

printRobot aRobot = aRobot (\ (n, a, h) -> n ++ " attack: " ++ (show a) ++ ", hitpoints: " ++ (show h))

damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, (h - attackDamage)))

fight aRobot defender = damage defender attackDamage
    where attackDamage = if getHP aRobot > 10
                            then getAttack aRobot
                            else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)

gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot

gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1

gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

robots = [gentleGiant, gentleGiantRound3, killerRobot]

threeRound a b = fight a (fight a (fight a b))


getWinner aRobot bRobot = if getHP aRobot > getHP bRobot
                then aRobot
                else bRobot

fourth = robot ("Mega Killer", 50, 300)