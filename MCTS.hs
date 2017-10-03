{-# LANGUAGE MultiParamTypeClasses #-}

module MCTS where

import GameState

import Prelude hiding (traverse)
import System.Random
import Data.Ord
import Data.List
import Data.Maybe

{-
    *** TODO ***

    Implementați tipul `Tree s a`, al arborilor de căutare, unde `s` reprezintă
    tipul stărilor, iar `a`, tipul acțiunilor.

    Sunt necesare câmpuri, precum:
    * starea curentă
    * acțiunea prin care s-a ajuns la stare
    * numărul de vizitări
    * scorul
    * copiii.
-}

data Tree s a = Node s a Int Float [Tree s a]


{-
    *** TODO ***

    Implementați tipul `Zipper s a`, pentru parcurgerea arborilor de căutare
    de tipul `Tree s a`, unde `s` reprezintă tipul stărilor, iar `a`, tipul
    acțiunilor.

    Pe lângă componentele specifice unui zipper (vezi tutorialul din enunț),
    se va reține și un generator de numere aleatoare, modificat pe parcursul
    explorării arborelui.
-}
data Breadcrumb s a = Breadcrumb s a Int Float [Tree s a] [Tree s a] deriving Show
data Zipper s a = MyZipper (Tree s a, [Breadcrumb s a], StdGen) deriving Show

{-
    *** TODO ***

    Instanțiați clasa `Show` cu tipul `Tree s a`.
-}

show_help [] space  = ")"
show_help ((Node state act scr nrv children):rem) space = "\n" ++ space ++ ">Node(" ++ show state ++ " " ++ show act ++ " "
                                                        ++ show scr ++ " " ++ show nrv 
                                                        ++ show_help children (space ++ "--") ++ show_help rem space

instance (Show s, Show a) => Show (Tree s a) where
    
    show (Node state act scr nrv []) = ">Node(" ++ show state ++ " " ++ show act ++ " " ++ show scr ++ " "
                                        ++ show nrv ++ " " ++ ")\n"
    show (Node state act scr nrv children) = "rNode(" ++ show state ++ " " ++ show act ++ " " ++ show scr ++ " "
                                                ++ show nrv ++ show_help children "--" ++ "\n"                                        



{-
    ****************
    Funcții de acces
    ****************
-}

{-
    *** TODO ***

    Întoarce starea asociată unui nod.
-}
treeState :: Tree s a -> s
treeState (Node s _ _ _ _) = s

{-
    *** TODO ***

    Întoarce starea asociată unui nod.
-}
treeAction :: Tree s a -> a
treeAction (Node _ a _ _ _) = a


{-
    *** TODO ***

    Întoarce scorul unui nod.
-}
treeScore :: Tree s a -> Float
treeScore (Node _ _ _ scr _ ) = scr

{-
    *** TODO ***

    Întoarce numărul de vizitări ale unui nod.
-}
treeVisits :: Tree s a -> Int
treeVisits (Node _ _ nrv _ _) = nrv
{-
    *** TODO ***

    Întoarce copiii unui nod.
-}
treeChildren :: Tree s a -> [Tree s a]
treeChildren (Node _ _ _ _ children) = children

{-
    *** TODO ***

    Întoarce nodul pe care este centrat zipper-ul.
-}
zipperTree :: Zipper s a -> Tree s a
zipperTree (MyZipper (root, _, _)) = root

{-
    *** TODO ***

    Întoarce generatorul de numere aleatoare din interiorul zipper-ului.
-}
zipperGen :: Zipper s a -> StdGen
zipperGen (MyZipper (_, _, rand)) = rand

{-
    *****************
    Funcții pe arbori
    *****************
-}

{-
    *** TODO ***

    Construiește un arbore de căutare (eventual infinit), pornind de la funcția
    de generare a succesoarelor unei stări și de la starea inițială.
-}

expand_help :: (s -> [(a, s)])
            -> [(a,s)]
            -> [Tree s a]
expand_help f listofpairs = map (\p -> Node (snd p) (fst p) 0 0 (expand_help f (f (snd p)))) listofpairs

expand :: (s -> [(a, s)])  -- Generatorul stărilor succesoare
       -> s                -- Starea inițială
       -> Tree s a         -- Arborele de căutare
expand f initState = Node initState act 0 0 children
                    where children = (expand_help f (f initState))
                          (Node _ act _ _ _) = children!!0 

{-
    *** TODO ***

    Explorează arborele, alegând la fiecare pas un succesor aleator,
    până la atingerea unei stări terminale (victorie/ remiză).

    Întoarce:
    * calea urmată, în ordine inversă, astfel că primul element din cale
      este nodul terminal
    * semnificația stării terminale (victorie/ remiză)
    * varianta finală a generatorului de numere aleatoare.
-}
rolloutTree_help :: GameState s a => Tree s a -> StdGen -> [Tree s a] -> ([Tree s a], Outcome, StdGen)
rolloutTree_help y@(Node state act nrv scr children) rand res 
    | (outcome state) == Ongoing      = rolloutTree_help (children!!((fromIntegral(fst $ next rand)) `mod` (length children))) (snd $ next rand) (y:res)
    | otherwise                       = (y:res, (outcome state), rand)


rolloutTree :: GameState s a => Tree s a -> StdGen -> ([Tree s a], Outcome, StdGen)
rolloutTree y@(Node state act nrv scr children) rand = rolloutTree_help y rand []

{-
    *** TODO ***

    Determină cel mai bun copil al unui nod, din perspectiva raportului
    scor / număr de vizitări.

    Hint: `maximumBy` și `comparing`.
-}
bestChild_help :: Tree s a -> [(Int, Float)]
bestChild_help (Node _ _ _ _ children) = zip [0..] (map (\y@(Node _ _ nrv scr _) -> scr/(fromIntegral nrv)) children)


bestChild :: Tree s a -> Tree s a
bestChild y@(Node _ _ _ _ children) = children!!(fst $ maximumBy (comparing snd) $ bestChild_help y)

{-
    *******************
    Funcții de zipper-e
    *******************
-}

{-
    *** TODO ***

    Construiește un zipper centrat pe arborele dat, care stochează generatorul
    de numere aleatoare precizat.
-}
getZipper :: Tree s a -> StdGen -> Zipper s a
getZipper y@(Node _ _ _ _ _) rand = MyZipper(y, [], rand)

{-
    *** TODO ***

    Verifică dacă zipper-ul este centrat pe rădăcina arborelui.
-}
isRoot :: Zipper s a -> Bool
isRoot (MyZipper (_, bread, _))
    | (length bread) == 0    = True
    | otherwise          = False

{-
    *** TODO ***

    Valoarea ucb1 din filmuleț (constanta C = 2).
-}
ucb1 :: Float  -- scorul copilului
     -> Int    -- numărul de vizitări ale copilului
     -> Int    -- numărul de vizitări ale părintelui
     -> Float  -- estimarea

maxnr :: Float
maxnr = 9999999

ucb1 scr vis visp
    | (vis == 0) == True  = maxnr
    | otherwise =  (scr / (fromIntegral vis)) + 2 *sqrt((log (fromIntegral visp))/(fromIntegral vis))
{-
    *** TODO ***

    Pentru nodul pe care este centrat zipper-ul dat ca parametru, selectează
    copilul având valoarea ucb1 maximă. Întoarce zipper-ul centrat pe copilul
    respectiv.

    Atenție! Așa cum rezultă și din filmuleț, un nod nevizitat are valoarea ucb1
    infinită, și va fi întotdeauna ales în defavoarea altor noduri deja vizitate.
-}



select :: Eq s => Zipper s a -> Zipper s a 
select (MyZipper (y@(Node s a nrvp scrp children),crumbs,rand))
    | (snd $ head listpair) == maxnr      = MyZipper ((children!!(fst $ head listpair)), (bread_tmp:crumbs), rand)
    | otherwise                         = MyZipper (child, (bread_index:crumbs), rand)
    where child = (children!!index)
          listpair = (zip [0..] (map (\x@(Node _ _ nrv scr _) -> ucb1 scr nrv nrvp) children))
          index = (fst $ maximumBy (comparing snd) (zip [0..] (map (\x@(Node _ _ nrv scr _) -> ucb1 scr nrv nrvp) children)))
          bread_tmp = Breadcrumb s a nrvp scrp (take (fst $ head listpair) children) (drop ((fst $ head listpair)+1) children)
          bread_index = Breadcrumb s a nrvp scrp (take index children) (drop (index+1) children)
{-  
    *** TODO ***

    Aplică repetat `select` până la atingerea unui nod nevizitat sau terminal.
-}
traverse :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
traverse z@(MyZipper (y@(Node s _ nrv _ _), bread, rand))
    |   nrv == 0                = MyZipper(y, bread, rand)
    |   outcome s == Ongoing    = traverse $ select z
    |   otherwise               = MyZipper(y, bread, rand)

{-
    *** TODO ***

    Aplică `rolloutTree` pentru arborele pe care este centrat zipper-ul.

    Întoarce:
    * scorul cu care vor fi actualizate nodurile de pe calea către rădăcină
    * numărul jucătorului pentru care se realizează actualizarea
      (se poate ignora pentru cerința cu un singur jucător)
    * noul zipper, actualizat cu generatorul de numere aleatoare întors
      de `rolloutTree`.

    Pentru cerința cu cel puțin doi jucători, scorul și numărul jucătorului
    se calculează astfel:
    * Pentru victorie, se utilizează scorul din obictul `Outcome` și numărul
      jucătorului aferent stării terminale.
    * Pentru remiză, se utilizează scorul din obiectul `Outcome` împărțit
      la numărul de jucători, și `Nothing`.
-}
extractOutcome :: Outcome -> Float
extractOutcome (Win x) = x
extractOutcome (Draw x) = x

rolloutZipper :: GameState s a => Zipper s a -> (Float, Maybe Int, Zipper s a)
rolloutZipper (MyZipper (tree, bread, rand))
            | scr == (Win $ extractOutcome scr)     = ((extractOutcome scr), (Just (playerIndex s)), (MyZipper (tree, bread, rand1)))
            | otherwise                             = (((extractOutcome scr) / (fromIntegral nrplayers)), (Nothing), (MyZipper (tree, bread, rand1)))                  
            where (lst, scr, rand1) = rolloutTree tree rand
                  n@(Node s _ _ _ _) = head lst
                  nrplayers = maxPlayers s 
                                         

{-
    *** TODO ***

    Urcă un nivel în arbore.
-}
toParent :: Zipper s a -> Zipper s a
toParent (MyZipper (tree, bread, rand)) = MyZipper(parent, (tail bread), rand)
                                    where   (Breadcrumb s a nrvp scrp l r) = head bread
                                            parent = (Node s a nrvp scrp (l ++ [tree] ++ r))

{-
    *** TODO ***

    Implementează pasul de backpropagation, unde cei trei parametri sunt cele
    trei componente întoarse de `rolloutZipper`.

    Astfel, se urmează calea către rădăcină și se crește cu 1 numărul
    de vizitări ale tuturor nodurilor. În plus, scorul se modifică astfel:
    * Pentru cerința cu un singur jucător, se modifică toate nodurile.
    * Pentru cerința cu mai mulți jucători, avem următoarele cazuri:
      * În caz de victorie, se actualizează doar nodurile cu numărul de jucător
        dat de parametru.
      * În caz de remiză, se actualizează toate nodurile.
    
    Zipper-ul final este centrat pe rădăcină.
-}
backProp :: GameState s a => Float -> Maybe Int -> Zipper s a -> Zipper s a
backProp newscr player z@(MyZipper ((Node s a vis scr children), bread, rand))
    | ((length bread == 0)   && (isNothing player))                             = newzip
    | ((length bread == 0)   && (isJust player) && ((playerIndex s) == (fromJust player)))  = newzip
    | ((length bread == 0)   && (isJust player)) && ((playerIndex s) /= (fromJust player))  = onlyzip 
    | (isNothing player)                                                                    = backProp newscr player $ toParent newzip
    | ((isJust player) && ((playerIndex s) == (fromJust player)))                           = backProp newscr player $ toParent newzip
    | otherwise                                                                             = backProp newscr player $ toParent onlyzip
    where newzip = MyZipper((Node s a (vis+1) (scr+newscr) children), bread, rand)
          onlyzip = MyZipper((Node s a (vis+1) scr children), bread, rand)

{-
    *** TODO ***

    Realizează o iterație completă a MCTS, incluzând toate etapele, pornind
    de la un nod oarecare din arbore și finalizând pe rădăcină.
-}
exploreOne :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
exploreOne z@(MyZipper(n@(Node s a nrv scr children), bread, rand)) = backProp scr player zip
                                                                    where (scr, player, zip) = rolloutZipper $ traverse z

{-
    *** TODO ***

    Realizează un număr dat de iterații complete ale MCTS.
-}
exploreMany :: (Eq s, GameState s a) => Int -> Zipper s a -> Zipper s a
exploreMany times z = (iterate (exploreOne) z) !! times 

{-
    *** TODO ***

    Alege o acțiune pornind de la o stare dată și un număr de iterații ale MCTS.
    Întoarce o pereche cu acțiunea și starea următoare.

    Funcția ar trebui să verifice mai întâi dacă nu cumva una dintre stările
    imediat următoare reprezintă o victorie, caz în care o alege direct.
    Altfel, demarează procesul standard de explorare.

    Atenție! La prima iterație a algoritmului, cu toate că numărul de vizitări
    ale rădăcinii este 0, NU se face rollout la rădăcină, ci se trece direct
    la explorarea copiilor acesteia. Acest lucru este vizibil și în filmuleț.

    După realizarea numărului dat de iterații, se alege efectiv acțiunea,
    utilizând `bestChild`.
-}
choose :: (Eq s, GameState s a) => Int -> s -> StdGen -> (a, s)
choose times s rand = (action, state)
        where (Node state action _ _ _) = bestChild $ zipperTree $ exploreMany times (getZipper (expand successors s) rand) 
