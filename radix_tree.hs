{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Radix_tree where

data Trie a = Node (Maybe a) [(String, Trie a)]
--    deriving (Show, Eq)

newtype Pars = Pars (Int, String, String, String)

--newtype Trie a = Node (String, Maybe a, [Trie a])
--    deriving (Show, Eq)



startNode :: Trie a
startNode = Node Nothing []


initTree :: (Eq a, Show a) => [(String, Maybe a)] -> Trie a -> Trie a
initTree [] t                         = t
initTree ((a,b):as) t                 = initTree as (addTree (a,b) t)


addTree :: (Eq a, Show a) => (String, Maybe a) -> Trie a -> Trie a
addTree (a,b) (Node Nothing [])                          = Node Nothing [(a,Node b [])]
addTree aa@(x:xs,_) (Node value ((s:ss, b):ys)) | x /= s = addTree aa (Node value (add'' aa ((s:ss, b):ys)))
                                                | x == s = Node value (add'  aa (s:ss, b):ys)                where

    add' :: (Eq a, Show a) => (String, Maybe a) -> (String, Trie a) -> (String, Trie a)
    add' ([], b) ([], Node v [])                                             = (s:ss, Node b [])
    add' (a, b) ([], Node v [])                                              = (s:ss, Node v [(a, Node b [])])
    add' ([], b) ([], Node v vs)                                             = (s:ss, Node b vs)
    add' (a, b) ([], nn)                                                     = (s:ss, addTree (a,b) nn)
    add' ([], val1) (hh, Node v vs)                                          = (x:xs, Node val1 [(hh, Node v vs)])
    add' (l:ls, val1) (p:ps, n@(Node v ys))                         | l == p = add' (ls, val1) (ps, n)
                                                                    | l /= p = (root (x:xs) (s:ss), Node Nothing [(l:ls, Node val1 []), (p:ps, n)])

    add'' :: (Eq a, Show a) => (String, Maybe a) -> [(String, Trie a)] -> [(String, Trie a)]
    add'' (g:gs,val1) []                     = [(g:gs, Node val1 [])]
    add'' (g:gs,val1) ((p:ps,n):ff) | p /= g = add'' (g:gs, val1) ff ++ [(p:ps,n)]
                                    | p == g = (p:ps,n) : ff


root :: String -> String -> String
root (x:xs) (y:ys)  | x == y = x : root xs ys
                    | x /= y = []


root' :: String -> String -> (String, String, String)
root' a@(x:xs) b@(y:ys) = (fst (root'' a b), snd (root'' a b), root a b)  where

    root'' (x:xs) (y:ys)    | x == y = root'' (x:xs) (y:ys)
                            | x /= y = (x:xs, y:ys)

thd (a,b,c) = c


find :: String -> Trie a -> Maybe a
find (x:xs) (Node v [])                          = Nothing
find (x:xs) (Node value ((s:ss, b):ys)) | x /= s = find  (x:xs) (Node value (find'' (x:xs) ((s:ss, b):ys)))
                                        | x == s = find' (x:xs) (s:ss, b)                               where
    
    find' :: String -> (String, Trie a) -> Maybe a
    find' [] ([], Node v _)                                              = v
    find' a  ([], Node v vs)                                             = find a (Node v vs)
    find' [] (p, _)                                                      = Nothing
    find' (l:ls) (p:ps, n@(Node v ys))                          | l == p = find' ls (ps, n)
                                                                | l /= p = Nothing

    find'' :: String -> [(String, Trie a)] -> [(String, Trie a)]
    find'' (g:gs) []                     = []
    find'' (g:gs) ((p:ps,n):ff)     | p /= g = find'' (g:gs) ff
                                    | p == g = [(p:ps,n)]


delete :: (Eq a, Show a) => String -> Trie a -> Trie a
delete a (Node Nothing [])                = Node Nothing []
delete (x:xs) (Node value ((s:ss, b):ys)) = Node value (delete'' (x:xs) ((s:ss, b):ys))              where


    delete'' :: (Eq a, Show a) => String -> [(String, Trie a)] -> [(String, Trie a)]
    delete'' (g:gs) []                      = []
    delete'' (g:gs) ((p:ps,n):ff)  | p /= g = (delete'' (g:gs) ff) ++ [(p:ps,n)]
                                   | p == g = (delete'  (g:gs) (p:ps,n)) ++ ff                                       where
        
        delete' :: (Eq a, Show a) => String -> (String, Trie a) -> [(String, Trie a)]
        delete' [] ([], Node v [])                                                  = []
        delete' [] ([], Node v vs)                                                  = [(p:ps, Node Nothing vs)]
        delete' a ([], Node v ((q:qs, w):vs))                                       = [(p:ps, Node v (delete'' a ((q:qs, w):vs)) )]
        delete' (l:ls) (h:hs, v)                                           | l == h = delete' ls (hs, v)
                                                                           | l /= h = [(p:ps, v)]



svertka :: (Eq a, Show a) => [(String, Maybe a)] -> Trie a
svertka = foldr addTree startNode 


lsvertka :: (Eq a, Show a) => [(String, Maybe a)] -> Trie a
lsvertka = foldl (flip addTree) startNode 


instance Show a => Show (Trie a) where
--    show :: Trie a -> String
    show x = formatin ("", x) (max_depth ("", x)) 1 where



        formatin :: Show a => (String, Trie a) -> Int -> Int -> String
        formatin b@(x, Node v vs) i n = (formatin'' b) ++ "\n" ++ formatin' vs i n where

            formatin' :: Show a => [(String, Trie a)] -> Int -> Int -> String
            formatin' [] _ _ = []
            formatin' y q e  = formatin''' y ++ "\n" ++ formatin' (countlvl' y q (e+1)) q (e+1)

            formatin'' :: Show a => (String, Trie a) -> String
            formatin'' a@(aa, Node (Just v) _) = let x = div ((lengthTree a) - 4 - length (show v)) 2 in (replicate x ' ') ++ aa ++ " " ++ show v ++ "   " ++ (replicate x ' ')
            formatin'' a@(aa, Node v _)        = let x = div ((lengthTree a) - 4 - length (show v)) 2 in (replicate x ' ') ++ aa ++ " " ++ show v ++ "   " ++ (replicate x ' ')

            formatin''' :: Show a => [(String, Trie a)] -> String
            formatin''' []     = []
            formatin''' (y:ys) = formatin'' y ++ formatin''' ys

            countlvl' :: Show a => [(String, Trie a)] -> Int -> Int -> [(String, Trie a)]
            countlvl' []                  _ _     = []
            countlvl' ((b, Node v []):ys) q e     | e <= q = (b, Node v []) : countlvl' ys q e
            countlvl' ((b, Node _ y):ys)  q e     = y ++ countlvl' ys q e


        max_depth :: Show a => (String, Trie a) -> Int
        max_depth (a, Node _ []) = 0
        max_depth (a, Node _ vs) = find_depth vs 0 where
                
            find_depth [] n = n
            find_depth vs n = find_depth (cnt vs) (n+1) 

            cnt []                 = []
            cnt ((b, Node _ y):ys) = y ++ cnt ys



        lengthTree :: Show a => (String, Trie a) -> Int
        lengthTree (a, Node _ []) = 0
        lengthTree (a, Node _ vs) = maximum $ map length (pluslvl vs) where
                
            pluslvl :: Show a => [(String, Trie a)] -> [String]
            pluslvl []                         = []
            pluslvl y                          = length' y : pluslvl (countlvl y)
                
            length' :: Show a => [(String, Trie a)] -> String
            length' []                         = []
            length' ((b, Node (Just v)  y):ys) = b ++ " " ++ show v ++ "   "  ++ length' ys
            length' ((b, Node Nothing y):ys)   = b ++ " " ++ "Nothing" ++ "   " ++ length' ys

            countlvl []                        = []
            countlvl ((b, Node _ []):ys)       = [] ++ countlvl ys
            countlvl ((b, Node _ y):ys)        = y  ++ countlvl ys








mainList = [("abc", Just 1), ("bac", Just 2), ("alfa", Just 3), ("omega", Just 4), ("abra", Just 5), ("abrakadabra", Just 6), ("pi", Just 3.14), ("piligr", Just 7), ("pilot", Just 8),
    ("antares", Just 8), ("antigua", Just 8), ("andar", Just 8), ("andalusia", Just 8)]

--find "abra" (initTree [("abc", Just 1), ("bac", Just 2), ("alfa", Just 3), ("omega", Just 4), ("abra", Just 5), ("abrakadabra", Just 6), ("pi", Just 3.14), ("pizdec", Just 7), ("pidar", Just 8)] startNode)
--wakeupneo :: (Eq a, Show a) => Trie a -> Int
--wakeupneo (Node v (y:t1:t2:t3:ys)) = lengthTree t1

main = do
    putStr $ show (initTree mainList startNode)
    putStr "\n" 
    putStr ("                                            !!!!!!!!!!!!!!!!!!!!                                                                  " ++ "\n")
    putStr $ show $ addTree ("antiguagua", Just 5) (initTree mainList startNode)
    putStr "\n"
    putStr ("                                            !!!!!!!!!!!!!!!!!!!!                                                                  " ++ "\n")
    putStr $ show $ find "alfa" (initTree mainList startNode)
    putStr "\n"
    putStr ("                                            !!!!!!!!!!!!!!!!!!!!                                                                  " ++ "\n")
    putStr $ show $ delete "alfa" (addTree ("antiguagua", Just 5) (initTree mainList startNode))
    putStr "\n"
    putStr ("                                            !!!!!!!!!!!!!!!!!!!!                                                                  " ++ "\n")
    putStr $ show $ find "alfa" (delete "alfa" (addTree ("antiguagua", Just 5) (initTree mainList startNode)))
    putStr "\n"
    putStr "4erez svertku \n"
    putStr $ show $ svertka mainList
    putStr "\n"
    putStr "4erez foldl \n"
    putStr $ show $ lsvertka mainList
    putStr "\n\n Est problema s otstupami, no eto 4isto arefmeti4eskaya problema, proect delal 2 mesyaca nazad, t9gelo isprvit problemu za ve4er(("