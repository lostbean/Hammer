-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  Private
--
-- Maintainer  :  Edgar Gomes de Araujo
-- Stability   :  No
-- Portability :
--
-- | Module to find conected componets in a graph.
-- Used to group neighbor point to form a grain
-----------------------------------------------------------------------------

module Hammer.Math.GroupGraph
(
-- Function to find conected componets in a graph
-- ref:
groupPoints
)where


import Data.List
import Data.Array
import qualified Data.Map as M

{-import Debug.Trace
debug :: Show a => String -> a -> a
debug s x = trace (s ++ show x) x   -}


-- Insert the value v at the index i on the given list of values; (i, v) -> [a1,a2,...]
writeInto::[(Int,Int)]-> [Int]-> [Int]
writeInto [] as = as
writeInto _ [] = []
writeInto ps as =
    let
        -- Zip the indew position and put into a Map (balanced trees)
        -- Maps have a good time complexcity O(log n) to update
        fm  = M.fromList $ zip [1..(length as)] as
        fm' = foldr (\(k,v) acc-> M.update (\x-> Just v) k acc) fm ps
    in map snd $ M.toList fm'


-- Read the values from the [v] at index i for each i in the list [i]  and return a [v@i]
readAt::[Int]-> [Int]-> [Int]
readAt [] _ = []
readAt _ [] = []
readAt as is =
    let
        -- Insert the values into an Array due the lower the complecity O(log n) to update
        las = length as
        arr = listArray (1, las) as
    in map (arr!) is


-- Return a list [0, (n+1) if True else n, (n+1) if True else n,....]
enumerate::[Bool]-> [Int]
enumerate = scanl (+) 1  .  map (\b -> if b then 1 else 0) -- Solved by changing scanr -> scanl


-- Retrun a list of the index where a True value match
pack_index::[Bool]-> [Int]
pack_index bs = map fst . filter snd $ zip [1..(length bs)] bs


-- convert all trees to star by reading the children and add them to the parents
shortcut_all::[Int]-> [Int]
shortcut_all p =
	let pp = readAt p p
	in if p==pp then pp else shortcut_all pp


-- takes a list of stars and regrounp in supernodes
compress_graph::[Int]-> [(Int, Int)]-> ([(Int, Int)], [Int])
compress_graph p es =
	let		-- update edges to point to new parent
    		(e1, e2) = unzip es
    		e' = zip (readAt p e1) (readAt p e2)

    		-- remove self edges and flip edges up
    		e'' = map (\(i, j) -> if i > j then (j,i) else (i,j)) $  filter (\(i, j) -> i /= j) e'

    		-- identify roots, relabel roots, and relabel edges
    		roots = zipWith (==) p [1..length p]
    		labels = enumerate roots
    		(e1', e2') = unzip e''
    		e''' =   zip (readAt labels e1') (readAt labels e2')
    		
	in (e''' ,pack_index roots)


-- Takes a list with valid edges and the number of nodes; return
-- ([where index -> nodeID; value apoint to root of the star]
-- , [list of nodeID of main roots and also sub roots])
hybrid_connected_components::[(Int, Int)]-> Int-> ([Int],[Int])
hybrid_connected_components es n
	| length es == 0 = ([1..n],[1..n])
	| otherwise =
		let 	stars = shortcut_all (writeInto es [1..n])
      			(es', roots) = compress_graph stars es
	      		r = hybrid_connected_components es'(length roots)
      			ins = writeInto (zip roots $ readAt roots (fst r)) stars
  		in (readAt ins ins, roots)


-- Make a list of boarder tag (0=inside, 1=board) for each node from unmatch edges (false edges)
boardNodes::[(Int, Int)]-> Int-> [Int]
boardNodes xs n = writeInto (splitPair xs) (map (\x-> 0) [1..n]) -- LOOK for a better snd argument
    where splitPair ys = case ys of
            [] -> []
            (a,b):xs -> (a,1):(b,1):splitPair xs


-- The main function of this module
-- Group the nodes from (1 to n) that are conected by matched edges (true edges)
-- Return list of list wich contains pair (nodeID, boarder tag)
-- The boarded tags are determined using a list of unmatched edges (false edges)
groupPoints::([(Int,Int)],[(Int,Int)])-> Int-> [[(Int,Int)]]
groupPoints (trueEdges, falseEdges) n =
    let
            (groupedStar, roots) = hybrid_connected_components trueEdges n
            -- Filter roots to get only the final roots (true roots), the sub roots
            validRoots = map fst $ filter (\(i,j) -> i==j) $ zip (readAt groupedStar roots) roots
            -- Make (NodeID, Root of node, Tag 0=inside, 1=board)
            pIDRootTag = zip3 [1..n] groupedStar (boardNodes falseEdges n)
            -- Regroup in pairs (ID, Tag) by root, i.e. list of node and board tag for each subregion
            pIDTagbyRoot = map (\x -> takeSameRoot x pIDRootTag) validRoots
                where takeSameRoot a b = map (\(x,y,z)-> (x,z)) $ filter (\(x,y,z) -> y == a) b
    in pIDTagbyRoot



-- Nodes from 1 to n
main = do
	let edges = [(1, 5), (5, 6), (1, 6), (6, 10), (5, 4), (4, 13), (13, 2), (11, 2), (2, 9), (9, 12), (8, 9), (3, 7),(15,3),(3,14)]
	
	
	putStrLn $ "---result: " ++ (show $ result edges 15)
	putStrLn $ "---result: " ++ (show $ result1 edges 15)
	    where   result es n = hybrid_connected_components es n
	            result1 es n = groupPoints (es,[(7,4),(14,1)]) n
