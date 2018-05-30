import Text.Read as TR
import System.Directory
import System.Environment
import Data.Maybe
import Matrix
import Data.List
import Data.Either
import Debug.Trace
import Control.Monad
import qualified Data.Map.Strict as DM
--import Numeric.LinearAlgebra as NLA
       
data Node = Node String (Maybe [Double]) deriving Show
data Edge = Edge String String (Maybe Double) deriving (Show,Ord)

data PScript =Line (Double,Double) (Double,Double) |  Point (Double,Double) | Label String (Double,Double)  deriving Show


instance Eq Edge where
      (Edge a b l) == (Edge x y ll) = (a == x && b ==y) || (a == y && b ==x)
      p /= q = not $ p == q

bbox psitems = let pure_coords' (Line (a,b) (c,d)) = [(a,b),(c,d)]
                   pure_coords' (Point (a,b)) = [(a,b)]
                   pure_coords' (Label _ (a,b)) = [(a,b)]
                   pure_coords = concat $ map pure_coords' psitems
                   corner (l:ls) fun  = foldl (\(p1,p2) (m1,m2)-> (fun p1 m1,fun p2 m2)) l ls

                   ll = corner pure_coords min 
                   ur = corner pure_coords max
                                                        
               in (ll,ur)


fapply (funx,funy) (Line (a,b) (c,d)) = Line (funx a,funy b) (funx c,funy d)
fapply (funx,funy) (Point (a,b))      = Point (funx a,funy b)
fapply (funx,funy) (Label s (a,b))    = Label s (funx a,funy b)


without (nodes,edges) edge =
      (nodes, filter (/=edge) edges) 

exclude g (Right path) = foldl (\s x -> s `without` x) g path
exclude g _ = g 

      
far_end (Edge a b _) n = if a == n then b else a

-- throw away duplicated and self paths
purify (nodes,edges) =
      let edges' = unique edges
          edges'' = filter (\(Edge a b x) -> a/=b) edges'
      in (nodes,edges'')
                                        

resize eps@(ps:items) =
    let ((a,b),(c,d)) =  bbox eps
        width=400
        height=400
        funx = \x -> 100+width*(x-a)/(c-a)
        funy = \y -> 100+height*(y-b)/(d-b)
    in map (fapply (funx,funy)) eps


psplot' (Line (a,b) (c,d)) =
    let m=(show a)++" "++(show b)++" moveto"
        s=(show (c))++" "++(show (d))++" lineto"
    in "newpath\n "++m ++ "\n" ++ s ++ "\nstroke\n"


psplot' (Point (a,b)) =
    let s=(show a)++" "++(show b)++" 3 0 360 arc"
    in "newpath\n "++ s ++ "\nfill\n"


psplot' (Label lab (a,b)) =
    let m=(show (5+a))++" "++(show (5+b))++" moveto"
        s="("++lab++") show"
    in "newpath\n "++m ++ "\n" ++ s ++ "\n"

psplot (e:ps) =
       let intro="%!PS-Adobe-3.0 EPSF-3.0\n/Times-Roman findfont\n12 scalefont\nsetfont\n"
       in intro ++  foldl (\s p -> s ++ (psplot' p)) (psplot' e)  ps
       
graph2dot (nodes,edges) =
      let intro ="digraph G {"
          entries = map (\(Edge f t _) -> f ++ "->" ++ t ++";") edges
      in (foldl (\s x -> s ++"\n"++x) intro entries) ++ "\n}\n"


graph2ps (nodes',edges') =
       let edges = filter (\(Edge _ l _) -> l /="tmp") edges'
           nodes = filter (\(Node l _) -> l /="tmp") nodes' 
           get_coords label = filter (\(Node n c) -> n==label) nodes
           lines' = map (\(Edge a b _)-> (get_coords a,get_coords b)) edges
           lines ((Node _ (Just (x1:y1:_))):_,(Node _ (Just (x2:y2:_))):_) = Just $ Line (x1,y1) (x2,y2)
           lines  _ = Nothing
           points (Node _ (Just (a:b:_))) = Just $ Point (a,b)
           points _ = Nothing
           labels (Node x (Just (a:b:_))) = Just $ Label x (a,b)
           labels _ = Nothing
       in map fromJust $ filter isJust $ (map lines lines')++(map points nodes)++(map labels nodes)
          
unique (x:xs) = x:(unique (filter (/=x) xs))
unique _ = []


           

parse_graph edges (Just nodes) =
     let eds = filter (\w -> length w > 1) $  map words $ lines edges
     in (nodes,map mkedge eds)
     where
       mkedge (n1:n2:w:[]) = Edge n1 n2 (TR.readMaybe w)
       mkedge (n1:n2:_)    = Edge n1 n2 Nothing

parse_graph edges Nothing = 
    let lns = map words $ lines edges
        nds = map (take 2) lns
        nodes' = unique $ concat nds
        nodes = map (\n -> Node n Nothing) nodes'
    in parse_graph edges (Just nodes)
        
parse_nodes nodes =
    let nds =  filter (\w -> length w > 0) $ map words $ lines nodes
    in map mknode nds
    where
      mknode (a:b:c:_) = Node a (coords (TR.readMaybe b) (TR.readMaybe c))
      mknode (a:_)      = Node a Nothing 
      coords (Just a) (Just b) = Just [a,b]
      coords _ _ = Nothing
                

                    
read_graph file = do
  let basename = takeWhile (/='.') file
      nfile    = basename ++ ".n"
      efile    = basename ++ ".e"
  isN <- doesFileExist nfile
  eF  <- readFile efile
                
  if (isN) then do
      nF  <- readFile nfile
      let nodes = parse_nodes nF
      return $ parse_graph eF (Just nodes)
  else
      return $ parse_graph eF Nothing


toMatrix' (nodes,edges) =
    let tnodes = zip [1..] (map (\(Node a _ ) -> a) nodes)
        mapN n = fst $ head $ filter (\(a,b)->b==n) tnodes
        matentries' =  map (\(Edge n1 n2 p) -> case p of
                  Nothing -> ((mapN n1),(mapN n2),1.0)
                  Just x  -> ((mapN n1),(mapN n2),x  )) edges
        matentries''' = map (\e@(n,m,v) -> if (n<m) then e else (m,n,v)) matentries'
        matentries'' = filter (\(n,m,v) -> n/=m) $ unique matentries'''

        matentries = sortBy (\(n1,m1,_) (n2,m2,_) -> if (n1 `compare` n2) /= EQ then (n1 `compare` n2) else  (m1 `compare` m2)) matentries''

        nn = (length nodes)
        rows = map (\r -> filter (\(n,m,v) -> n==r) matentries) [1..nn]

        mat' =  map (create_row nn) rows

        mat  = mat' -- `madd` (t mat')
        some_noise = take nn $ map (\x-> fromIntegral $ mod (x*x) 17) [1..]
        on_diag' = sumrows mat

        on_diag = zipWith (\a b -> a+0.0*b) on_diag' some_noise
        dg = diag nn (map (*(-1)) on_diag)
--    in scalar_mul (mat `madd` dg) (-1)
    in head mat'



toMatrix (nodes,edges) =
    let tnodes = zip [1..] (map (\(Node a _ ) -> a) nodes)
        mapN n = fst $ head $ filter (\(a,b)->b==n) tnodes
        matentries' =  map (\(Edge n1 n2 p) -> case p of
                  Nothing -> ((mapN n1),(mapN n2),1.0)
                  Just x  -> ((mapN n1),(mapN n2),x  )) edges
        matentries''' = map (\e@(n,m,v) -> if (n<m) then e else (m,n,v)) matentries'
        matentries'' = filter (\(n,m,v) -> n/=m) $ unique matentries'''
        matentries = sortBy (\(n1,m1,_) (n2,m2,_) -> if (n1 `compare` n2) /= EQ then (n1 `compare` n2) else  (m1 `compare` m2)) matentries''

        nn = (length nodes)
        rows = map (\r -> filter (\(n,m,v) -> n==r) matentries) [1..nn]

        mat' =  map (create_row nn) rows

        mat  = mat' `madd` (t mat')
        some_noise = take nn $ map (\x-> fromIntegral $ mod (x*x) 17) [1..]
        on_diag' = sumrows mat

        on_diag = zipWith (\a b -> a+0.0*b) on_diag' some_noise
        dg = diag nn (map (*(-1)) on_diag)
    in scalar_mul (mat `madd` dg) (-1)
--    in scalar_mul (mat) (-1)

add_centre (nodes,edges) =
      let nnodes = (Node "tmp" Nothing):nodes
          nedges' = map (\(Node l _) -> Edge l "tmp" (Just 0.707)) nodes
      in (nnodes,nedges'++edges)

--embed _ _ | trace "EMBED" False = undefined   
embed corr (nodes,edges) =
      let matrix = toMatrix (nodes,edges)
          (v',d) = qr_iter (2*(toInteger $ length $ nodes)) (eye (length nodes),matrix)
          v = tail $ reverse $ t v'
          nn = length nodes
          nn' = fromIntegral nn
          sn = map (\t -> (sin t*3.14/(nn'-1))) $ map fromIntegral [0..nn-1]
          cs = map (\t -> (cos t*3.14/(nn'-1))) $ map fromIntegral [0..nn-1]
          (xx':yy':zz':vv':_) = v
--          xx = zipWith (\a b -> a+b/(sqrt nn')*corr) xx' sn
--          xx=xx'
--          yy = zipWith (\a b -> a+b/(sqrt nn')*corr) (zipWith (\a b -> a*(sin corr)+) yy' zz') cs
          yy = zipWith (\a b -> a*(cos corr)+b*(sin corr)) yy' zz'
          xx = zipWith (\a b -> a*(cos corr)+b*(sin corr)) xx' vv'
          new_nodes = zipWith3 (\(Node l _) x y -> Node l (Just [x,y])) nodes xx yy
      in (new_nodes,edges)

--embed3 _ (n,e) | trace ("EMBED3 "++show (length e)) False = undefined   
embed3 corr (nodes,edges) =
      let matrix' = toMatrix (nodes,edges)
--          matrix = matrix' `madd` (scalar_mul (eye $ length matrix') 4)
          (v',d) = qr_iter  (2*(toInteger $ length $ nodes)) (eye (length nodes),matrix')
          v = tail $ reverse $ t v'
          nn = length nodes
          nn' = fromIntegral nn
          sn = map (\t -> (sin t*3.14/(nn'-1))) $ map fromIntegral [0..nn-1]
          cs = map (\t -> (cos t*3.14/(nn'-1))) $ map fromIntegral [0..nn-1]
          (xx':yy':zz':vv':_) = v

          min_x = foldl1 min xx'
          max_x = foldl1 max xx'

          min_y = foldl1 min yy'
          max_y = foldl1 max yy'

          xx = map (\p -> (p-min_x)/(max_x-min_x)) xx'
          yy = map (\p -> (p-min_y)/(max_y-min_y)) yy'

          new_nodes = zipWith4 (\(Node l _) x y z -> Node l (Just [x,y,z  ] )) nodes xx yy ( map (*0.05) zz')
      in (new_nodes,edges)

--(Node,(Double,[Edge))

dijkstra'' (processed,[]) edges = processed
      
dijkstra'' (processed,nweights) edges =
       let avaliable_nodes (Node node_label _) es = foldl (\s e@(Edge a b _) -> if a == node_label || b == node_label then (e:s) else s) [] es
           get_or (Edge _ _ (Just x)) = x
           get_or (Edge _ _ Nothing) = 1.0

           (current_node:rest) = sortBy (\a b -> (fst $ snd a) `compare` (fst $ snd b)) nweights
           posible_ones = avaliable_nodes (fst current_node) edges
           current_distance = fst $ snd current_node
           current_track    = snd $ snd current_node

           new_nodes = map (\(n@(Node lab _),(v,e)) -> let the_edge = filter (\(Edge a b _) ->  a==lab || b == lab) posible_ones
                                                       in if (not $ null the_edge) && (current_distance + (get_or $ (head the_edge)) < v)
                                                          then (n,(current_distance + (get_or $ (head the_edge)),the_edge++current_track))
                                                          else (n,(v,e))) rest

       in dijkstra'' (current_node:processed,new_nodes) edges



dijkstra' (nodes,edges) from =
      let very_big = 1e10
      in map (\(n@(Node l _)) -> if l==from then (n,(0,[])) else (n,(very_big,[]))) nodes
                         

dijkstra (nodes,edges) from to =
      let edges' = zipWith (\(Edge a b _) c -> (Edge a b (Just (1+c)))) edges (map (*0.001) [1..])
          nnodes = dijkstra' (nodes,edges') from
          paths  = dijkstra'' ([],nnodes) edges
          the_path = filter (\(Node l _,_) -> l == to) paths
      in if null the_path then Left ([]::[Edge]) else Right $ snd $ snd $ head the_path

--clean_path p | trace ("-----fromR  "++ show p ) False = undefined
clean_path l@(p:th) = if any (==p) th then clean_path (filter (/=p) th) else p:(clean_path th)
clean_path _ = []


walker'' path iter gr@(nodes,edges) from to =

       let avaliable_nodes node_label es = foldl (\s e@(Edge a b _) -> if a == node_label || b == node_label then (e:s) else s) [] es
           direction (Node _ (Just p1)) (Node _ (Just p2)) =  zipWith (-) p2 p1 

           current_node = head $ filter (\(Node l _) -> l == from) nodes
           far_node = head $ filter (\(Node l _) -> l == to) nodes
           distant_nodes = concat $ map (\(Edge f t _) -> filter (\(Node l _)-> (f == l || t == l) && l/=from) nodes ) $  avaliable_nodes from edges
           to_hell = direction current_node far_node
           dirs    = map (direction current_node ) distant_nodes 
           angles  = map (\v1 -> dprod (unit v1) (unit to_hell)) dirs
           one_of = zip3 angles distant_nodes (avaliable_nodes from edges) 

           nexts = map (\(_,(Node nx _),e) -> (nx,e)) $ sortBy (\n1@(s,t,u) n2@(a,b,c) -> compare a s) one_of

           testers = dropWhile isLeft $ map (\(n,e) -> walker'' (e:path) iter (gr `without` e) n to) nexts

       in if null nexts then (Left path) 
          else let (n_node,n_edge) = head nexts -- trace ("TT "++"P"++(show path)) $ head nexts
               in if n_node == to then (Right (n_edge:path))
                  else if null testers then (Left path)
                       else head testers 
                     
      


--walker' p _ _ from _  | trace ("-----walker  "++ show p ++ " " ++ from  ++ "\n") False = undefined
walker' path iter gr@(nodes,edges) from to =

       let avaliable_nodes node_label es = foldl (\s e@(Edge a b _) -> if a == node_label || b == node_label then (e:s) else s) [] es
           direction (Node _ (Just p1)) (Node _ (Just p2)) =  zipWith (-) p2 p1 
                                           
           current_node = head $ filter (\(Node l _) -> l == from) nodes
           far_node = head $ filter (\(Node l _) -> l == to) nodes
           distant_nodes = concat $ map (\(Edge f t _) -> filter (\(Node l _)-> (f == l || t == l) && l/=from) nodes ) $  avaliable_nodes from edges
           
           to_hell = direction current_node far_node
           dirs    = map (direction current_node ) distant_nodes 
                                                                    
           angles  = map (\v1 -> dprod (unit v1) (unit to_hell)) dirs

           one_of = zip3 angles distant_nodes (avaliable_nodes from edges) 

           (n_node,n_edge) = (\(_,(Node nx _),e) -> (nx,e)) $ foldl (\n1@(s,t,u) n2@(a,b,c) -> if a>s then n2 else n1) (head one_of) (tail one_of)  

           purified =  clean_path path

       in if (null $ avaliable_nodes from edges) then
                if (null purified ) then trace ("Left "++(show path)++" "++(show (from,to))++(show purified)) (Left path)
                else walker'((head purified):path) (iter+1) (gr `without` (head $ clean_path path)) (far_end (head $ clean_path path) from ) to 
          else if n_node == to then (Right $ n_edge:path)
               else walker' (n_edge:path) (iter+1) (gr `without` n_edge) n_node to 
                      
       -- in if (iter>500) || (null $ avaliable_nodes from edges) then
       --          if from == to || (null purified ) then (Left path)
       --          else walker'((head purified):path) (iter+1) (gr `without` (head $ clean_path path)) (far_end (head $ clean_path path) from ) to 
       --    else if n_node == to then (Right $ n_edge:path)
       --         else walker' (n_edge:path) (iter+1) (gr `without` n_edge) n_node to 

walker = walker'' [] 0
          
throw_edge (nodes,edges) edge =
      (nodes,filter (/=edge) edges)
          
      
alldo (opt,cr) f = do
  g' <-  read_graph f

  let g'' = if opt =="x" then add_centre g' else g'
      g = if opt == "e" then embed cr g'' else g''
  
      
--  putStrLn $ show g
--  putStrLn ""
--  prmat $ toMatrix g
--  prmat $ snd $ qr_iter 300 (eye (length $ fst g),toMatrix g)
  prmat $  toMatrix g
--  prmat $ fst $ qr_iter 300 (eye (length $ fst g),toMatrix g)
  let g1 = graph2ps g
      g2 = resize g1
      g3 = psplot g2
  writeFile "xx.ps" g3
--           current_node = head $ filter (\(Node l _) -> l == from) nodes           current_node = head $ filter (\(Node l _) -> l == from) nodes           current_node = head $ filter (\(Node l _) -> l == from) nodes


analyze_paths g paths =
      let rpaths = map clean_path $ rights $ filter isRight paths
          lpaths = lefts  $ filter isLeft paths
          load'   = DM.fromList $ zip (snd g) (repeat (0::Integer))
          load'' = foldl (\s e -> DM.adjust (+1) e s) load' $ concat rpaths
          load = sortBy (\a b -> snd a `compare` snd b) $ DM.toList load''
          no_rpaths = length rpaths
          no_lpaths = length lpaths
          sum_len   = length $ concat rpaths
          empty_hist = DM.fromList [(1,0)]
          hist = foldl (\h p -> DM.insertWith (+) (length p) 1 h) empty_hist rpaths
          
      in (load,no_rpaths,no_lpaths,sum_len,hist)

dpaths _ _ _ a b | trace ("DPATHS "++a++" "++b) False = undefined
dpaths factor g fun from to = let p1 = fun g from to
--                                  new_graph = embed3 factor (exclude g (fmap clean_path p1))
                                  new_graph = (exclude g (fmap clean_path p1))     
                                  p2 = fun new_graph to from
                              in (p1,p2)

dpaths2 _ _ _ _ _ | trace "DPATHS2" False = undefined
dpaths2 factor g@(nodes,edges) fun from to = let p1 = fun g from to
                                                 all_nodes' = (map (\(Edge i e _) -> (i,e))) $ fromRight p1
                                                 (l1,l2) = unzip all_nodes'
                                                 inner_nodes = unique $ filter (/=from ) $ filter (/= to) (l1++l2)
                                                 n_edges = filter (\(Edge a b _) -> (not $ a `elem` inner_nodes) && (not $ b `elem` inner_nodes)) edges
--                                                 new_graph = embed3 factor (nodes,n_edges)
                                                 new_graph = (nodes,n_edges)
                                                 p2 = p1 `seq` new_graph `seq` fun new_graph to from 
                                             in (p1,p2)
                                
state_paths alg g demands =
      let load0   = DM.fromList $ zip (snd g) (repeat (0::Integer))
      in  foldl (\(g,paths,load) (from,to) -> let path = alg g from to
                                                  load' = foldl (\s e -> DM.adjust (+1) e s) load $ (\(Right x) -> x) path
                                                  overloads = map fst $ DM.toList $ DM.filter (>79) load'
                                                  g' = exclude g (Right overloads)
                                              in (g',path:paths,load')) (g,[],load0) demands


fromRight (Right x) = x
fromRight _ = []

crossection a b = filter (`elem` b) a 
          
state_paths2 link_exclusion factor link_limit alg g0 demands =
      let load0   = DM.fromList $ zip (snd g0) (repeat (0::Integer))
      in  foldl (\(gs,paths,load) (from,to) -> let g = head gs
                                                   (path1,path2) = link_exclusion factor g alg from to
                                                   load'' = foldl (\s e -> DM.adjust (+1) e s) load $ clean_path $ fromRight path1
                                                   load' = foldl (\s e -> DM.adjust (+1) e s) load'' $ clean_path $ fromRight path2
                                                   overloads' = map fst $ DM.toList $ DM.filter (>link_limit-1) load'
                                                   overloads  = crossection overloads' (snd g)
                                                   g' = if null overloads then gs else ((embed3 factor (exclude g (Right overloads))):gs)
                                               in (g',path1:path2:paths,load')) ([g0],[],load0) demands


pair_stuff (a:b:c) = (a,b):(pair_stuff c)
pair_stuff _ = []

startswith x (a:as) = x == a
startswith x _ = False
               
all_paths (opt,cr) f = do
  g' <- fmap purify $ read_graph f

  let g   = if any ( == "e") opt  then embed3 factor g' else g'
      alg = if any ( == "d") opt  then dijkstra else walker
      twowaypath = if any ( == "n") opt  then dpaths2 else dpaths

      linklimit' = filter (startswith 'L') opt
      linklimit  = if null linklimit' then 1000 else (read $ tail $ head linklimit' :: Integer)

      factor' = filter (startswith 'C') opt
      factor  = if null factor' then 0.0 else (read $ tail $ head factor' :: Double)

      labels = map (\(Node l _) -> l) $ fst g


      demands' = if any ( == "H") opt  then  [ (p,q) | p <- labels, q <- labels , p>q ] else  [ (p,q) | p <- labels, q <- labels , p/=q]

      demands = demands'
--    Some summary
      (gg,stuff,load') = state_paths2 twowaypath factor linklimit alg g demands
      load = sortBy (\a b -> snd a `compare` snd b) $ DM.toList load'      

      sm  = sum $ map snd load

      fpaths = map fst $ pair_stuff stuff
      bpaths = map snd $ pair_stuff stuff
            
      gnames = map (\n -> f++"_gr"++(show n)) [1..]
  zipWithM_ (\n g -> writeFile n (psplot $ resize $ graph2ps g)) gnames (reverse gg)
  return $ (length gg,analyze_paths g stuff,analyze_paths g fpaths,analyze_paths g bpaths)


--writeCsv :: ([(Edge, Integer)], Int, Int, Int) -> String -> IO ()
writeCsv (load,no_rpaths,no_lpaths,sum_len,hist) file = do

  let hist_in_csv = foldl1 (++) $ zipWith (\(a,b) n -> (show n) ++ ","++(show a)++","++(show b)++"\n")  (DM.toList hist) [1..]
      lines_in_csv = foldl1 (++) $ zipWith (\(node,ld) n -> (show n) ++ ",\""++(show node)++"\","++(show ld)++"\n") load [1..]
      head_  = "\"no\",\"edge\",\"load\"\n"
      head__ = "\"no\",\"len\",\"count\"\n"
      intro1 = "rpaths,-,"++show no_rpaths++"\n"
      intro2 = "lpaths,-,"++show no_lpaths++"\n"
      intro3 = "len,-,"++show sum_len++"\n"
  writeFile file (head_ ++ intro1 ++ intro2 ++ intro3 ++ lines_in_csv )
  writeFile ("cnt_"++file) (head__ ++ hist_in_csv )
         
main = do
  args <-getArgs

  let opts = init args
      f    = last args

  g' <- fmap purify $ read_graph f
--  putStrLn $ show $ embed3  0.1 g'
  (r1,r2,r3,r4) <- all_paths (opts,0.02) f
  putStrLn $ show r1
  putStrLn $ show r2
  putStrLn $ show r3
  putStrLn $ show r4
  let fix = foldl1 (\a b -> ""++a++"_"++b) args 
  writeCsv r2 (fix++"f1.csv")
  writeCsv r3 (fix++"f2.csv")
  writeCsv r4 (fix++"f3.csv")

  writeFile "Plik.ps"  (psplot $ resize $ graph2ps $ embed 0.1 g')

  prmat $ toMatrix g'
  prmat $ snd $ qr_iter (2*(toInteger $ length $ fst g')) (eye (length $ fst g'),toMatrix g')
  prmat $ fst $ qr_iter (2*(toInteger $ length $ fst g')) (eye (length $ fst g'),toMatrix g')
