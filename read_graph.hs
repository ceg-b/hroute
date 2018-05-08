import Text.Read as TR
import System.Directory
import Data.Maybe
import Matrix
import Data.List
--import Numeric.LinearAlgebra as NLA
       
data Node = Node String (Maybe (Double,Double)) deriving Show
data Edge = Edge String String (Maybe Double) deriving Show

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
           lines ((Node _ (Just (x1,y1))):_,(Node _ (Just (x2,y2))):_) = Just $ Line (x1,y1) (x2,y2)
           lines  _ = Nothing
           points (Node _ (Just (a,b))) = Just $ Point (a,b)
           points _ = Nothing
           labels (Node x (Just (a,b))) = Just $ Label x (a,b)
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
      coords (Just a) (Just b) = Just (a,b)
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
       
embed corr (nodes,edges) =
      let matrix = toMatrix (nodes,edges)
          (v',d) = qr_iter 100 (eye (length nodes),matrix)
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
          new_nodes = zipWith3 (\(Node l _) x y -> Node l (Just (x,y))) nodes xx yy
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
      let nnodes = dijkstra' (nodes,edges) from
          paths  = dijkstra'' ([],nnodes) edges
      in filter (\(Node l _,_) -> l == to) paths



walker (nodes,edges) from to =

       let avaliable_nodes node_label es = foldl (\s e@(Edge a b _) -> if a == node_label || b == node_label then (e:s) else s) [] es
           direction (Node _ (Just (x,y))) (Node _ (Just (a,b))) =  [a-x,b-y]
                                           
           current_node = head $ filter (\(Node l _) -> l == from) nodes
           far_node = head $ filter (\(Node l _) -> l == to) nodes
           distant_nodes = concat $ map (\(Edge f t _) -> filter (\(Node l _)-> (f == l || t == l) && l/=from) nodes ) $  avaliable_nodes from edges
           
           to_hell = direction current_node far_node
           dirs    = map (direction current_node ) distant_nodes 
                                                                    
           angles  = map (\v1 -> dprod (unit v1) (unit to_hell)) dirs

           one_of = zip3 angles distant_nodes (avaliable_nodes from edges) 

           next_one = foldl (\n1@(s,t,u) n2@(a,b,c) -> if a>s then n2 else n1) (head one_of) (tail one_of)  

       in next_one

          
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
