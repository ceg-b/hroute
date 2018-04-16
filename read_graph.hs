import Text.Read as TR
import System.Directory
import Data.Maybe
import Matrix
--import Numeric.LinearAlgebra as NLA
       
data Node = Node String (Maybe (Double,Double)) deriving Show
data Edge = Edge String String (Maybe Double) deriving Show

data PScript =Line (Double,Double) (Double,Double) |  Point (Double,Double) | Label String (Double,Double)  deriving Show

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


resize eps@(ps:items) =
    let ((a,b),(c,d)) =  bbox eps
        width=200
        height=200
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
       let intro="%!PS-Adobe-3.0 EPSF-3.0\n/Times-Roman findfont\n14 scalefont\nsetfont\n"
       in intro ++  foldl (\s p -> s ++ (psplot' p)) (psplot' e)  ps
       
graph2dot (nodes,edges) =
      let intro ="digraph G {"
          entries = map (\(Edge f t _) -> f ++ "->" ++ t ++";") edges
      in (foldl (\s x -> s ++"\n"++x) intro entries) ++ "\n}\n"


graph2ps (nodes,edges) =
       let get_coords label = filter (\(Node n c) -> n==label) nodes
           lines'            = map (\(Edge a b _)-> (get_coords a,get_coords b)) edges
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
    let nds = map words $ lines nodes
    in map mknode nds
    where
      mknode (a:b:c:[]) = Node a (coords (TR.readMaybe b) (TR.readMaybe c))
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



toMatrix (nodes,edges) =
    let tnodes = zip [1..] (map (\(Node a _ ) -> a) nodes)
        mapN n = fst $ head $ filter (\(a,b)->b==n) tnodes
    in map (\(Edge n1 n2 p) -> case p of
                  Nothing -> ((mapN n1),(mapN n2),1.0)
                  Just x  -> ((mapN n1),(mapN n2),x  )
           ) edges

alldo f = do
  g <- read_graph f
  let g1 = graph2ps g
      g2 = resize g1
      g3 = psplot g2
  writeFile "xx.ps" g3
