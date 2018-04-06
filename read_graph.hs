import Text.Read as TR
import System.Directory
--import Numeric.LinearAlgebra as NLA
       
data Node = Node String (Maybe (Double,Double)) deriving Show
data Edge = Edge String String (Maybe Double) deriving Show


graph2dot (nodes,edges) =
      let intro ="digraph G {"
          entries = map (\(Edge f t _) -> f ++ "->" ++ t ++";") edges
      in (foldl (\s x -> s ++"\n"++x) intro entries) ++ "\n}\n"


graph2ps (nodes,edges) =
--      hashed_nodes = map (\(Node n c) -> (hsh n
       let get_coords (Node n c) = c
           
          
unique (x:xs) = x:(unique (filter (/=x) xs))
unique _ = []

parse_graph edges (Just nodes) =
     let eds = map words $ lines edges
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
  nF  <- readFile nfile
  eF  <- readFile efile
                
  let nodes = parse_nodes nF

  if (isN) then
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
