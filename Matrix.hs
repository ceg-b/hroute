module Matrix where
import qualified Data.List  as DL
import qualified Text.Printf as TP
import Debug.Trace

import qualified Data.Vector.Unboxed as DU
      
mat1 = [[1.0,2,3,4],[5,1,2,3],[5,4,3,1],[12,11,3,1]]
mat2 = [[1.0,2,3,4],[5,1,2,3],[5,4,3,1],[12,11,3,2]]
mat3=[[1,2,3,4]]
mat4=[[4,1,-2,2,1],[1,2,0,1,2],[-2,0,3,-2,1],[2,1,-1,-1.0,2],[2,1,1,-1.0,2]]
mat5=mat4 `madd` (t mat4)
v1 = [1,2,4,5.0]
v2 = [5,4,3,1]

norm v = sqrt $ dprod v v

unit v = map (\p -> p/(norm v)) v
         
transpose ([]:_) = []
transpose rows@(r:ws) = (map head rows):(transpose $ map tail rows)

t=transpose

rowcat mat1 mat2 = mat1 ++ mat2
colcat mat1 mat2 = zipWith (++) mat1 mat2

size m@(a:t) = (length m,length a)
size _ = (0,0)


create_row nn entries =
      let magic = 0.0
          (l',r') = foldl (\(l,r) (m,n,v) ->
                   if n-l == 1 then (l+1,(magic+v):r)
                   else (n,((magic+v):(replicate (n-l-1) magic))++r)) (0,[]) entries
      in if l'==nn then reverse $ r'
         else reverse $ (replicate (nn-l') magic) ++ r'
      

blkdiag mat1 mat2 =
     let (n1,m1) = size mat1
         (n2,m2) = size mat2
     in rowcat (colcat mat1 (zeros n1 m2))
               (colcat (zeros n2 m1) mat2)

--dprod v1 v2 =  v1 `seq` v2 `seq` DL.foldl' (\s (p1,p2) -> p1 `seq` p2 `seq` s+p1*p2) 0 (zip v1 v2)
--dprod v1 v2 =  DL.foldl' (\s (p1,p2) ->  p1 `seq` p2 `seq` s `seq` s+p1*p2) 0 (zip v1 v2)
dprod v1 v2 =  DL.foldl' (\s (p1,p2) ->  p1 `seq` p2 `seq` s `seq` s+p1*p2) 0 (zip v1 v2)
--dprod v1 v2 = v1 `seq` v2 `seq` DL.foldl' (+) 0 $ zipWith (*) v1 v2 
--dprod (v:v1) (w:w1) = w `seq` v `seq` w*v + (dprod v1 w1)
--dprod _ _ = 0


mmul' m1 m2 =
      let m2' = m2 `seq` transpose m2
      in map (\column -> map (\row -> row `seq` column `seq`  dprod row column) m2') m1
--      in map (\column -> map (\row -> foldl1 (+) $ zipWith (*) row column)  m2') m1

mmul :: [[Double]]->[[Double]]->[[Double]]
mmul m1' m2' =
      let m2 = map (DU.fromList) $ transpose m2'
          m1 = map DU.fromList m1'
          m3 = map (\column -> map (\row -> DU.sum $ DU.zipWith (*)  row column) m2) m1
      in m3



vals val  n m  = replicate n $ replicate m val
zeros = vals (0.0::Double)
ones  = vals (1.0::Double)

eye n = let bigrow = concat $ repeat $ 1:(replicate n 0)
            take' n inp = take n inp : take' n (drop n inp)
        in take n $ take' n bigrow

diag n list =  let bigrow = concat $ reverse $ foldl (\s x -> (x:(replicate n 0)):s) [] list
                   take' n inp = take n inp : take' n (drop n inp)
               in take n $ take' n bigrow


diag' (m:at) = (head m):(diag' $ map tail at)
diag' _ = []

sumrows mat = map sum mat                  


per_element mat1 mat2 fun = zipWith (\a b -> zipWith (\x y -> fun x y) a b ) mat1 mat2
scalar_mul mat scalar = map (map (*scalar)) mat

madd mat1 mat2 = per_element mat1 mat2 (+)

hholder' v' =
      let id = eye $ length v'
          q  = mmul (transpose [v]) [v]
          v1 = head v'
          v = if v1>0 then unit $ (head v' + norm v'):(tail v') else unit $ (head v' - norm v'):(tail v')
      in per_element id (scalar_mul q 2) (-)

hholder n v =
      let submat = hholder' (drop n v)
      in blkdiag (eye n ) submat

hred n mat =
      let hh = hholder n (mat!!(n-1))
          nm =  hh `mmul` mat `mmul` hh
      in if n+1 == length mat then nm
         else hred (n+1) nm


hred'' n (q,mat) =
      let hh = hholder n (mat!!(n-1))
          nm =  hh `mmul` mat `mmul` hh
      in if n+1 == length mat then (hh:q,nm)
         else hred'' (n+1) (hh:q,nm)

hred' mat = let (q,m) = hred'' 1 ([],mat)
                qq = foldl1 mmul $ map t  q
            in qq

qr' n (q,r) =               
      let hh = hholder n ((t r)!!(n))
          nm =  hh `mmul` r
      in if n+1  == length r then  (hh `mmul` q,nm)
         else qr' (n+1) (hh `mmul` q,nm)

qr mat =
      let n = length mat
      in mat `seq` qr' 0 (eye n,mat)


mx mat = foldl1 max $ map (\r -> foldl1 max r) mat

mxx t = foldl1 max t


qr_iter'' n (q0,r) =
      let q = hred' r
          mat =  q `mmul` r `mmul` (t q)
          (q',r') = qr_iter' n (q0,mat)
      in ((t q) `mmul` q',r')

qr_iter = qr_iter''
      
qr_iter' n (q,r) =
      if n==0 then (t q,r)
      else
            let (a1,a2)  = r `seq` qr r
                next_r = (a1) `mmul` r `mmul` (t a1)
--            in trace ("T" ++ show  (diag' $ per_element r next_r (\a b -> abs $ (a-b)))) (a1 `seq` a2 `seq` qr_iter (n-1) (((( a1) `mmul` q)),next_r))
--            in trace ("T" ++ show  (mxx $ map abs $ zipWith (-) (diag' r) (diag'  next_r))) (a1 `seq` a2 `seq` qr_iter (n-1) (((( a1) `mmul` q)),next_r))
            in (a1 `seq` a2 `seq` qr_iter' (n-1) (((( a1) `mmul` q)),next_r))

               
prmat :: [[Double]] -> IO ()
prmat mat = do
  mapM_ (\v -> do
          mapM_ (\m -> do TP.printf "%5.2f\t" m) v
          TP.printf "\n" ) mat
  TP.printf "\n" 
      
