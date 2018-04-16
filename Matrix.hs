module Matrix where

import qualified Text.Printf as TP


      
mat1 = [[1.0,2,3,4],[5,1,2,3],[5,4,3,1],[12,11,3,1]]
mat2 = [[1.0,2,3,4],[5,1,2,3],[5,4,3,1],[12,11,3,2]]
mat3=[[1,2,3,4]]
mat4=[[4,1,-2,2],[1,2,0,1],[-2,0,3,-2],[2,1,-1,-1.0]]
mat5=mat4 `add` (t mat4)
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
         
blkdiag mat1 mat2 =
     let (n1,m1) = size mat1
         (n2,m2) = size mat2
     in rowcat (colcat mat1 (zeros n1 m2))
               (colcat (zeros n2 m1) mat2)

dprod v1 v2 = foldl (\s (p1,p2) -> s+p1*p2) 0 (zip v1 v2)

mmul m1 m2 =
      let m2' = transpose m2
      in map (\column -> map (\row ->  dprod row column) m2') m1

vals val  n m  = replicate n $ replicate m val
zeros = vals (0.0::Double)
ones  = vals (1.0::Double)

eye n = let bigrow = concat $ repeat $ 1:(replicate n 0)
            take' n inp = take n inp : take' n (drop n inp)
        in take n $ take' n bigrow

per_element mat1 mat2 fun = zipWith (\a b -> zipWith (\x y -> fun x y) a b ) mat1 mat2
scalar_mul mat scalar = map (map (*scalar)) mat

add mat1 mat2 = per_element mat1 mat2 (+)

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

qr' n (q,r) =               
      let hh = hholder n ((t r)!!(n))
          nm =  hh `mmul` r
      in if n+1  == length r then  (hh `mmul` q,nm)
         else qr' (n+1) (hh `mmul` q,nm)

qr mat =
      let n = length mat
      in qr' 0 (eye n,mat)


qr_iter n (q,r) =
      if n==0 then (t q,r)
      else
            let (a1,a2)  = qr r
            in qr_iter (n-1) (((( a1) `mmul` q)),(a1) `mmul` r `mmul` (t a1))

               
prmat :: [[Double]] -> IO ()
prmat mat = do
  mapM_ (\v -> do
          mapM_ (\m -> do TP.printf "%5.2f\t" m) v
          TP.printf "\n" ) mat
      
