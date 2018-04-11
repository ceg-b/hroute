
mat1 = [[1.0,2,3,4],[5,1,2,3],[5,4,3,1],[12,11,3,1]]
mat2 = [[1.0,2,3,4],[5,1,2,3],[5,4,3,1],[12,11,3,2]]
mat3=[[1,2,3,4]]
mat4=[[4,1,-2,2],[1,2,0,1],[-2,0,3,-2],[2,1,-1,-1.0]]
v1 = [1,2,4,5.0]
v2 = [5,4,3,1]

norm v = sqrt $ dprod v v

unit v = map (\p -> p/(norm v)) v
         
transpose ([]:_) = []
transpose rows@(r:ws) = (map head rows):(transpose $ map tail rows)

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
      in map (\column -> map (\row ->  dprod row column) m1) m2'

vals val  n m  = replicate n $ replicate m val
zeros = vals (0.0::Double)
ones  = vals (1.0::Double)

eye n = let bigrow = concat $ repeat $ 1:(replicate n 0)
            take' n inp = take n inp : take' n (drop n inp)
        in take n $ take' n bigrow

per_element mat1 mat2 fun = zipWith (\a b -> zipWith (\x y -> fun x y) a b ) mat1 mat2
scalar_mul mat scalar = map (map (*scalar)) mat

hholder v' =
      let id = eye $ length v'
          q  = mmul (transpose [v]) [v]
          v = unit v'
      in per_element id (scalar_mul q 2) (-)


prmat mat = do
  mapM_ (putStrLn.show) mat
      
