
mat1 = [[1,2,3,4],[5,1,2,3],[5,4,3,1],[12,11,3,1]]
mat2 = [[1,2,3,4],[5,1,2,3],[5,4,3,1],[12,11,3,2]]
mat3=[[1,2,3,4]]

v1 = [1,2,4,5]
v2 = [5,4,3,1]
     
transpose ([]:_) = []
transpose rows@(r:ws) = (map head rows):(transpose $ map tail rows)


dprod v1 v2 = foldl (\s (p1,p2) -> s+p1*p2) 0 (zip v1 v2)

mmul m1 m2 =
      let m2' = transpose m2
      in map (\column -> map (\row ->  dprod row column) m1) m2'

vals  n m val = replicate n $ replicate m val
zeros = vals 0
ones  = vals 1

eye n = let bigrow = concat $ repeat $ 1:(replicate n 0)
            take' n inp = take n inp : take' n (drop n inp)
        in take n $ take' n bigrow

per_element mat1 mat2 fun = zipWith (\a b -> zipWith (\x y -> fun x y) a b ) mat1 mat2
