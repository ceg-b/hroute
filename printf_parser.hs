
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts  #-}
-- {-# LANGUAGE InstanceSigs #-}
-- import Debug.Trace
import Data.List (isInfixOf)

test="ala ma kota  printf(\"ala ma kota%d\n\"); ala nie ma kota\n snprintf(\"aa%d\",var1,var2);\n"
test2="ala ma"
test3="ala "

data Context a b  =  Context (a,b) deriving Show
getContext (Context (a,b)) = (a,b)

finish (Context (a,b)) = if b == mempty then True else False

data Ccommand = Printf String  |  Anyother String | SpaceTabNL String | Null | CTRACE String | CERR String | OCTRACE String | OCERR String | OSTRACE String | OSERR String | OTRACE String | OERR String
data Cmd = Cons Ccommand Cmd | Nil 
--rconcat  =
instance Show Ccommand where
    show (Anyother x)   = x
    show (SpaceTabNL x) = x
    show (Printf x) = x
    show (CERR x) = "CERR" ++ x
    show (CTRACE x) = "CTRACE" ++ x
    show (OCTRACE x) = "OCTRACE" ++ x
    show (OCERR x) = "OCERR" ++ x
    show (OSTRACE x) = "OSTRACE" ++ x
    show (OSERR x) = "OSERR" ++ x
    show (OTRACE x) = "OTRACE" ++ x
    show (OERR x) = "OERR" ++ x

    show _ = "\n\\null\n"

instance Show Cmd where
     show (Cons (Printf s ) (Cons (SpaceTabNL p) rest)) = (show rest) ++ (printf s p)
     show (Cons (CTRACE s ) (Cons (SpaceTabNL p) rest)) = (show rest) ++ (ttrace id "CTRACE" s p)
     show (Cons (CERR   s ) (Cons (SpaceTabNL p) rest)) = (show rest) ++ (ttrace id "CERR" s p)
     show (Cons (OCTRACE s ) (Cons (SpaceTabNL p) rest)) = (show rest) ++ (ttrace unbra "OCTRACE" s p)
     show (Cons (OCERR   s ) (Cons (SpaceTabNL p) rest)) = (show rest) ++ (ttrace unbra "OCERR" s p)
     show (Cons (OSTRACE s ) (Cons (SpaceTabNL p) rest)) = (show rest) ++ (ttrace unbra "OSTRACE" s p)
     show (Cons (OSERR   s ) (Cons (SpaceTabNL p) rest)) = (show rest) ++ (ttrace unbra "OSERR" s p)
     show (Cons (OTRACE s ) (Cons (SpaceTabNL p) rest)) = (show rest) ++ (ttrace unbra "OTRACE" s p)
     show (Cons (OERR   s ) (Cons (SpaceTabNL p) rest)) = (show rest) ++ (ttrace unbra "OERR" s p)

     show (Cons a rest) = (show rest) ++ (show a)
     show _ = ""

-- instance Show Cmd where
--     show (Cons (Printf s ) (Cons (SpaceTabNL p) rest))  = p++s ++"\n\n"++ (printf s) ++ "\n\n" ++ (show rest)
--     show (Cons (Printf s ) rest)  = s ++"\n\n"++ (printf s) ++ "\n\n" ++ (show rest)
--     show (Cons _ Nil) = ""
--     show (Cons p rest) = (show p ) ++ show rest
                         
instance Applicative (Context a) where
instance Functor (Context a) where

instance Monoid a => Monad (Context a)  where
    (>>=) (Context (a,b)) f = let (a',b') = getContext $ f b
                               in Context (a `mappend` a' ,  b')

instance Monoid Cmd where
       (Cons (SpaceTabNL s) (Cons (Anyother p) rest)) `mappend`  (Cons (Anyother q) _) = Cons (Anyother (p++s++q)) rest

       (Cons (Anyother p) rest) `mappend`  (Cons (Anyother q) _) = Cons (Anyother (p++q)) rest
                                                                                         
       (Cons (SpaceTabNL n) rest) `mappend`  (Cons (Printf q) _) =  (Cons (Printf q) (Cons (SpaceTabNL n) rest))
       (Cons (SpaceTabNL n) rest) `mappend`  (Cons (SpaceTabNL q) _) =  (Cons (SpaceTabNL (n++q)) rest)
       (Cons (SpaceTabNL n) rest) `mappend`  (Cons (Anyother q) _) =  (Cons (Anyother (n++q)) rest)

       (Cons (Anyother   n) rest) `mappend`  (Cons (Printf q) _) =  (Cons (Anyother (n++"printf"++q)) rest)

       (Cons (Anyother   n) rest) `mappend`  (Cons (OERR q) _) =  (Cons (Anyother (n++"OERR"++q)) rest)


             
       cmd `mappend`  (Cons sth' _) = Cons sth' cmd 
       cmd `mappend` Nil = cmd

                                      
       mempty = Nil

---------------------------------------------------------------------
---------------------------------------------------------------------
to_the_end o s@(t:r) =
    if t ==';' && mod (length $ filter (=='"') o) 2 == 0 then reverse (t:o)
    else to_the_end (t:o) r

to_the_end o _ = reverse o
                 
get_command' ('p':'r':'i':'n':'t':'f':str) =  Context (Printf  (rest_of_printf),rest)
                                              where rest_of_printf = to_the_end "" str
                                                    rest           = drop (length rest_of_printf) str

                 
get_command' ('C':'T':'R':'A':'C':'E':str) =  Context (CTRACE (rest_of_line),rest)
                                              where rest_of_line  = to_the_end "" str
                                                    rest          = drop (length rest_of_line) str


get_command' ('O':'C':'T':'R':'A':'C':'E':str) =  Context (OCTRACE (rest_of_line),rest)
                                              where rest_of_line  = to_the_end "" str
                                                    rest          = drop (length rest_of_line) str


get_command' ('O':'S':'T':'R':'A':'C':'E':str) =  Context (OSTRACE (rest_of_line),rest)
                                              where rest_of_line  = to_the_end "" str
                                                    rest          = drop (length rest_of_line) str


get_command' ('O':'T':'R':'A':'C':'E':str) =  Context (OTRACE (rest_of_line),rest)
                                              where rest_of_line  = to_the_end "" str
                                                    rest          = drop (length rest_of_line) str

                                                                    
get_command' ('C':'E':'R':'R':str) =  Context (CERR (rest_of_line),rest)
                                              where rest_of_line  = to_the_end "" str
                                                    rest          = drop (length rest_of_line) str

get_command' ('O':'C':'E':'R':'R':str) =  Context (OCERR (rest_of_line),rest)
                                              where rest_of_line  = to_the_end "" str
                                                    rest          = drop (length rest_of_line) str


get_command' ('O':'E':'R':'R':str) =  Context (OERR (rest_of_line),rest)
                                              where rest_of_line  = to_the_end "" str
                                                    rest          = drop (length rest_of_line) str


get_command' ('O':'S':'E':'R':'R':str) =  Context (OSERR (rest_of_line),rest)
                                              where rest_of_line  = to_the_end "" str
                                                    rest          = drop (length rest_of_line) str


                                                                    
get_command' (' ':str) = Context (SpaceTabNL " ",str)
get_command' ('\t':str) = Context (SpaceTabNL "\t",str)
get_command' ('\n':str) = Context (SpaceTabNL "\n",str)
                       
get_command' (x:str) = Context (Anyother [x],str)
get_command' _ = Context (Anyother "" ,[])
            
get_command s = let tmp = get_command' s
                in case tmp of
                     Context (a,(x:xs)) -> Context ((Cons a Nil),x:xs)
                     Context (a,[]) -> Context ((Cons a Nil),[])
                     otherwise          -> Context (Nil,[])

---------------------------------------------------------------------
---------------------------------------------------------------------
parse fun x = let starter = fun x
              in infinity starter
              where infinity arg =
                        let tmp = arg >>= fun
                        in if finish tmp then tmp
                           else infinity tmp

inparen str =
    let c = dropWhile (/='"') str
        d = takeWhile (/='"') $ tail c
        e = dropWhile (/='"') $ tail c
    in if null c then Context(mempty,mempty)
       else Context (d++" ",if not $null e then tail e else e)
                      
formats str =
    let (before,after') = split (/='%') str
        after'' = dropWhile (\x -> not $ x `elem` ['A'..'z']) $ after'
        after = if null after'' then after'' else tail after''
    in if null before && null after' then Context (mempty,mempty)
       else Context ([before],after)

split fun str = (takeWhile fun str,if null after then after else tail after)
                where after = dropWhile fun str
                
csplit fun str = let (a,b) = split fun str
                 in if null a then (Context (mempty,b))
                    else Context ([a],b)
        
minimain x = let table = parse get_command x
             in show $ fst $ getContext table


printf str blank =
    let (a,b) = getContext $ parse inparen $ clean str
        outparen' = dropWhile (/=',') $
                    reverse $ takeWhile (/='"') $
                    tail $ dropWhile (/=')') $
                    reverse $ filter (/='\n') $ filter (/='\r')  str
        outparen''  = if null outparen' then outparen' else tail outparen'
        args = fst $ getContext $ parse (csplit (/=',')) outparen''
        strs = fst $ getContext $ parse formats a

        datas = zipWith (\a b -> " << " ++ b ++ " << \"" ++ a ++"\"" ) (tail strs) (args)

        log_string = (foldl (\s x -> s ++ x ) ("\""++(head strs)++"\"") datas ) ++ ("<< std::endl;")

        level = if "Error" `isInfixOf` log_string ||  "error" `isInfixOf` log_string then "error" else "info"
            
        cout =  blank ++ "BOOST_LOG("++level++",PRINTF) << " ++ log_string
        pf = blank++"printf"++str

    in cout -- "\n#ifdef LOGGER"++cout++"\n#else"++pf++"\n#endif\n"

-- Czasami TRACE nei jest w nowej linii, a poiwnien, żeby opakowanie w #ifdef działało       
correct_nl str =
      if any (=='\n') str then str
      else '\n':str

unbra inbra = str'
       where str'' = tail $ dropWhile (/='(') inbra
             str'''= tail $ dropWhile (/=')') $ reverse str''
             str'  = reverse $ ';':str'''

logtype tp =
      case (reverse tp) of
        'R':'R':'E':p -> "error"
        otherwise     -> "trace"
      
                     
ttrace extract_fun label str blank =
       let  cout = (correct_nl blank) ++ "BOOST_LOG("++(logtype label)++","++label++")" ++ (extract_fun str)
            pf = (correct_nl blank)++label++str
--       in  "\n#ifdef LOGGER"++cout++"\n#else"++pf++"\n#endif\n"
       in cout       
          
main = do
  interact minimain

clean ('\\':'r':sth) = clean sth
clean ('\\':'n':sth) = clean sth
clean (x:sth) = x:(clean sth)
clean _ = []
