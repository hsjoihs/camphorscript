{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- Functional macro expansion -}
module Camphor.Base.Base_Step2.Base_Step2
(step2
) where

import Camphor.SafePrelude
import Camphor.Base.Base_Step2.Type
import Camphor.Base.Base_Step2.UserState
import Camphor.Base.Base_Step2.Def
import Camphor.Base.Base_Step2.Call
import Camphor.Base.Base_Step2.Auxilary
import Camphor.Base.Base_Step2.ErrList
import Camphor.Base.Base_Step2.Base_Step2_2(parser2_2)
import Camphor.Base.Base_Step2.PCS_Parser(parser2')
import Camphor.Global.Synonyms
import Text.Parsec
import Control.Monad.State
import Control.Monad.Reader
 
step2 ::  FilePath -> Txt -> Either ParseError Txt
step2 file txt = do
 xs <- parse parser2'  (file ++ "-step2") txt
 ys <- runParser parser2_2 [] (file ++ "-step2-2") xs
 zs <- convert ys
 return zs
 
defaultStat :: UserState 
defaultStat = emptyState

convert :: Sents -> Either ParseError Txt
convert xs = convert2 defaultStat xs 


convert2 :: UserState -> Sents -> Either ParseError Txt
convert2 stat sents = evalStateT (convert2_2 sents) stat 

convert2_3 :: Sents -> StateT UserState (Either ParseError) Txt
convert2_3 sents = do
 modify clearTmp
 convert2_2 sents

infixl 4 <++?>
(<++?>) :: (Monad m,Monoid a) => a -> StateT s m a -> StateT s m a
a <++?> f = do
 b <- f 
 return(a `mappend` b)

define :: (Monoid a,Monad m) => StateT s m a -> StateT s m () -> a -> StateT s m a
define g f result = f >> result <++?> g
 
call :: (Monoid a,Monad m) => StateT s m a -> ReaderT (s,Cnv2) m a -> StateT s m a
call g f = changeState fst (\x -> (x,convert2)) (toState f) >>= (<++?> g)


{------------------------------------------------------------------------------------- 
 -                              * definition of convert2_2 *                           -
 -------------------------------------------------------------------------------------}

convert2_2 :: Sents -> StateT UserState (Either ParseError) Txt
convert2_2 []                                    = return "" 
convert2_2 (Single _    (Comm comm):xs)          = ("/*" ++ comm ++ "*/")                         <++?> convert2_3 xs 
convert2_2 (Single _    (SynBlock ):xs)          = ("/*block*/")                                  <++?> convert2_3 xs 
convert2_2 (Single _    (Sp   sp  ):xs)          = sp                                             <++?> convert2_2 xs  -- convert2_2 INTENTIONAL
convert2_2 (Single _    (Scolon   ):xs)          = ""                                             <++?> convert2_3 xs 
convert2_2 (Single _    (Pleq ident integer):xs) = (unId ident ++ "+=" ++ showNum integer ++ ";") <++?> convert2_3 xs 
convert2_2 (Single _    (Mneq ident integer):xs) = (unId ident ++ "-=" ++ showNum integer ++ ";") <++?> convert2_3 xs 
convert2_2 (Single _    (Rd   idnt):xs)          = ("read("  ++ unId idnt ++ ")" ++ ";")          <++?> convert2_3 xs 
convert2_2 (Single _    (Wrt  idnt):xs)          = ("write(" ++ unId idnt ++ ")" ++ ";")          <++?> convert2_3 xs 

convert2_2 (Single pos  (Pragma prgm):xs) = case prgm of
 ("MEMORY":"using":vars) -> do
  let vars' = forM vars toIdent2
  case vars' of 
   Left e -> err$toPE pos $ Step2 <!> Prag <!> Memory <!> Using <!> NotValidIdent e
   Right vars_ -> 
    define (convert2_2 xs)(modify(setTmp vars_))(concatMap (\iden -> "assert_zero " ++ unId iden ++ ";") vars_) -- INTENTIONALLY LEFT AS convert2_2 
 _                       -> ("/*# " ++ unwords prgm ++ " #*/") <++?> convert2_3 xs

convert2_2 (Single pos (Char iden)                  :xs) = define (convert2_3 xs) (defC pos iden)                         ("char "   ++ unId iden ++ ";")
convert2_2 (Single pos (Del  iden)                  :xs) = define (convert2_3 xs) (defD pos iden)                         ("delete " ++ unId iden ++ ";")
convert2_2 (Single pos (Infl fixity op)             :xs) = define (convert2_3 xs) (defFix pos fixity op InfixL)            ""
convert2_2 (Single pos (Infr fixity op)             :xs) = define (convert2_3 xs) (defFix pos fixity op InfixR)            ""
convert2_2 (Single pos (Func1 name typelist sent)   :xs) = define (convert2_3 xs) (defF1 pos name typelist (Just sent))    ""
convert2_2 (Single pos (Func1Nul name typelist)     :xs) = define (convert2_3 xs) (defF1 pos name typelist Nothing    )    ""
convert2_2 (Single pos (Func2 op tlist1 tlist2 sent):xs) = define (convert2_3 xs) (defF2 pos op tlist1 tlist2 (Just sent)) ""
convert2_2 (Single pos (Func2Nul op tlist1 tlist2)  :xs) = define (convert2_3 xs) (defF2 pos op tlist1 tlist2 Nothing    ) ""
convert2_2 (Single pos (Syntax1 name tl bl)         :xs) = define (convert2_3 xs) (defS pos name (West tl) bl)             ""
convert2_2 (Single pos (Syntax2 name ttl bl)        :xs) = define (convert2_3 xs) (defS pos name (East ttl) bl)            ""
 
convert2_2 (Single pos (Call1 name valuelist)          :xs) = call (convert2_3 xs) (callK1 pos name valuelist) 
convert2_2 (Single pos (Call2 op valuelist1 valuelist2):xs) = call (convert2_3 xs) (callK2 pos op valuelist1 valuelist2) --- (val [op val])op(val [op val]);
convert2_2 (Single pos (Call3 op valuelist1 valuelist2):xs) = call (convert2_3 xs) (callK3 pos op valuelist1 valuelist2) --- (val [op val])op val [op val] ; 
convert2_2 (Single pos (Call4 list valuelist)          :xs) = call (convert2_3 xs) (callK4 pos list valuelist) --- [val op] (val [op val]) ; 
convert2_2 (Single pos (Call5 valuelist)               :xs) = call (convert2_3 xs) (callK5 pos valuelist) --- val [op val] op val [op val] ;
convert2_2 (Single _   (SynCall1 name vl pos2 block)   :xs) = call (convert2_3 xs) (callSC1 pos2 name vl (pos2,block) convert2_3) 
convert2_2 (Single _   (SynCall2 name tvl pos2 block)  :xs) = call (convert2_3 xs) (callSC2 pos2 name tvl (pos2,block) )

convert2_2 (Block p ys:xs)                                  = call (convert2_3 xs) (newStat3getter p ys convert2_3)
{-----------------------------------------------------------
 -                   * end of convert2_2 *                 -
 -----------------------------------------------------------}