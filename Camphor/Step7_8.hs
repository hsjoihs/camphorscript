{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Camphor.Step7_8
(Com7(..)
,step7
,step8
,step7_8
,example7
) where


import Text.Parsec
import Control.Applicative hiding ((<|>),many)
import Data.List(genericTake)
import Control.Monad((>=>))



step7 str=convert7 <$> (parse parser7 "step7" str) {-turn into symbols -}
step8 str=concat   <$> (parse parser8 "step8" str) {-removes unnecessary letters-}
step7_8 = step7>=>step8

parser8 = many char'
 where char' = do{x<-oneOf "+-<>[],.";return [x]} <|> do{noneOf "+-<>[],.";return ""}

data Com7=INC|DEC|MOV|LOOP|POOL|IN|OUT|NUL deriving(Show)

parser7::Stream s m Char=>ParsecT s u m [(Com7,String)]
parser7 = many sentences
 where
  sentences = inc<|>dec<|>loop<|>pool<|>mov<|>output<|>input<|>nul<|>comm
  inc    = do{string "inc";spaces;num<-option "1" (many1 digit);spaces;char ';'; return (INC,num)}
  dec    = do{string "dec";spaces;num<-option "1" (many1 digit);spaces;char ';'; return (DEC,num)}
  mov    = do{string "mov";spaces;num<-many1 digit;spaces;char ';'; return (MOV,num)}
  loop   = do{string "loop";spaces;char ';'; return (LOOP,"")}
  pool   = do{string "pool";spaces;char ';'; return (POOL,"")}
  input  = do{string "_input";spaces;char ';'; return (IN,"")} {- "_input" rather than "input" to avoid 'try' -}
  output = do{string "output";spaces;char ';'; return (OUT,"")}
  nul    = do{sp<-many1 space;return (NUL,sp)}
  comm   = do{string "/*";comment<-many(noneOf"*");string "*/";return(NUL,"/*"++(comment>>=escape)++"*/")}
  escape '+' ="_plus_" 
  escape '-' ="_minus_" 
  escape ',' ="_comma_" 
  escape '.' ="_dot_" 
  escape '[' ="{(" 
  escape ']' =")}" 
  escape '>' ="_gt_" 
  escape '<' ="_lt_" 
  escape x = [x]
  
example7 = "mov 0; /*comment +-,.[]><*/ inc; loop; mov 1; output; _input; mov 0; pool;"
example7_= either undefined id$parse parser7 "step7" example7


convert7_::(Integer,[(Com7,String)])->(String,(Integer,[(Com7,String)]))
convert7_ (n,((INC,num):xs)) = ( genericTake(read num)(repeat '+'), (n,xs) )
convert7_ (n,((DEC,num):xs)) = ( genericTake(read num)(repeat '-'), (n,xs) )
convert7_ (n,((LOOP,_):xs)) = ( "[", (n,xs) )
convert7_ (n,((POOL,_):xs)) = ( "]", (n,xs) )
convert7_ (n,((IN,_):xs)) = ( ",", (n,xs) )
convert7_ (n,((OUT,_):xs)) = ( ".", (n,xs) )
convert7_ (n,((MOV,num):xs)) = if(n<=num')then(genericTake(num'-n)(repeat '>'),(num',xs))else(genericTake(n-num')(repeat '<'),(num',xs))
 where num' = read num ::Integer
convert7_ (n,((NUL,sp):xs)) =(sp,(n,xs))

   
convert7'::(Integer,[(Com7,String)])->String

convert7' (_  ,[]) = ""
convert7' x        = fst(convert7_ x) ++ convert7'(snd(convert7_ x))

convert7 x=convert7'(0,x)



  
{-

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b


number  = many1 digit
plus    = char '+' *> number
minus   = (:) <$> char '-' <*> number
integer = plus <|> minus <|> number

float   =(integer <++> decimal <++> exponent)
 where 
  decimal = option "" (char '.' <:> number)
  exponent= option "" (oneOf "eE" <:> integer)
  
  -}