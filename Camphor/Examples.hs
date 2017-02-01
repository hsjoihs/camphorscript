{-# OPTIONS -Wall  #-}
module Camphor.Examples
(example7
,err1
,example1
,example1'
)where
example7::String
example7 = "mov 0; /*comment +-,.[]><*/ inc; loop; mov 1; output; _input; mov 0; pool;"


err1::String 
err1="#define 1"

example1::String
example1=
 "#ifndef ABC\n"++
 "#define ABC\n"++
 "\n"++
 "#define windows 1\n"++
 "#def(+=~)(char& a;char& b)\n"++
 "  while(b){\n"++
 "   b-=1;a+=1;\n"++
 "   }\n"++
 "#end_def\n"++
 "#define_ del(char& x) clear(x); delete x\n"++
 "char a;\n"++
 "#define C 3\n"++
 "a+=C; \n"++
 "#undef C\n"++
 "char C;\n"++
 "\n"++
 "#ifdef windows\n"++
 "char b;\n"++
 "b+=7;\n"++
 "#endif\n"++
 "\n"++
 "#ifndef windows\n"++
 "char x;\n"++
 "x+=8;\n"++
 "#endif\n"++
 "\n"++
 "#endif\n"
 
example1'::String
example1' = example1++example1