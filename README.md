# CamphorScript
A C-like language designed to help write brainf*ck

## Documentations
[Syntactic structures (English)](https://hsjoihs.github.io/camphorscript/docs/preprocessed_camphorscript.html) (Possibly outdated)  
[Library (English)](https://hsjoihs.github.io/camphorscript/docs/library_en.htm)  

[Rationales (Japanese)](https://hsjoihs.github.io/camphorscript/docs/Rationales.htm)  
[Library (Japanese)](https://hsjoihs.github.io/camphorscript/docs/library.htm)  
[Tutorial (Japanese)](https://hsjoihs.github.io/camphorscript/docs/camphorscript_tutorial_old.html) (Possibly outdated)

## Getting Started

### Prerequisites

- Haskell Platform (>= 7.10.2)

### Installing
#### Windows
- clone the repo
- execute `!exec.bat`
- run `ghc --make ccsc`
- run `ghc --make ccsrc`

#### Linux
clone the repo and run:
```
$ make
```


## Examples

CamphorScript
```
#include <stdcalc>
#include <div>
char a;char b; char c; char d;
read(a);
b += 48; c += 48; d += 48;
sprint(b,c,d + <-~ a);
write(b);write(c);write(d);
```

↓compile (with:  
Windows: `ccsc -C18 examples\__CS\xShow__CS.txt -o con`  
Linux: `$ ./ccsc -C18 examples/__CS/xShow__CS.txt -o con`  
)

Brainf*ck
```
,>++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++++++++++++++++++<<<[[>>>>>+>>>>>+<<<<<<<<<<-]>>>>>>>>>>[<<<<<<<<<<+>>>>>>>>>>-]++++++++++[<<<<<<<<<<[>>>>>>>>>>>+>+<<<<<<<<<<<<-]>>>>>>>>>>>>[<<<<<<<<<<<<+>>>>>>>>>>>>-]<[[-]<<<<<<<<<<<->>>>>>>>>>>]<-]<<<<<<<<<<[>>>>>>>>>>+>+<<<<<<<<<<<-]>>>>>>>>>>>[<<<<<<<<<<<+>>>>>>>>>>>-]<[<+<<<+>>>>-]<<<+>>[[-]<<->>]<<<[[-]<<+>>]>[-<<[<<+>>>>>>>+<<<<<-]>>>>>[<<<<<+>>>>>-]<<<]<<[-]<<<<<]>>>>[[>+>>>>>+<<<<<<-]>>>>>>[<<<<<<+>>>>>>-]++++++++++[<<<<<<[>>>>>>>+>+<<<<<<<<-]>>>>>>>>[<<<<<<<<+>>>>>>>>-]<[[-]<<<<<<<->>>>>>>]<-]<<<<<<[>>>>>>+>+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[<+<<<+>>>>-]<<<+>>[[-]<<->>]<<<[[-]<<<<<+>>>>>]>[-<<[<<<+>>>>>>>>+<<<<<-]>>>>>[<<<<<+>>>>>-]<<<]<<[-]<]<<<.>.>.
```

<br>
<br>
<br>

Brainf*ck
```
+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+.
```

↓decompile (with:  
Windows: `ccsrc -C84 examples\__BF_c\xHello__BF_c.txt -o con`  
Linux: `$ ./ccsrc -C84 examples/__BF_c/xHello__BF_c.txt -o con`  
)

```
char v_0;char v_1;char v_2;char v_3;
v_0+=9;
while(v_0){
	v_1+=8;
	v_2+=11;
	v_3+=5;
	v_0-=1;
}
write(v_1);
v_2+=2;write(v_2);
v_2+=7;write(v_2);
write(v_2);
v_2+=3;write(v_2);
v_3-=1;write(v_3);
v_3-=12;write(v_3);
v_2+=8;write(v_2);
v_2-=8;write(v_2);
v_2+=3;write(v_2);
v_2-=6;write(v_2);
v_2-=8;write(v_2);
v_3+=1;write(v_3);
```
<br>
<br>
<br>
CamphorScript
```
char a;char b;char c;char d;char e;char f;char g;char h;char i;char j;
b+=10;while(b){a+=10;b-=1;}e+=3;f+=5;while(a){h+=1;while(h){c+=1;b+=1;
h-=1;}while(c){h+=1;c-=1;}c+=10;while(b){c-=1;b-=1;}d+=1;while(c){
while(d){d-=1;}while(c){c-=1;}}while(d){while(h){h-=1;}i+=1;d-=1;}
while(i){c+=1;b+=1;i-=1;}while(c){i+=1;c-=1;}c+=10;while(b){c-=1;b-=1;}
d+=1;while(c){while(d){d-=1;}while(c){c-=1;}}while(d){while(i){i-=1;}
j+=1;d-=1;}g+=1;e-=1;while(e){c+=1;b+=1;e-=1;}while(b){e+=1;b-=1;}d+=1;
while(c){while(d){d-=1;}while(c){c-=1;}}while(d){e+=3;c+=7;while(c){
b+=10;c-=1;}write(b);c+=5;while(c){b+=7;c-=1;}write(b);c+=3;while(c){
b+=5;c-=1;}b+=2;write(b);write(b);while(b){b-=1;}while(g){g-=1;}d-=1;}
f-=1;while(f){c+=1;b+=1;f-=1;}while(c){f+=1;c-=1;}d+=1;while(b){while(
d){d-=1;}while(b){b-=1;}}while(d){f+=5;c+=8;while(c){b+=8;c-=1;}b+=2;
write(b);c+=6;while(c){b+=8;c-=1;}b+=3;write(b);b+=5;write(b);write(b);
while(b){b-=1;}while(g){g-=1;}d-=1;}while(g){j+=48;write(j);j-=48;i+=48;
write(i);i-=48;h+=48;write(h);h-=48;g-=1;}c+=4;while(c){b+=11;c-=1;}
write(b);c+=3;while(c){b-=4;c-=1;}write(b);while(b){b-=1;}a-=1;}
```

↓execute directly (with:  
Windows: `ccsc -run examples\__CS\xFizzbuzz_compressed__CS.txt`  
Linux: `$ ./ccsc -run examples/__CS/xFizzbuzz_compressed__CS.txt`  
)

```
001, 002, Fizz, 004, Buzz, Fizz, 007, 008, Fizz, Buzz, 011, Fizz, 013, 014, FizzBuzz, 016, 017, Fizz, 019, Buzz, Fizz, 022, 023, Fizz, Buzz, 026, Fizz, 028, 029, FizzBuzz, 031, 032, Fizz, 034, Buzz, Fizz, 037, 038, Fizz, Buzz, 041, Fizz, 043, 044, FizzBuzz, 046, 047, Fizz, 049, Buzz, Fizz, 052, 053, Fizz, Buzz, 056, Fizz, 058, 059, FizzBuzz, 061, 062, Fizz, 064, Buzz, Fizz, 067, 068, Fizz, Buzz, 071, Fizz, 073, 074, FizzBuzz, 076, 077, Fizz, 079, Buzz, Fizz, 082, 083, Fizz, Buzz, 086, Fizz, 088, 089, FizzBuzz, 091, 092, Fizz, 094, Buzz, Fizz, 097, 098, Fizz, Buzz, 
```
