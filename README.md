#ErlangEOS ver 0.01e - 2014/03/09(JST)

A dialect of Erlang that uses indentation like Python, and has many improvement like ErlangObjectSystem.

#about ErlangEOS

- ErlangEOS is a acceptable dialect of Erlang, influenced by Python,F#,Haskell,Lisp,Ruby,IO,JavaScript,PHP.
- Whitespace indentation like Python, to delimit blocks rather than ",.; end".
- Functional programming, imperative and object-oriented programming.
- More improved functional programming.
- Integration with "rebar" for ease.

#License
- distributed under BSD LICENSE.
```
Copyright (c) 2014, zed(http://throben.org)
All rights reserved.
```

#online demo
http://throben.org/erleos/erleos_online_compiler.html

#SYSTEM REQUIREMENT

ErlangEOS needs only Erlang/OTP environment.
I'v tested on Mac,FreeBSD,Linux,Windows.

#compile from source

```bash
git clone https://github.com/zed-throben/erlangeos
cd erlangeos
./rebar compile
make install
```

#install binary on Unix system(Mac,FreeBSD,Linux)

```bash
wget http://throben.org/erlangeos/erleos_0.01e.tar.gz
tar xvf erleos_0.01e.tar.gz
cd erleos_0.01e
make install
```

Files will be installed to
  /usr/local/lib/erleos/
  /usr/local/bin/eos

#install binary on Windows

```bash
wget http://throben.org/erlangeos/erleos_0.01e.tar.gz
tar xvf erleos_0.01e.tar.gz
cd erleos_0.01e
make install_cygpath
```

Files will be installed to
  /usr/local/lib/erleos/	(C:\cygwin64\usr\local\lib\erleos\)
  /usr/local/bin/erleos		(C:\cygwin64\usr\local\bin\erl\)


#How to use

##REPL

```bash
eos
```


##create application

```bash
eos new APPNAME
```

ErlangEOS is integrated with "rebar".
This command uses "rebar create-app appid=APPID" internally.

##compile application

###compile .eos to .erl

- eos c
converts ./src/*.eos -> ./src/*.erl

- eos c SRCDIR
converts SRCDIR/*.eos -> SRCDIR/*.erl

###compile .eos to .erl, and rebar compile
- eos b
converts ./src/*.eos -> ./src/*.erl
./rebar compile



##difference between Erlang and ErlangEOS

###Erlang -> ErlangEOS

- module:function(Param) -> module.function(Param)

- /=  ---> !=
- =/= ---> !==
- =:= ---> ===
- =>  ---> >=
- !   ---> not exist, use erlang.send()
- %   ---> //

- $C  ---> \'C'
- 'symbol' ---> :'symbol' or :symbol
- if  ---> cond


##ErlangEOS ---> Erlang

###atom(symbol)

- symbol
- :symbol
- :'symbol'

:symbol is delimited only whitespace and "'()[],
```erlang
    :'192.168.0.2@main' = :192.168.0.2@main
    :'alpha->beta' = :alpha->beta
    :'./-+*=<>%$#&:;' = :./-+*=<>%$#&:;
```

###comments
- //	  line comment
- /* */ block comment

###binary string
- 'binary string'
	---> <<"binary string">>

###formatted string
- @"string with format #{X} #{~2..0B|Y}¥n"
	---> eosstd:fmt("string with format ~s ~2..0B¥n",[erleos:to_str(X),Y}])

- @'string with format #{X} #{~2..0B|Y}¥n'
	---> list_to_binary( eosstd:fmt("string with format ~s ~2..0B¥n",[erleos:to_str(X),Y]) )

###process dictionary as mutable variables
- $a
	---> get('$a')

- $a := value
	---> put('$a',value)

###list range
- [A..B]
	----> lists:seq(A,B)

- [A...B]
	----> lists:seq(A,B-1)

###function call
- symbol.function(param)
	----> module's function call


###if
'if' can be used like so-called general languages. You can use any 'condition' not only 'guard sequences'.
- if CONDITION then A
- if CONDITION then A else B
- if CONDITION then A elif CONTITION then B else C

###lambda

```erlang
    fun(X,Y)->AA,BB end
    fun(X,Y)->
        AA
        BB
    (¥ (X,Y)->AA,BB)
```

###pipeline
- X |> function(P1,P2) ---> function(P1,P2,X)
- function(P1,P2) <| ---> function(X,P1,P2)
- function(P1,P2) do block
	----> function(P1,P2,fun()->block end)
- function(P1,P2) do |X,Y| block
	----> function(P1,P2,fun(X,Y)->block end)

###do
do is the special case of pipeline

```erlang
function(PARAM) do BLOCK
```
is the same as
```erlang
function(PARAM) <| fun()->BLOCK
```

and

```erlang
function(PARAM) do |X,Y| BLOCK
```
is the same as
```erlang
function(PARAM) <| fun(X,Y)->BLOCK
```

ex.
```erlang
    [6,4,2] = lists.reverse( lists.map( fun(X)->X*2 end,[1,2,3] ) )
    [6,4,2] = lists.reverse <| lists.map( fun(X)->X*2 end,[1,2,3] )
    [6,4,2] = lists.reverse <| lists.map( (\ (X)->X*2 ),[1,2,3] )
    [6,4,2] = lists.reverse <| lists.map( [1,2,3] ) do |X| X*2
    [6,4,2] = lists.map( [1,2,3] ) do |X| X*2
              |> lists.reverse
```


###indentation of list,tuple,param,record
You can omit "," by indentation.
```python
[1,2,3]
  [1
   2
   3]

function(Param1,Param2,Param3)
function(
  Param1
  Param2
  Param3)

{1,2,3}
{1
 2
 3}
```


###while loop
```python
    $a := 0
    $sum := 0
    {ok,4} = while $a < 10 do
                $a := $a + 1
                io.format( @"No.#{$a}\n" )
                if ($a rem 2) == 0 then
                    io.format( @"No.#{$a} continue\n" )
                    continue()
                elif $a >= 5 then
                    io.format( @"No.#{$a} break\n" )
                    break({ok,$sum})

                $sum := $sum + $a
```

###haskell like type definition

function_name::Type->Type
The first character of the type name start with an uppercase letter.
- Int ---> int()
- ABCType ---> abctype()

- funcation::Unit->Float
---> - spec function() -> float().

- bmiTell::Float->Float->Float
---> - spec bmiTell(float(),float()) -> float().


###haskell like "where"

"where" creates local variable scope in previous block.

```python
    30 = A+B
         where A=10
               B=20
```

###OCaml like "let"

"let" creates local variable scope.

```python
    let A = 10 in
    let B = 20 in
        A+B

    let A = 10
        B = 20 in
        A+B

    let A = 10
        B = 20
    in
    A+B
```

###module name
If source file has no module definition, ErlangEOS adds "-module(FILENAME)." into .erl file automatically.

###function exports
If source file has no export definition, ErlangEOS adds "-compile(export_all)." into .erl file automatically.



##ErlangObjectSystem

###EOS function call
- VARIABLE.function(param)
- LIST.function(param)
- TUPLE.function(param)
	----> ErlangObjectSystem's function call

- {eos,Module,ObjParam}.function(Param)
    ----> erlang:apply(Module,function,[ObjParam]++Param)

###EOS object initialization

```erlang
    #<TYPE>{
      key=value
      key=value
    }
```
---> eos:new('eos@TYPE',[{key,value},{key,value}])


###object using Erlang's process

```erlang
    Dog = #<obj>{
        barkPhrase = "woof!"
        bark = method()-> io.format( @"#{This.barkPhrase}\n" )
    }

    Chiwawa = Dog.clone()
    Chiwawa.barkPhrase := "yip!"

    io.format("Dog bark: ")
    Dog.bark()

    io.format("Chiwawa bark: ")
    Chiwawa.bark()

    // make an instance
    MyChiwawa = Chiwawa.clone()
    MyChiwawa.barkPhrase := "Yo Quiero Taco Bell"

    io.format("myChiwawa bark: ")
    MyChiwawa.bark()
```

###object using ETS
```erlang
    Dog = #<etsobj>{
        barkPhrase = "woof!"
        bark = method()-> io.format( @"#{This.barkPhrase}\n" )
    }

    Chiwawa = Dog.clone()
    Chiwawa.barkPhrase := "yip!"

    io.format("Dog bark: ")
    Dog.bark()

    io.format("Chiwawa bark: ")
    Chiwawa.bark()

    // make an instance
    MyChiwawa = Chiwawa.clone()
    MyChiwawa.barkPhrase := "Yo Quiero Taco Bell"

    io.format("myChiwawa bark: ")
    MyChiwawa.bark()
```


###EOS member access

{eos,Module,ObjParam}.member
    ---> erlang:apply(Module,get_slot,[ObjParam,member])

{eos,Module,ObjParam}.member := VALUE
    ---> erlang:apply(Module,set_slot,[ObjParam,member,VALUE])

###RPC using EOS
```erlang
    RPC = #<rpc>{
      node=:192.168.0.2@main
      module=main
    }
RPC.hello(world)
```
is similer to rpc:call('192.168.0.2@main',main,hello,[world])

###dictionary,proplists with EOS

```erlang
    PropList = [
        {a,alpha}
        {b,beta}
        {c,gamma}
    ]

    3 = PropList.length()
    alpha = PropList.a
    beta  = PropList.b
    gamma = PropList.c

    Dict = dict.from_list(PropList)
    3 = Dict.length()
    alpha = Dict.a
    beta  = Dict.b
    gamma = Dict.c

    //

    PropList2 = #[
        a=alpha
        b=beta
        c=gamma
    ]

    PropList = PropList2

    Dict2 = #<dict>{
        a=alpha
        b=beta
        c=gamma
    }

    Dict = Dict2
    //

    Dict3 = Dict.a := aaa
    Dict4 = Dict3.d := ddd
    aaa = Dict4.a
    ddd = Dict4.d

    Dict5 = Dict4.map() do |Key,Value|
                            @"#{Key} -> #{Value}"
    "a -> aaa" = Dict5.a
    "b -> beta" = Dict5.b
    "c -> gamma" = Dict5.c
    "d -> ddd" = Dict5.d
```


##coming later
- monad,computation expression
- lisp like macro

#samples
check it out "erlangeos/test/" and "erlangeos/sample/".


#author,homepage
- http://throben.org/ (currently, written in Japanese only)
- twitter: @zed_throben https://twitter.com/zed_throben





