-module(erleos_keyword).
-compile(export_all).

is_keyword('of')->true;
is_keyword('in')->true;
is_keyword('do')->true;
is_keyword('case')->true;
is_keyword('if')->true;
is_keyword('then')->true;
is_keyword('elif')->true;
is_keyword('else')->true;
is_keyword('cond')->true;
is_keyword('when')->true;
is_keyword('begin')->true;
is_keyword('end')->true;
is_keyword('fun')->true;
is_keyword('method')->true;
is_keyword('member')->true;
is_keyword('receive')->true;
is_keyword('after')->true;
is_keyword('try')->true;
is_keyword('catch')->true;
is_keyword('where')->true;
is_keyword('let')->true;
%is_keyword('local')->true;
is_keyword('isolate')->true;
is_keyword('monad')->true;

is_keyword('bnot')->true;
is_keyword('div')->true;
is_keyword('rem')->true;
is_keyword('band')->true;
is_keyword('bor')->true;
is_keyword('bxor')->true;
is_keyword('bsl')->true;
is_keyword('bsr')->true;

is_keyword('not')->true;
is_keyword('and')->true;
is_keyword('or')->true;
is_keyword('xor')->true;

is_keyword('while')->true;
is_keyword('repeat')->true;
is_keyword('for')->true;
is_keyword('foreach')->true;

is_keyword(X)->false.
