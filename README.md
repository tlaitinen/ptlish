ptlish
======

DSL for functional embedded device operational specifications using integer arithmetic and past time operators.

Example:
```
changed? := \x -> x /= old x;

changedTo? := \what -> \to -> changed? what && what == to;

ioEvent? := \n -> \m -> \v -> 
    (input:n & m) == m 
     && (input:n & m) == m;

button1Pressed := ioEvent? 1 512 1;     
button1Released := ioEvent? 1 512 0;

timeWhenPressed := time: when button1Pressed;
timeWhenReleased := time: when button1Released;

pressed3Seconds := timeWhenReleased - timeWhenPressed > 3;
pressed3Seconds -> alarm 1; 

-- single line comment
{- comment block in single line -}
```

Grammar:
```

statements:
          | statements stmt

stmt: id ":=" expr ";"
    | id actions ";"

actions: action
       | actions action

action: "->" id exprs

exprs: expr
     | exprs "," expr

expr: "not" expr
    | "abs" expr 
    | "old" expr
    | "~" expr 
    | expr "&&" expr 
    | expr "||" expr 
    | expr "<" expr 
    | expr ">" expr 
    | expr ">=" expr 
    | expr ">=" expr 
    | expr "==" expr 
    | expr "/=" expr 
    | expr "+" expr 
    | expr "-" expr 
    | expr "*" expr 
    | expr "/" expr 
    | expr "mod" expr 
    | expr "&" expr 
    | expr "|" expr 
    | expr "<<" expr 
    | expr ">>" expr 
    | expr "^" expr 
    | expr "when" expr 
    | id ":" maybeExpr 
    | id 
    | "(" expr ")"
    | expr expr  
    | "\" id "->" expr  
    | int 

maybeExpr: 
         | expr
```



