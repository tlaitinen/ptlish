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
```

Grammar:
```

statements:
          | statements stmt

stmt: id def expr semicolon 
    | id actions semicolon

actions: action
       | actions action

action: rarrow id exprs

exprs: expr
     | exprs comma expr

expr: not expr
    | abs expr 
    | old exp
    | tilde expr 
    | expr and expr 
    | expr or expr 
    | expr lt expr 
    | expr gt expr 
    | expr le expr 
    | expr ge expr 
    | expr equals expr 
    | expr ne expr 
    | expr plus expr 
    | expr minus expr 
    | expr asterisk expr 
    | expr slash expr 
    | expr mod expr 
    | expr amp expr 
    | expr pipe expr 
    | expr shl expr 
    | expr shr expr 
    | expr caret expr 
    | expr when expr 
    | id colon expr 
    | id 
    | lparen expr rparen 
    | expr expr  
    | backslash id rarrow expr  
    | int 
```



