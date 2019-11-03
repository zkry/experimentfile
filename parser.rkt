#lang brag
e-program    : [e-line] (/NEWLINE [e-line])*
@e-line      : e-package-stmt | e-global-var | e-experiment | e-distribution | e-scheduled-distribution
e-experiment : ["deferred"] /"experiment" IDENTIFIER  /":" /NEWLINE
               (
                /INDENT [e-statement] (/NEWLINE [e-statement])* /DEDENT
               )?

;; TODO: change way of writing distributions to fit better with Python-like syntax
e-scheduled-distribution : /"scheduled" /"distribution" /":" /NEWLINE
                           /INDENT
                             e-option*
                             e-time-stmt
                               /INDENT e-distr-case /DEDENT /NEWLINE
                             e-time-stmt
                               (/INDENT e-distr-case /DEDENT [/NEWLINE]  e-time-stmt)*
                           /DEDENT
e-time-stmt     : /"~" (e-date-time | e-relative-time) /"~" /NEWLINE?
e-date-time     : e-date [e-time]
e-relative-time : INTEGER IDENTIFIER

e-distribution : /"distribution" /":" /NEWLINE
                 /INDENT e-distr-case /DEDENT
e-distr-case   : ([e-distr | e-option] /NEWLINE?)+ 
e-distr        : e-number /":" IDENTIFIER (/"when" BT_STRING)?
e-option       : e-id STRING ; possible support for non-string options in future

@e-statement  : e-export-param | e-var-assign | e-subexperiment

e-package-stmt : /"package" e-id

e-global-var    : e-global-id-defn /"=" e-expr
e-export-param  : e-id-defn /":=" e-expr
e-var-assign    : e-id-defn /"=" e-expr
e-subexperiment : /"subexperiment" IDENTIFIER /":" /NEWLINE
                  /INDENT [e-statement] (/NEWLINE [e-statement])* /DEDENT

e-multi-expr : /L-BRACKET (e-for-expr | e-list-expr) /R-BRACKET
e-for-expr   : e-number "to" e-number ["step" e-number]
e-list-expr  : e-expr [/"," e-expr]*

e-expr     : e-sum
e-sum      : [e-sum ("+"|"-")] e-product
e-product  : [e-product ("*"|"/"|"%")] e-neg
e-neg      : ["-"] e-func-app
e-func-app : (e-id /L-PAREN e-expr (/"," e-expr)* /R-PAREN) | e-val
@e-val     : e-number | e-id | e-bool | e-string | /"(" e-expr /")" | e-multi-expr
@e-number  : DECIMAL | INTEGER
e-bool     : BOOLEAN
@e-id      : IDENTIFIER
@e-id-defn : IDENTIFIER
@e-global-id-defn : IDENTIFIER
e-no-ind   : /INDENT* /DEDENT* [/DEDENT /NEWLINE]

@e-string  : STRING

e-date : DATE
e-time : TIME
