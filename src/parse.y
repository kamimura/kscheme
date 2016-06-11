%{
#include "object.h"
extern void yyerror(char *s);
extern int yylex(void);
%}

%union {
  Object obj;
}
%token
                        TOKEN_SHARP_OP
                        TOKEN_SHARP_U_EIGHT_OP
                        
%token  <obj>           TOKEN_BOOLEAN
                        TOKEN_IDENTIFIER
                        TOKEN_NUMBERZ
                        TOKEN_NUMBERQ
                        TOKEN_NUMBERR
                        TOKEN_NUMBERC
                        TOKEN_CHARACTER
                        TOKEN_STRING_EMPTY
                        TOKEN_STRING_ELEMENT
                        TOKEN_EOF_OBJ                        

%type   <obj>           datum
                        simple_datum
                        symbol
                        compound_datum
                        list
                        abbreviation
                        vector
                        identifier
                        number
                        string
                        bytevector

%%
program:        datum
                {
                  kread_obj = $1;
                  return 1;
                }
        ;
datum:          simple_datum
        |       compound_datum
        ;
simple_datum:   TOKEN_BOOLEAN
        |       number
        |       TOKEN_CHARACTER
        |       string
        |       symbol
        |       TOKEN_SHARP_U_EIGHT_OP bytevector
                {
                  $$ = list2bytevector($2);
                }
        |       TOKEN_EOF_OBJ
        ;
symbol:         identifier
        ;
compound_datum: '(' list
                {
                  $$ = $2;
                }
        |       abbreviation
        |       TOKEN_SHARP_OP vector
                {
                  $$ = list2vector($2);
                }
        ;
list:           ')'
                {
                  $$ = empty;
                }
        |       datum '.' datum ')'
                {
                  $$ = cons($1, $3);
                }
        |       datum list
                {
                  $$ = cons($1, $2);
                }
        ;
abbreviation:   '\'' datum
                {
                  $$ = cons(quote_sym, cons($2, empty));
                }
        ;
vector:         ')'
                {
                  $$ = empty;
                }
        |       datum vector
                {
                  $$ = cons($1, $2);
                }
        ;
identifier:     TOKEN_IDENTIFIER
        ;
number:         TOKEN_NUMBERZ
        |       TOKEN_NUMBERQ
        |       TOKEN_NUMBERR
        |       TOKEN_NUMBERC
        ;
string:         TOKEN_STRING_EMPTY
        |       TOKEN_STRING_ELEMENT string
                {
                  $$ = string_cons($1, $2);
                }
        ;
bytevector:     ')'
                {
                  $$ = empty;
                }
        |       TOKEN_NUMBERZ bytevector
                {
                  $$ = cons($1, $2);
                }
        ;
%%