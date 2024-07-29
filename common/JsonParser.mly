%{
open Json
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token COLON
%token COMMA
%token EOF

%start <Json.json> value
%%

value:
| LBRACE sep_pairs RBRACE
  { Object $2 }
| LBRACKET sep_values RBRACKET
  { Array $2 }
| INT
  { Number (Float.of_int $1) }
| FLOAT
  { Number $1 }
| STRING
  { String $1 }
| TRUE
  { Bool true }
| FALSE
  { Bool false }
| NULL
  { Null }

sep_values:
| { [] }
| value
  { [$1] }
| value COMMA sep_values
  { $1 :: $3 }

sep_pairs:
| { [] }
| pair
  { [$1] }
| pair COMMA sep_pairs
  { $1 :: $3 }

pair:
| STRING COLON value
  { ($1, $3) }
