%{
    open Asm
%}


%token <int> INT
%token NOP MOV ADD ADDI SUB SUBI CMPI CMP JMP LD ST IN OUT JLE JLT JEQ JNE
%token COMA COLON LPAR RPAR
%token <int> REG
%token <string> LABEL
%token EOL
%token <int> PRED

%start main     
%type <(Asm.label * Asm.instr option)> main
%type <Asm.cond> cjump

%%

main:
    LABEL COLON EOL              { ($1, None) }
  | EOL                          { ("", None) }
  | LABEL COLON code EOL         { ($1, Some $3) }
  | code EOL                     { ("", Some $1) }
;

code:
  | NOP                                  { Nop }
  | MOV REG COMA INT                     { Ldi ($2,$4) }
  | MOV REG COMA REG                     { Addi ($2,$4,0,false)     }
  | ADD REG COMA REG COMA REG COMA PRED  { assert (0<=$8 && $8<=3); Add  ($2,$4,$6,false,$8) }
  | ADD REG COMA REG COMA REG            { Add  ($2,$4,$6,false,0)  }
  | ADDI REG COMA REG COMA INT           { assert (0<=$6 && $6<32); Addi ($2,$4,$6,false) }
  | ADD REG COMA REG COMA INT            { assert (0<=$6 && $6<32); Addi ($2,$4,$6,false) }
  | ADD REG COMA INT COMA REG            { assert (0<=$4 && $4<32); Addi ($2,$6,$4,false) }
  | SUB REG COMA REG COMA REG COMA PRED  { assert (0<=$8 && $8<=3); Add  ($2,$4,$6,true,$8)  }
  | SUB REG COMA REG COMA REG            { Add  ($2,$4,$6,true,0)   }
  | SUB REG COMA REG COMA INT            { assert (0<=$6 && $6<32); Addi ($2,$4,$6,true)  }
  | SUBI REG COMA REG COMA INT           { assert (0<=$6 && $6<32); Addi ($2,$4,$6,true)  }
  | CMPI REG COMA INT                    { assert (-128<=$4 && $4<128); Cmpi ($2, $4)     }
  | CMP REG COMA REG COMA PRED           { assert (0<=$6 && $6<=3); Cmp ($2, $4, $6)   }
  | CMP REG COMA REG                     { Cmp ($2, $4, 0)    }
  | LD REG COMA REG COMA PRED            { assert (0<=$6 && $6<=3); Load ($2, $4, $6)  }
  | LD REG COMA REG                      { Load ($2, $4, 0)   }
  | MOV REG COMA LPAR REG RPAR COMA PRED { assert (0<=$8 && $8<=3); Load ($2, $5, $8)  }
  | MOV REG COMA LPAR REG RPAR           { Load ($2, $5, 0)   }
  | ST REG COMA REG COMA PRED            { Store ($4, $2, $6) }
  | ST REG COMA REG                      { Store ($4, $2, 0)  }
  | MOV LPAR REG RPAR COMA REG COMA PRED { assert (0<=$8 && $8<=3); Store ($3, $6, $8) }
  | MOV LPAR REG RPAR COMA REG           { Store ($3, $6, 0)  }
  | IN REG                               { In $2  }
  | OUT REG                              { Out $2 }
  | cjump REG COMA REG COMA LABEL        { CJmp ($2,$4,$6,$1) }
  | JMP LABEL                            { Jmp $2 }
;

cjump:
  | JLE  { LE }
  | JLT  { LT }
  | JEQ  { EQ }
  | JNE  { NE }
;
