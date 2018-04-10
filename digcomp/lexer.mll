{
open Parser
exception Eof

let keyword_table = Hashtbl.create 53
let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "nop",  NOP;
      "ldi",  MOV;
      "mov",  MOV;
      "add",  ADD;
      "addi", ADDI;
      "sub",  SUB;
      "subi", SUBI;
      "ld",   LD;
      "st",   ST;
      "in",   IN;
      "out",  OUT;
      "jlt",  JLT;
      "jle",  JLE;
      "je",   JEQ;
      "jeq",  JEQ;
      "jne",  JNE;
      "jmp",  JMP;
      "j",    JMP
    ]

(* @requires: w contains at least one character *)
let token_of_word = fun w ->
    try
        Hashtbl.find keyword_table w
    with Not_found ->
        if String.length w = 2 && w.[0] = 'r'
           && w.[1] >= '0' && w.[1] <= '7'
        then REG (Char.code w.[1] - Char.code '0')
        else LABEL w

}

let ivalue = '-'?['0'-'9']+
let word   = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] *
let comment = '#'[^'\n']*


rule token = parse
    comment        { token lexbuf }     (* skip comments *)
  | [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | ['\n']         { EOL }
  | ivalue as lxm  { INT(int_of_string lxm) }
  | word as lxm    { token_of_word lxm }
  | ':'            { COLON }
  | ','            { COMA }
  | '('            { LPAR }
  | ')'            { RPAR }
  | eof            { raise Eof }
