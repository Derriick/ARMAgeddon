(**
    Module providing types and functions to handle assembly code.
 *)


(**
    Type for the conditions in conditional jumps
 *)
type cond = LT | LE | EQ | NE


(**
    Type for the name of labels
 *)
type label = string



(**
    Type for one assembly instruction:

    {ul 
      {- rd = register where we will store the result of the operation}
      {- rs = first source register}
      {- rt = second source register}
      {- immX = immediate integer value on X bits}
      {- uimmX = immediate unsigned-integer value on X bits}
    }
 *)
type instr =
      Nop
    | Ldi   of (int * int)   (** rd, imm8 *)
    | Add   of (int * int * int * bool) (** rd, rs, rt, sub? *)
    | Addi  of (int * int * int * bool) (** rd, rs, imm5, sub? *)
    | Load  of (int * int) (** rd, rs *)
    | Store of (int * int) (** rs, rd *)
    | In    of int         (** rd *)
    | Out   of int         (** rs *)
    | CJmp  of (int * int * label * cond) (** rs, rt, addr, cond *)
    | Jmp   of label (** label name *)
    


(**
    dump_instr i pretty-prints instruction i to the standard output.  
    @param i  some instruction i
 *)
let dump_instr = fun i -> match i with
| Nop -> Printf.printf ".\n"
| Ldi (r,v) -> Printf.printf "r%d <- %d\n" r v
| Add (rd,rs,rt,b) ->
        let c = if b then '-' else '+' in
        Printf.printf "r%d <- r%d %c r%d\n" rd rs c rt
| Addi (rd,rs,v,b) ->
        let c = if b then '-' else '+' in
        Printf.printf "r%d <- r%d %c %d\n" rd rs c v
| Load (rd, rs) ->
        Printf.printf "r%d <- MEM[r%d]\n" rd rs
| Store (rs, rd) ->
        Printf.printf "MEM[r%d] <- r%d\n" rs rd
| In rd ->
        Printf.printf "r%d <- getchar()\n" rd
| Out rs ->
        Printf.printf "putchar(r%d)\n" rs
| CJmp (rs,rt,lbl,cd) ->
        let op = match cd with
                 | LT -> "<"
                 | LE -> "<="
                 | EQ -> "="
                 | NE -> "!="
        in
        Printf.printf "if r%d %s r%d: goto %s\n" rs op rt lbl
| Jmp lbl ->
        Printf.printf "goto %s\n" lbl


(*
 * requires: 0 < s < 30
 *)
let imm_of_int = fun s v ->
    let m = (1 lsl s) in      (* s-bit encoding = computing modulo m *)
    let v' = (v mod m) in     (* v' = v [m] and v' in [-m,m] *)
    let v'' = v'+m in         (* v'' = v [m] and v'' >= 0 *)
    v'' mod m


(* Type 1a: [xxx] [xx] [xxx]    [xxx] [xxx] 00
 *           cat  flags rd       rs    rt
 *)
let instr_to_bin_type1 = fun c f1 f2 r1 r2 r3 ->
    let hi = (c lsl 5) + (f1 lsl 4) + (f2 lsl 3) + r1 in
    let lo = (r2 lsl 5) + (r3 lsl 2) in
    (Printf.sprintf "%02x" hi, Printf.sprintf "%02x" lo)

(* Type 1b: [xxx] [xx] [xxx]    [xxx] [xxxxx]
 *           cat  flags rd       rs     imm5
 *)
let instr_to_bin_type1b = fun c f1 f2 r1 r2 v ->
    let hi = (c lsl 5) + (f1 lsl 4) + (f2 lsl 3) + r1 in
    let lo = (r2 lsl 5) + (imm_of_int 5 v) in
    (Printf.sprintf "%02x" hi, Printf.sprintf "%02x" lo)


(* Type 2: [xxx] [xx] [xxx]      [xxxxxxxx]
 *          cat  flags rd           imm8
 *)
let instr_to_bin_type2 = fun c f1 f2 r v ->
    let hi = (c lsl 5) + (f1 lsl 4) + (f2 lsl 3) + r in
    let lo = imm_of_int 8 v in
    (Printf.sprintf "%02x" hi, Printf.sprintf "%02x" lo)

(* Type 3: [xxx] [xxxxx           xxxxxxxx]
 *          cat             imm13
 *)
let instr_to_bin_type3 = fun c v ->
    let v' = imm_of_int 13 v in
    let hi = (c lsl 5) + (v' lsr 8) in
    let lo = v' mod 256 in
    (Printf.sprintf "%02x" hi, Printf.sprintf "%02x" lo)



(**
    instr_to_bin i nb assoc computes the encoding of instruction i.
    @param i instruction to encode
    @param caddr line number of instruction i
    @param assoc list that associates a label to its line number
    @return a pair of strings (s1,s2), where
    {ul
      {- s1 = hexadecimal code that can be loaded in the "high" SRAM}
      {- s2 = hexadecimal code that can be loaded in the "low" SRAM}
    }
*)
let instr_to_bin = fun i caddr assoc ->
    match i with
    | Nop ->
            instr_to_bin_type2 0b000 0 0 0 0
    | Ldi (r,v) ->
            instr_to_bin_type2 0b000 0 1 r v
    | Add (rd,rs,rt,b) ->
            let f1 = if b then 1 else 0 in
            instr_to_bin_type1 0b010 f1 1 rd rs rt
    | Addi (rd,rs,v,b) ->
            let f1 = if b then 1 else 0 in
            instr_to_bin_type1b 0b010 f1 0 rd rs v
    | Load (rd,rs) ->
            instr_to_bin_type1b 0b100 0 1 rd rs 0
    | Store (rs,rd) ->
            instr_to_bin_type1b 0b100 0 0 rd rs 0
    | In rd ->
            instr_to_bin_type2 0b100 1 1 rd 0
    | Out rs ->
            instr_to_bin_type2 0b100 1 0 rs 0
    | CJmp (rs,rt,lbl,cd) ->
            let (f1,f2) = match cd with
                          | EQ -> (0,0)
                          | LE -> (0,1)
                          | LT -> (1,0)
                          | NE -> (1,1)
            in
            let addr = List.assoc lbl assoc in
            let v = addr - caddr - 1 in
            if -16 <= v && v <= 15 then
                instr_to_bin_type1b 0b110 f1 f2 rs rt v
            else
                failwith ("Conditionnal jump: target ("
                          ^ lbl ^ ":" ^ string_of_int addr
                          ^ ") is too far from current address ("
                          ^ string_of_int caddr ^ ")")
    | Jmp lbl ->
            let addr = List.assoc lbl assoc in
            instr_to_bin_type3 0b111 addr

