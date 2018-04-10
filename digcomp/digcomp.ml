(**
    Main module.

    Usage:
        ./digcomp <file>

    Parses assembly code in <file>, creates files <file>.hi and <file>.lo,
    and fills these files with the high and low parts of the encoding for
    the instructions in <file>.
 *)
open Asm


(** 
    insert l nb assoc adds the entry (l,nb) in the association list assoc.
    @param l label name
    @param nb line number
    @param assoc list of all the already-seen labels with their respective line numbers
    @return an updated association list
    @raise Failure when l belongs to the labels in assoc
 *)
let insert = fun l nb assoc ->
    if List.mem_assoc l assoc
    then
        let nb' = string_of_int (List.assoc l assoc) in
        failwith ("Label "^l^" already defined at line "^nb')
    else
        (l,nb) :: assoc
;;


(**
    parse_file lexbuf (0,0) ([],[]) recursively parses the stream of tokens
    in lexbuf and returns the list of instructions and the list that
    associates each label to its line number.
    @param lexbuf stream of tokens that we have to parse
    @param nl number of lines with assembly code already parsed
    @param nb total number of lines already read from lexbuf
    @param acc1 list of the instructions already parsed, in reverse order
    @param acc2 list of labels with their line numbers
    @return acc1 reversed and acc2
 *)
let rec parse_file = fun lexbuf (nl,nb) (acc1,acc2) ->
    try
        let (l,c) = Parser.main Lexer.token lexbuf in
        let acc2' = if String.length l = 0 then acc2
                    else insert l nb acc2
        in
        match c with
        | None -> parse_file lexbuf (nl+1,nb) (acc1, acc2')
        | Some i -> parse_file lexbuf (nl+1,nb+1) (i::acc1, acc2')
    with
    | Lexer.Eof -> (List.rev acc1, acc2)
    | Parsing.Parse_error ->
            let _ = Printf.fprintf stderr "Parsing error on line %d\n" nl in
            exit 1
;;



(**
    dump_label (l,n) writes l and n to the standard output.
    @param l label name
    @param n line number
*)
let dump_label = fun (l,n) ->
    Printf.printf "%s: %d\n" l n
;;


(**
    write_instr out_hi out_lo labels nb i computes then encoding for i and writes
    the first half to out_hi, and the second half to out_lo.
    @param out_hi output stream for the first byte of the encoding of i
    @param out_lo output stream for the second byte of the encoding of i
    @param labels list that associate a label to its line number
    @param nb current line number
    @param i instruction at line nb
    @return next line number
*)
let write_instr = fun out_hi out_lo labels nb i ->
    let (s1,s2) = instr_to_bin i nb labels in
    let sa = Printf.sprintf "%04x:" nb in
    let _ = output_string out_hi (sa^s1^"\n") in
    let _ = output_string out_lo (sa^s2^"\n") in
    nb+1
;;


(* main function *)
let _ =
    if Array.length Sys.argv != 2 then
        Printf.printf "usage: %s <file>\n" Sys.argv.(0)
    else
        try
            let stream = open_in Sys.argv.(1) in
            let out_hi = open_out (Sys.argv.(1) ^ ".hi") in
            let out_lo = open_out (Sys.argv.(1) ^ ".lo") in
            let lexbuf = Lexing.from_channel stream in
            let (code,labels) = parse_file lexbuf (1,0) ([],[]) in
            let _ = List.iter dump_label labels in
            let _ = List.iter dump_instr code in
            let _ = List.fold_left (write_instr out_hi out_lo labels) 0 code in
            let _ = close_in stream in
            let _ = close_out out_hi in
            let _ = close_out out_lo in
            ()
        with
        | Sys_error msg -> Printf.fprintf stderr "%s\n" msg
;;

