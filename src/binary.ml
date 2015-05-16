open Printf
open Utils

type byte = int

exception OutOfRange
exception InvalidMode

(* pop word from stream according to little-endian *)
let popw stream =
    try
        ((List.nth stream 0) + ((List.nth stream 1) lsl 8),
        ExtList.List.drop 2 stream)
    with Failure("nth") ->
        raise OutOfRange

(* read word from stream according to little-endian *)
let rec readw i stream =
    if i = 0 then
        try
            (List.nth stream 0) + ((List.nth stream 1) lsl 8)
        with e -> raise e
    else
        match stream with
        | _::tl -> readw (i-1) tl
        | [] -> raise OutOfRange

(* interpreter object of byte code *)
class interpreter (pc: int) (stream: byte list) =
    object(self)
        val inst = fst (popw stream)
        val mutable pc = pc
        val mutable bytecode = [fst (popw stream)]
        val mutable residue = snd (popw stream)
        method residue = residue
        method addressing ?(fp=false) mode =
            let oprand ?(deferred=true) m =
                let i = (m land 7) in
                if i = 7 then
                    let (v,code) = popw residue in
                    pc <- pc + 2;
                    bytecode <- bytecode @ [v];
                    residue <- code;
                    match (m lsr 3) land 7 with
                    | 2 -> sprintf "$%s"  (signedo v)
                    | 3 -> sprintf "*#%d" (signedw v)
                    | 6 -> string_of_int  ((signedw v) + pc + 2)
                    | 7 -> sprintf "*%d"  (signedw v)
                    | _ -> raise InvalidMode
                else if i = 6 then
                    "sp"
                else sprintf (if fp && not deferred then "fr%d" else "r%d") i
            in
            if (mode land 7) = 7 then
                match (mode lsr 3) land 7 with
                | 0 -> "pc"
                | 1 -> "(pc)"
                | 4 -> "-(pc)"
                | 5 -> "*-(pc)"
                | _ -> oprand mode
            else
                match (mode lsr 3) land 7 with
                | 0 -> sprintf "%s"     (oprand mode ~deferred:false)
                | 1 -> sprintf "(%s)"   (oprand mode)
                | 2 -> sprintf "(%s)+"  (oprand mode)
                | 3 -> sprintf "*(%s)+" (oprand mode)
                | 4 -> sprintf "-(%s)"  (oprand mode)
                | 5 -> sprintf "*-(%s)" (oprand mode)
                | 6 -> begin
                    let (v,code) = popw residue in
                    pc <- pc + 2;
                    residue <- code;
                    bytecode <- bytecode @ [v];
                    sprintf "%s(%s)" (signedo v) (oprand mode)
                end
                | 7 -> sprintf "*X(%s)" (oprand mode)
                | _ -> raise InvalidMode

        method single_op_inst op inst =
            let dst = self#addressing inst in
            sprintf "%s\t%s" op dst

        method single_reg_inst op inst =
            let reg = self#addressing (inst land 0b111) in
            sprintf "%s\t%s" op reg

        method double_op_inst op inst =
            let src = self#addressing (inst lsr 6) in
            let dst = self#addressing inst in
            sprintf "%s\t%s, %s" op src dst

        method eis_inst ?(dst=false) op inst =
            let reg = self#addressing ((inst lsr 6) land 7) in
            let src = self#addressing inst in
            if dst then
                sprintf "%s\t%s, %s" op reg src
            else
                sprintf "%s\t%s, %s" op src reg

        method branch_insr op inst =
            let offset = (inst land 0xff) in
            sprintf "%s\t%x" op ((signedb offset) * 2 + pc + 2)

        method halt = "halt"
        method wait = "wait"
        method rti = "rti"
        method bpt = "bpt"
        method iot = "iot"
        method reset = "reset"
        method rtt = "rtt"
        method rts = self#single_reg_inst "rts" inst
        method nop = "nop"
        method clc = "clc"
        method clv = "clv"
        method clz = "clz"
        method cln = "cln"
        method ccc = "ccc"
        method sec = "sec"
        method sev = "sev"
        method sez = "sez"
        method sen = "sen"
        method scc = "scc"
        method jmp = self#single_op_inst "jmp" inst
        method swab = self#single_op_inst "swab" inst
        method mark = self#single_op_inst "mark" inst
        method mfpi = self#single_op_inst "mfpi" inst
        method mtpi = self#single_op_inst "mtpi" inst
        method sxt = self#single_op_inst "sxt" inst
        method clr = self#single_op_inst (if (inst lsr 15) == 1 then "clrb" else "clr") inst
        method com = self#single_op_inst (if (inst lsr 15) == 1 then "comb" else "com") inst
        method inc = self#single_op_inst (if (inst lsr 15) == 1 then "incb" else "inc") inst
        method dec = self#single_op_inst (if (inst lsr 15) == 1 then "decb" else "dec") inst
        method neg = self#single_op_inst (if (inst lsr 15) == 1 then "negb" else "neg") inst
        method adc = self#single_op_inst (if (inst lsr 15) == 1 then "adcb" else "adc") inst
        method sbc = self#single_op_inst (if (inst lsr 15) == 1 then "sbcb" else "sbc") inst
        method tst = self#single_op_inst (if (inst lsr 15) == 1 then "tstb" else "tst") inst
        method ror = self#single_op_inst (if (inst lsr 15) == 1 then "rorb" else "ror") inst
        method rol = self#single_op_inst (if (inst lsr 15) == 1 then "rolb" else "rol") inst
        method asr' = self#single_op_inst (if (inst lsr 15) == 1 then "asrb" else "asr") inst
        method asl = self#single_op_inst (if (inst lsr 15) == 1 then "aslb" else "asl") inst
        method emt = "emt"
        method trap = "trap"
        method br = self#branch_insr "br" inst
        method bne = self#branch_insr "bne" inst
        method beq = self#branch_insr "beq" inst
        method bge = self#branch_insr "bge" inst
        method blt = self#branch_insr "blt" inst
        method bgt = self#branch_insr "bgt" inst
        method ble = self#branch_insr "ble" inst
        method bpl = self#branch_insr "bpl" inst
        method bmi = self#branch_insr "bmi" inst
        method bhi = self#branch_insr "bhi" inst
        method blos = self#branch_insr "blos" inst
        method bvc = self#branch_insr "bvc" inst
        method bvs = self#branch_insr "bvs" inst
        method bcc = self#branch_insr "bcc" inst
        method bcs = self#branch_insr "bcs" inst
        method jsr = self#eis_inst "jsr" inst ~dst:true
        method mul = self#eis_inst "mul" inst
        method div = self#eis_inst "div" inst
        method ash = self#eis_inst "ash" inst
        method ashc = self#eis_inst "ashc" inst
        method xor = self#eis_inst "xor" inst ~dst:true
        method sob = self#eis_inst "sob" inst
        method add = self#double_op_inst "add" inst
        method sub = self#double_op_inst "sub" inst
        method mov = self#double_op_inst (if (inst lsr 15) == 1 then "movb" else "mov") inst
        method cmp = self#double_op_inst (if (inst lsr 15) == 1 then "cmpb" else "cmp") inst
        method bit = self#double_op_inst (if (inst lsr 15) == 1 then "bitb" else "bit") inst
        method bic = self#double_op_inst (if (inst lsr 15) == 1 then "bicb" else "bic") inst
        method bis = self#double_op_inst (if (inst lsr 15) == 1 then "bisb" else "bis") inst

        method interpret =
            let asm = if ((inst lsr 12) land 15) = 0o17 then self#fp else
            match inst with
            | 0 -> self#halt
            | 1 -> self#wait
            | 2 -> self#rti
            | 3 -> self#bpt
            | 4 -> self#iot
            | 5 -> self#reset
            | 6 -> self#rtt
            | _ -> begin
                match (inst lsr 3) with
                | 0o00020 -> self#rts
                | _ -> begin
                    match (inst lsr 5) with
                    | 2 -> begin
                        match (inst land 0o77) with
                        | 0o40 -> self#nop
                        | 0o41 -> self#clc
                        | 0o42 -> self#clv
                        | 0o44 -> self#clz
                        | 0o50 -> self#cln
                        | 0o57 -> self#ccc
                        | 0o61 -> self#sec
                        | 0o62 -> self#sev
                        | 0o64 -> self#sez
                        | 0o70 -> self#sen
                        | 0o77 -> self#scc
                        | _ -> "[unknown cond-op]"
                    end
                    | _ -> begin
                        match (inst lsr 6) land 0o1777 with
                        | 0o0001 -> self#jmp 
                        | 0o0003 -> self#swab
                        | 0o0064 -> self#mark
                        | 0o0065 -> self#mfpi
                        | 0o0066 -> self#mtpi
                        | 0o0067 -> self#sxt 
                        | _ -> begin
                            match (inst lsr 6) land 0o777 with
                            | 0o050 -> self#clr 
                            | 0o051 -> self#com 
                            | 0o052 -> self#inc 
                            | 0o053 -> self#dec 
                            | 0o054 -> self#neg 
                            | 0o055 -> self#adc 
                            | 0o056 -> self#sbc 
                            | 0o057 -> self#tst 
                            | 0o060 -> self#ror 
                            | 0o061 -> self#rol 
                            | 0o062 -> self#asr'
                            | 0o063 -> self#asl 
                            | _ -> begin
                                match (inst lsr 7) with
                                | 0b10001000 -> self#emt
                                | 0b10001001 -> self#trap
                                | _ -> begin
                                    match (inst lsr 8) with
                                    | 1 -> self#br 
                                    | 2 -> self#bne
                                    | 3 -> self#beq
                                    | 4 -> self#bge
                                    | 5 -> self#blt
                                    | 6 -> self#bgt
                                    | 7 -> self#ble
                                    | 128 -> self#bpl 
                                    | 129 -> self#bmi 
                                    | 130 -> self#bhi 
                                    | 131 -> self#blos
                                    | 132 -> self#bvc 
                                    | 133 -> self#bvs 
                                    | 134 -> self#bcc 
                                    | 135 -> self#bcs 
                                    | _ -> begin
                                        match (inst lsr 9) land 0o177 with
                                        | 0o004 -> self#jsr 
                                        | 0o070 -> self#mul 
                                        | 0o071 -> self#div 
                                        | 0o072 -> self#ash 
                                        | 0o073 -> self#ashc
                                        | 0o074 -> self#xor 
                                        | 0o077 -> self#sob 
                                        | _ -> begin
                                            match (inst lsr 12) land 0o17 with
                                            | 0o06 -> self#add
                                            | 0o16 -> self#sub
                                            | _ -> begin
                                                match (inst lsr 12) land 0o7 with
                                                | 0o1 -> self#mov
                                                | 0o2 -> self#cmp
                                                | 0o3 -> self#bit
                                                | 0o4 -> self#bic
                                                | 0o5 -> self#bis
                                                | _ -> sprintf ".word\t%o" inst
                                            end
                                        end
                                    end
                                end
                            end
                        end
                    end
                end
            end
            in (bytecode, asm)

        method f1_inst ?(store=false) op inst =
            let ac = self#addressing ((inst lsr 6) land 3) ~fp:true in
            let fsrc = self#addressing (inst land 0o77) ~fp:true in
            if store then
                sprintf "%s\t%s, %s" op ac fsrc
            else
                sprintf "%s\t%s, %s" op fsrc ac

        method f2_inst op inst =
            let target = self#addressing (inst land 0o77) ~fp:true in
            sprintf "%s\t%s" op target

        method f3_inst ?(store=false) op inst =
            let ac = self#addressing ((inst lsr 6) land 3) ~fp:true in
            let src = self#addressing (inst land 0o77) ~fp:false in
            if store then
                sprintf "%s\t%s, %s" op ac src
            else
                sprintf "%s\t%s, %s" op src ac

        method f4_inst op inst =
            let target = self#addressing (inst land 0o77) ~fp:false in
            sprintf "%s\t%s" op target

        method fp =
            match inst with
            | 0o170000 -> "cfcc"
            | 0o170001 -> "setf"
            | 0o170002 -> "seti"
            | 0o170011 -> "setd"
            | 0o170012 -> "setl"
            | _ -> begin
                match ((inst lsr 6) land 0o77) with
                | 0o01 -> self#f4_inst "ldfps" inst
                | 0o02 -> self#f4_inst "stfps" inst
                | 0o03 -> self#f4_inst "stst" inst
                | 0o04 -> self#f2_inst "clrf" inst
                | 0o05 -> self#f2_inst "tstf" inst
                | 0o06 -> self#f2_inst "absf" inst
                | 0o07 -> self#f2_inst "negf" inst
                | _ -> begin
                    match ((inst lsr 8) land 15) with
                    | 0b0010 -> self#f1_inst "mulf" inst
                    | 0b0011 -> self#f1_inst "modf" inst
                    | 0b0100 -> self#f1_inst "addf" inst
                    | 0b0101 -> self#f1_inst "ldf" inst
                    | 0b0110 -> self#f1_inst "subf" inst
                    | 0b0111 -> self#f1_inst "cmpf" inst
                    | 0b1000 -> self#f1_inst "stf" inst ~store:true
                    | 0b1001 -> self#f1_inst "divf" inst
                    | 0b1010 -> self#f3_inst "stexp" inst ~store:true
                    | 0b1011 -> self#f3_inst "stcfi" inst ~store:true
                    | 0b1100 -> self#f1_inst "stcfd" inst ~store:true
                    | 0b1101 -> self#f3_inst "ldexp" inst
                    | 0b1110 -> self#f3_inst "ldcif" inst
                    | 0b1111 -> self#f1_inst "ldcdf" inst
                    | _ -> "?"
                end
            end
    end

(* a.out file format *)
class a_out_format (stream: byte list) =
    object(self)
        val a_magic  = readw  0 stream
        val a_text   = readw  2 stream
        val a_data   = readw  4 stream
        val a_bss    = readw  6 stream
        val a_syms   = readw  8 stream
        val a_entry  = readw 10 stream
        val a_trsize = readw 12 stream
        val a_drsize = readw 14 stream
        val mutable code = ExtList.List.take (readw 2 stream) (ExtList.List.drop 16 stream)
        val mutable pc = 0
        method magic     = a_magic
        method text_size = a_text
        method data_size = a_data
        method entry     = a_entry
        method code      = code
        method disas     =
            try
                let itp             = new interpreter pc code in
                let (bytecode, asm) = itp#interpret in
                let cur_pc          = pc in
                code <- itp#residue;
                pc <- pc + (List.length bytecode) * 2;
                (cur_pc, bytecode, asm)
            with e ->
                raise e
    end

(* read in_channel in binary mode and returns byte list *)
let rec read_bin ic = 
    try
        let b = input_byte ic in
        b::(read_bin ic)
    with End_of_file -> []

(* receive byte list then print it like xxd-format *)
let xxd s =
    let rec xxd_sub stream counter =
        match stream with
        | byte::s -> (
            printf "%02x%s" byte (
                if counter mod 16 = 0 then "\n"
                else if counter mod 2 = 0 then " " else "");
            xxd_sub s (counter+1))
        | [] -> ()
    in xxd_sub s 1
