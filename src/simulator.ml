open Printf
open Utils

(* interpreter object of byte code *)
class simulator (arch: architecture) (stream: byte list) mem =
    object(self)
        val inst = fst (popw stream)
        val mutable pc = arch.r7
        val mutable bytecode = [fst (popw stream)]
        val mutable residue = snd (popw stream)
        method residue = residue
        method addressing ?(fp=false) mode =
            let oprand m =
                let i = (m land 7) in
                if fp then
                    raise InvalidMode
                else
                    match i with
                    | 0 -> arch.r0
                    | 1 -> arch.r1
                    | 2 -> arch.r2
                    | 3 -> arch.r3
                    | 4 -> arch.r4
                    | 5 -> arch.r5
                    | 6 -> arch.r6
                    | 7 -> arch.r7
                    | _ -> raise InvalidMode
            in
            match (mode lsr 3) land 7 with
            | 0 -> (oprand mode)
            | 1 -> readw (oprand mode) mem
            | 2 -> begin
                match (mode land 7) with
                | 0 -> arch.r0 <- arch.r0 + 2; readw (arch.r0 - 2) mem;
                | 1 -> arch.r1 <- arch.r1 + 2; readw (arch.r1 - 2) mem;
                | 2 -> arch.r2 <- arch.r2 + 2; readw (arch.r2 - 2) mem;
                | 3 -> arch.r3 <- arch.r3 + 2; readw (arch.r3 - 2) mem;
                | 4 -> arch.r4 <- arch.r4 + 2; readw (arch.r4 - 2) mem;
                | 5 -> arch.r5 <- arch.r5 + 2; readw (arch.r5 - 2) mem;
                | 6 -> arch.r6 <- arch.r6 + 2; readw (arch.r6 - 2) mem;
                | 7 -> arch.r7 <- arch.r7 + 2; readw (arch.r7 - 2) mem;
                | _ -> raise InvalidMode
            end
            | 3 -> begin
                match (mode land 7) with
                | 0 -> arch.r0 <- arch.r0 + 2; readw (readw (arch.r0 - 2) mem) mem;
                | 1 -> arch.r1 <- arch.r1 + 2; readw (readw (arch.r1 - 2) mem) mem;
                | 2 -> arch.r2 <- arch.r2 + 2; readw (readw (arch.r2 - 2) mem) mem;
                | 3 -> arch.r3 <- arch.r3 + 2; readw (readw (arch.r3 - 2) mem) mem;
                | 4 -> arch.r4 <- arch.r4 + 2; readw (readw (arch.r4 - 2) mem) mem;
                | 5 -> arch.r5 <- arch.r5 + 2; readw (readw (arch.r5 - 2) mem) mem;
                | 6 -> arch.r6 <- arch.r6 + 2; readw (readw (arch.r6 - 2) mem) mem;
                | 7 -> arch.r7 <- arch.r7 + 2; readw (readw (arch.r7 - 2) mem) mem;
                | _ -> raise InvalidMode
            end
            | 4 -> begin
                match (mode land 7) with
                | 0 -> arch.r0 <- arch.r0 - 2; readw arch.r0 mem;
                | 1 -> arch.r1 <- arch.r1 - 2; readw arch.r1 mem;
                | 2 -> arch.r2 <- arch.r2 - 2; readw arch.r2 mem;
                | 3 -> arch.r3 <- arch.r3 - 2; readw arch.r3 mem;
                | 4 -> arch.r4 <- arch.r4 - 2; readw arch.r4 mem;
                | 5 -> arch.r5 <- arch.r5 - 2; readw arch.r5 mem;
                | 6 -> arch.r6 <- arch.r6 - 2; readw arch.r6 mem;
                | 7 -> arch.r7 <- arch.r7 - 2; readw arch.r7 mem;
                | _ -> raise InvalidMode
            end
            | 5 -> begin
                match (mode land 7) with
                | 0 -> arch.r0 <- arch.r0 - 2; readw (readw arch.r0 mem) mem;
                | 1 -> arch.r1 <- arch.r1 - 2; readw (readw arch.r1 mem) mem;
                | 2 -> arch.r2 <- arch.r2 - 2; readw (readw arch.r2 mem) mem;
                | 3 -> arch.r3 <- arch.r3 - 2; readw (readw arch.r3 mem) mem;
                | 4 -> arch.r4 <- arch.r4 - 2; readw (readw arch.r4 mem) mem;
                | 5 -> arch.r5 <- arch.r5 - 2; readw (readw arch.r5 mem) mem;
                | 6 -> arch.r6 <- arch.r6 - 2; readw (readw arch.r6 mem) mem;
                | 7 -> arch.r7 <- arch.r7 - 2; readw (readw arch.r7 mem) mem;
                | _ -> raise InvalidMode
            end
            | 6 -> begin
                let (v,code) = popw residue in
                pc <- pc + 2;
                residue <- code;
                bytecode <- bytecode @ [v];
                readw ((oprand mode) + (signedw v)) mem
            end
            | 7 -> begin
                let (v,code) = popw residue in
                pc <- pc + 2;
                residue <- code;
                bytecode <- bytecode @ [v];
                readw (readw ((oprand mode) + (signedw v)) mem) mem
            end
            | _ -> raise InvalidMode

        method single_op_inst op inst =
            let dst = self#addressing inst in
            sprintf "%s\t%d" op dst

        method single_reg_inst op inst =
            let reg = self#addressing (inst land 0b111) in
            sprintf "%s\t%d" op reg

        method double_op_inst op inst =
            let src = self#addressing (inst lsr 6) in
            let dst = self#addressing inst in
            sprintf "%s\t%d, %d" op src dst

        method eis_inst ?(dst=false) op inst =
            let reg = self#addressing ((inst lsr 6) land 7) in
            let src = self#addressing inst in
            if dst then
                sprintf "%s\t%d, %d" op reg src
            else
                sprintf "%s\t%d, %d" op src reg

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
                sprintf "%s\t%d, %d" op ac fsrc
            else
                sprintf "%s\t%d, %d" op fsrc ac

        method f2_inst op inst =
            let target = self#addressing (inst land 0o77) ~fp:true in
            sprintf "%s\t%d" op target

        method f3_inst ?(store=false) op inst =
            let ac = self#addressing ((inst lsr 6) land 3) ~fp:true in
            let src = self#addressing (inst land 0o77) ~fp:false in
            if store then
                sprintf "%s\t%d, %d" op ac src
            else
                sprintf "%s\t%d, %d" op src ac

        method f4_inst op inst =
            let target = self#addressing (inst land 0o77) ~fp:false in
            sprintf "%s\t%d" op target

        method cfcc = "cfcc"
        method setf = "setf"
        method seti = "seti"
        method setd = "setd"
        method setl = "setl"
        method ldfps = self#f4_inst "ldfps" inst
        method stfps = self#f4_inst "stfps" inst
        method stst = self#f4_inst "stst" inst
        method clrf = self#f2_inst "clrf" inst
        method tstf = self#f2_inst "tstf" inst
        method absf = self#f2_inst "absf" inst
        method negf = self#f2_inst "negf" inst
        method mulf = self#f1_inst "mulf" inst
        method modf = self#f1_inst "modf" inst
        method addf = self#f1_inst "addf" inst
        method ldf = self#f1_inst "ldf" inst
        method subf = self#f1_inst "subf" inst
        method cmpf = self#f1_inst "cmpf" inst
        method stf = self#f1_inst "stf" inst ~store:true
        method divf = self#f1_inst "divf" inst
        method stexp = self#f3_inst "stexp" inst ~store:true
        method stcfi = self#f3_inst "stcfi" inst ~store:true
        method stcfd = self#f1_inst "stcfd" inst ~store:true
        method ldexp = self#f3_inst "ldexp" inst
        method ldcif = self#f3_inst "ldcif" inst
        method ldcdf = self#f1_inst "ldcdf" inst

        method fp =
            match inst with
            | 0o170000 -> self#cfcc
            | 0o170001 -> self#setf
            | 0o170002 -> self#seti
            | 0o170011 -> self#setd
            | 0o170012 -> self#setl
            | _ -> begin
                match ((inst lsr 6) land 0o77) with
                | 0o01 -> self#ldfps
                | 0o02 -> self#stfps
                | 0o03 -> self#stst 
                | 0o04 -> self#clrf 
                | 0o05 -> self#tstf 
                | 0o06 -> self#absf 
                | 0o07 -> self#negf 
                | _ -> begin
                    match ((inst lsr 8) land 15) with
                    | 0b0010 -> self#mulf 
                    | 0b0011 -> self#modf 
                    | 0b0100 -> self#addf 
                    | 0b0101 -> self#ldf
                    | 0b0110 -> self#subf 
                    | 0b0111 -> self#cmpf 
                    | 0b1000 -> self#stf
                    | 0b1001 -> self#divf 
                    | 0b1010 -> self#stexp
                    | 0b1011 -> self#stcfi
                    | 0b1100 -> self#stcfd
                    | 0b1101 -> self#ldexp
                    | 0b1110 -> self#ldcif
                    | 0b1111 -> self#ldcdf
                    | _ -> "?"
                end
            end
    end

