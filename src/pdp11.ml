open Printf
open Utils
open Binary

let _ =
    if Array.length Sys.argv < 2 then
        printf "Too few args\n"
    else
        let dmode = if Sys.argv.(1) = "-d" then true else false in
        let ic = open_in_bin Sys.argv.(if dmode then 2 else 1) in
        try
            let bin = new a_out_format (read_bin ic) in
            if dmode then
                let rec disasall binary =
                    try
                        let (pc, bytecode, asm) = binary#disas in
                        let hexcode bl =
                            String.concat " " (List.map (fun x -> sprintf "%04x" x) bl)
                        in
                        printf "%+4x:\t%-*s %s\n" pc 15 (hexcode bytecode) asm;
                        disasall binary;
                    with OutOfRange -> ()
                in
                disasall bin;
            else
                bin#exec;
            close_in ic
        with e ->
            close_in_noerr ic;
            raise e
