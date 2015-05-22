open Printf
open Utils
open Binary

let _ =
    if Array.length Sys.argv < 2 then
        printf "Too few args\n"
    else
        let ic = open_in_bin Sys.argv.(1) in
        try
            let bin = new a_out_format (read_bin ic) in
            let rec disasall binary =
                try
                    let (pc, bytecode, asm) = binary#disas in
                    let hexcode bl =
                        String.concat " " (List.map (fun x -> sprintf "%04x" x) bl)
                    in
                    printf "%+4x:\t%-*s %s\n" pc 15 (hexcode bytecode) asm;
                    disasall binary;
                with OutOfRange ->
                    ()
            in
            printf "TextSize=%d(B), DataSize=%d(B)\n" bin#text_size bin#data_size;
            disasall bin;
            close_in ic
        with e ->
            close_in_noerr ic;
            raise e
