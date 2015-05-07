open Printf
open Binary

let _ =
    if Array.length Sys.argv < 2 then
        printf "Too few args\n"
    else
        let ic = open_in_bin Sys.argv.(1) in
        try
            let bin = new a_out_format (read_bin ic) in
            printf "TextSize=%d(B), DataSize=%d(B)\n"
                bin#text_size bin#data_size;
            xxd bin#code;
            flush stdout;
            close_in ic
        with e ->
            close_in_noerr ic;
            raise e
