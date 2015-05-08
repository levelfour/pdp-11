open Printf
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
                    printf "%s\n" binary#disas;
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
