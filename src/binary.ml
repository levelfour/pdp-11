open Printf

type byte = int

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

(* extract a.out header info from byte list *)
open ExtLib

let a_out_header stream =
    (ExtList.List.take 16 stream, ExtList.List.drop 16 stream)

let _ =
    if Array.length Sys.argv < 2 then
        printf "Too few args\n"
    else
        let ic = open_in_bin Sys.argv.(1) in
        try
            xxd (read_bin ic);
            flush stdout;
            close_in ic
        with e ->
            close_in_noerr ic;
            raise e
