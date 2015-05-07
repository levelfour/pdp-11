open Printf

type byte = int

exception OutOfRange

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
        val code     = ExtList.List.drop 16 stream
        method magic     = a_magic
        method text_size = a_text
        method data_size = a_data
        method entry     = a_entry
        method code      = code
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
