open Core

  let ctoi = function
    | 'I' -> 1
    | 'V' -> 5
    | 'X' -> 10
    | 'L' -> 50
    | 'C' -> 100
    | 'D' -> 500
    | 'M' -> 1000
    | _   -> failwith "Undefined character"

  let rec parse_decrementing acc = function
    | [] -> acc
    | c :: rest -> parse_decrementing (acc - (ctoi c)) rest

  (* HELL *)
  let cause_decrement dec_mode fst_val snd_val =
    if dec_mode
    then fst_val = snd_val
    else
      match (snd_val / fst_val) with
      | 2 | 5 | 10 -> true
      | _          -> false

  let rec parse dec_mode last_char_val acc = function
    | []        -> acc
    | c :: rest ->
      let c_num = ctoi c in
      if cause_decrement dec_mode c_num last_char_val
      then parse true  c_num (acc - c_num) rest
      else parse false c_num (acc + c_num) rest

  let rtoi str =
    String.to_list str |>
    List.rev           |>
    parse false 0 0

