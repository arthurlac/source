
  let parse_digit big med small digit =
    match digit with
    | 1 -> [small]
    | 2 -> [small; small]
    | 3 -> [small; small; small]
    | 4 -> [small; med]
    | 5 -> [med]
    | 6 -> [med; small]
    | 7 -> [med; small; small]
    | 8 -> [med; small; small; small]
    | 9 -> [small; big]
    | 0 -> []
    | _ -> assert false

  let parse_thou = function
    | 1 -> ['M']
    | 2 -> ['M'; 'M']
    | 3 -> ['M'; 'M'; 'M']
    | 4 -> ['M'; 'M'; 'M'; 'M']
    | 5 -> ['Z']
    | 6 -> ['Z'; 'M']
    | 7 -> ['Z'; 'M'; 'M']
    | 8 -> ['Z'; 'M'; 'M'; 'M'] (* "Z"  means V under a bar *)
    | 9 -> ['H'; 'K']           (* "HK" means IX under a bar *)
    | 0 -> [] (* Here for sake of completeness e.g. 0912 *)
    | _ -> assert false

  let parse_ones = parse_digit 'X' 'V' 'I'
  let parse_tens = parse_digit 'C' 'L' 'X'
  let parse_hund = parse_digit 'M' 'D' 'C'

  let rec aux pos digits =
    match pos, digits with
    | 4, t :: rest -> (parse_thou t) @ (aux 3 rest)
    | 3, h :: rest -> (parse_hund h) @ (aux 2 rest)
    | 2, c :: rest -> (parse_tens c) @ (aux 1 rest)
    | 1, o :: rest -> (parse_ones o)
    |            _ -> failwith "m8 u fucked"

  let parse ls = aux (List.length ls) ls |> String.of_char_list

  let itor num =
    Int.to_string num          |>
    String.to_list             |>
    List.map ~f:String.of_char |>
    List.map ~f:Int.of_string  |>
    parse

