#use "/home/raheeqi/cs320/CS320-2023-Fall/CS320-2023-Fall/classlib/OCaml/MyOCaml.ml";;


let rec intrev10_tr (n: int) (acc: int): int =
  if n = 0 then acc else
    let last_digit = n mod 10 in
    let new_reversed_result = (acc * 10) + last_digit in
    let remaining_digits = n / 10 in
    intrev10_tr remaining_digits new_reversed_result

let intrev10 n =
   if n < 10 then
    n
  else
    intrev10_tr n 0

  