#use "./../assign0.ml";;



let count_digits d =
  let rec count_helper n count =
    if n = 0 then
      if d = 0 then 1 (* Special case for the number 0 *)
      else count
    else
      count_helper (n / 10) (count + 1)
  in
  count_helper (abs d) 0


(**
  let rec int2str(i0: int): string =
    if count_digits i0 = 1 then string_init 1 (fun (i : int) -> chr (i0 + ord '0'))
    else string_init (count_digits i0) (fun (i : int ) -> chr (((i0 - (i0 mod 10))/10) + ord '0'))
*)

let int2str i0 =
  let num_digits = count_digits i0 in
  let result = string_init num_digits (fun (i : int)  ->
    let digit = i0 / int_of_float (10. ** float_of_int (num_digits - i - 1)) mod 10 in
    chr (digit + ord '0')
  )
  in
    result
  
(**
let l = count_digits i;;

let s0 = string_init i (fun (i: int) -> chr (48))



let s0 = string_init 6 (fun (i: int) -> chr (48))
*)