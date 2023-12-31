#use "./../assign0.ml";;

 
 (*Checking if first char is a negative symbol****)

let isNeg (str: string): bool =
    if String.length str > 0 && str.[0] = '-' then true else false
        
let rec str2int(cs: string): int =
    let strLen = String.length cs in
        if strLen = 0 then 0
      else
        let lastCharStr = String.get cs (strLen - 1) in
        let ordVal = ord lastCharStr - 48 in
            let index = if isNeg cs then 1 else 0 in
            let addTostring = string_init (strLen - 1) (fun i -> String.get cs (index + i)) in
            let newString = str2int addTostring in
              if isNeg cs then -((10 * newString) + ordVal) else (10 * newString) + ordVal