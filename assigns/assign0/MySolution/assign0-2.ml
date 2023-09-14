let isPrime n =
  let rec is_prime_helper n divisor =
    if divisor * divisor > n then true
    else if n mod divisor = 0 then false
    else is_prime_helper n (divisor + 1)
  in
  is_prime_helper n 2
;;