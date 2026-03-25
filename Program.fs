module CS220.Program

/// How many different ways can we make change of a given amount of money in
/// Korean coins? Suppose we have 5 different kinds of coins: 500-won coins,
/// 100-won coins, 50-won coins, 10-won coins, and 1-won coins. Write a function
/// prob1 that takes in an amount of money in won, and returns the number of
/// possible combinations. For example, when the given amount is 10, then the
/// function should return 2, because there are 2 different ways to make change
/// for 10 won: (1) one 10-won, and (2) ten 1-won. The function should return -1
/// if an errorneous input is given, e.g., when negative amount is given.
let prob1 amount =
  let unit= if amount>=500 then 500 elif amount>=100 then 100 else 50
  let rec iter amount unit=
    if amount<0 then -1
    elif amount>=500 && unit>=500 then (iter (amount-500) 500) + (iter (amount-100) 100) + (iter (amount-50) 50) + 1 + (amount/10)
    elif amount>=100 && unit>=100 then (iter (amount-100) 100) + (iter (amount-50) 50) + 1 + (amount/10)
    elif amount>=50 then (iter (amount-50) 50) + 1 + (amount/10)
    else 1 + (amount/10)
  iter amount unit

/// Write a function `prob2` that computes GCD (Greatest Common Divisor) of two
/// given integers. This function should return -1 if both inputs are 0.
let prob2 a b =
  let a_2=
    if a<0 then (-1) * a
    else a
  let b_2=
    if b<0 then (-1) * b
    else b
  let rec gcd a b=
    if a=0 && b=0 then -1
    elif a=0 || b=0 then a+b
    elif a<b then gcd b a
    else gcd b (a%b)
  gcd a_2 b_2

/// Write a function `prob3` that takes in a string s and an integer n, and
/// returns a string that repeats s for n times. For example, if "abc" and 3 are
/// given, the function should return "abcabcabc". When n is zero, the function
/// returns an empty string. When n is negative, it returns a string that
/// repeats reversed s for -n times. For example, pow "abc" -3 will return
/// "cbacbacba".
let prob3 s n =
  let rec rep s n=
    if n=0 then ""
    else s + (rep s (n-1))
  let rec rev s=
    if s="" then ""
    elif s.Length=1 then s
    else (rev s[1..]) + (string s[0])
  if n=0 then ""
  elif n<0 then rep (rev s) ((-1)*n)
  else rep s n

/// Write a function `prob4` that takes in an unsigned integer n (uint32), and
/// returns the smallest integral divisor of n that is greater than 1. For
/// example, given 45, the function will return 3 (45 % 3 = 0). This function
/// returns 0 for all error cases, e.g., when the given number is 1u. This
/// function only considers non-zero numbers as valid inputs.
let prob4 (n: uint32) =
  let rec div n d=
    if n<(d*d) then n
    elif (n%d) = 0u then d
    else div n (d+1u)
  if n<=1u then 0u
  else div n 2u

/// Write a function `prob5` that takes in an unsigned integer as input, and
/// checks if the number is a prime number or not. If the number is prime, then
/// the function returns true. Otherwise, it returns false. Hint: you can use
/// the `prob4` function above.
let prob5 (n: uint32) =
  if prob4(n)<n || n<=1u then false
  else true

[<EntryPoint>]
let main _args =
  0
