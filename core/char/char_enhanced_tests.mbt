// Enhanced test cases for char module

///|
test "char comparison operations" {
  let a = 'a'
  let z = 'z'
  let A = 'A'
  let Z = 'Z'
  
  inspect(a < z, content="true")
  inspect(A < Z, content="true")
  inspect(A < a, content="true")
  inspect(z < a, content="false")
  
  inspect(a <= a, content="true")
  inspect(z >= a, content="true")
  inspect(A > Z, content="false")
}

///|
test "char arithmetic operations" {
  let a = 'a'
  let b = 'b'
  let zero = '0'
  
  inspect(a + 1, content="'b'")
  inspect(b - 1, content="'a'")
  inspect(zero + 5, content="'5'")
  
  // Test char to int conversion
  inspect(a.to_int(), content="97")
  inspect(z.to_int(), content="122")
  inspect(A.to_int(), content="65")
  inspect(Z.to_int(), content="90")
  inspect(zero.to_int(), content="48")
}

///|
test "char classification functions" {
  let letters = ['a', 'z', 'A', 'Z']
  let digits = ['0', '5', '9']
  let whitespace = [' ', '\t', '\n', '\r']
  let special = ['!', '@', '#', '$']
  
  // Test if characters are alphabetic
  for ch in letters {
    inspect(ch.is_alpha(), content="true")
  }
  
  // Test if characters are digits
  for ch in digits {
    inspect(ch.is_digit(), content="true")
  }
  
  // Test if characters are whitespace
  for ch in whitespace {
    inspect(ch.is_whitespace(), content="true")
  }
  
  // Test if special characters are not alphanumeric
  for ch in special {
    inspect(ch.is_alphanumeric(), content="false")
  }
}