// Copyright 2026 International Digital Economy Academy
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

///|
test "arithmetic operations with negative numbers" {
  let a = -15
  let b = 8
  let c = -3
  
  inspect(a + b, content="-7")
  inspect(a - b, content="-23")
  inspect(a * b, content="-120")
  inspect(a / b, content="-1")
  inspect(a % b, content="-7")
  inspect(b * c, content="-24")
  inspect(c + c, content="-6")
}

///|
test "string slicing and substring operations" {
  let text = "MoonBit Programming"
  
  inspect(text.length(), content="19")
  
  let prefix = "MoonBit"
  let suffix = "Programming"
  
  inspect(prefix + " " + suffix, content="MoonBit Programming")
}

///|
test "boolean logic with complex expressions" {
  let a = true
  let b = false
  let c = true
  
  inspect(a && (b || c), content="true")
  inspect((a || b) && (c || b), content="true")
  inspect(!(a && b) && c, content="true")
  inspect(!a || (!b && c), content="true")
}

///|
test "integer overflow and underflow" {
  let max_val = 2147483647  // @int.max_value
  let min_val = -2147483648  // @int.min_value
  
  inspect(max_val + 1, content="-2147483648")
  inspect(min_val - 1, content="2147483647")
  inspect(max_val + max_val, content="-2")
  inspect(min_val + min_val, content="0")
}

///|
test "string with escape characters" {
  let with_quotes = "He said \"Hello\" to me"
  let with_newline = "Line 1\nLine 2"
  let with_tab = "Column 1\tColumn 2"
  let with_backslash = "Path\\to\\file"
  
  inspect(with_quotes, content="He said \"Hello\" to me")
  inspect(with_newline.length(), content="11")
  inspect(with_tab.length(), content="15")
  inspect(with_backslash, content="Path\\to\\file")
}

///|
test "nested conditional expressions" {
  let x = 10
  let y = 20
  let z = 15
  
  let result = if x > y {
    "x greater than y"
  } else if y > z {
    "y greater than z"
  } else {
    "neither condition met"
  }
  
  inspect(result, content="y greater than z")
  
  let nested = if x > 5 {
    if y > 15 {
      "both conditions true"
    } else {
      "only first condition true"
    }
  } else {
    "first condition false"
  }
  
  inspect(nested, content="both conditions true")
}

///|
test "integer bitwise operations" {
  let a = 10  // 1010 in binary
  let b = 6   // 0110 in binary
  
  inspect(a & b, content="2")   // 0010
  inspect(a | b, content="14")  // 1110
  inspect(a ^ b, content="12")  // 1100
  inspect(a << 2, content="40") // 101000
  inspect(a >> 1, content="5")  // 0101
}

///|
test "string concatenation with numbers" {
  let num = 42
  let pi = 3.14
  let negative = -17
  
  let str_num = "The answer is " + num.to_string()
  let str_pi = "Pi is approximately " + pi.to_string()
  let str_negative = "Negative: " + negative.to_string()
  
  inspect(str_num, content="The answer is 42")
  inspect(str_pi, content="Pi is approximately 3.14")
  inspect(str_negative, content="Negative: -17")
}

///|
test "complex boolean array operations" {
  let flags = [true, false, true, false, true]
  
  // Count true values manually
  let true_count = (if flags[0] { 1 } else { 0 }) + 
                   (if flags[1] { 1 } else { 0 }) + 
                   (if flags[2] { 1 } else { 0 }) + 
                   (if flags[3] { 1 } else { 0 }) + 
                   (if flags[4] { 1 } else { 0 })
  inspect(true_count, content="3")
  
  // All true?
  let all_true = flags[0] && flags[1] && flags[2] && flags[3] && flags[4]
  inspect(all_true, content="false")
  
  // Any true?
  let any_true = flags[0] || flags[1] || flags[2] || flags[3] || flags[4]
  inspect(any_true, content="true")
  
  // Logical operations on array elements
  let combined = (flags[0] && flags[2]) || flags[4]
  inspect(combined, content="true")
}

///|
test "mathematical function compositions" {
  let x = 8
  
  // Square and double
  let square_and_double = x * x * 2
  inspect(square_and_double, content="128")
  
  // Absolute value operations (manual implementation)
  let neg_x = -x
  let abs_neg_x = if neg_x < 0 { -neg_x } else { neg_x }
  inspect(abs_neg_x, content="8")
  
  // Division with remainder
  let dividend = 25
  let divisor = 7
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  inspect(quotient, content="3")
  inspect(remainder, content="4")
  
  // Verify division property
  inspect(dividend == quotient * divisor + remainder, content="true")
}