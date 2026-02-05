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
test "int arithmetic operations with overflow" {
  let max_int = Int::max_value
  let min_int = Int::min_value
  
  // Test overflow behavior
  assert_eq(max_int + 1, min_int)
  assert_eq(min_int - 1, max_int)
  
  // Test multiplication overflow
  assert_eq(max_int * 2, -2)
  assert_eq(min_int * 2, 0)
  
  // Test division edge cases
  assert_eq(max_int / 1, max_int)
  assert_eq(min_int / -1, max_int)
  assert_eq(0 / max_int, 0)
}

///|
test "int bit manipulation and shifting" {
  let value = 0b10101010  // 170 in decimal
  
  // Test bit operations
  assert_eq(value & 0b11110000, 0b10100000)  // 160
  assert_eq(value | 0b00001111, 0b10101111)  // 175
  assert_eq(value ^ 0b11110000, 0b01011010)  // 90
  
  // Test bit shifting
  assert_eq(value << 2, 0b1010101000)  // 680
  assert_eq(value >> 2, 0b00101010)    // 42
  
  // Test shift with edge cases
  assert_eq(1 << 31, -2147483648)  // Overflow to min value
  assert_eq(-1 >> 1, -1)  // Sign extension
}

///|
test "list advanced operations" {
  let list1 = @list.from_array([1, 2, 3, 4, 5])
  let list2 = @list.from_array([6, 7, 8, 9, 10])
  
  // Test concatenation and reversal
  let concatenated = list1.concat(list2)
  assert_eq(concatenated, @list.from_array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
  
  let reversed = concatenated.rev()
  assert_eq(reversed, @list.from_array([10, 9, 8, 7, 6, 5, 4, 3, 2, 1]))
  
  // Test flat_map
  let flat_mapped = list1.flat_map(x => @list.from_array([x, x * 2]))
  assert_eq(flat_mapped, @list.from_array([1, 2, 2, 4, 3, 6, 4, 8, 5, 10]))
  
  // Test zip and unzip
  let zipped = @list.zip(list1, list2)
  let (unzipped1, unzipped2) = zipped.unzip()
  assert_eq(unzipped1, list1)
  assert_eq(unzipped2, list2)
}

///|
test "list fold and scan operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // Test fold operations
  let sum = numbers.fold(0, (acc, x) => acc + x)
  assert_eq(sum, 15)
  
  let product = numbers.fold(1, (acc, x) => acc * x)
  assert_eq(product, 120)
  
  let concatenated_string = numbers.fold("", (acc, x) => acc + x.to_string())
  assert_eq(concatenated_string, "12345")
  
  // Test fold with index
  let weighted_sum = numbers.foldi(0, (i, acc, x) => acc + x * i)
  assert_eq(weighted_sum, 40)  // 0*1 + 1*2 + 2*3 + 3*4 + 4*5 = 40
}

///|
test "option chaining and error handling" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  // Test and_then chaining
  let chained_result = some_value
    .and_then(x => if x > 40 { Some(x * 2) } else { None })
    .and_then(x => if x % 4 == 0 { Some(x / 4) } else { None })
  assert_eq(chained_result, Some(21))
  
  let none_chained = none_value
    .and_then(x => Some(x * 2))
    .and_then(x => Some(x + 1))
  assert_eq(none_chained, None)
  
  // Test map with operations that might fail
  let mapped_result = some_value.map(x => x / 0)  // Division by zero
  inspect(mapped_result, content="None")  // Should handle error gracefully
}

///|
test "option combination and validation" {
  let name : String? = Some("Alice")
  let age : Int? = Some(25)
  let empty_name : String? = None
  let empty_age : Int? = None
  
  // Test combining options
  let valid_person = name.and_then(n => 
    age.map(a => "Name: " + n + ", Age: " + a.to_string())
  )
  assert_eq(valid_person, Some("Name: Alice, Age: 25"))
  
  let invalid_person = empty_name.and_then(n =>
    age.map(a => "Name: " + n + ", Age: " + a.to_string())
  )
  assert_eq(invalid_person, None)
  
  // Test filter with complex conditions
  let filtered_age = age.filter(a => a >= 18 && a <= 65)
  assert_eq(filtered_age, Some(25))
  
  let filtered_invalid_age = empty_age.filter(a => a >= 18)
  assert_eq(filtered_invalid_age, None)
}

///|
test "string operations and transformations" {
  let text = "Hello, World!"
  let empty = ""
  let spaces = "   "
  
  // Test basic string properties
  assert_eq(text.length(), 13)
  assert_eq(empty.length(), 0)
  assert_eq(spaces.length(), 3)
  
  // Test string concatenation
  let greeting = text + " How are you?"
  assert_eq(greeting, "Hello, World! How are you?")
  
  // Test character access
  assert_eq(text[0], 'H')
  assert_eq(text[4], 'o')
  assert_eq(text[12], '!')
  
  // Test string comparison
  assert_eq(text == "Hello, World!", true)
  assert_eq(text != "hello, world!", true)
  assert_eq(empty == "", true)
}

///|
test "string parsing and conversion" {
  let number_string = "12345"
  let negative_string = "-987"
  let float_string = "3.14159"
  let invalid_number = "abc123"
  
  // Test string to int conversion
  assert_eq(number_string.to_int(), Some(12345))
  assert_eq(negative_string.to_int(), Some(-987))
  assert_eq(invalid_number.to_int(), None)
  
  // Test numeric string validation
  let is_numeric = number_string.all(c => c >= '0' && c <= '9')
  assert_eq(is_numeric, true)
  
  let is_negative_numeric = negative_string.all((c, i) => 
    if i == 0 { c == '-' } else { c >= '0' && c <= '9' }
  )
  assert_eq(is_negative_numeric, true)
  
  // Test string transformations
  let uppercased = "hello".to_upper()
  let lowercased = "WORLD".to_lower()
  assert_eq(uppercased, "HELLO")
  assert_eq(lowercased, "world")
}

///|
test "mathematical functions and properties" {
  // Test basic math functions
  assert_eq(@math.abs(-5), 5)
  assert_eq(@math.abs(0), 0)
  assert_eq(@math.abs(7), 7)
  
  // Test min and max functions
  assert_eq(@math.min(3, 7), 3)
  assert_eq(@math.max(3, 7), 7)
  assert_eq(@math.min(-5, -2), -5)
  assert_eq(@math.max(-5, -2), -2)
  
  // Test power and square root
  assert_eq(@math.pow(2, 3), 8)
  assert_eq(@math.pow(5, 0), 1)
  assert_eq(@math.sqrt(16), 4)
  assert_eq(@math.sqrt(0), 0)
  
  // Test rounding functions
  assert_eq(@math.floor(3.7), 3)
  assert_eq(@math.ceil(3.2), 4)
  assert_eq(@math.round(3.5), 4)
  assert_eq(@math.round(3.4), 3)
}

///|
test "mathematical edge cases and special values" {
  // Test operations with zero
  assert_eq(@math.pow(0, 5), 0)
  assert_eq(@math.pow(5, 0), 1)
  assert_eq(@math.pow(0, 0), 1)
  
  // Test negative powers
  assert_eq(@math.pow(2, -2), 0.25)
  assert_eq(@math.pow(10, -1), 0.1)
  
  // Test trigonometric functions (basic checks)
  let pi_value = @math.pi
  assert_eq(@math.sin(0), 0)
  assert_eq(@math.cos(0), 1)
  
  // Test logarithmic functions
  assert_eq(@math.log(@math.e), 1)
  assert_eq(@math.log(1), 0)
  assert_eq(@math.log10(10), 1)
  assert_eq(@math.log10(100), 2)
  
  // Test special mathematical constants
  inspect(@math.pi, content="3.14159")
  inspect(@math.e, content="2.71828")
}