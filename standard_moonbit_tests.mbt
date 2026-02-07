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
test "string concatenation and length" {
  let s1 = "Hello"
  let s2 = "MoonBit"
  let combined = s1 + ", " + s2 + "!"
  
  inspect(combined, content="Hello, MoonBit!")
  inspect(s1.length(), content="5")
  inspect(s2.length(), content="7")
  inspect(combined.length(), content="14")
}

///|
test "integer arithmetic with negative numbers" {
  let a = 42
  let b = -17
  
  inspect(a + b, content="25")
  inspect(a - b, content="59")
  inspect(a * b, content="-714")
  inspect(a / b, content="-2")
  inspect(a % b, content="8")
  
  assert_eq(Int::abs(b), 17)
  assert_eq(Int::abs(-a), 42)
}

///|
test "boolean logic with complex expressions" {
  let x = true
  let y = false
  let z = true
  
  inspect(x && y, content="false")
  inspect(x || y, content="true")
  inspect(!y, content="true")
  
  assert_eq((x && y) || (x && z), true)
  assert_eq((x || y) && (y || z), true)
  assert_eq(x && (y || z), true)
}

///|
test "array and list operations" {
  let arr = [10, 20, 30, 40, 50]
  let list = @list.from_array(arr)
  
  inspect(arr.length(), content="5")
  inspect(arr[0], content="10")
  inspect(arr[4], content="50")
  
  inspect(list.length(), content="5")
  inspect(list.head(), content="Some(10)")
  inspect(list.last(), content="Some(50)")
}

///|
test "option type with pattern matching" {
  let some_value : Int? = Some(123)
  let none_value : Int? = None
  
  inspect(some_value, content="Some(123)")
  inspect(none_value, content="None")
  
  let result = match some_value {
    Some(x) => x * 2
    None => 0
  }
  inspect(result, content="246")
  
  let none_result = match none_value {
    Some(x) => x * 2
    None => 0
  }
  inspect(none_result, content="0")
}

///|
test "character operations and conversions" {
  let char_a = 'a'
  let char_A = 'A'
  let char_digit = '5'
  
  inspect(char_a.to_string(), content="a")
  inspect(char_A.to_string(), content="A")
  inspect(char_digit.to_string(), content="5")
  
  inspect(char_a.to_byte(), content="97")
  inspect(char_A.to_byte(), content="65")
  inspect(char_digit.to_byte(), content="53")
  
  assert_eq(char_a.to_byte() - char_A.to_byte(), 32)
}

///|
test "tuple operations and destructuring" {
  let point = (3, 4)
  let person = ("Alice", 25, "Engineer")
  let coordinates = (1.5, 2.5, 3.5)
  
  inspect(point.0, content="3")
  inspect(point.1, content="4")
  
  let (name, age, profession) = person
  inspect(name, content="Alice")
  inspect(age, content="25")
  inspect(profession, content="Engineer")
  
  let (x, y, z) = coordinates
  inspect(x, content="1.5")
  inspect(y, content="2.5")
  inspect(z, content="3.5")
}

///|
test "result type error handling" {
  let success : Result[String, String] = Ok("Operation completed")
  let failure : Result[String, String] = Err("Network error")
  
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(failure.is_ok(), content="false")
  inspect(failure.is_err(), content="true")
  
  inspect(success.unwrap(), content="Operation completed")
  inspect(failure.unwrap_err(), content="Network error")
  inspect(success.unwrap_or("Default"), content="Operation completed")
  inspect(failure.unwrap_or("Default"), content="Default")
}

///|
test "type conversions and comparisons" {
  let int_val = 42
  let double_val = 3.14
  
  inspect(int_val.to_int64(), content="42")
  inspect(int_val.to_uint(), content="42")
  inspect(int_val.to_double(), content="42.0")
  
  assert_eq(10 > 5, true)
  assert_eq(10 < 5, false)
  assert_eq(10 >= 10, true)
  assert_eq(10 <= 5, false)
  assert_eq(10 == 10, true)
  assert_eq(10 != 5, true)
}

///|
test "bitwise operations and overflow" {
  let a = 12  // 1100 in binary
  let b = 10  // 1010 in binary
  
  inspect(a & b, content="8")   // 1000
  inspect(a | b, content="14")  // 1110
  inspect(a ^ b, content="6")   // 0110
  inspect(a << 2, content="48") // 110000
  inspect(a >> 1, content="6")  // 110
  
  let max_val = 2147483647
  let overflow = max_val + 1
  inspect(overflow, content="-2147483648")
}