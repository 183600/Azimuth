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
  let str1 = "Hello"
  let str2 = "MoonBit"
  let combined = str1 + " " + str2
  
  assert_eq(combined, "Hello MoonBit")
  assert_eq(combined.length(), 11)
  
  // Test string length
  let empty = ""
  assert_eq(empty.length(), 0)
  
  let single = "A"
  assert_eq(single.length(), 1)
}

///|
test "list basic operations" {
  let empty_list : List[Int] = []
  let list1 = [1, 2, 3]
  
  assert_eq(list1.length(), 3)
  assert_eq(empty_list.length(), 0)
  
  // Test list head
  assert_eq(list1.head(), 1)
  
  // Test list tail
  assert_eq(list1.tail(), [2, 3])
}

///|
test "math operations and functions" {
  let x = 16.0
  let y = 4.0
  
  // Basic arithmetic
  assert_eq(x + y, 20.0)
  assert_eq(x - y, 12.0)
  assert_eq(x * y, 64.0)
  assert_eq(x / y, 4.0)
  
  // Test absolute value
  assert_eq(Float::abs(-3.14), 3.14)
  assert_eq(Float::abs(0.0), 0.0)
  assert_eq(Float::abs(5.5), 5.5)
}

///|
test "option type operations" {
  let some_value : String? = Some("test")
  let none_value : String? = None
  
  // Test is_some and is_none
  assert_eq(some_value.is_some(), true)
  assert_eq(some_value.is_none(), false)
  assert_eq(none_value.is_some(), false)
  assert_eq(none_value.is_none(), true)
  
  // Test unwrap_or
  assert_eq(some_value.unwrap_or("default"), "test")
  assert_eq(none_value.unwrap_or("default"), "default")
}

///|
test "result type operations" {
  let ok_value : Result[String, String] = Ok("success")
  let err_value : Result[String, String] = Err("error")
  
  // Test is_ok and is_err
  assert_eq(ok_value.is_ok(), true)
  assert_eq(ok_value.is_err(), false)
  assert_eq(err_value.is_ok(), false)
  assert_eq(err_value.is_err(), true)
  
  // Test unwrap_or
  assert_eq(ok_value.unwrap_or("default"), "success")
  assert_eq(err_value.unwrap_or("default"), "default")
}

///|
test "integer boundary and operations" {
  let x = 42
  let y = -17
  let zero = 0
  
  // Basic arithmetic
  assert_eq(x + y, 25)
  assert_eq(x - y, 59)
  assert_eq(x * y, -714)
  assert_eq(x / y, -2)
  assert_eq(x % y, 8)
  
  // Test absolute value
  assert_eq(Int::abs(y), 17)
  assert_eq(Int::abs(zero), 0)
  assert_eq(Int::abs(x), 42)
}

///|
test "boolean logic operations" {
  let true_val = true
  let false_val = false
  
  // Basic boolean operations
  assert_eq(true_val && false_val, false)
  assert_eq(true_val || false_val, true)
  assert_eq(!true_val, false)
  assert_eq(!false_val, true)
  
  // Equality
  assert_eq(true_val == true_val, true)
  assert_eq(false_val == false_val, true)
  assert_eq(true_val == false_val, false)
  assert_eq(true_val != false_val, true)
}

///|
test "tuple operations and destructuring" {
  let pair = (42, "hello")
  let triple = (1, 2, 3)
  
  // Test tuple access
  assert_eq(pair.0, 42)
  assert_eq(pair.1, "hello")
  
  assert_eq(triple.0, 1)
  assert_eq(triple.1, 2)
  assert_eq(triple.2, 3)
  
  // Test tuple equality
  assert_eq(pair == (42, "hello"), true)
  assert_eq(pair == (43, "hello"), false)
  assert_eq(triple == (1, 2, 3), true)
}

///|
test "array operations and indexing" {
  let arr = Array::with_capacity(5)
  arr.push(10)
  arr.push(20)
  arr.push(30)
  
  // Test array length and indexing
  assert_eq(arr.length(), 3)
  assert_eq(arr[0], 10)
  assert_eq(arr[1], 20)
  assert_eq(arr[2], 30)
  
  // Test array modification
  arr[0] = 15
  assert_eq(arr[0], 15)
  
  // Test array operations
  arr.push(40)
  assert_eq(arr.length(), 4)
  assert_eq(arr[3], 40)
  
  let popped = arr.pop()
  assert_eq(popped, Some(40))
  assert_eq(arr.length(), 3)
}

///|
test "comparison and ordering operations" {
  let a = 5
  let b = 10
  let c = 5
  
  // Test comparisons
  assert_eq(a < b, true)
  assert_eq(a <= b, true)
  assert_eq(a > b, false)
  assert_eq(a >= b, false)
  assert_eq(a == c, true)
  assert_eq(a != b, true)
  
  // Test string comparison
  let str1 = "apple"
  let str2 = "banana"
  let str3 = "apple"
  
  assert_eq(str1 < str2, true)
  assert_eq(str1 == str3, true)
  assert_eq(str1 != str2, true)
}