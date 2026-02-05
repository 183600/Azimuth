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
test "int basic operations" {
  let max_val = @int.max_value
  let min_val = @int.min_value
  
  inspect(max_val, content="2147483647")
  inspect(min_val, content="-2147483648")
  
  // Test absolute value
  inspect(@int.abs(42), content="42")
  inspect(@int.abs(-42), content="42")
  inspect(@int.abs(0), content="0")
  
  // Test arithmetic operations
  inspect(10 + 5, content="15")
  inspect(10 - 5, content="5")
  inspect(10 * 5, content="50")
  inspect(10 / 5, content="2")
  inspect(10 % 3, content="1")
}

///|
test "string basic operations" {
  let default_str = @string.default()
  inspect(default_str, content="")
  
  // Test string concatenation
  let str1 = "Hello"
  let str2 = " World"
  let result = str1 + str2
  inspect(result, content="Hello World")
  
  // Test string length
  inspect(result.length(), content="11")
  
  // Test string comparison
  inspect("hello" == "hello", content="true")
  inspect("hello" != "world", content="true")
  inspect("apple" < "banana", content="true")
}

///|
test "list comprehensive operations" {
  // Test list creation
  let empty_list : @list.List[Int] = @list.new()
  inspect(empty_list.is_empty(), content="true")
  
  let list1 = @list.from_array([1, 2, 3, 4, 5])
  inspect(list1.length(), content="5")
  
  // Test list prepend
  let list2 = list1.prepend(0)
  inspect(list2, content="@list.from_array([0, 1, 2, 3, 4, 5])")
  
  // Test list head and tail
  inspect(list2.head(), content="Some(0)")
  inspect(list2.last(), content="Some(5)")
  
  // Test list filter
  let even_list = list1.filter(x => x % 2 == 0)
  inspect(even_list, content="@list.from_array([2, 4])")
  
  // Test list map
  let doubled_list = list1.map(x => x * 2)
  inspect(doubled_list, content="@list.from_array([2, 4, 6, 8, 10])")
  
  // Test list fold
  let sum = list1.fold(init=0, (acc, x) => acc + x)
  inspect(sum, content="15")
  
  // Test list reverse
  let reversed_list = list1.rev()
  inspect(reversed_list, content="@list.from_array([5, 4, 3, 2, 1])")
}

///|
test "bool logical operations" {
  let t = true
  let f = false
  
  // Test basic logical operations
  inspect(t && f, content="false")
  inspect(t || f, content="true")
  inspect(!t, content="false")
  inspect(!f, content="true")
  
  // Test comparison
  inspect(t == t, content="true")
  inspect(t != f, content="true")
  
  // Test conditional expression
  let result = if t { "yes" } else { "no" }
  inspect(result, content="yes")
  
  let result2 = if f { 42 } else { 0 }
  inspect(result2, content="0")
}

///|
test "char operations" {
  // Test character creation and comparison
  let char_a = 'a'
  let char_b = 'b'
  
  inspect(char_a.to_string(), content="a")
  inspect(char_b.to_string(), content="b")
  
  // Test character comparison
  inspect(char_a < char_b, content="true")
  inspect(char_a == char_a, content="true")
  inspect(char_b != char_a, content="true")
  
  // Test character to int conversion
  inspect('0'.to_int(), content="48")
  inspect('A'.to_int(), content="65")
  inspect('a'.to_int(), content="97")
}

///|
test "option type operations" {
  // Test Some and None
  let some_value = Some(42)
  let none_value : Int? = None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  
  // Test unwrap with default
  inspect(some_value.unwrap_or(0), content="42")
  inspect(none_value.unwrap_or(0), content="0")
  
  // Test map operation
  let mapped_some = some_value.map(x => x * 2)
  inspect(mapped_some, content="Some(84)")
  
  let mapped_none = none_value.map(x => x * 2)
  inspect(mapped_none, content="None")
}

///|
test "result type operations" {
  // Test Ok and Error
  let ok_result : Result[String, String] = Ok("success")
  let error_result : Result[String, String] = Error("failure")
  
  inspect(ok_result.is_ok(), content="true")
  inspect(ok_result.is_error(), content="false")
  inspect(error_result.is_ok(), content="false")
  inspect(error_result.is_error(), content="true")
  
  // Test unwrap with default
  inspect(ok_result.unwrap_or("default"), content="success")
  inspect(error_result.unwrap_or("default"), content="default")
  
  // Test map operation
  let mapped_ok = ok_result.map(s => s.length())
  inspect(mapped_ok, content="Ok(7)")
  
  let mapped_error = error_result.map(s => s.length())
  inspect(mapped_error, content="Error(\"failure\")")
}

///|
test "array operations" {
  // Test array creation and basic operations
  let empty_array : Array[Int] = []
  inspect(empty_array.length(), content="0")
  
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  
  // Test array modification
  numbers.push(6)
  inspect(numbers.length(), content="6")
  inspect(numbers[5], content="6")
  
  let popped = numbers.pop()
  inspect(popped, content="Some(6)")
  inspect(numbers.length(), content="5")
  
  // Test array slice
  let slice = numbers.slice(1, 4)
  inspect(slice, content="[2, 3, 4]")
}

///|
test "math functions" {
  // Test basic math functions
  inspect(@math.max(10, 20), content="20")
  inspect(@math.max(20, 10), content="20")
  inspect(@math.min(10, 20), content="10")
  inspect(@math.min(20, 10), content="10")
  
  // Test power and square root
  inspect(@math.pow(2, 3), content="8")
  inspect(@math.pow(3, 2), content="9")
  inspect(@math.sqrt(16), content="4")
  inspect(@math.sqrt(9), content="3")
  
  // Test rounding functions
  inspect(@math.floor(3.7), content="3")
  inspect(@math.ceil(3.2), content="4")
  inspect(@math.round(3.5), content="4")
  inspect(@math.round(3.4), content="3")
}

///|
test "type conversions and utilities" {
  // Test int to string conversions
  inspect(42.to_string(), content="42")
  inspect((-42).to_string(), content="-42")
  
  // Test string to int conversions
  inspect("42".to_int(), content="Some(42)")
  inspect("-42".to_int(), content="Some(-42)")
  inspect("abc".to_int(), content="None")
  
  // Test byte conversions
  let byte_val = 65.to_byte()
  inspect(byte_val.to_int(), content="65")
  
  // Test tuple operations
  let tuple = (1, "hello", true)
  inspect(tuple.0, content="1")
  inspect(tuple.1, content="hello")
  inspect(tuple.2, content="true")
  
  // Test array to list conversion
  let arr = [1, 2, 3, 4, 5]
  let list_from_arr = @list.from_array(arr)
  inspect(list_from_arr, content="@list.from_array([1, 2, 3, 4, 5])")
  
  // Test list to array conversion
  let arr_from_list = list_from_arr.to_array()
  inspect(arr_from_list, content="[1, 2, 3, 4, 5]")
}