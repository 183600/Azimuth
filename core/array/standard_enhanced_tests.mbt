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
test "array_basic_operations" {
  // Test array creation
  let empty_array : Array[Int] = []
  inspect(empty_array.length(), content="0")
  
  let single_element = [1]
  inspect(single_element.length(), content="1")
  inspect(single_element[0], content="1")
  
  let multiple_elements = [1, 2, 3, 4, 5]
  inspect(multiple_elements.length(), content="5")
  inspect(multiple_elements[0], content="1")
  inspect(multiple_elements[4], content="5")
}

///|
test "array_access_operations" {
  let arr = [10, 20, 30, 40, 50]
  
  // Test element access
  inspect(arr[0], content="10")
  inspect(arr[1], content="20")
  inspect(arr[2], content="30")
  inspect(arr[3], content="40")
  inspect(arr[4], content="50")
}

///|
test "array_length_operations" {
  // Test various array lengths
  let empty : Array[Int] = []
  assert_eq(empty.length(), 0)
  
  let single = [1]
  assert_eq(single.length(), 1)
  
  let multiple = [1, 2, 3, 4, 5]
  assert_eq(multiple.length(), 5)
  
  let long_array = Array::make(100, 0)
  assert_eq(long_array.length(), 100)
}

///|
test "array_make_operations" {
  // Test Array::make with different sizes and values
  let zeros = Array::make(5, 0)
  assert_eq(zeros.length(), 5)
  inspect(zeros[0], content="0")
  inspect(zeros[4], content="0")
  
  let ones = Array::make(3, 1)
  assert_eq(ones.length(), 3)
  inspect(ones[0], content="1")
  inspect(ones[1], content="1")
  inspect(ones[2], content="1")
  
  let strings = Array::make(2, "hello")
  assert_eq(strings.length(), 2)
  inspect(strings[0], content="hello")
  inspect(strings[1], content="hello")
}

///|
test "array_makei_operations" {
  // Test Array::makei with index-based initialization
  let squares = Array::makei(5, fn(i) { i * i })
  assert_eq(squares.length(), 5)
  inspect(squares[0], content="0")
  inspect(squares[1], content="1")
  inspect(squares[2], content="4")
  inspect(squares[3], content="9")
  inspect(squares[4], content="16")
  
  let doubles = Array::makei(3, fn(i) { i * 2 })
  assert_eq(doubles.length(), 3)
  inspect(doubles[0], content="0")
  inspect(doubles[1], content="2")
  inspect(doubles[2], content="4")
}

///|
test "array_string_operations" {
  // Test string arrays
  let words = ["hello", "world", "moonbit"]
  assert_eq(words.length(), 3)
  inspect(words[0], content="hello")
  inspect(words[1], content="world")
  inspect(words[2], content="moonbit")
  
  // Test empty string array
  let empty_strings : Array[String] = []
  assert_eq(empty_strings.length(), 0)
  
  // Test single string array
  let single_string = ["test"]
  assert_eq(single_string.length(), 1)
  inspect(single_string[0], content="test")
}

///|
test "array_float_operations" {
  // Test float arrays
  let floats = [1.0, 2.5, 3.14, 4.2]
  assert_eq(floats.length(), 4)
  inspect(floats[0], content="1.0")
  inspect(floats[1], content="2.5")
  inspect(floats[2], content="3.14")
  inspect(floats[3], content="4.2")
  
  // Test Array::make with floats
  let pi_array = Array::make(3, 3.14159)
  assert_eq(pi_array.length(), 3)
  inspect(pi_array[0], content="3.14159")
  inspect(pi_array[1], content="3.14159")
  inspect(pi_array[2], content="3.14159")
}

///|
test "array_bool_operations" {
  // Test boolean arrays
  let bools = [true, false, true, true, false]
  assert_eq(bools.length(), 5)
  inspect(bools[0], content="true")
  inspect(bools[1], content="false")
  inspect(bools[2], content="true")
  inspect(bools[3], content="true")
  inspect(bools[4], content="false")
  
  // Test Array::make with booleans
  let true_array = Array::make(4, true)
  assert_eq(true_array.length(), 4)
  inspect(true_array[0], content="true")
  inspect(true_array[3], content="true")
  
  let false_array = Array::make(3, false)
  assert_eq(false_array.length(), 3)
  inspect(false_array[0], content="false")
  inspect(false_array[2], content="false")
}

///|
test "array_edge_cases" {
  // Test very small arrays
  let empty : Array[Int] = []
  assert_eq(empty.length(), 0)
  
  let single = [42]
  assert_eq(single.length(), 1)
  inspect(single[0], content="42")
  
  // Test arrays with boundary values
  let max_ints = Array::make(2, @int.max_value)
  assert_eq(max_ints.length(), 2)
  inspect(max_ints[0], content="2147483647")
  inspect(max_ints[1], content="2147483647")
  
  let min_ints = Array::make(2, @int.min_value)
  assert_eq(min_ints.length(), 2)
  inspect(min_ints[0], content="-2147483648")
  inspect(min_ints[1], content="-2147483648")
}

///|
test "array_type_consistency" {
  // Test that arrays maintain type consistency
  let int_array = [1, 2, 3]
  let string_array = ["a", "b", "c"]
  let float_array = [1.0, 2.0, 3.0]
  
  // Test lengths are correct regardless of type
  assert_eq(int_array.length(), 3)
  assert_eq(string_array.length(), 3)
  assert_eq(float_array.length(), 3)
  
  // Test access works for all types
  inspect(int_array[0], content="1")
  inspect(string_array[0], content="a")
  inspect(float_array[0], content="1.0")
}