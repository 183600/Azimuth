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
test "integer absolute value" {
  assert_eq(Int::abs(5), 5)
  assert_eq(Int::abs(-5), 5)
  assert_eq(Int::abs(0), 0)
}

///|
test "integer comparison operations" {
  // Basic comparisons
  assert_eq(5 > 3, true)
  assert_eq(5 < 3, false)
  assert_eq(5 >= 5, true)
  assert_eq(5 <= 3, false)
  assert_eq(5 == 5, true)
  assert_eq(5 != 3, true)
}

///|
test "integer arithmetic operations" {
  // Addition
  assert_eq(3 + 5, 8)
  
  // Subtraction
  assert_eq(10 - 3, 7)
  
  // Multiplication
  assert_eq(4 * 5, 20)
  assert_eq(0 * 42, 0)
  
  // Division
  assert_eq(20 / 4, 5)
  assert_eq(7 / 3, 2) // Integer division
  assert_eq(0 / 5, 0)
}

///|
test "integer bit operations" {
  // Bitwise AND
  assert_eq(6 & 3, 2) // 110 & 011 = 010
  assert_eq(15 & 0, 0)
  
  // Bitwise OR
  assert_eq(6 | 3, 7) // 110 | 011 = 111
  assert_eq(15 | 0, 15)
  
  // Bitwise XOR
  assert_eq(6 ^ 3, 5) // 110 ^ 011 = 101
  assert_eq(15 ^ 15, 0)
  
  // Left shift
  assert_eq(1 << 3, 8)
  assert_eq(0 << 5, 0)
  
  // Right shift
  assert_eq(8 >> 2, 2)
  assert_eq(7 >> 1, 3)
}

///|
test "string basic operations" {
  let empty = ""
  let hello = "Hello"
  let world = "World"
  
  // String concatenation
  let greeting = hello + " " + world
  inspect(greeting, content="\"Hello World\"")
  
  // String length
  assert_eq(hello.length(), 5)
  assert_eq(empty.length(), 0)
  assert_eq(greeting.length(), 11)
  
  // String comparison
  assert_eq(hello == "Hello", true)
  assert_eq(hello != "World", true)
  assert_eq(empty == "", true)
}

///|
test "list basic operations" {
  let empty_list = @list.empty()
  let single_element = @list.from_array([42])
  let multiple_elements = @list.from_array([1, 2, 3, 4, 5])
  
  // List length
  assert_eq(empty_list.length(), 0)
  assert_eq(single_element.length(), 1)
  assert_eq(multiple_elements.length(), 5)
  
  // List head and tail
  assert_eq(single_element.head(), Some(42))
  assert_eq(multiple_elements.head(), Some(1))
  assert_eq(empty_list.head(), None)
  
  // List is_empty
  assert_eq(empty_list.is_empty(), true)
  assert_eq(single_element.is_empty(), false)
  assert_eq(multiple_elements.is_empty(), false)
}

///|
test "list map and filter operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // Map operation
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, @list.from_array([2, 4, 6, 8, 10]))
  
  // Filter operation
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens, @list.from_array([2, 4]))
  
  // Map with index
  let indexed = numbers.mapi((i, x) => x + i)
  assert_eq(indexed, @list.from_array([1, 3, 5, 7, 9]))
}

///|
test "list fold operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // Fold (reduce) operation
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  assert_eq(sum, 15)
  
  let product = numbers.fold(init=1, (acc, x) => acc * x)
  assert_eq(product, 120)
  
  // Fold with index
  let indexed_sum = numbers.foldi(init=0, (i, acc, x) => acc + x * i)
  assert_eq(indexed_sum, 40) // 0*1 + 1*2 + 2*3 + 3*4 + 4*5 = 0 + 2 + 6 + 12 + 20 = 40
}

///|
test "list reverse and concat operations" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  
  // Reverse operation
  let reversed = list1.rev()
  assert_eq(reversed, @list.from_array([3, 2, 1]))
  
  // Concat operation
  let concatenated = list1.concat(list2)
  assert_eq(concatenated, @list.from_array([1, 2, 3, 4, 5, 6]))
  
  // Rev concat operation
  let rev_concatenated = list1.rev_concat(list2)
  assert_eq(rev_concatenated, @list.from_array([3, 2, 1, 4, 5, 6]))
}

///|
test "option type basic operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  // is_some and is_none
  assert_eq(some_value.is_some(), true)
  assert_eq(some_value.is_none(), false)
  assert_eq(none_value.is_some(), false)
  assert_eq(none_value.is_none(), true)
  
  // unwrap and unwrap_or
  assert_eq(some_value.unwrap(), 42)
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
}

///|
test "option type map operations" {
  let some_value : Int? = Some(5)
  let none_value : Int? = None
  
  // Map operation
  let mapped_some = some_value.map(x => x * 2)
  assert_eq(mapped_some, Some(10))
  
  let mapped_none = none_value.map(x => x * 2)
  assert_eq(mapped_none, None)
}

///|
test "result type basic operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error message")
  
  // is_ok and is_err
  assert_eq(ok_result.is_ok(), true)
  assert_eq(ok_result.is_err(), false)
  assert_eq(err_result.is_ok(), false)
  assert_eq(err_result.is_err(), true)
  
  // unwrap operations
  assert_eq(ok_result.unwrap(), 42)
  assert_eq(ok_result.unwrap_or(0), 42)
  assert_eq(err_result.unwrap_or(0), 0)
  assert_eq(err_result.unwrap_err(), "error message")
}

///|
test "result type map operations" {
  let ok_result : Result[Int, String] = Ok(5)
  let err_result : Result[Int, String] = Err("error")
  
  // Map operation
  let mapped_ok = ok_result.map(x => x * 2)
  assert_eq(mapped_ok, Ok(10))
  
  let mapped_err = err_result.map(x => x * 2)
  assert_eq(mapped_err, Err("error"))
  
  // Map error operation
  let mapped_err_ok = ok_result.map_err(e => e + " (mapped)")
  assert_eq(mapped_err_ok, Ok(5))
  
  let mapped_err_err = err_result.map_err(e => e + " (mapped)")
  assert_eq(mapped_err_err, Err("error (mapped)"))
}

///|
test "result type and_then operation" {
  let ok_result : Result[Int, String] = Ok(10)
  let err_result : Result[Int, String] = Err("initial error")
  
  // Safe division function
  let safe_divide = (x : Int) => if x == 0 { Err("division by zero") } else { Ok(100 / x) }
  
  // And then operation
  let result_ok = ok_result.and_then(safe_divide)
  assert_eq(result_ok, Ok(10))
  
  let result_err = err_result.and_then(safe_divide)
  assert_eq(result_err, Err("initial error"))
  
  let result_zero = Ok(0).and_then(safe_divide)
  assert_eq(result_zero, Err("division by zero"))
}