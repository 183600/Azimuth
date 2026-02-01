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

// 综合 MoonBit 标准测试用例 - 覆盖核心功能

///|
test "integer arithmetic operations" {
  let a = 42
  let b = 17
  
  inspect(a + b, content="59")
  inspect(a - b, content="25")
  inspect(a * b, content="714")
  inspect(a / b, content="2")
  inspect(a % b, content="8")
  
  // Test bounds
  inspect(Int::max_value, content="2147483647")
  inspect(Int::min_value, content="-2147483648")
}

///|
test "string manipulation operations" {
  let hello = "Hello"
  let world = "World"
  let combined = hello ++ ", " ++ world ++ "!"
  
  inspect(combined, content="\"Hello, World!\"")
  inspect(hello.length(), content="5")
  inspect(world.length(), content="5")
  inspect(combined.length(), content="13")
  
  // Test string indexing
  inspect(hello[0], content="'H'")
  inspect(world[4], content="'d'")
}

///|
test "list basic operations" {
  let empty_list : @list.List[Int] = @list.empty()
  let list1 = @list.cons(1, @list.cons(2, @list.cons(3, empty_list)))
  let list2 = @list.from_array([4, 5, 6])
  
  inspect(@list.length(list1), content="3")
  inspect(@list.length(list2), content="3")
  inspect(@list.nth(list1, 0), content="Some(1)")
  inspect(@list.nth(list1, 2), content="Some(3)")
  inspect(@list.nth(list1, 3), content="None")
}

///|
test "array operations and access" {
  let numbers = [10, 20, 30, 40, 50]
  let strings = ["apple", "banana", "cherry"]
  
  inspect(numbers.length(), content="5")
  inspect(strings.length(), content="3")
  inspect(numbers[2], content="30")
  inspect(strings[1], content="\"banana\"")
  
  // Test array modification
  let mutable_numbers = [1, 2, 3]
  mutable_numbers[1] = 99
  inspect(mutable_numbers[1], content="99")
}

///|
test "boolean logic operations" {
  let true_val = true
  let false_val = false
  
  inspect(true_val && false_val, content="false")
  inspect(true_val || false_val, content="true")
  inspect(not true_val, content="false")
  inspect(not false_val, content="true")
  
  // Test comparisons
  inspect(5 > 3, content="true")
  inspect(2 >= 2, content="true")
  inspect(1 < 4, content="true")
  inspect(3 <= 3, content="true")
  inspect(5 == 5, content="true")
  inspect(4 != 2, content="true")
}

///|
test "option type operations" {
  let some_value = Some(42)
  let none_value : Option[Int] = None
  
  inspect(some_value, content="Some(42)")
  inspect(none_value, content="None")
  
  // Test option operations
  inspect(Option::is_some(some_value), content="true")
  inspect(Option::is_none(some_value), content="false")
  inspect(Option::is_none(none_value), content="true")
  inspect(Option::unwrap_or(some_value, 0), content="42")
  inspect(Option::unwrap_or(none_value, 0), content="0")
}

///|
test "result type operations" {
  let ok_result : Result[Int, String] = Ok(100)
  let err_result : Result[Int, String] = Err("error message")
  
  inspect(ok_result, content="Ok(100)")
  inspect(err_result, content="Err(\"error message\")")
  
  // Test result operations
  inspect(Result::is_ok(ok_result), content="true")
  inspect(Result::is_err(ok_result), content="false")
  inspect(Result::is_err(err_result), content="true")
  inspect(Result::unwrap_or(ok_result, 0), content="100")
  inspect(Result::unwrap_or(err_result, 0), content="0")
}

///|
test "type conversions" {
  let int_val = 65
  let byte_val = int_val.to_byte()
  let char_val = 'A'
  
  inspect(byte_val, content="65'A'")
  inspect(char_val.to_int(), content="65")
  
  // Test string to int conversion
  inspect("123".to_int(), content="Some(123)")
  inspect("abc".to_int(), content="None")
  
  // Test int to string
  inspect(42.to_string(), content="\"42\"")
  inspect(-7.to_string(), content="\"-7\"")
}

///|
test "tuple operations" {
  let pair = (10, "hello")
  let triple = (1, 2, 3)
  
  inspect(pair, content="(10, \"hello\")")
  inspect(triple, content="(1, 2, 3)")
  
  // Test tuple access
  inspect(pair.0, content="10")
  inspect(pair.1, content="\"hello\"")
  inspect(triple.0, content="1")
  inspect(triple.2, content="3")
}

///|
test "comprehensive functionality test" {
  // Combine multiple operations
  let numbers = [1, 2, 3, 4, 5]
  let sum = numbers.fold_left(0, fn(acc, x) { acc + x })
  let doubled = numbers.map(fn(x) { x * 2 })
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  
  inspect(sum, content="15")
  inspect(doubled.length(), content="5")
  inspect(doubled[0], content="2")
  inspect(doubled[4], content="10")
  inspect(evens.length(), content="2")
  inspect(evens[0], content="2")
  inspect(evens[1], content="4")
  
  // Test string operations with numbers
  let text_numbers = ["1", "2", "3"]
  let parsed_numbers = text_numbers.map(fn(s) { Option::unwrap_or(s.to_int(), 0) })
  let text_sum = parsed_numbers.fold_left(0, fn(acc, x) { acc + x })
  
  inspect(text_sum, content="6")
}