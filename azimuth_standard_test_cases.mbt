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

// 标准MoonBit测试用例集合

///|
test "option_map_transformation" {
  let some_value = Some(5)
  let none_value : Int? = None
  
  let mapped_some = some_value.map(x => x * 2)
  let mapped_none = none_value.map(x => x * 2)
  
  inspect(mapped_some, content="Some(10)")
  inspect(mapped_none, content="None")
}

///|
test "option_is_some_and_none" {
  let some_value = Some(42)
  let none_value : Int? = None
  
  assert_true(some_value.is_some())
  assert_false(some_value.is_none())
  assert_false(none_value.is_some())
  assert_true(none_value.is_none())
}

///|
test "list_fold_left_sum" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  let sum = numbers.fold_left(0, (acc, x) => acc + x)
  inspect(sum, content="15")
}

///|
test "list_filter_even_numbers" {
  let numbers = @list.from_array([1, 2, 3, 4, 5, 6])
  let even_numbers = numbers.filter(x => x % 2 == 0)
  inspect(even_numbers, content="@list.from_array([2, 4, 6])")
}

///|
test "list_map_transformation" {
  let numbers = @list.from_array([1, 2, 3])
  let squared = numbers.map(x => x * x)
  inspect(squared, content="@list.from_array([1, 4, 9])")
}

///|
test "string_operations" {
  let greeting = "Hello"
  let world = "World"
  let combined = greeting + ", " + world + "!"
  inspect(combined, content="Hello, World!")
  inspect(combined.length(), content="13")
}

///|
test "int_max_min_functions" {
  let a = 10
  let b = 20
  inspect(Int::max(a, b), content="20")
  inspect(Int::min(a, b), content="10")
  inspect(Int::max(-5, -3), content="-3")
  inspect(Int::min(-5, -3), content="-5")
}

///|
test "int_power_function" {
  inspect(Int::pow(2, 3), content="8")
  inspect(Int::pow(5, 0), content="1")
  inspect(Int::pow(0, 5), content="0")
  inspect(Int::pow(-2, 3), content="-8")
}

///|
test "option_unwrap_or_default" {
  let some_value = Some(100)
  let none_value : Int? = None
  
  inspect(some_value.unwrap_or(0), content="100")
  inspect(none_value.unwrap_or(0), content="0")
  
  let some_string = Some("test")
  let none_string : String? = None
  
  inspect(some_string.unwrap_or("default"), content="\"test\"")
  inspect(none_string.unwrap_or("default"), content="\"default\"")
}

///|
test "list_reverse_function" {
  let original = @list.from_array([1, 2, 3, 4, 5])
  let reversed = original.reverse()
  inspect(reversed, content="@list.from_array([5, 4, 3, 2, 1])")
  
  let empty_list : @list.List[Int] = @list.empty()
  let reversed_empty = empty_list.reverse()
  inspect(reversed_empty, content="@list.from_array([])")
}