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
test "int_basic_operations" {
  let x = 42
  let y = -42
  let z = 0
  
  inspect(x, content="42")
  inspect(y, content="-42")
  inspect(z, content="0")
  
  let sum = x + y
  inspect(sum, content="0")
  
  let product = x * 2
  inspect(product, content="84")
}

///|
test "list_basic_operations" {
  let empty_list : @list.List[Int] = @list.empty()
  let list1 = @list.cons(1, empty_list)
  let list2 = @list.cons(2, list1)
  let list3 = @list.cons(3, list2)
  
  inspect(list3.length(), content="3")
  
  let array_list = @list.from_array([1, 2, 3, 4, 5])
  inspect(array_list.length(), content="5")
}

///|
test "list_map_and_filter" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  let doubled = @list.map(numbers, fn(x) { x * 2 })
  let evens = @list.filter(numbers, fn(x) { x % 2 == 0 })
  
  inspect(doubled.length(), content="5")
  inspect(evens.length(), content="2")
  
  let sum = @list.fold_left(numbers, 0, fn(acc, x) { acc + x })
  inspect(sum, content="15")
}

///|
test "string_basic_operations" {
  let empty_str = ""
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  
  inspect(combined, content="Hello World")
  inspect(hello.length(), content="5")
  inspect(empty_str.length(), content="0")
  
  let chars = combined.to_array()
  inspect(chars.length(), content="11")
}

///|
test "string_operations" {
  let text = "moonbit"
  let text_length = text.length()
  
  inspect(text, content="moonbit")
  inspect(text_length, content="7")
  
  let substr = text.substring(0, 4)
  inspect(substr, content="moon")
}

///|
test "option_basic_operations" {
  let some_value = Some(42)
  let none_value : Option[Int] = None
  
  let is_some_some = some_value.is_some()
  let is_none_some = some_value.is_none()
  let is_some_none = none_value.is_some()
  let is_none_none = none_value.is_none()
  
  inspect(is_some_some, content="true")
  inspect(is_none_some, content="false")
  inspect(is_some_none, content="false")
  inspect(is_none_none, content="true")
}

///|
test "option_map_operation" {
  let some_value = Some(42)
  let mapped = some_value.map(fn(x) { x * 2 })
  
  let default_value = mapped.unwrap_or(0)
  inspect(default_value, content="84")
}

///|
test "option_chain_operations" {
  let opt1 = Some(5)
  let opt2 : Option[Int] = None
  
  let result1 = opt1.and_then(fn(x) { if x > 3 { Some(x * 2) } else { None } })
  let result2 = opt2.and_then(fn(x) { Some(x * 2) })
  
  let final_value = result1.unwrap_or(0)
  let fallback_value = result2.unwrap_or(0)
  
  inspect(final_value, content="10")
  inspect(fallback_value, content="0")
}

///|
test "math_basic_operations" {
  let abs_negative = @math.abs(-3.14)
  let abs_positive = @math.abs(3.14)
  
  inspect(abs_negative, content="3.14")
  inspect(abs_positive, content="3.14")
  
  let power = @math.pow(2.0, 3.0)
  let floor_power = @math.floor(power)
  
  inspect(floor_power, content="8")
  
  let sqrt_val = @math.sqrt(16.0)
  inspect(sqrt_val, content="4")
}

///|
test "math_trigonometric_functions" {
  let angle = @math.pi() / 4.0
  
  let sin_val = @math.sin(angle)
  let cos_val = @math.cos(angle)
  
  let rounded_sin = @math.round(sin_val * 1000.0) / 1000.0
  let rounded_cos = @math.round(cos_val * 1000.0) / 1000.0
  
  inspect(rounded_sin, content="0.707")
  inspect(rounded_cos, content="0.707")
  
  let tan_val = @math.tan(angle)
  let rounded_tan = @math.round(tan_val * 1000.0) / 1000.0
  
  inspect(rounded_tan, content="1")
}