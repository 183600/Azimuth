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
test "int basic arithmetic operations" {
  inspect(1 + 2, content="3")
  inspect(5 - 3, content="2")
  inspect(4 * 3, content="12")
  inspect(10 / 2, content="5")
  inspect(7 % 3, content="1")
}

///|
test "int comparison operations" {
  inspect(5 > 3, content="true")
  inspect(3 >= 3, content="true")
  inspect(2 < 4, content="true")
  inspect(1 <= 1, content="true")
  inspect(5 == 5, content="true")
  inspect(3 != 4, content="true")
}

///|
test "string basic operations" {
  let s1 = "Hello"
  let s2 = "World"
  inspect(s1.length(), content="5")
  inspect(s2.length(), content="5")
  inspect(s1 + " " + s2, content="Hello World")
  inspect("".length(), content="0")
}

///|
test "list basic operations" {
  let empty_list : @list.List[Int] = @list.empty()
  let single_element = @list.cons(42, empty_list)
  let multiple_elements = @list.from_array([1, 2, 3, 4, 5])
  
  inspect(empty_list.length(), content="0")
  inspect(single_element.length(), content="1")
  inspect(multiple_elements.length(), content="5")
  inspect(@list.singleton(99), content="@list.from_array([99])")
}

///|
test "list map and filter operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  let doubled = numbers.map(x => x * 2)
  let evens = numbers.filter(x => x % 2 == 0)
  
  inspect(doubled, content="@list.from_array([2, 4, 6, 8, 10])")
  inspect(evens, content="@list.from_array([2, 4])")
}

///|
test "array basic operations" {
  let empty_array : Array[Int] = []
  let single_array = [42]
  let multi_array = [1, 2, 3, 4, 5]
  
  inspect(empty_array.length(), content="0")
  inspect(single_array.length(), content="1")
  inspect(multi_array.length(), content="5")
  inspect(multi_array[0], content="1")
  inspect(multi_array[4], content="5")
}

///|
test "array zip_with operation" {
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let sum_array = @array.zip_with(arr1, arr2, (a, b) => a + b)
  let product_array = @array.zip_with(arr1, arr2, (a, b) => a * b)
  
  inspect(sum_array, content="[5, 7, 9]")
  inspect(product_array, content="[4, 10, 18]")
}

///|
test "boolean logic operations" {
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false || true, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
}

///|
test "option type operations" {
  let some_value : Option[Int] = Some(42)
  let no_value : Option[Int] = None
  let doubled_some = some_value.map(x => x * 2)
  let doubled_none = no_value.map(x => x * 2)
  
  inspect(some_value.is_some(), content="true")
  inspect(no_value.is_none(), content="true")
  inspect(doubled_some, content="Some(84)")
  inspect(doubled_none, content="None")
}

///|
test "result type operations" {
  let success_result : Result[Int, String] = Ok(42)
  let error_result : Result[Int, String] = Err("Error message")
  let mapped_success = success_result.map(x => x * 2)
  let mapped_error = error_result.map(x => x * 2)
  
  inspect(success_result.is_ok(), content="true")
  inspect(error_result.is_err(), content="true")
  inspect(mapped_success, content="Ok(84)")
  inspect(mapped_error, content="Err(\"Error message\")")
}