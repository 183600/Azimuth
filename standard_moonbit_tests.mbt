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
test "int max_value and min_value" {
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
}

///|
test "int abs function" {
  inspect(@int.abs(5), content="5")
  inspect(@int.abs(-5), content="5")
  inspect(@int.abs(0), content="0")
  inspect(@int.abs(@int.min_value), content="-2147483648") // Edge case: min_value abs is still min_value
}

///|
test "string default function" {
  let empty_string = @string.default()
  inspect(empty_string, content="")
}

///|
test "list basic operations" {
  let empty_list : @list.List[Int] = @list.empty()
  let single_element = @list.cons(1, empty_list)
  let multiple_elements = @list.from_array([1, 2, 3, 4, 5])
  
  inspect(empty_list.length(), content="0")
  inspect(single_element.length(), content="1")
  inspect(multiple_elements.length(), content="5")
}

///|
test "list head and tail operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let empty : @list.List[Int] = @list.empty()
  
  inspect(list.head(), content="Some(1)")
  inspect(empty.head(), content="None")
  inspect(list.unsafe_tail(), content="@list.from_array([2, 3, 4, 5])")
}

///|
test "list map and filter operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="@list.from_array([2, 4, 6, 8, 10])")
  
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="@list.from_array([2, 4])")
  
  let squares = numbers.map(x => x * x)
  inspect(squares, content="@list.from_array([1, 4, 9, 16, 25])")
}

///|
test "list fold operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  let empty : @list.List[Int] = @list.empty()
  
  let sum = numbers.fold((acc, x) => acc + x, init=0)
  inspect(sum, content="15")
  
  let product = numbers.fold((acc, x) => acc * x, init=1)
  inspect(product, content="120")
  
  let empty_sum = empty.fold((acc, x) => acc + x, init=0)
  inspect(empty_sum, content="0")
}

///|
test "result basic operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error message")
  
  inspect(ok_result is Ok(_), content="true")
  inspect(err_result is Ok(_), content="false")
  inspect(ok_result is Err(_), content="false")
  inspect(err_result is Err(_), content="true")
}

///|
test "result unwrap operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  inspect(ok_result.unwrap(), content="42")
  inspect(ok_result.or(0), content="42")
  inspect(err_result.or(0), content="0")
  inspect(err_result.or_else(() => -1), content="-1")
}

///|
test "result map operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  let mapped_ok = ok_result.map(x => x * 2)
  inspect(mapped_ok, content="Ok(84)")
  
  let mapped_err = err_result.map(x => x * 2)
  inspect(mapped_err, content="Err(\"error\")")
  
  let mapped_err_msg = err_result.map_err(s => s + "!")
  inspect(mapped_err_msg, content="Err(\"error!\")")
}