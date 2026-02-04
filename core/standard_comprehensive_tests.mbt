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

test "int_basic_arithmetic" {
  inspect(1 + 2, content="3")
  inspect(10 - 3, content="7")
  inspect(4 * 5, content="20")
  inspect(20 / 4, content="5")
  inspect(15 % 4, content="3")
}

test "int_comparison" {
  inspect(5 > 3, content="true")
  inspect(3 >= 3, content="true")
  inspect(2 < 4, content="true")
  inspect(4 <= 4, content="true")
  inspect(5 == 5, content="true")
  inspect(5 != 3, content="true")
}

test "int_abs_function" {
  inspect(Int::abs(5), content="5")
  inspect(Int::abs(-5), content="5")
  inspect(Int::abs(0), content="0")
}

test "int_min_max_values" {
  inspect(Int.max_value, content="2147483647")
  inspect(Int.min_value, content="-2147483648")
}

test "string_concatenation" {
  let s1 = "Hello"
  let s2 = " "
  let s3 = "World"
  inspect(s1 + s2 + s3, content="Hello World")
}

test "string_length" {
  inspect("".length(), content="0")
  inspect("a".length(), content="1")
  inspect("hello".length(), content="5")
}

test "string_equality" {
  inspect("abc" == "abc", content="true")
  inspect("abc" != "def", content="true")
}

test "array_basic_operations" {
  let arr : Array[Int] = [1, 2, 3, 4, 5]
  inspect(arr.length(), content="5")
  inspect(arr[0], content="1")
  inspect(arr[4], content="5")
}

test "array_creation" {
  let empty : Array[Int] = []
  inspect(empty.length(), content="0")
  
  let single : Array[Int] = [42]
  inspect(single.length(), content="1")
  inspect(single[0], content="42")
}

test "option_some_none" {
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  inspect(some_value is Some[_], content="true")
  inspect(none_value is None, content="true")
}

test "option_unwrap" {
  let some_value : Option[Int] = Some(42)
  inspect(some_value.unwrap(), content="42")
}

test "option_map" {
  let some_value : Option[Int] = Some(5)
  let mapped = some_value.map(fn(x) { x * 2 })
  inspect(mapped.unwrap(), content="10")
}

test "result_ok_err" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  inspect(ok_result is Ok[_], content="true")
  inspect(err_result is Err[_], content="true")
}

test "result_unwrap" {
  let ok_result : Result[Int, String] = Ok(42)
  inspect(ok_result.unwrap(), content="42")
}

test "result_map" {
  let ok_result : Result[Int, String] = Ok(5)
  let mapped = ok_result.map(fn(x) { x * 2 })
  inspect(mapped.unwrap(), content="10")
}

test "char_basic_operations" {
  let c = 'A'
  inspect(c.to_int(), content="65")
  inspect(Char::from_int(65), content="'A'")
}

test "char_comparison" {
  inspect('A' == 'A', content="true")
  inspect('A' != 'B', content="true")
  inspect('A' < 'B', content="true")
}

test "bool_operations" {
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
}

test "unit_type" {
  let u : Unit = ()
  inspect(u, content="()")
}