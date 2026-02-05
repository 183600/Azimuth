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

// 标准 MoonBit 核心测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能

test "int_basic_arithmetic" {
  inspect(1 + 1, content="2")
  inspect(10 - 3, content="7")
  inspect(4 * 5, content="20")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
}

test "int_comparisons" {
  inspect(5 > 3, content="true")
  inspect(5 >= 5, content="true")
  inspect(3 < 5, content="true")
  inspect(5 <= 5, content="true")
  inspect(5 == 5, content="true")
  inspect(5 != 3, content="true")
}

test "int_bitwise_operations" {
  inspect(1 << 3, content="8")
  inspect(8 >> 1, content="4")
  inspect(6 & 3, content="2")
  inspect(5 | 3, content="7")
  inspect(5 ^ 3, content="6")
}

test "int_boundary_values" {
  inspect(Int::max_value, content="2147483647")
  inspect(Int::min_value, content="-2147483648")
  inspect(Int::max_value + 1, content="-2147483648")
  inspect(Int::min_value - 1, content="2147483647")
}

test "string_basic_operations" {
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  inspect(combined, content="Hello World")
  inspect(combined.length(), content="11")
  inspect(hello.length(), content="5")
  inspect("".length(), content="0")
}

test "string_character_access" {
  let text = "MoonBit"
  inspect(text[0], content="'M'")
  inspect(text[3], content="'n'")
  inspect(text.length(), content="7")
}

test "option_type_operations" {
  let some_value = Some(42)
  let none_value : Option[Int] = None
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  inspect(some_value.unwrap_or(0), content="42")
  inspect(none_value.unwrap_or(0), content="0")
}

test "result_type_operations" {
  let ok_value = Ok("success")
  let err_value = Err("failure")
  inspect(ok_value.is_ok(), content="true")
  inspect(ok_value.is_err(), content="false")
  inspect(err_value.is_ok(), content="false")
  inspect(err_value.is_err(), content="true")
  inspect(ok_value.unwrap_or("default"), content="success")
  inspect(err_value.unwrap_or("default"), content="default")
}

test "boolean_logic" {
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
}

test "array_basic_operations" {
  let numbers = [1, 2, 3, 4, 5]
  let empty : Array[Int] = []
  inspect(numbers.length(), content="5")
  inspect(empty.length(), content="0")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
}