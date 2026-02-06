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

// 导入必要的函数
using @builtin { inspect }

// 用户请求的 MoonBit 测试用例
// 覆盖核心功能的10个测试用例

test "int_arithmetic_operations" {
  // 测试整数基本算术运算
  inspect(10 + 5, content="15")
  inspect(10 - 5, content="5")
  inspect(10 * 5, content="50")
  inspect(10 / 5, content="2")
  inspect(10 % 3, content="1")
}

test "string_operations" {
  // 测试字符串基本操作
  let hello = "Hello"
  let world = "World"
  let greeting = hello + " " + world
  inspect(greeting, content="\"Hello World\"")
  inspect(greeting.length(), content="11")
  inspect(hello.length(), content="5")
  inspect("".length(), content="0")
}

test "array_operations" {
  // 测试数组基本操作
  let numbers = [1, 2, 3, 4, 5]
  let empty : Array[Int] = []
  inspect(numbers.length(), content="5")
  inspect(empty.length(), content="0")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
}

test "option_type_functionality" {
  // 测试 Option 类型操作
  let some_value = Some(42)
  let none_value : Option[Int] = None
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  inspect(some_value.unwrap_or(0), content="42")
  inspect(none_value.unwrap_or(0), content="0")
}

test "result_type_functionality" {
  // 测试 Result 类型操作
  let ok_value = Ok("success")
  let err_value = Err("failure")
  inspect(ok_value.is_ok(), content="true")
  inspect(ok_value.is_err(), content="false")
  inspect(err_value.is_ok(), content="false")
  inspect(err_value.is_err(), content="true")
  inspect(ok_value.unwrap_or("default"), content="success")
  inspect(err_value.unwrap_or("default"), content="default")
}

test "boolean_logic_operations" {
  // 测试布尔逻辑运算
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false && true, content="false")
  inspect(false && false, content="false")
  inspect(true || true, content="true")
  inspect(true || false, content="true")
  inspect(false || true, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
}

test "list_basic_operations" {
  // 测试 List 基本操作
  let ls = @list.from_array([1, 2, 3, 4, 5])
  let empty_list = @list.from_array([])
  inspect(ls.length(), content="5")
  inspect(empty_list.length(), content="0")
  inspect(@list.singleton(42), content="@list.from_array([42])")
}

test "character_operations" {
  // 测试字符操作
  let char_a = 'a'
  let char_Z = 'Z'
  inspect(char_a.to_string(), content="\"a\"")
  inspect(char_Z.to_string(), content="\"Z\"")
  inspect(char_a.to_ascii_code(), content="97")
  inspect(char_Z.to_ascii_code(), content="90")
}

test "math_functions" {
  // 测试数学函数
  inspect(@math.abs(-5), content="5")
  inspect(@math.abs(5), content="5")
  inspect(@math.max(3, 7), content="7")
  inspect(@math.min(3, 7), content="3")
  inspect(@math.pow(2, 3), content="8")
}

test "boundary_values" {
  // 测试边界值情况
  inspect(Int::max_value, content="2147483647")
  inspect(Int::min_value, content="-2147483648")
  inspect(Int::max_value + 1, content="-2147483648")
  inspect(Int::min_value - 1, content="2147483647")
}