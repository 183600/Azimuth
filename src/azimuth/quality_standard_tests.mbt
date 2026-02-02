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

// 高质量标准 MoonBit 测试用例
// 包含 10 个核心功能测试，使用标准 MoonBit 测试语法

test "int_basic_arithmetic" {
  // 测试整数基本算术运算
  inspect(2 + 3, content="5")
  inspect(10 - 4, content="6")
  inspect(3 * 4, content="12")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
}

test "string_operations" {
  // 测试字符串操作
  let hello = "Hello"
  let world = "World"
  let empty = ""
  
  inspect(hello.length(), content="5")
  inspect(hello + " " + world, content="Hello World")
  inspect(empty.length(), content="0")
  inspect(String::default(), content="")
}

test "boolean_logic" {
  // 测试布尔逻辑运算
  let t = true
  let f = false
  
  inspect(t && t, content="true")
  inspect(t && f, content="false")
  inspect(t || f, content="true")
  inspect(f || f, content="false")
  inspect(!t, content="false")
  inspect(!f, content="true")
}

test "array_operations" {
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  let empty = []
  
  inspect(numbers.length(), content="5")
  inspect(empty.length(), content="0")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
}

test "option_type_handling" {
  // 测试 Option 类型处理
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
}

test "result_type_operations" {
  // 测试 Result 类型操作
  let success : Result[String, Int] = Ok("success")
  let failure : Result[String, Int] = Err(404)
  
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(failure.is_ok(), content="false")
  inspect(failure.is_err(), content="true")
}

test "comparison_operations" {
  // 测试比较操作
  inspect(5 > 3, content="true")
  inspect(5 < 3, content="false")
  inspect(5 >= 5, content="true")
  inspect(5 <= 3, content="false")
  inspect(5 == 5, content="true")
  inspect(5 != 3, content="true")
}

test "char_operations" {
  // 测试字符操作
  let char_a = 'a'
  let char_A = 'A'
  let digit_5 = '5'
  
  inspect(char_a.to_string(), content="a")
  inspect(char_A.to_string(), content="A")
  inspect(digit_5.to_string(), content="5")
  inspect('a' < 'b', content="true")
  inspect('A' < 'a', content="true")
}

test "math_functions" {
  // 测试数学函数
  inspect(@int.max_value() > 0, content="true")
  inspect(@int.min_value() < 0, content="true")
  inspect(Int::abs(42), content="42")
  inspect(Int::abs(-42), content="42")
  inspect(Int::abs(0), content="0")
}

test "type_conversions" {
  // 测试类型转换
  let int_val = 42
  let uint_val = int_val.reinterpret_as_uint()
  
  inspect(uint_val > 0, content="true")
  inspect(uint_val.to_int(), content="42")
  
  // 测试字符串到整数转换
  inspect("42".to_int(), content="Some(42)")
  inspect("abc".to_int(), content="None")
}