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
// 包含8个基本测试用例，涵盖核心功能

///|
test "integer_arithmetic_operations" {
  // 测试整数算术运算
  inspect(3 + 7, content="10")
  inspect(15 - 9, content="6")
  inspect(3 * 7, content="21")
  inspect(20 / 4, content="5")
  inspect(13 % 4, content="1")
  
  // 测试负数运算
  inspect(-3 + 8, content="5")
  inspect(-3 * 2, content="-6")
  inspect(-9 + 12, content="3")
}

///|
test "string_operations" {
  // 测试字符串基本操作
  let hello = "Hello"
  let world = "World"
  let moonbit = "MoonBit"
  
  // 字符串连接
  inspect(hello + world, content="HelloWorld")
  inspect(hello + " " + world, content="Hello World")
  
  // 字符串长度
  inspect(hello.length(), content="5")
  inspect(moonbit.length(), content="7")
  
  // 字符串比较
  inspect(hello == "Hello", content="true")
  inspect(hello == world, content="false")
  inspect(hello != world, content="true")
}

///|
test "boolean_logic_operations" {
  // 测试布尔逻辑运算
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false && false, content="false")
  
  inspect(true || true, content="true")
  inspect(true || false, content="true")
  inspect(false || false, content="false")
  
  inspect(!false, content="true")
  inspect(!true, content="false")
  
  // 复合布尔表达式
  inspect(true && (false || true), content="true")
  inspect(false || (true && false), content="false")
}

///|
test "option_type_operations" {
  // 测试 Option 类型操作
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  // 测试 is_some 和 is_none
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  
  // 测试 unwrap
  inspect(some_value.unwrap(), content="42")
  
  // 测试 unwrap_or
  inspect(some_value.unwrap_or(0), content="42")
  inspect(none_value.unwrap_or(0), content="0")
  
  // 测试 map
  let doubled = some_value.map(n => n * 2)
  inspect(doubled, content="Some(84)")
  
  let none_doubled = none_value.map(n => n * 2)
  inspect(none_doubled, content="None")
}

///|
test "result_type_operations" {
  // 测试 Result 类型操作
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  // 测试 is_ok 和 is_err
  inspect(ok_result.is_ok(), content="true")
  inspect(ok_result.is_err(), content="false")
  inspect(err_result.is_ok(), content="false")
  inspect(err_result.is_err(), content="true")
  
  // 测试 unwrap
  inspect(ok_result.unwrap(), content="42")
  
  // 测试 unwrap_or
  inspect(ok_result.unwrap_or(0), content="42")
  inspect(err_result.unwrap_or(0), content="0")
  
  // 测试 map
  let doubled = ok_result.map(n => n * 2)
  inspect(doubled, content="Ok(84)")
  
  let err_mapped = err_result.map(n => n * 2)
  inspect(err_mapped, content="Err(\"error\")")
}

///|
test "comparison_operations" {
  // 测试比较操作
  inspect(5 > 3, content="true")
  inspect(3 > 5, content="false")
  inspect(5 >= 5, content="true")
  inspect(3 >= 5, content="false")
  
  inspect(3 < 5, content="true")
  inspect(5 < 3, content="false")
  inspect(5 <= 5, content="true")
  inspect(5 <= 3, content="false")
  
  inspect(5 == 5, content="true")
  inspect(5 == 3, content="false")
  inspect(5 != 3, content="true")
  inspect(5 != 5, content="false")
  
  // 字符串比较
  inspect("apple" < "banana", content="true")
  inspect("zebra" < "apple", content="false")
}

///|
test "complex_expressions" {
  // 测试复杂表达式
  inspect(2 + 3 * 4, content="14")
  inspect(2 * 3 + 4, content="10")
  inspect(10 - 2 * 3, content="4")
  inspect(10 / 2 + 3, content="8")
  
  // 带括号的复杂表达式
  inspect((1 + 2) * (3 + 4), content="21")
  inspect(10 / (5 - 3), content="7")
  inspect(5 * (2 + 3), content="25")
  
  // 布尔和算术混合
  inspect(5 > 3 && 2 < 4, content="true")
  inspect(5 > 3 && 5 < 3, content="false")
  inspect(5 > 3 || 5 < 3, content="true")
}

///|
test "type_coercion_and_conversion" {
  // 测试类型转换和强制转换
  let int_val : Int = 42
  
  // 整数到字符串转换
  let int_as_string = int_val.to_string()
  inspect(int_as_string, content="\"42\"")
  
  // 测试不同数值类型的运算
  let small_int : Int = 5
  let large_int : Int = 1000000
  let sum = small_int + large_int
  inspect(sum, content="1000005")
  
  // 测试边界值
  inspect(0 + 0, content="0")
  inspect(0 + 1, content="1")
  inspect(0 - 1, content="-1")
}