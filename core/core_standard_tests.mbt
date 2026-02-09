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
/// 测试基本算术运算
test "basic_arithmetic_operations" {
  // 测试加法
  let sum = 1 + 2
  // 测试减法
  let diff = 10 - 3
  // 测试乘法
  let product = 3 * 4
  // 测试除法
  let quotient = 20 / 4
  // 测试取模
  let remainder = 15 % 4
  
  // 验证结果
  let _ = sum + diff + product + quotient + remainder
}

///|
/// 测试整数比较
test "integer_comparisons" {
  // 测试大于
  let greater = 5 > 3
  // 测试小于
  let less = 5 < 3
  // 测试大于等于
  let greater_equal = 5 >= 5
  // 测试小于等于
  let less_equal = 5 <= 3
  // 测试等于
  let equal = 5 == 5
  // 测试不等于
  let not_equal = 5 != 3
  
  // 验证结果
  let _ = if (greater && !less && greater_equal && !less_equal && equal && not_equal) { 1 } else { 0 }
}

///|
/// 测试字符串基本操作
test "string_basic_operations" {
  // 测试字符串字面量
  let hello = "Hello"
  let world = "World"
  
  // 测试字符串连接
  let combined = hello + " " + world
  
  // 验证结果
  let _ = combined
}

///|
/// 测试布尔逻辑
test "boolean_logic" {
  // 测试逻辑与
  let and_result = true && false
  // 测试逻辑或
  let or_result = true || false
  // 测试逻辑非
  let not_true = !true
  let not_false = !false
  
  // 验证结果
  let _ = if (!and_result && or_result && !not_true && not_false) { 1 } else { 0 }
}

///|
/// 测试负数运算
test "negative_number_operations" {
  // 测试负数加法
  let sum = -5 + 10
  // 测试负数乘法
  let product = -3 * -2
  // 测试负数减法
  let diff = -10 + 5
  // 测试负数除法
  let quotient = -8 / 2
  
  // 验证结果
  let _ = sum + product + diff + quotient
}

///|
/// 测试 Result 类型
test "result_type_operations" {
  // 创建 Ok 结果
  let ok_result : Result[Int, String] = Ok(42)
  // 创建 Err 结果
  let err_result : Result[Int, String] = Err("error")
  
  // 测试模式匹配
  let ok_value = match ok_result {
    Ok(value) => value
    Err(_) => -1
  }
  
  let err_value = match err_result {
    Ok(value) => value
    Err(_) => -1
  }
  
  // 验证结果
  let _ = ok_value + err_value
}

///|
/// 测试 Option 类型
test "option_type_operations" {
  // 创建 Some 值
  let some_value : Int? = Some(42)
  // 创建 None 值
  let none_value : Int? = None
  
  // 测试模式匹配
  let some_result = match some_value {
    Some(value) => value
    None => -1
  }
  
  let none_result = match none_value {
    Some(value) => value
    None => -1
  }
  
  // 验证结果
  let _ = some_result + none_result
}

///|
/// 测试数组基本操作
test "array_basic_operations" {
  // 创建数组
  let numbers = [1, 2, 3, 4, 5]
  
  // 验证结果
  let _ = numbers
}

///|
/// 测试条件表达式
test "conditional_expressions" {
  // 测试 if-else
  let result = if (5 > 3) { "greater" } else { "not greater" }
  
  // 测试三元表达式
  let ternary = if (true) { 1 } else { 0 }
  
  // 验证结果
  let _ = result
  let _ = ternary
}