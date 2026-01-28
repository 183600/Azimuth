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
test "int_basic_arithmetic" {
  // 测试整数基本算术运算
  let a = 10
  let b = 3
  assert_eq(a + b, 13)
  assert_eq(a - b, 7)
  assert_eq(a * b, 30)
  assert_eq(a / b, 3)
  assert_eq(a % b, 1)
}

///|
test "int_boundary_values" {
  // 测试整数边界值
  assert_eq(@int.max_value, 2147483647)
  assert_eq(@int.min_value, -2147483648)
  // 测试溢出行为
  assert_eq(@int.max_value + 1, @int.min_value)
  assert_eq(@int.min_value - 1, @int.max_value)
}

///|
test "string_concatenation" {
  // 测试字符串连接
  let s1 = "Hello"
  let s2 = "MoonBit"
  let result = s1 + " " + s2
  assert_eq(result, "Hello MoonBit")
  
  // 测试空字符串
  let empty = ""
  assert_eq(empty + "test", "test")
  assert_eq("test" + empty, "test")
}

///|
test "string_length_and_comparison" {
  // 测试字符串长度和比较
  let s1 = "apple"
  let s2 = "banana"
  let s3 = "apple"
  
  assert_eq(s1.length(), 5)
  assert_eq(s2.length(), 6)
  assert_true(s1 == s3)
  assert_false(s1 == s2)
  assert_true(s1 != s2)
}

///|
test "boolean_logical_operations" {
  // 测试布尔逻辑运算
  let t = true
  let f = false
  
  assert_true(t && t)
  assert_false(t && f)
  assert_false(f && t)
  assert_false(f && f)
  
  assert_true(t || t)
  assert_true(t || f)
  assert_true(f || t)
  assert_false(f || f)
  
  assert_true(!f)
  assert_false(!t)
}

///|
test "option_type_operations" {
  // 测试Option类型操作
  let some_value : Int = Some(42)
  let none_value : Int = None
  
  assert_true(some_value.is_some())
  assert_false(some_value.is_none())
  assert_false(none_value.is_some())
  assert_true(none_value.is_none())
  
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
}

///|
test "result_type_operations" {
  // 测试Result类型操作
  let ok_value : String = Ok("success")
  let err_value : String = Err("error")
  
  assert_true(ok_value.is_ok())
  assert_false(ok_value.is_err())
  assert_false(err_value.is_ok())
  assert_true(err_value.is_err())
  
  assert_eq(ok_value.unwrap_or("default"), "success")
  assert_eq(err_value.unwrap_or("default"), "default")
}

///|
test "array_basic_operations" {
  // 测试数组基本操作
  let arr = [1, 2, 3, 4, 5]
  
  assert_eq(arr.length(), 5)
  assert_eq(arr[0], 1)
  assert_eq(arr[4], 5)
  
  // 测试数组连接
  let arr2 = [6, 7, 8]
  let combined = arr.concat(arr2)
  assert_eq(combined.length(), 8)
  assert_eq(combined[5], 6)
}

///|
test "type_conversions" {
  // 测试类型转换
  let int_val = 42
  let string_val = "42"
  
  // 测试整数到字符串的隐式转换
  let converted = int_val.to_string()
  assert_eq(converted, string_val)
  
  // 测试字符串解析
  let parsed = string_val.parse_int()
  assert_eq(parsed.unwrap_or(0), 42)
}

///|
test "comparison_operations" {
  // 测试比较操作
  let a = 10
  let b = 20
  let c = 10
  
  assert_true(a < b)
  assert_false(b < a)
  assert_true(a <= b)
  assert_true(a <= c)
  assert_true(b > a)
  assert_false(a > b)
  assert_true(b >= a)
  assert_true(a >= c)
  assert_true(a == c)
  assert_false(a == b)
  assert_true(a != b)
  assert_false(a != c)
}