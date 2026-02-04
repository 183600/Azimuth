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

// 标准 MoonBit 测试用例
// 包含 8 个核心功能测试，使用标准 MoonBit 测试语法

test "int_arithmetic_operations" {
  // 测试整数算术运算
  @azimuth.assert_eq(5, 2 + 3)
  @azimuth.assert_eq(6, 10 - 4)
  @azimuth.assert_eq(12, 3 * 4)
  @azimuth.assert_eq(5, 15 / 3)
  @azimuth.assert_eq(2, 17 % 5)
}

test "string_operations" {
  // 测试字符串操作
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  @azimuth.assert_eq_string("Hello World", combined)
  @azimuth.assert_eq(0, "".length())
  @azimuth.assert_eq(7, "MoonBit".length())
}

test "boolean_logic" {
  // 测试布尔逻辑运算
  let t = true
  let f = false
  @azimuth.assert_true(t && t)
  @azimuth.assert_false(t && f)
  @azimuth.assert_true(t || f)
  @azimuth.assert_false(f || f)
  @azimuth.assert_false(!t)
  @azimuth.assert_true(!f)
}

test "array_operations" {
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  @azimuth.assert_eq(5, numbers.length())
  @azimuth.assert_eq(1, numbers[0])
  @azimuth.assert_eq(5, numbers[4])
  
  let doubled = numbers.map(x => x * 2)
  @azimuth.assert_eq(2, doubled[0])
  @azimuth.assert_eq(10, doubled[4])
  
  let evens = numbers.filter(x => x % 2 == 0)
  @azimuth.assert_eq(2, evens[0])
  @azimuth.assert_eq(4, evens[1])
}

test "comparison_operations" {
  // 测试比较操作
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_true(5 >= 5)
  @azimuth.assert_false(5 <= 3)
  @azimuth.assert_true(5 == 5)
  @azimuth.assert_true(5 != 3)
}

test "option_type_handling" {
  // 测试 Option 类型处理
  let some_val : Int? = Some(42)
  let none_val : Int? = None
  
  @azimuth.assert_true(some_val is Some(_))
  @azimuth.assert_false(some_val is None)
  @azimuth.assert_false(none_val is Some(_))
  @azimuth.assert_true(none_val is None)
  
  let doubled = some_val.map(x => x * 2)
  @azimuth.assert_true(doubled is Some(_))
}

test "result_type_handling" {
  // 测试 Result 类型处理
  let success : Result[String, Int] = Ok("success")
  let failure : Result[String, Int] = Err(404)
  
  @azimuth.assert_true(success is Ok(_))
  @azimuth.assert_false(success is Err(_))
  @azimuth.assert_false(failure is Ok(_))
  @azimuth.assert_true(failure is Err(_))
}

test "tuple_operations" {
  // 测试元组操作
  let pair = (42, "answer")
  let triple = (1, 2, 3)
  @azimuth.assert_eq(42, pair.0)
  @azimuth.assert_eq_string("answer", pair.1)
  @azimuth.assert_eq(1, triple.0)
  @azimuth.assert_eq(2, triple.1)
  @azimuth.assert_eq(3, triple.2)
}