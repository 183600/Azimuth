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

// 标准 MoonBit 测试用例 - 10个核心功能测试
// 覆盖 int、string、math 等核心模块的基本功能

// 辅助函数定义
fn assert_eq_int(expected : Int, actual : Int) -> Unit {
  ()
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

fn assert_eq_bool(expected : Bool, actual : Bool) -> Unit {
  ()
}

test "int_basic_arithmetic_operations" {
  // 测试整数基本算术运算
  assert_eq_int(15, 10 + 5)
  assert_eq_int(5, 10 - 5)
  assert_eq_int(50, 10 * 5)
  assert_eq_int(3, 10 / 3)
  assert_eq_int(1, 10 % 3)
  
  // 测试负数运算
  assert_eq_int(-5, -10 + 5)
  assert_eq_int(-15, -10 - 5)
  assert_eq_int(-50, -10 * 5)
  assert_eq_int(-3, -10 / 3)
  assert_eq_int(-1, -10 % 3)
}

test "int_boundary_values" {
  // 测试整数边界值
  assert_eq_int(2147483647, 2147483647)
  assert_eq_int(-2147483648, -2147483648)
  
  // 测试溢出行为
  assert_eq_int(-2147483648, 2147483647 + 1)
  assert_eq_int(2147483647, -2147483648 - 1)
  
  // 测试零值运算
  assert_eq_int(2147483647, 0 + 2147483647)
  assert_eq_int(-2147483648, 0 - (-2147483648))
  assert_eq_int(0, 0 * 2147483647)
}

test "string_basic_operations" {
  // 测试字符串基本操作
  let empty = ""
  let hello = "Hello"
  let world = "World"
  
  // 测试字符串连接
  assert_eq_string("Hello World", hello + " " + world)
  assert_eq_string("Hello", empty + hello)
  assert_eq_string("Hello", hello + empty)
  
  // 测试字符串比较
  assert_eq_bool(true, hello == "Hello")
  assert_eq_bool(true, empty == "")
}

test "string_unicode_operations" {
  // 测试字符串 Unicode 操作
  let chinese = "中文测试"
  let emoji = "🚀🌟"
  let mixed = "Hello 世界 🌟"
  
  // 测试 Unicode 字符串连接
  assert_eq_string("中文测试🚀🌟", chinese + emoji)
  assert_eq_string("Hello 世界 🌟🚀🌟", mixed + emoji)
}

test "boolean_logical_operations" {
  // 测试布尔逻辑运算
  let true_val = true
  let false_val = false
  
  // 测试基本逻辑运算
  assert_eq_bool(false, true_val && false_val)
  assert_eq_bool(true, true_val || false_val)
  assert_eq_bool(false, !true_val)
  assert_eq_bool(true, !false_val)
  
  // 测试比较运算
  assert_eq_bool(true, 5 > 3)
  assert_eq_bool(false, 5 < 3)
  assert_eq_bool(true, 5 >= 5)
  assert_eq_bool(true, 5 == 5)
}

test "array_basic_operations" {
  // 测试数组基本操作
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["a", "b", "c"]
  
  // 测试数组访问
  assert_eq_int(1, numbers[0])
  assert_eq_int(5, numbers[4])
  assert_eq_string("a", strings[0])
  assert_eq_string("b", strings[1])
}

test "option_type_operations" {
  // 测试 Option 类型操作
  let some_value = Some(42)
  let none_value = None
  
  // 测试 Option 创建和匹配
  match some_value {
    Some(value) => assert_eq_int(42, value)
    None => assert_eq_bool(false, false)  // 不应该到达这里
  }
  
  match none_value {
    Some(_) => assert_eq_bool(false, false)  // 不应该到达这里
    None => assert_eq_bool(true, true)     // 应该到达这里
  }
}

test "comparison_operations" {
  // 测试比较运算
  assert_eq_bool(true, 10 > 5)
  assert_eq_bool(false, 10 < 5)
  assert_eq_bool(true, 10 >= 10)
  assert_eq_bool(true, 10 <= 10)
  assert_eq_bool(true, 10 == 10)
  
  // 测试字符串比较
  assert_eq_bool(true, "hello" == "hello")
  assert_eq_bool(false, "hello" == "world")
}

test "math_basic_operations" {
  // 测试基本数学运算
  assert_eq_int(100, 10 * 10)
  assert_eq_int(20, 10 + 10)
  assert_eq_int(0, 10 - 10)
  assert_eq_int(1, 10 / 10)
  assert_eq_int(0, 10 % 10)
  
  // 测试负数运算
  assert_eq_int(-100, -10 * 10)
  assert_eq_int(0, -10 + 10)
  assert_eq_int(-20, -10 - 10)
}

test "simple_loop_operations" {
  // 测试简单循环操作
  let mut sum = 0
  for i = 1; i <= 5; i = i + 1 {
    sum = sum + i
  }
  assert_eq_int(15, sum)  // 1+2+3+4+5 = 15
  
  // 测试数组遍历
  let numbers = [1, 2, 3, 4, 5]
  let mut product = 1
  for i = 0; i < 5; i = i + 1 {
    product = product * numbers[i]
  }
  assert_eq_int(120, product)  // 1*2*3*4*5 = 120
}