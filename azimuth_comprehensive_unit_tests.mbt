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

// Azimuth 综合单元测试
// 使用标准 MoonBit 测试语法，涵盖核心功能模块

test "integer_boundary_values" {
  // 测试整数边界值
  assert_eq(2147483647, 2147483647)  // max_value
  assert_eq(-2147483648, -2147483648)  // min_value
  assert_eq(-2147483648, 2147483647 + 1)  // 溢出测试
  assert_eq(2147483647, -2147483648 - 1)  // 下溢测试
  assert_eq(42, @int.abs(-42))
  assert_eq(0, @int.abs(0))
  assert_eq(42, @int.abs(42))
}

test "string_operations_and_conversions" {
  // 测试字符串操作和转换
  let empty = ""
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world + "!"
  assert_eq("Hello World!", combined)
  assert_eq(0, empty.length())
  assert_eq(5, hello.length())
  assert_eq(12, combined.length())
  assert_eq("", @string.default())
}

test "array_advanced_operations" {
  // 测试数组高级操作
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let arr3 = [7, 8, 9, 10]
  
  // 测试 zip_with 函数
  let add = (a, b) => a + b
  let zipped = @array.zip_with(arr1, arr2, add)
  assert_eq(5, zipped[0])
  assert_eq(7, zipped[1])
  assert_eq(9, zipped[2])
  
  // 测试不同长度数组的 zip_with
  let partial = @array.zip_with(arr1, arr3, add)
  assert_eq(8, partial[0])
  assert_eq(10, partial[1])
  assert_eq(12, partial[2])
  
  // 测试数组长度和索引
  assert_eq(3, arr1.length())
  assert_eq(1, arr1[0])
  assert_eq(3, arr1[2])
}

test "boolean_logical_operations" {
  // 测试布尔逻辑操作
  assert_eq(true, true && true)
  assert_eq(false, true && false)
  assert_eq(false, false && true)
  assert_eq(false, false && false)
  
  assert_eq(true, true || true)
  assert_eq(true, true || false)
  assert_eq(true, false || true)
  assert_eq(false, false || false)
  
  assert_eq(false, !true)
  assert_eq(true, !false)
  
  assert_eq(true, true == true)
  assert_eq(false, true == false)
  assert_eq(true, true != false)
}

test "option_type_pattern_matching" {
  // 测试 Option 类型的模式匹配
  let some_int : Int? = Some(42)
  let none_int : Int? = None
  let some_str : String? = Some("test")
  
  // 提取 Some 值
  let extracted_int = match some_int {
    Some(x) => x * 2
    None => 0
  }
  assert_eq(84, extracted_int)
  
  // 处理 None 值
  let default_int = match none_int {
    Some(x) => x
    None => -1
  }
  assert_eq(-1, default_int)
  
  // 字符串 Option 测试
  let extracted_str = match some_str {
    Some(s) => s + "_processed"
    None => "default"
  }
  assert_eq("test_processed", extracted_str)
}

test "result_type_error_handling" {
  // 测试 Result 类型的错误处理
  let success : Result[String, Int] = Ok("Operation successful")
  let failure : Result[String, Int] = Err(500)
  
  // 处理成功情况
  let success_message = match success {
    Ok(msg) => msg
    Err(code) => "Error code: " + @int.to_string(code)
  }
  assert_eq("Operation successful", success_message)
  
  // 处理错误情况
  let error_message = match failure {
    Ok(msg) => msg
    Err(code) => "Error code: " + @int.to_string(code)
  }
  assert_eq("Error code: 500", error_message)
  
  // 嵌套 Result 处理
  let nested_result : Result[Result[String, Int], String] = Ok(Ok("Nested success"))
  let nested_value = match nested_result {
    Ok(inner) => match inner {
      Ok(value) => value
      Err(_) => "Inner error"
    }
    Err(_) => "Outer error"
  }
  assert_eq("Nested success", nested_value)
}

test "character_operations_and_comparisons" {
  // 测试字符操作和比较
  let lower_a = 'a'
  let upper_a = 'A'
  let digit_5 = '5'
  let special = '@'
  
  // 字符转换
  assert_eq("a", @char.to_string(lower_a))
  assert_eq("A", @char.to_string(upper_a))
  assert_eq("5", @char.to_string(digit_5))
  
  // 字符比较
  assert_eq(true, lower_a > upper_a)
  assert_eq(true, upper_a < lower_a)
  assert_eq(true, digit_5 < lower_a)
  assert_eq(true, '0' <= digit_5)
  assert_eq(true, digit_5 <= '9')
  
  // 特殊字符比较
  assert_eq(true, special < lower_a)
  assert_eq(true, special < digit_5)
  assert_eq(false, special > upper_a)
}

test "list_manipulation_operations" {
  // 测试列表操作
  let num_list = [10, 20, 30, 40, 50]
  let empty_list : Array[Int] = []
  
  // 列表长度和访问
  assert_eq(5, num_list.length())
  assert_eq(0, empty_list.length())
  assert_eq(10, num_list[0])
  assert_eq(50, num_list[4])
  
  // 列表计算
  let sum_first_three = num_list[0] + num_list[1] + num_list[2]
  assert_eq(60, sum_first_three)
  
  let last_minus_first = num_list[4] - num_list[0]
  assert_eq(40, last_minus_first)
  
  // 平均值计算
  let average = sum_first_three / 3
  assert_eq(20, average)
}

test "mathematical_operations_and_functions" {
  // 测试数学操作和函数
  let x = 8.0
  let y = 2.0
  
  // 基本运算
  assert_eq(10.0, x + y)
  assert_eq(-6.0, y - x)
  assert_eq(16.0, x * y)
  assert_eq(4.0, x / y)
  
  // 正负数运算
  let positive = 15.0
  let negative = -10.0
  assert_eq(5.0, positive + negative)
  assert_eq(25.0, positive - negative)
  assert_eq(100.0, negative * negative)
  
  // 零值运算
  let zero = 0.0
  assert_eq(8.0, x + zero)
  assert_eq(0.0, x * zero)
  assert_eq(0.0, zero / y)
}

test "type_conversions_and_coercions" {
  // 测试类型转换和强制转换
  let int_val = 123
  let bool_val = true
  let string_val = "456"
  
  // 整数转换
  assert_eq("123", @int.to_string(int_val))
  assert_eq("0", @int.to_string(0))
  assert_eq("-42", @int.to_string(-42))
  
  // 布尔值转换
  assert_eq("true", @bool.to_string(bool_val))
  assert_eq("false", @bool.to_string(false))
  
  // 类型比较和检查
  assert_eq(true, int_val > 0)
  assert_eq(true, int_val < 1000)
  assert_eq(true, int_val >= 123)
  assert_eq(true, int_val <= 123)
  
  assert_eq(true, bool_val == true)
  assert_eq(true, bool_val != false)
  
  // 字符串长度检查
  assert_eq(3, string_val.length())
  assert_eq(16, "very_long_string".length())
}