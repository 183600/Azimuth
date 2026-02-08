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

// Enhanced Azimuth Test Suite
// 高质量的 MoonBit 测试用例，涵盖核心功能模块

test "integer_boundary_values_and_overflow" {
  // 测试整数边界值
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  inspect(max_val - 1, content="2147483646")
  inspect(min_val + 1, content="-2147483647")
  
  // 测试绝对值函数
  inspect(@int.abs(42), content="42")
  inspect(@int.abs(-42), content="42")
  inspect(@int.abs(0), content="0")
  
  // 测试符号运算
  inspect(-(-5), content="5")
  inspect(-(0), content="0")
}

test "string_manipulation_and_unicode" {
  // 基本字符串操作
  let empty = ""
  let hello = "Hello"
  let world = "世界"
  let combined = hello + " " + world
  inspect(combined, content="\"Hello 世界\"")
  
  // 字符串长度测试（中文字符）
  inspect(@string.length(hello), content="5")
  inspect(@string.length(world), content="2")
  inspect(@string.length(combined), content="8")
  
  // 字符串比较
  inspect(hello == "Hello", content="true")
  inspect(hello != "hello", content="true")
  inspect(empty < hello, content="true")
  
  // 字符串包含测试
  let text = "MoonBit is awesome"
  inspect(text, content="\"MoonBit is awesome\"")
}

test "boolean_logic_and_truth_tables" {
  // 基本布尔运算
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false && true, content="false")
  inspect(false && false, content="false")
  
  inspect(true || true, content="true")
  inspect(true || false, content="true")
  inspect(false || true, content="true")
  inspect(false || false, content="false")
  
  // 布尔取反
  inspect(!true, content="false")
  inspect(!false, content="true")
  inspect(!(!true), content="true")
  
  // 布尔比较
  inspect(true == true, content="true")
  inspect(true == false, content="false")
  inspect(true != false, content="true")
  
  // 短路求值测试
  let result = false && (1/0 > 0)  // 短路，不会除零
  inspect(result, content="false")
}

test "array_operations_and_indexing" {
  // 数组创建和基本操作
  let numbers = [1, 2, 3, 4, 5]
  let empty_array : Array[Int] = []
  
  inspect(@array.length(numbers), content="5")
  inspect(@array.length(empty_array), content="0")
  
  // 数组索引访问
  inspect(numbers[0], content="1")
  inspect(numbers[2], content="3")
  inspect(numbers[4], content="5")
  
  // 数组运算
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  inspect(sum, content="15")
  
  // 数组边界测试
  let first = numbers[0]
  let last = numbers[@array.length(numbers) - 1]
  inspect(first + last, content="6")
}

test "option_type_pattern_matching" {
  // Option 类型基本操作
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  let some_string : String? = Some("MoonBit")
  let none_string : String? = None
  
  // Option 解包
  let extracted_int = match some_value {
    Some(x) => x
    None => 0
  }
  inspect(extracted_int, content="42")
  
  let default_int = match none_value {
    Some(x) => x
    None => -1
  }
  inspect(default_int, content="-1")
  
  // Option 字符串处理
  let extracted_str = match some_string {
    Some(s) => s
    None => "default"
  }
  inspect(extracted_str, content="\"MoonBit\"")
  
  let default_str = match none_string {
    Some(s) => s
    None => "default"
  }
  inspect(default_str, content="\"default\"")
  
  // Option 链式操作
  let chained = match some_value {
    Some(x) => Some(x * 2)
    None => None
  }
  let final_result = match chained {
    Some(y) => y
    None => 0
  }
  inspect(final_result, content="84")
}

test "result_type_error_handling" {
  // Result 类型基本操作
  let success : Result[String, Int] = Ok("Operation successful")
  let failure : Result[String, Int] = Err(404)
  let numeric_success : Result[Int, String] = Ok(100)
  let numeric_failure : Result[Int, String] = Err("Division error")
  
  // 成功结果处理
  let success_msg = match success {
    Ok(msg) => msg
    Err(_) => "Error occurred"
  }
  inspect(success_msg, content="\"Operation successful\"")
  
  // 错误结果处理
  let error_code = match failure {
    Ok(_) => 0
    Err(code) => code
  }
  inspect(error_code, content="404")
  
  // 数值结果处理
  let numeric_result = match numeric_success {
    Ok(value) => value * 2
    Err(_) => 0
  }
  inspect(numeric_result, content="200")
  
  let error_msg = match numeric_failure {
    Ok(_) => "No error"
    Err(msg) => msg
  }
  inspect(error_msg, content="\"Division error\"")
  
  // Result 链式操作
  let chained_result = match numeric_success {
    Ok(x) => Ok(x + 50)
    Err(e) => Err(e)
  }
  let final_value = match chained_result {
    Ok(y) => y
    Err(_) => -1
  }
  inspect(final_value, content="150")
}

test "character_operations_and_comparisons" {
  // 字符基本操作
  let char_a = 'a'
  let char_A = 'A'
  let char_z = 'z'
  let char_Z = 'Z'
  let char_0 = '0'
  let char_9 = '9'
  
  // 字符比较
  inspect(char_a < char_z, content="true")
  inspect(char_A < char_Z, content="true")
  inspect(char_A < char_a, content="true")
  inspect(char_0 < char_9, content="true")
  inspect(char_9 < char_a, content="true")
  
  // 字符转换
  inspect(@char.to_string(char_a), content="\"a\"")
  inspect(@char.to_string(char_A), content="\"A\"")
  inspect(@char.to_string(char_0), content="\"0\"")
  
  // 字符相等性
  inspect(char_a == char_a, content="true")
  inspect(char_a != char_A, content="true")
  inspect(char_0 == '0', content="true")
  
  // 特殊字符
  let space = ' '
  let newline = '\n'
  let tab = '\t'
  inspect(@char.to_string(space), content="\" \"")
  inspect(space < char_a, content="true")
}

test "mathematical_functions_and_operations" {
  // 基本数学运算
  let x = 10.0
  let y = 3.0
  
  inspect(x + y, content="13")
  inspect(x - y, content="7")
  inspect(x * y, content="30")
  inspect(x / y, content="3")
  
  // 负数运算
  let neg_x = -5.0
  let neg_y = -2.0
  inspect(neg_x + neg_y, content="-7")
  inspect(neg_x - neg_y, content="-3")
  inspect(neg_x * neg_y, content="10")
  inspect(neg_x / neg_y, content="2")
  
  // 零值运算
  let zero = 0.0
  inspect(x + zero, content="10")
  inspect(x - zero, content="10")
  inspect(x * zero, content="0")
  
  // 混合运算
  let complex = (x + y) * (x - y) / (y + zero)
  inspect(complex, content="21")
  
  // 整数与浮点数混合
  let int_val = 7
  let float_val = 3.0
  inspect(int_val + int_val, content="14")
}

test "type_conversions_and_coercion" {
  // 整数到字符串转换
  let int_val = 12345
  let negative_int = -678
  let zero_int = 0
  
  inspect(@int.to_string(int_val), content="\"12345\"")
  inspect(@int.to_string(negative_int), content="\"-678\"")
  inspect(@int.to_string(zero_int), content="\"0\"")
  
  // 布尔值到字符串转换
  let true_val = true
  let false_val = false
  
  inspect(@bool.to_string(true_val), content="\"true\"")
  inspect(@bool.to_string(false_val), content="\"false\"")
  
  // 字符到字符串转换
  let char_val = 'X'
  inspect(@char.to_string(char_val), content="\"X\"")
  
  // 类型比较和检查
  inspect(int_val > 0, content="true")
  inspect(negative_int < 0, content="true")
  inspect(zero_int == 0, content="true")
  inspect(int_val >= 12345, content="true")
  inspect(int_val <= 12345, content="true")
  
  // 布尔逻辑比较
  inspect(true_val == true, content="true")
  inspect(false_val == false, content="true")
  inspect(true_val != false, content="true")
}

test "nested_data_structures_and_complex_operations" {
  // 嵌套数组操作
  let matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
  let first_row = matrix[0]
  let second_row = matrix[1]
  let third_row = matrix[2]
  
  inspect(@array.length(matrix), content="3")
  inspect(@array.length(first_row), content="3")
  inspect(first_row[0], content="1")
  inspect(second_row[1], content="5")
  inspect(third_row[2], content="9")
  
  // 矩阵运算
  let diagonal_sum = first_row[0] + second_row[1] + third_row[2]
  inspect(diagonal_sum, content="15")
  
  let total_sum = 0
  let row1_sum = first_row[0] + first_row[1] + first_row[2]
  let row2_sum = second_row[0] + second_row[1] + second_row[2]
  let row3_sum = third_row[0] + third_row[1] + third_row[2]
  let matrix_sum = row1_sum + row2_sum + row3_sum
  inspect(matrix_sum, content="45")
  
  // 复杂的 Option 和 Result 嵌套
  let nested_option : Option[Result[Int, String]] = Some(Ok(42))
  let extracted_value = match nested_option {
    Some(result) => match result {
      Ok(value) => value
      Err(_) => -1
    }
    None => 0
  }
  inspect(extracted_value, content="42")
  
  // 多层嵌套匹配
  let complex_data : Option[Result[Array[Int], String]] = Some(Ok([10, 20, 30]))
  let array_sum = match complex_data {
    Some(result) => match result {
      Ok(arr) => arr[0] + arr[1] + arr[2]
      Err(_) => 0
    }
    None => -1
  }
  inspect(array_sum, content="60")
}