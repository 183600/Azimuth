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

// 标准 MoonBit 测试用例集
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "int_boundary_values" {

using @builtin { inspect }

test "int_boundary_values" {
  // 测试整数边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  inspect(max_int, content="2147483647")
  inspect(min_int, content="-2147483648")
  
  // 边界值运算
  inspect(max_int - 1, content="2147483646")
  inspect(min_int + 1, content="-2147483647")
  
  // 绝对值测试
  inspect(-42 * -1, content="42")
  inspect(0, content="0")
  inspect(42, content="42")
}

test "string_advanced_operations" {
  // 测试字符串高级操作
  let text = "Hello, MoonBit!"
  let empty = ""
  
  // 字符串长度
  inspect(text.length(), content="15")
  inspect(empty.length(), content="0")
  
  // 字符串连接
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  inspect(combined, content="\"Hello World\"")
  
  // 字符串比较
  inspect(hello == "Hello", content="true")
  inspect(hello != world, content="true")
  inspect(empty == "", content="true")
}

test "list_functional_programming" {
  // 测试列表函数式编程
  let numbers = [1, 2, 3, 4, 5]
  let empty_list = []
  
  // 列表基本操作
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(empty_list.length(), content="0")
  
  // 简单的列表操作
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  inspect(sum, content="15")
  
  // 列表连接
  let more_numbers = [6, 7, 8]
  let combined = numbers + more_numbers
  inspect(combined.length(), content="8")
  inspect(combined[0], content="1")
  inspect(combined[7], content="8")
}

test "option_type_pattern_matching" {
  // 测试 Option 类型模式匹配
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  // Option 基本检查
  match some_value {
    Some(x) => inspect(x, content="42")
    None => inspect(0, content="0")
  }
  
  match none_value {
    Some(x) => inspect(x, content="0")
    None => inspect(0, content="0")
  }
  
  // 简单的 Option 操作
  let result = match some_value {
    Some(x) => x * 2
    None => 0
  }
  inspect(result, content="84")
  
  let none_result = match none_value {
    Some(x) => x * 2
    None => 0
  }
  inspect(none_result, content="0")
}

test "result_type_error_propagation" {
  // 测试 Result 类型错误传播
  let success : Result[Int, String] = Ok(42)
  let failure : Result[Int, String] = Err("Calculation failed")
  
  // Result 基本操作
  let success_value = match success {
    Ok(x) => x
    Err(_) => 0
  }
  inspect(success_value, content="42")
  
  let failure_value = match failure {
    Ok(x) => x
    Err(_) => 0
  }
  inspect(failure_value, content="0")
  
  // Result 简单映射
  let doubled_success = match success {
    Ok(x) => Ok(x * 2)
    Err(e) => Err(e)
  }
  
  let doubled_value = match doubled_success {
    Ok(x) => x
    Err(_) => 0
  }
  inspect(doubled_value, content="84")
}

test "character_operations_and_unicode" {
  // 测试字符操作和 Unicode
  let ascii_char = 'A'
  let digit_char = '5'
  let unicode_char = '世'
  
  // 字符转换
  let ascii_str = ascii_char.to_string()
  let digit_str = digit_char.to_string()
  let unicode_str = unicode_char.to_string()
  
  inspect(ascii_str, content="\"A\"")
  inspect(digit_str, content="\"5\"")
  inspect(unicode_str, content="\"世\"")
  
  // 字符比较
  inspect('a' > 'A', content="true")
  inspect('Z' < 'a', content="true")
  inspect('0' < '9', content="true")
  
  // Unicode 字符串操作
  let unicode_text = "Hello 世界"
  inspect(unicode_text.length(), content="9")
}

test "mathematical_calculations" {
  // 测试基本数学计算
  let x = 2.0
  let y = 8.0
  
  // 基本运算
  inspect(x + y, content="10")
  inspect(y - x, content="6")
  inspect(x * y, content="16")
  inspect(y / x, content="4")
  
  // 幂运算（使用乘法模拟）
  inspect(x * x * x * x, content="16")  // 2^4
  inspect(x * x * x, content="8")      // 2^3
  
  // 平方根（使用已知值）
  let sqrt16 = 4.0
  let sqrt9 = 3.0
  inspect(sqrt16, content="4")
  inspect(sqrt9, content="3")
  
  // 绝对值
  let pos = 5.0
  let neg = -5.0
  inspect(pos, content="5")
  inspect(neg * -1.0, content="5")
}

test "array_comprehensive_operations" {
  // 测试数组综合操作
  let numbers = [10, 20, 30, 40, 50]
  let mixed = ["apple", "banana", "cherry"]
  let empty = []
  
  // 数组基本操作
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="10")
  inspect(numbers[4], content="50")
  inspect(empty.length(), content="0")
  
  // 数组求和
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  inspect(sum, content="150")
  
  // 数组翻倍（手动模拟）
  let doubled = [numbers[0] * 2, numbers[1] * 2, numbers[2] * 2, numbers[3] * 2, numbers[4] * 2]
  inspect(doubled[0], content="20")
  inspect(doubled[4], content="100")
  
  // 字符串数组操作
  let first = mixed[0]
  let second = mixed[1]
  let third = mixed[2]
  inspect(first, content="\"apple\"")
  inspect(second, content="\"banana\"")
  inspect(third, content="\"cherry\"")
  
  // 数组查找（手动模拟）
  let found_30 = numbers[0] == 30 || numbers[1] == 30 || numbers[2] == 30 || numbers[3] == 30 || numbers[4] == 30
  let found_99 = numbers[0] == 99 || numbers[1] == 99 || numbers[2] == 99 || numbers[3] == 99 || numbers[4] == 99
  inspect(found_30, content="true")
  inspect(found_99, content="false")
}

test "type_conversions_and_casting" {
  // 测试类型转换和强制类型转换
  let int_val = 123
  
  // 数值到字符串转换
  inspect(int_val.to_string(), content="\"123\"")
  
  // 字符串和数值转换（简单示例）
  let str_num = "456"
  inspect(str_num, content="\"456\"")
  
  // 布尔值转换
  inspect(true.to_string(), content="\"true\"")
  inspect(false.to_string(), content="\"false\"")
  
  // 简单的数值操作
  let doubled = int_val * 2
  inspect(doubled, content="246")
  
  let halved = int_val / 2
  inspect(halved, content="61")
  
  // 浮点数操作
  let float_val = 123.0
  inspect(float_val, content="123")
  
  let float_doubled = float_val * 2.0
  inspect(float_doubled, content="246")
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边缘情况
  // 除法操作
  inspect(5 / 1, content="5")
  inspect(10 / 2, content="5")
  
  // 空值和默认值处理
  let empty_string = ""
  let empty_array = []
  let none_option : String? = None
  let error_result : Result[String, String] = Err("Error occurred")
  
  inspect(empty_string.length(), content="0")
  inspect(empty_array.length(), content="0")
  
  // Option 处理
  let option_result = match none_option {
    Some(s) => s.length()
    None => 0
  }
  inspect(option_result, content="0")
  
  // Result 处理
  let result_value = match error_result {
    Ok(s) => s
    Err(e) => "Error"
  }
  inspect(result_value, content="\"Error\"")
  
  // 边界值操作
  let max_int = 2147483647
  let min_int = -2147483648
  
  inspect(max_int > 0, content="true")
  inspect(min_int < 0, content="true")
  inspect(max_int + min_int, content="-1")
  
  // 特殊字符和字符串处理
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  inspect(special_chars.length(), content="29")
  
  // 字符串查找（手动模拟）
  let has_at = special_chars[0] == '@' || special_chars[1] == '@' || special_chars[2] == '@'
  inspect(has_at, content="true")
}