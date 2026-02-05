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

// 核心 MoonBit 标准库测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "int_boundary_values" {
  // 测试整数边界值
  inspect(Int::max_value, content="2147483647")
  inspect(Int::min_value, content="-2147483648")
  
  // 测试绝对值函数
  inspect(Int::abs(42), content="42")
  inspect(Int::abs(-42), content="42")
  inspect(Int::abs(0), content="0")
  inspect(Int::abs(Int::min_value), content="-2147483648") // 边界情况：最小值的绝对值仍为最小值
}

test "string_operations_comprehensive" {
  // 测试字符串基本操作
  let empty = ""
  let text = "MoonBit"
  let combined = text + " Standard Library"
  
  inspect(empty.length(), content="0")
  inspect(text.length(), content="7")
  inspect(combined, content=""MoonBit Standard Library"")
  inspect(combined.length(), content="21")
  
  // 测试字符串比较
  inspect(text == "MoonBit", content="true")
  inspect(text != "moonbit", content="true")
  inspect(empty == "", content="true")
}

test "array_index_operations" {
  // 测试数组索引操作
  let numbers = [10, 20, 30, 40, 50]
  let chars = ['a', 'b', 'c', 'd', 'e']
  
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="10")
  inspect(numbers[4], content="50")
  inspect(chars[2], content="'c'")
  
  // 测试数组边界
  inspect([1, 2, 3].length(), content="3")
  inspect([].length(), content="0")
}

test "option_type_pattern_matching" {
  // 测试 Option 类型的高级模式匹配
  let some_int : Int? = Some(123)
  let none_int : Int? = None
  let some_str : String? = Some("hello")
  
  // 嵌套模式匹配
  let result = match (some_int, some_str) {
    (Some(x), Some(y)) => x + y.length()
    (Some(x), None) => x
    (None, Some(y)) => y.length()
    (None, None) => 0
  }
  inspect(result, content="128") // 123 + 5
  
  // 链式 Option 处理
  let chained = match some_int {
    Some(x) => Some(x * 2)
    None => None
  }
  inspect(chained, content="Some(246)")
}

test "result_type_error_handling" {
  // 测试 Result 类型的错误处理
  let success : Result[Int, String] = Ok(42)
  let error : Result[Int, String] = Err("Invalid input")
  let another_error : Result[Int, String] = Err("Division by zero")
  
  // 使用模式匹配检查状态
  let success_is_ok = match success {
    Ok(_) => true
    Err(_) => false
  }
  let success_is_err = match success {
    Ok(_) => false
    Err(_) => true
  }
  let error_is_ok = match error {
    Ok(_) => true
    Err(_) => false
  }
  let error_is_err = match error {
    Ok(_) => false
    Err(_) => true
  }
  
  inspect(success_is_ok, content="true")
  inspect(success_is_err, content="false")
  inspect(error_is_ok, content="false")
  inspect(error_is_err, content="true")
  
  // 错误消息处理
  let error_msg = match error {
    Ok(_) => "No error"
    Err(msg) => "Error: " + msg
  }
  inspect(error_msg, content=""Error: Invalid input"")
}

test "character_type_operations" {
  // 测试字符类型操作
  let lower_a = 'a'
  let upper_A = 'A'
  let digit_9 = '9'
  let space = ' '
  
  // 字符转换和比较
  inspect(lower_a.to_string(), content="'a'")
  inspect(upper_A.to_string(), content="'A'")
  inspect(digit_9.to_string(), content="'9'")
  
  // 字符顺序比较
  inspect(lower_a > upper_A, content="true")
  inspect(digit_9 < 'a', content="true")
  inspect(space < '0', content="true")
  
  // 字符边界情况
  inspect('A' <= 'Z', content="true")
  inspect('0' <= '9', content="true")
}

test "boolean_logical_combinations" {
  // 测试布尔逻辑组合
  let t = true
  let f = false
  
  // 基本逻辑运算
  inspect(t && t, content="true")
  inspect(t && f, content="false")
  inspect(f && t, content="false")
  inspect(f && f, content="false")
  
  inspect(t || t, content="true")
  inspect(t || f, content="true")
  inspect(f || t, content="true")
  inspect(f || f, content="false")
  
  // 复合逻辑表达式
  inspect((t && f) || (t || f), content="true")
  inspect(!(t && f) && (t || f), content="true")
  inspect(!(!t), content="true")
  inspect(!(!f), content="false")
}

test "numeric_type_conversions" {
  // 测试数值类型转换
  let int_val = 123
  let neg_int = -456
  let zero = 0
  
  // 整数转字符串
  inspect(int_val.to_string(), content=""123"")
  inspect(neg_int.to_string(), content=""-456"")
  inspect(zero.to_string(), content=""0"")
  
  // 字符串转整数（如果支持）
  let string_num = "789"
  inspect(string_num.length(), content="3")
  
  // 数值运算验证
  inspect(int_val + neg_int, content="-333")
  inspect(int_val * zero, content="0")
  inspect(neg_int / int_val, content="-4") // 整数除法
  inspect(neg_int % int_val, content="-54")
}

test "pattern_matching_complexity" {
  // 测试复杂模式匹配
  type MaybeInt = Option[Int]
  type ResultString = Result[String, Int]
  
  let some_val : MaybeInt = Some(42)
  let none_val : MaybeInt = None
  let ok_result : ResultString = Ok("success")
  let err_result : ResultString = Err(404)
  
  // 嵌套模式匹配
  let complex_result = match (some_val, ok_result) {
    (Some(x), Ok(msg)) => x + msg.length()
    (Some(x), Err(_)) => x
    (None, Ok(msg)) => msg.length()
    (None, Err(_)) => 0
  }
  inspect(complex_result, content="49") // 42 + 7
  
  // 多层错误处理
  let error_chain = match err_result {
    Ok(msg) => "Success: " + msg
    Err(code) => match (code > 400, code < 500) {
      (true, true) => "Client error: " + code.to_string()
      (false, true) => "Success: " + code.to_string()
      (true, false) => "Server error: " + code.to_string()
      (false, false) => "Unknown error: " + code.to_string()
    }
  }
  inspect(error_chain, content=""Client error: 404"")
}

test "edge_cases_and_special_values" {
  // 测试边界情况和特殊值
  let max_int = Int::max_value
  let min_int = Int::min_value
  let empty_string = ""
  let single_char = "x"
  
  // 整数边界运算
  inspect(max_int + 1, content="-2147483648") // 溢出环绕
  inspect(min_int - 1, content="2147483647") // 溢出环绕
  inspect(max_int - min_int, content="-1") // 2147483647 - (-2147483648) = -1 (溢出)
  
  // 字符串边界情况
  inspect(empty_string.length(), content="0")
  inspect(single_char.length(), content="1")
  inspect((empty_string + single_char).length(), content="1")
  
  // 数组边界情况
  let empty_array = []
  let single_element = [42]
  
  inspect(empty_array.length(), content="0")
  inspect(single_element.length(), content="1")
  inspect(single_element[0], content="42")
  
  // 布尔边界情况
  inspect(true.to_string(), content=""true"")
  inspect(false.to_string(), content=""false"")
  inspect((true == false), content="false")
  inspect((true != false), content="true")
}