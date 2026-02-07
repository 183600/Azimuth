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

// 新增的标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法
// 测试基本数据类型、操作和常见编程场景

using @builtin { inspect }

test "advanced_arithmetic_operations" {
  // 高级算术运算测试
  let x = 15
  let y = 4
  inspect(x + y, content="19")
  inspect(x - y, content="11")
  inspect(x * y, content="60")
  inspect(x / y, content="3")
  inspect(x % y, content="3")
  
  // 复合运算
  let result = (x + y) * (x - y)
  inspect(result, content="119")  // (15+4)*(15-4) = 19*11 = 209
  
  // 负数运算
  let neg_x = -x
  inspect(neg_x + y, content="-11")
  inspect(neg_x * y, content="-60")
}

test "string_manipulation_techniques" {
  // 字符串操作技术测试
  let base = "MoonBit"
  let prefix = "Hello "
  let suffix = "!"
  
  let greeting = prefix + base + suffix
  inspect(greeting, content="\"Hello MoonBit!\"")
  
  // 字符串长度和索引
  inspect(@string.length(base), content="7")
  inspect(@string.length(greeting), content="14")
  
  // 字符串比较
  inspect(base == "MoonBit", content="true")
  inspect(base != "moonbit", content="true")
  
  // 空字符串处理
  let empty = ""
  inspect(@string.length(empty), content="0")
  let combined_empty = empty + base
  inspect(combined_empty, content="\"MoonBit\"")
}

test "array_comprehensive_operations" {
  // 数组综合操作测试
  let numbers = [2, 4, 6, 8, 10]
  inspect(@array.length(numbers), content="5")
  
  // 数组元素访问
  inspect(numbers[0], content="2")
  inspect(numbers[2], content="6")
  inspect(numbers[4], content="10")
  
  // 数组计算
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  inspect(sum, content="30")
  
  // 数组比较
  let same_numbers = [2, 4, 6, 8, 10]
  let different_numbers = [1, 3, 5, 7, 9]
  inspect(numbers[0] == same_numbers[0], content="true")
  inspect(numbers[0] != different_numbers[0], content="true")
  
  // 空数组处理
  let empty_array = []
  inspect(@array.length(empty_array), content="0")
}

test "boolean_logic_and_truth_tables" {
  // 布尔逻辑和真值表测试
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
  
  // 否定运算
  inspect(!t, content="false")
  inspect(!f, content="true")
  inspect(!(!t), content="true")
  
  // 复合逻辑
  let complex = (t && f) || (f || t)
  inspect(complex, content="true")
}

test "option_type_advanced_patterns" {
  // Option 类型高级模式匹配测试
  let present : Int? = Some(42)
  let absent : Int? = None
  let zero : Int? = Some(0)
  
  // 基本模式匹配
  let extract_present = match present {
    Some(x) => x * 2
    None => -1
  }
  inspect(extract_present, content="84")
  
  let extract_absent = match absent {
    Some(x) => x * 2
    None => -1
  }
  inspect(extract_absent, content="-1")
  
  // 零值处理
  let extract_zero = match zero {
    Some(x) => x + 100
    None => -1
  }
  inspect(extract_zero, content="100")
  
  // 嵌套 Option
  let nested : Int?? = Some(Some(99))
  let extract_nested = match nested {
    Some(Some(x)) => x
    Some(None) => -1
    None => -2
  }
  inspect(extract_nested, content="99")
}

test "result_type_error_handling_patterns" {
  // Result 类型错误处理模式测试
  let success : Result[String, Int] = Ok("Operation completed")
  let failure : Result[String, Int] = Err(500)
  let warning : Result[String, Int] = Err(404)
  
  // 成功处理
  let success_message = match success {
    Ok(msg) => msg + " successfully"
    Err(code) => "Error code: " + code.to_string()
  }
  inspect(success_message, content="\"Operation completed successfully\"")
  
  // 错误处理
  let error_code = match failure {
    Ok(_) => 0
    Err(code) => code
  }
  inspect(error_code, content="500")
  
  // 条件错误处理
  let warning_status = match warning {
    Ok(_) => "No warning"
    Err(404) => "Not found"
    Err(500) => "Server error"
    Err(_) => "Unknown error"
  }
  inspect(warning_status, content="\"Not found\"")
  
  // Result 链式处理模拟
  let chain_result = match success {
    Ok(msg) => Ok(msg.length().to_string())
    Err(code) => Err(code)
  }
  inspect(chain_result, content="\"Ok(\\\"20\\\")\"")
}

test "character_encoding_and_comparisons" {
  // 字符编码和比较测试
  let uppercase = 'A'
  let lowercase = 'a'
  let digit = '5'
  let special = '@'
  
  // 字符转换
  inspect(@char.to_string(uppercase), content="\"A\"")
  inspect(@char.to_string(lowercase), content="\"a\"")
  inspect(@char.to_string(digit), content="\"5\"")
  inspect(@char.to_string(special), content="\"@\"")
  
  // 字符比较
  inspect(uppercase < lowercase, content="true")
  inspect(lowercase > uppercase, content="true")
  inspect(digit < uppercase, content="true")
  inspect(special > digit, content="true")
  
  // ASCII 值比较（通过字符比较推断）
  inspect('0' < '1', content="true")
  inspect('9' > '8', content="true")
  inspect('Z' < 'a', content="true")
  inspect('z' > 'Z', content="true")
}

test "complex_data_structure_operations" {
  // 复杂数据结构操作测试
  // 创建嵌套数组结构
  let matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]
  
  // 访问嵌套元素
  inspect(matrix[0][0], content="1")
  inspect(matrix[1][1], content="5")
  inspect(matrix[2][2], content="9")
  
  // 计算对角线元素和
  let diagonal_sum = matrix[0][0] + matrix[1][1] + matrix[2][2]
  inspect(diagonal_sum, content="15")
  
  // 复杂的 Option 和 Result 组合
  let maybe_number : Int? = Some(10)
  let result_operation : Result[Int, String] = Ok(20)
  
  let complex_calculation = match maybe_number {
    Some(x) => match result_operation {
      Ok(y) => x + y
      Err(_) => x
    }
    None => 0
  }
  inspect(complex_calculation, content="30")
  
  // 错误路径测试
  let error_result : Result[Int, String] = Err("Calculation failed")
  let error_calculation = match maybe_number {
    Some(x) => match error_result {
      Ok(y) => x + y
      Err(_) => x * 2
    }
    None => 0
  }
  inspect(error_calculation, content="20")
}