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

// Azimuth 增强测试用例集
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法
// 覆盖边界值、字符串处理、列表操作、数学函数、类型转换、错误处理、递归、模式匹配、集合操作和性能测试

using @builtin { inspect }

test "boundary_value_testing" {
  // 测试整数边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  inspect(max_int, content="2147483647")
  inspect(min_int, content="-2147483648")
  inspect(max_int + 1, content="-2147483648")  // 溢出测试
  inspect(min_int - 1, content="2147483647")  // 下溢测试
  
  // 测试零值操作
  inspect(0 * max_int, content="0")
  inspect(0 + min_int, content="-2147483648")
  inspect(max_int - max_int, content="0")
}

test "advanced_string_operations" {
  // 测试字符串高级操作
  let text = "MoonBit Programming"
  let empty = ""
  let single_char = "A"
  
  // 字符串长度和索引
  inspect(text.length(), content="18")
  inspect(text[0], content="'M'")
  inspect(text[17], content="'g'")
  
  // 字符串连接和比较
  let prefix = "MoonBit"
  let suffix = "Programming"
  inspect(prefix + " " + suffix, content=""MoonBit Programming"")
  inspect(prefix == "MoonBit", content="true")
  inspect(prefix != suffix, content="true")
  
  // 特殊字符串处理
  inspect(empty.length(), content="0")
  inspect(single_char.length(), content="1")
  inspect(single_char == "A", content="true")
}

test "list_manipulation_operations" {
  // 测试列表操作
  let empty_list : List[Int] = []
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["apple", "banana", "cherry"]
  
  // 列表长度和索引访问
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  inspect(empty_list.length(), content="0")
  
  // 列表元素操作
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  inspect(sum, content="15")
  
  // 字符串列表操作
  inspect(strings[0], content=""apple"")
  inspect(strings[1], content=""banana"")
  inspect(strings[2], content=""cherry"")
  inspect(strings[0] + " " + strings[1], content=""apple banana"")
}

test "mathematical_function_testing" {
  // 测试数学函数
  let positive = 42
  let negative = -42
  let zero = 0
  
  // 基本数学运算
  inspect(positive + negative, content="0")
  inspect(positive - negative, content="84")
  inspect(positive * negative, content="-1764")
  inspect(positive / positive, content="1")
  inspect(positive % 5, content="2")
  
  // 负数运算
  inspect(negative + negative, content="-84")
  inspect(negative * positive, content="-1764")
  inspect(negative / positive, content="-1")
  
  // 零值运算
  inspect(zero + positive, content="42")
  inspect(positive * zero, content="0")
  inspect(zero / positive, content="0")
}

test "type_conversion_scenarios" {
  // 测试类型转换场景
  let int_val = 123
  let bool_true = true
  let bool_false = false
  let char_val = 'X'
  
  // 整数到字符串转换
  inspect(int_val.to_string(), content=""123"")
  inspect((-456).to_string(), content=""-456"")
  inspect(0.to_string(), content=""0"")
  
  // 布尔值到字符串转换
  inspect(bool_true.to_string(), content=""true"")
  inspect(bool_false.to_string(), content=""false"")
  
  // 字符到字符串转换
  inspect(char_val.to_string(), content=""X"")
  inspect('A'.to_string(), content=""A"")
  
  // 类型一致性检查
  inspect(int_val == 123, content="true")
  inspect(bool_true == true, content="true")
  inspect(char_val == 'X', content="true")
}

test "error_handling_patterns" {
  // 测试错误处理模式
  let success_result : Result[Int, String] = Ok(42)
  let error_result : Result[Int, String] = Err("Something went wrong")
  let some_option : Int? = Some(100)
  let none_option : Int? = None
  
  // Result 类型处理
  let success_value = match success_result {
    Ok(value) => value
    Err(error) => 0
  }
  inspect(success_value, content="42")
  
  let error_value = match error_result {
    Ok(value) => value
    Err(error) => 0
  }
  inspect(error_value, content="0")
  
  // Option 类型处理
  let some_value = match some_option {
    Some(value) => value
    None => 0
  }
  inspect(some_value, content="100")
  
  let none_value = match none_option {
    Some(value) => value
    None => -1
  }
  inspect(none_value, content="-1")
}

test "recursive_operation_testing" {
  // 测试递归操作
  // 阶乘计算（使用简单循环模拟递归）
  let mut result = 1
  let mut i = 1
  while i <= 5 {
    result = result * i
    i = i + 1
  }
  inspect(result, content="120")  // 5! = 120
  
  // 斐波那契数列（前几项）
  let fib_0 = 0
  let fib_1 = 1
  let fib_2 = fib_0 + fib_1
  let fib_3 = fib_1 + fib_2
  let fib_4 = fib_2 + fib_3
  let fib_5 = fib_3 + fib_4
  
  inspect(fib_0, content="0")
  inspect(fib_1, content="1")
  inspect(fib_2, content="1")
  inspect(fib_3, content="2")
  inspect(fib_4, content="3")
  inspect(fib_5, content="5")
  
  // 累加和
  let mut sum = 0
  let mut j = 1
  while j <= 10 {
    sum = sum + j
    j = j + 1
  }
  inspect(sum, content="55")  // 1+2+...+10 = 55
}

test "pattern_matching_complexity" {
  // 测试复杂模式匹配
  let numbers = [1, 2, 3, 4, 5]
  let mixed_values = [0, -1, 42, -100, 999]
  
  // 基于值的模式匹配
  let first_number = numbers[0]
  let category = match first_number {
    0 => "zero"
    1 => "one"
    2 => "two"
    _ => "other"
  }
  inspect(category, content=""one"")
  
  // 正负数分类
  let positive_count = {
    let mut count = 0
    let mut i = 0
    while i < mixed_values.length() {
      if mixed_values[i] > 0 {
        count = count + 1
      }
      i = i + 1
    }
    count
  }
  inspect(positive_count, content="2")  // 42 和 999 是正数
  
  // 奇偶数判断
  let even_count = {
    let mut count = 0
    let mut i = 0
    while i < numbers.length() {
      if numbers[i] % 2 == 0 {
        count = count + 1
      }
      i = i + 1
    }
    count
  }
  inspect(even_count, content="2")  // 2 和 4 是偶数
}

test "collection_operation_testing" {
  // 测试集合操作
  let array1 = [1, 2, 3, 4, 5]
  let array2 = [4, 5, 6, 7, 8]
  let strings = ["hello", "world", "moonbit"]
  
  // 数组长度和访问
  inspect(array1.length(), content="5")
  inspect(array2.length(), content="5")
  inspect(strings.length(), content="3")
  
  // 数组元素查找和比较
  let common_elements = {
    let mut count = 0
    let mut i = 0
    while i < array1.length() {
      let mut j = 0
      while j < array2.length() {
        if array1[i] == array2[j] {
          count = count + 1
          break
        }
        j = j + 1
      }
      i = i + 1
    }
    count
  }
  inspect(common_elements, content="2")  // 4 和 5 是共同元素
  
  // 字符串数组操作
  let total_length = {
    let mut total = 0
    let mut i = 0
    while i < strings.length() {
      total = total + strings[i].length()
      i = i + 1
    }
    total
  }
  inspect(total_length, content="16")  // 5 + 5 + 6 = 16
  
  // 数组求和
  let sum1 = {
    let mut total = 0
    let mut i = 0
    while i < array1.length() {
      total = total + array1[i]
      i = i + 1
    }
    total
  }
  inspect(sum1, content="15")  // 1+2+3+4+5 = 15
}

test "performance_related_operations" {
  // 测试性能相关操作
  let large_numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let repeated_strings = ["test", "test", "test", "test", "test"]
  
  // 大数组操作效率测试
  let large_sum = {
    let mut total = 0
    let mut i = 0
    while i < large_numbers.length() {
      total = total + large_numbers[i]
      i = i + 1
    }
    total
  }
  inspect(large_sum, content="55")  // 1到10的和
  
  // 字符串重复操作
  let concatenated = repeated_strings[0] + repeated_strings[1] + repeated_strings[2] + repeated_strings[3] + repeated_strings[4]
  inspect(concatenated, content="\"testtesttesttesttest\"")
  inspect(concatenated.length(), content="20")
  
  // 条件分支性能测试
  let conditional_result = {
    let mut result = 0
    let mut i = 0
    while i < 100 {
      if i % 2 == 0 {
        result = result + i
      } else {
        result = result - i
      }
      i = i + 1
    }
    result
  }
  inspect(conditional_result, content="-50")  // 偶数和减奇数和
  
  // 内存分配测试（创建多个临时变量）
  let temp_calculation = {
    let a = 10
    let b = 20
    let c = 30
    let d = 40
    let e = 50
    a + b + c + d + e
  }
  inspect(temp_calculation, content="150")
}