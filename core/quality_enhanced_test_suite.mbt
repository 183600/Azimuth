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

// 高质量 MoonBit 标准测试用例
// 包含 10 个核心功能测试，使用标准 MoonBit 测试语法

test "int_advanced_operations" {
  // 测试整数位运算
  inspect(5 & 3, content="1")    // 101 & 011 = 001
  inspect(5 | 3, content="7")    // 101 | 011 = 111
  inspect(5 ^ 3, content="6")    // 101 ^ 011 = 110
  inspect(5 << 2, content="20")  // 101 << 2 = 10100
  inspect(16 >> 2, content="4")  // 10000 >> 2 = 100
  
  // 测试整数除法和取模
  inspect(17 / 5, content="3")
  inspect(17 % 5, content="2")
  inspect(-17 / 5, content="-3")
  inspect(-17 % 5, content="-2")
}

test "string_advanced_operations" {
  // 测试字符串连接和比较
  let s1 = "MoonBit"
  let s2 = "Programming"
  let s3 = "Language"
  
  // 测试字符串连接
  inspect(s1 + " " + s2, content="MoonBit Programming")
  inspect(s2 + " " + s3, content="Programming Language")
  
  // 测试字符串比较
  inspect(s1 == s1, content="true")
  inspect(s1 != s2, content="true")
  inspect(s1 < s2, content="true")
  
  // 测试空字符串
  let empty = ""
  inspect(empty == "", content="true")
  inspect(s1 != "", content="true")
}

test "array_basic_operations" {
  // 测试数组基本操作
  let numbers = [1, 2, 3, 4, 5]
  let letters = ['a', 'b', 'c']
  
  // 测试数组访问
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  inspect(letters[1], content="'b'")
  
  // 测试数组比较
  inspect(numbers[0] == 1, content="true")
  inspect(letters[2] == 'c', content="true")
  
  // 测试数组边界
  inspect(numbers.length() > 0, content="true")
  inspect(letters.length() == 3, content="true")
}

test "boolean_logic_comprehensive" {
  // 测试布尔逻辑综合
  let t = true
  let f = false
  
  // 基本逻辑运算
  inspect(t && t, content="true")
  inspect(t && f, content="false")
  inspect(t || f, content="true")
  inspect(f || f, content="false")
  inspect(!t, content="false")
  inspect(!f, content="true")
  
  // 复合逻辑运算
  inspect((t && f) || (t && t), content="true")
  inspect((f || f) && (t || f), content="false")
  
  // 布尔相等性
  inspect(t == t, content="true")
  inspect(t == f, content="false")
  inspect(t != f, content="true")
}

test "character_operations" {
  // 测试字符操作
  let lower_a = 'a'
  let upper_a = 'A'
  let digit_5 = '5'
  
  // 测试字符比较
  inspect(lower_a == lower_a, content="true")
  inspect(lower_a != upper_a, content="true")
  inspect(lower_a < upper_a, content="true")
  inspect(digit_5 < '9', content="true")
  
  // 测试字符到字符串转换
  inspect(lower_a.to_string(), content="a")
  inspect(upper_a.to_string(), content="A")
  inspect(digit_5.to_string(), content="5")
  
  // 测试 Unicode 字符
  let unicode_char = '世'
  inspect(unicode_char.to_string(), content="世")
}

test "numeric_operations" {
  // 测试数值操作
  // 整数运算
  inspect(10 + 5, content="15")
  inspect(10 - 5, content="5")
  inspect(10 * 5, content="50")
  inspect(10 / 5, content="2")
  inspect(10 % 3, content="1")
  
  // 负数运算
  inspect(-5 + 3, content="-2")
  inspect(-10 * 2, content="-20")
  inspect(-15 / 3, content="-5")
  
  // 零运算
  inspect(0 + 10, content="10")
  inspect(10 - 0, content="10")
  inspect(0 * 10, content="0")
  inspect(0 / 1, content="0")
  
  // 比较运算
  inspect(10 > 5, content="true")
  inspect(5 < 10, content="true")
  inspect(10 >= 10, content="true")
  inspect(5 <= 10, content="true")
}

test "type_conversions_basic" {
  // 测试基本类型转换
  // 字符到字符串
  let char_a = 'a'
  let char_str = char_a.to_string()
  inspect(char_str, content="a")
  
  // 单元类型
  let unit_val : Unit = ()
  inspect(unit_val, content="()")
  
  // 数值比较
  let int_val = 42
  let another_int = 42
  inspect(int_val == another_int, content="true")
  inspect(int_val != 0, content="true")
  
  // 字符串比较
  let str1 = "hello"
  let str2 = "hello"
  let str3 = "world"
  inspect(str1 == str2, content="true")
  inspect(str1 != str3, content="true")
}

test "pattern_matching_basic" {
  // 测试基本模式匹配
  let some_option : Int? = Some(42)
  let none_option : Int? = None
  
  // 模式匹配 Option
  let result1 = match some_option {
    Some(x) => x * 2
    None => 0
  }
  inspect(result1, content="84")
  
  let result2 = match none_option {
    Some(x) => x * 2
    None => 0
  }
  inspect(result2, content="0")
  
  // 模式匹配 Result
  let success : Result[String, Int] = Ok("success")
  let failure : Result[String, Int] = Err(404)
  
  let result3 = match success {
    Ok(s) => s + "!"
    Err(_) => "error"
  }
  inspect(result3, content="success!")
  
  let result4 = match failure {
    Ok(s) => s + "!"
    Err(_) => "error"
  }
  inspect(result4, content="error")
}

test "array_functional_operations" {
  // 测试数组函数式操作
  let numbers = [1, 2, 3, 4, 5]
  
  // 测试数组长度
  inspect(numbers.length(), content="5")
  
  // 测试数组 map 操作
  let doubled = numbers.map(x => x * 2)
  inspect(doubled[0], content="2")
  inspect(doubled[4], content="10")
  
  // 测试数组 filter 操作
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens.length(), content="2")
  inspect(evens[0], content="2")
  inspect(evens[1], content="4")
  
  // 测试数组包含操作
  let has_three = numbers.contains(3)
  let has_ten = numbers.contains(10)
  inspect(has_three, content="true")
  inspect(has_ten, content="false")
}

test "error_handling_patterns" {
  // 测试错误处理模式
  // 测试 Option 类型操作
  let some_val : Int? = Some(100)
  let none_val : Int? = None
  
  inspect(some_val.is_some(), content="true")
  inspect(some_val.is_none(), content="false")
  inspect(none_val.is_some(), content="false")
  inspect(none_val.is_none(), content="true")
  
  // 测试 Option 的 map 操作
  let doubled = some_val.map(x => x * 2)
  let mapped_none = none_val.map(x => x * 2)
  
  inspect(doubled, content="Some(200)")
  inspect(mapped_none, content="None")
  
  // 测试 Option 的 unwrap_or
  let unwrapped = some_val.unwrap_or(0)
  let default_val = none_val.unwrap_or(0)
  
  inspect(unwrapped, content="100")
  inspect(default_val, content="0")
  
  // 测试 Result 类型操作
  let success : Result[String, Int] = Ok("success")
  let failure : Result[String, Int] = Err(404)
  
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(failure.is_ok(), content="false")
  inspect(failure.is_err(), content="true")
}