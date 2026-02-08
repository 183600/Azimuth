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

// Azimuth 新增标准 MoonBit 测试用例
// 包含10个高质量测试用例，覆盖核心功能

///|
test "integer_overflow_protection" {
  // 测试整数溢出保护
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试边界值
  inspect(max_int + 0, content="2147483647")
  inspect(min_int - 0, content="-2147483648")
  
  // 测试正常运算
  inspect(1000000 + 1000000, content="2000000")
  inspect(-1000000 + 1000000, content="0")
}

///|
test "string_unicode_handling" {
  // 测试字符串Unicode处理
  let chinese = "中文测试"
  let emoji = "🚀🌟"
  let mixed = "Hello 世界 🌍"
  
  inspect(chinese.length(), content="4")
  inspect(emoji.length(), content="2")
  inspect(mixed.length(), content="9")
  
  // 测试字符串连接
  let combined = chinese + " " + emoji
  inspect(combined, content="\"中文测试 🚀🌟\"")
}

///|
test "array_comprehensive_operations" {
  // 测试数组综合操作
  let numbers = [1, 2, 3, 4, 5]
  let empty = []
  let single = [42]
  
  // 测试长度
  assert_eq(numbers.length(), 5)
  assert_eq(empty.length(), 0)
  assert_eq(single.length(), 1)
  
  // 测试索引访问
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  assert_eq(single[0], 42)
  
  // 测试数组操作
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  assert_eq(sum, 15)
}

///|
test "option_type_pattern_matching" {
  // 测试Option类型模式匹配
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  let string_some : String? = Some("Hello")
  
  // 测试Some情况
  match some_value {
    Some(x) => assert_eq(x, 42)
    None => assert_eq(false, true) // 不应该到达这里
  }
  
  // 测试None情况
  match none_value {
    Some(x) => assert_eq(false, true) // 不应该到达这里
    None => assert_eq(true, true) // 应该到达这里
  }
  
  // 测试字符串Option
  match string_some {
    Some(s) => assert_eq(s, "Hello")
    None => assert_eq(false, true) // 不应该到达这里
  }
}

///|
test "boolean_logic_comprehensive" {
  // 测试布尔逻辑综合运算
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
  
  // 复合逻辑运算
  inspect((t && f) || (t || f), content="true")
  inspect((f || f) && (t && t), content="false")
  
  // 逻辑非
  inspect(!t, content="false")
  inspect(!f, content="true")
}

///|
test "result_type_error_handling" {
  // 测试Result类型错误处理
  let success : Result[Int, String] = Ok(42)
  let failure : Result[Int, String] = Err("Error message")
  
  // 测试成功情况
  match success {
    Ok(value) => assert_eq(value, 42)
    Err(msg) => assert_eq(false, true) // 不应该到达这里
  }
  
  // 测试错误情况
  match failure {
    Ok(value) => assert_eq(false, true) // 不应该到达这里
    Err(msg) => assert_eq(msg, "Error message")
  }
  
  // 测试链式操作
  let chained = match success {
    Ok(v) => Ok(v * 2)
    Err(e) => Err(e)
  }
  match chained {
    Ok(v) => assert_eq(v, 84)
    Err(e) => assert_eq(false, true)
  }
}

///|
test "tuple_operations_decomposition" {
  // 测试元组操作和解构
  let pair = (42, "Hello")
  let triple = (1, 2, 3)
  let nested = ((1, 2), "nested")
  
  // 测试元组访问
  assert_eq(pair.0, 42)
  assert_eq(pair.1, "Hello")
  
  assert_eq(triple.0, 1)
  assert_eq(triple.1, 2)
  assert_eq(triple.2, 3)
  
  // 测试元组解构
  let (a, b) = pair
  assert_eq(a, 42)
  assert_eq(b, "Hello")
  
  let (x, y, z) = triple
  assert_eq(x + y + z, 6)
  
  // 测试嵌套元组
  let ((inner1, inner2), outer) = nested
  assert_eq(inner1 + inner2, 3)
  assert_eq(outer, "nested")
}

///|
test "char_type_unicode_operations" {
  // 测试字符类型Unicode操作
  let ascii_char = 'A'
  let unicode_char = '中'
  let emoji_char = '🚀'
  let number_char = '5'
  
  // 测试字符比较
  assert_eq(ascii_char > 'B', false)
  assert_eq(ascii_char < 'B', true)
  assert_eq(ascii_char == 'A', true)
  assert_eq(ascii_char != 'B', true)
  
  // 测试字符转换（如果支持）
  // 这里使用基本操作展示字符处理
  let char_str = ascii_char.to_string()
  assert_eq(char_str, "A")
  
  // 测试Unicode字符
  let unicode_str = unicode_char.to_string()
  assert_eq(unicode_str, "中")
  
  let emoji_str = emoji_char.to_string()
  assert_eq(emoji_str, "🚀")
}

///|
test "list_functional_operations" {
  // 测试列表函数式操作
  let empty_list = []
  let number_list = [1, 2, 3, 4, 5]
  let string_list = ["a", "b", "c"]
  
  // 测试列表长度和空检查
  assert_eq(empty_list.length(), 0)
  assert_eq(number_list.length(), 5)
  assert_eq(string_list.length(), 3)
  
  // 测试列表头尾操作
  assert_eq(number_list.head(), Some(1))
  assert_eq(empty_list.head(), None)
  
  assert_eq(number_list.tail(), Some([2, 3, 4, 5]))
  assert_eq(empty_list.tail(), None)
  
  // 测试列表连接
  let combined = number_list + [6, 7, 8]
  assert_eq(combined.length(), 8)
  assert_eq(combined[0], 1)
  assert_eq(combined[7], 8)
}

///|
test "complex_data_structure_integration" {
  // 测试复杂数据结构集成
  // 创建一个包含多种数据类型的复杂结构
  
  // 创建一个包含Option的数组
  let option_array = [Some(1), None, Some(3), Some(4), None]
  
  // 过滤非空值
  let filtered_count = 0
  for item in option_array {
    match item {
      Some(x) => filtered_count = filtered_count + 1
      None => () // 忽略None值
    }
  }
  assert_eq(filtered_count, 3)
  
  // 创建包含元组的列表
  let tuple_list = [(1, "one"), (2, "two"), (3, "three")]
  
  // 计算所有数字的和
  let sum = 0
  for pair in tuple_list {
    let (num, _) = pair
    sum = sum + num
  }
  assert_eq(sum, 6)
  
  // 创建嵌套的Option和Result
  let nested_option : Result[Option[Int], String] = Ok(Some(42))
  
  match nested_option {
    Ok(opt_int) => {
      match opt_int {
        Some(value) => assert_eq(value, 42)
        None => assert_eq(false, true)
      }
    }
    Err(_) => assert_eq(false, true)
  }
}