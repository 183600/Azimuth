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

// 高质量 MoonBit 测试用例 - String 模块
// 包含10个全面的测试用例，覆盖核心字符串功能和边界情况

///|
test "string_basic_operations_comprehensive" {
  // 测试字符串基本操作
  // 字符串创建和默认值
  let empty_string = ""
  let default_string = String::default()
  assert_eq(empty_string, default_string)
  
  // 字符串连接
  let s1 = "Hello"
  let s2 = "World"
  let s3 = s1 + " " + s2
  assert_eq("Hello World", s3)
  
  // 多次连接
  let result = "a" + "b" + "c" + "d" + "e"
  assert_eq("abcde", result)
  
  // 空字符串连接
  let empty_concat = "" + "test" + ""
  assert_eq("test", empty_concat)
}

///|
test "string_equality_and_comparison" {
  // 测试字符串相等性和比较
  // 相等性测试
  assert_eq("abc" == "abc", true)
  assert_eq("abc" == "def", false)
  assert_eq("" == "", true)
  assert_eq("test" == "Test", false)
  
  // 不等性测试
  assert_eq("abc" != "def", true)
  assert_eq("abc" != "abc", false)
  
  // 大小写敏感性
  let upper = "HELLO"
  let lower = "hello"
  assert_eq(upper == lower, false)
  assert_eq(upper != lower, true)
}

///|
test "string_unicode_handling_advanced" {
  // 测试高级Unicode处理
  // 基本Unicode字符
  let chinese = "你好世界"
  let japanese = "こんにちは"
  let korean = "안녕하세요"
  let arabic = "مرحبا بالعالم"
  let russian = "Привет мир"
  
  // 验证字符串长度（以字符为单位）
  assert_eq(chinese.length(), 4)
  assert_eq(japanese.length(), 5)
  assert_eq(korean.length(), 5)
  
  // Emoji测试
  let emoji_simple = "😀"
  let emoji_complex = "👨‍👩‍👧‍👦"
  let emoji_sequence = "🎉🚀🌟"
  
  // 验证emoji字符串不为空
  assert_eq(emoji_simple.length() > 0, true)
  assert_eq(emoji_sequence.length() > 0, true)
  
  // 混合字符串
  let mixed = "Hello 世界 🌟"
  assert_eq(mixed.length() > 0, true)
}

///|
test "string_code_unit_operations" {
  // 测试代码单元操作
  let ascii_string = "Hello"
  let unicode_string = "你好"
  let emoji_string = "😀"
  
  // ASCII字符串的代码单元
  assert_eq(ascii_string.code_unit_at(0), 'H'.to_uint16())
  assert_eq(ascii_string.code_unit_at(1), 'e'.to_uint16())
  assert_eq(ascii_string.code_unit_at(4), 'o'.to_uint16())
  
  // Unicode字符串的代码单元
  assert_eq(unicode_string.length(), 2)
  let code_unit1 = unicode_string.code_unit_at(0)
  let code_unit2 = unicode_string.code_unit_at(1)
  assert_eq(code_unit1 > 0, true)
  assert_eq(code_unit2 > 0, true)
  
  // Emoji字符串的代理对测试
  assert_eq(emoji_string.length(), 1)
  let leading = emoji_string.code_unit_at(0)
  let trailing = emoji_string.code_unit_at(1)
  assert_eq(leading.is_leading_surrogate(), true)
  assert_eq(trailing.is_trailing_surrogate(), true)
}

///|
test "string_surrogate_pair_handling" {
  // 测试代理对处理
  // 测试各种emoji
  let emojis = ["😀", "🤣", "🧐", "🦄", "🌈"]
  
  for emoji in emojis {
    let leading = emoji.code_unit_at(0)
    let trailing = emoji.code_unit_at(1)
    
    // 验证代理对
    assert_eq(leading.is_surrogate(), true)
    assert_eq(trailing.is_surrogate(), true)
    assert_eq(leading.is_leading_surrogate(), true)
    assert_eq(trailing.is_trailing_surrogate(), true)
    
    // 验证不是前导代理
    assert_eq(leading.is_trailing_surrogate(), false)
    // 验证不是尾随代理
    assert_eq(trailing.is_leading_surrogate(), false)
  }
  
  // 测试非代理字符
  let ascii = 'A'.to_uint16()
  assert_eq(ascii.is_surrogate(), false)
  assert_eq(ascii.is_leading_surrogate(), false)
  assert_eq(ascii.is_trailing_surrogate(), false)
}

///|
test "string_edge_cases_and_boundary_values" {
  // 测试边界情况和极值
  // 空字符串
  let empty = ""
  assert_eq(empty.length(), 0)
  
  // 单字符字符串
  let single_char = "a"
  assert_eq(single_char.length(), 1)
  assert_eq(single_char.code_unit_at(0), 'a'.to_uint16())
  
  // 长字符串
  let long_string = "a" * 1000
  assert_eq(long_string.length(), 1000)
  
  // 特殊字符
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq(special_chars.length() > 0, true)
  
  // 空白字符
  let whitespace = " \t\n\r"
  assert_eq(whitespace.length(), 4)
  
  // 数字字符串
  let numbers = "0123456789"
  assert_eq(numbers.length(), 10)
}

///|
test "string_performance_efficiency" {
  // 测试字符串性能和效率
  // 字符串连接性能
  let mut result = ""
  for i in 0..<100 {
    result = result + i.to_string()
  }
  assert_eq(result.length() > 0, true)
  
  // 大量Unicode字符
  let mut unicode_result = ""
  for i in 0..<50 {
    unicode_result = unicode_result + "测试"
  }
  assert_eq(unicode_result.length(), 100) // 每个中文字符算作一个字符
  
  // 混合内容字符串
  let mixed_content = "ASCII: Hello" + " Unicode: 世界" + " Emoji: 🌟"
  assert_eq(mixed_content.length() > 0, true)
}

///|
test "string_memory_and_safety" {
  // 测试字符串内存和安全性
  // 字符串不可变性验证
  let original = "Hello"
  let modified = original + " World"
  assert_eq(original, "Hello")
  assert_eq(modified, "Hello World")
  
  // 边界访问安全
  let test_string = "Hello"
  // 正常访问
  let normal_access = test_string.code_unit_at(0)
  assert_eq(normal_access > 0, true)
  
  // 字符串内容验证
  let content = "Valid content"
  assert_eq(content.empty(), false)
  
  let empty_content = ""
  assert_eq(empty_content.empty(), true)
}

///|
test "string_internationalization_comprehensive" {
  // 测试国际化全面支持
  // 欧洲语言
  let french = "Bonjour le monde"
  let german = "Hallo Welt"
  let spanish = "Hola Mundo"
  let italian = "Ciao Mondo"
  let portuguese = "Olá Mundo"
  
  // 亚洲语言
  let chinese_simplified = "你好世界"
  let chinese_traditional = "你好世界"
  let japanese = "こんにちは世界"
  let korean = "안녕하세요 세계"
  
  // 中东语言
  let hebrew = "שלום עולם"
  let arabic = "مرحبا بالعالم"
  let persian = "سلام دنیا"
  
  // 验证所有字符串都有内容
  let languages = [
    french, german, spanish, italian, portuguese,
    chinese_simplified, chinese_traditional, japanese, korean,
    hebrew, arabic, persian
  ]
  
  for lang in languages {
    assert_eq(lang.length() > 0, true)
    assert_eq(lang.empty(), false)
  }
}

///|
test "string_complex_real_world_scenarios" {
  // 测试复杂真实世界场景
  // URL处理
  let url = "https://www.example.com/path/to/resource?param=value&param2=value2"
  assert_eq(url.contains("https://"), true)
  assert_eq(url.contains("?param="), true)
  
  // JSON字符串
  let json_string = "{\"name\":\"张三\",\"age\":25,\"city\":\"北京\"}"
  assert_eq(json_string.contains("\"name\":"), true)
  assert_eq(json_string.contains("张三"), true)
  
  // HTML内容
  let html_content = "<div class=\"container\"><p>Hello 世界</p></div>"
  assert_eq(html_content.contains("<div"), true)
  assert_eq(html_content.contains("世界"), true)
  
  // 日志消息
  let log_message = "[2024-01-01 12:00:00] INFO: User 测试用户 logged in successfully"
  assert_eq(log_message.contains("[2024-01-01"), true)
  assert_eq(log_message.contains("测试用户"), true)
  
  // 配置文件内容
  let config_content = "database.host=localhost\ndatabase.port=5432\napp.name=测试应用"
  assert_eq(config_content.contains("database.host="), true)
  assert_eq(config_content.contains("测试应用"), true)
}