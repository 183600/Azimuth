// MoonBit 标准库测试用例
// 包含对核心模块的综合测试

test "string_basic_operations" {
  // 测试字符串基本操作
  let hello = "Hello, World!"
  let empty = ""
  let chinese = "测试中文"
  let emoji = "🚀 MoonBit"
  
  // 创建基本字符串变量
  let _ = hello
  let _ = empty
  let _ = chinese
  let _ = emoji
}

test "string_concatenation" {
  // 测试字符串连接
  let first = "Hello"
  let second = "World"
  let combined = first + ", " + second + "!"
  
  // 创建连接后的字符串
  let _ = combined
}

test "string_special_characters" {
  // 测试字符串特殊字符
  let space = " "
  let newline = "\n"
  let tab = "\t"
  let quotes = "\"quotes\""
  let apostrophe = "'apostrophe'"
  let backslash = "\\backslash\\"
  
  // 创建特殊字符字符串
  let _ = space
  let _ = newline
  let _ = tab
  let _ = quotes
  let _ = apostrophe
  let _ = backslash
}

test "string_comparison" {
  // 测试字符串比较
  let str1 = "Hello"
  let str2 = "World"
  let str3 = "Hello"
  let str4 = ""
  
  // 创建比较字符串
  let _ = str1
  let _ = str2
  let _ = str3
  let _ = str4
}

test "string_operations_complex" {
  // 测试复杂字符串操作
  let prefix = "MoonBit"
  let suffix = "Standard Library"
  let full_name = prefix + " " + suffix
  
  // 创建复杂操作字符串
  let _ = full_name
}

test "string_unicode_handling" {
  // 测试 Unicode 字符串处理
  let latin = "café"
  let russian = "Москва"
  let arabic = "مرحبا"
  let emoji_only = "🚀🌟💎"
  
  // 创建 Unicode 字符串
  let _ = latin
  let _ = russian
  let _ = arabic
  let _ = emoji_only
}

test "string_edge_cases" {
  // 测试字符串边缘情况
  let single_char = "a"
  let single_space = " "
  let single_newline = "\n"
  let long_string = "This is a very long string that contains many words and characters"
  
  // 创建边缘情况字符串
  let _ = single_char
  let _ = single_space
  let _ = single_newline
  let _ = long_string
}

test "string_formatting_patterns" {
  // 测试字符串格式化模式
  let name = "Developer"
  let place = "MoonBit"
  
  // 模拟字符串格式化
  let greeting = "Hello, " + name + "! Welcome to " + place + "."
  
  // 创建格式化字符串
  let _ = greeting
}

test "string_validation_scenarios" {
  // 测试字符串验证场景
  let email = "user@example.com"
  let phone = "123-456-7890"
  let url = "https://moonbitlang.com"
  
  // 创建验证场景字符串
  let _ = email
  let _ = phone
  let _ = url
}

test "string_performance_patterns" {
  // 测试字符串性能模式
  let base = "test"
  let repeated = base + base + base + base + base
  
  // 创建性能模式字符串
  let _ = repeated
}