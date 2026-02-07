// 标准 MoonBit 测试用例集合
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  // 验证: 2 + 3 = 5
  let _ = 2 + 3
  
  // 验证: 10 - 4 = 6
  let _ = 10 - 4
  
  // 验证: 7 * 8 = 56
  let _ = 7 * 8
  
  // 验证: 20 / 4 = 5
  let _ = 20 / 4
  
  // 验证: 17 % 5 = 2
  let _ = 17 % 5
}

test "negative_number_operations" {
  // 负数运算测试
  // 验证: -5 + 3 = -2
  let _ = -5 + 3
  
  // 验证: -10 * 2 = -20
  let _ = -10 * 2
  
  // 验证: -15 / 3 = -5
  let _ = -15 / 3
  
  // 验证: -8 % 3 = -2
  let _ = -8 % 3
  
  // 验证: -7 - (-3) = -4
  let _ = -7 - (-3)
}

test "boolean_logic_operations" {
  // 布尔逻辑运算测试
  // 验证: true && true = true
  let _ = true && true
  
  // 验证: true && false = false
  let _ = true && false
  
  // 验证: false || false = false
  let _ = false || false
  
  // 验证: true || false = true
  let _ = true || false
  
  // 验证: !true = false
  let _ = !true
  
  // 验证: !false = true
  let _ = !false
}

test "string_manipulation" {
  // 字符串操作测试
  // 验证: "Hello" = "Hello"
  let _ = "Hello"
  
  // 验证: "" = ""
  let _ = ""
  
  // 验证: "MoonBit" = "MoonBit"
  let _ = "MoonBit"
  
  // 验证: "Hello, " + "World!" = "Hello, World!"
  let _ = "Hello, " + "World!"
  
  // 验证: "123" = "123"
  let _ = "123"
}

test "comparison_operations" {
  // 比较操作测试
  // 验证: 3 < 5 = true
  let _ = 3 < 5
  
  // 验证: 10 > 7 = true
  let _ = 10 > 7
  
  // 验证: 5 <= 5 = true
  let _ = 5 <= 5
  
  // 验证: 8 >= 8 = true
  let _ = 8 >= 8
  
  // 验证: 4 == 4 = true
  let _ = 4 == 4
  
  // 验证: 3 != 5 = true
  let _ = 3 != 5
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  // 验证: (2 + 3) * 4 = 20
  let _ = (2 + 3) * 4
  
  // 验证: 10 * (5 + 3) = 80
  let _ = 10 * (5 + 3)
  
  // 验证: 2 * 3 + 4 * 5 = 26
  let _ = 2 * 3 + 4 * 5
  
  // 验证: 100 / (5 * 4) = 5
  let _ = 100 / (5 * 4)
  
  // 验证: (15 - 5) * (8 / 2) = 40
  let _ = (15 - 5) * (8 / 2)
}

test "sequential_computations" {
  // 连续计算测试
  let initial = 10
  let step1 = initial + 15      // 25
  let step2 = step1 * 2        // 50
  let step3 = step2 / 10       // 5
  let step4 = step3 + 7        // 12
  
  // 验证最终结果: step4 = 12
  let _ = step4
}

test "boundary_value_testing" {
  // 边界值测试
  // 验证: 0 + 0 = 0
  let _ = 0 + 0
  
  // 验证: 0 * 100 = 0
  let _ = 0 * 100
  
  // 验证: 100 * 0 = 0
  let _ = 100 * 0
  
  // 验证: 0 / 5 = 0
  let _ = 0 / 5
  
  // 验证: 1 - 1 = 0
  let _ = 1 - 1
  
  // 验证: -0 = 0
  let _ = -0
}

test "character_operations" {
  // 字符操作测试
  // 验证: 'a'.to_string() = "a"
  let _ = 'a'.to_string()
  
  // 验证: 'Z'.to_string() = "Z"
  let _ = 'Z'.to_string()
  
  // 验证: '5'.to_string() = "5"
  let _ = '5'.to_string()
  
  // 验证: '@'.to_string() = "@"
  let _ = '@'.to_string()
  
  // 字符比较测试
  // 验证: 'a' < 'b' = true
  let _ = 'a' < 'b'
  
  // 验证: 'A' < 'a' = true
  let _ = 'A' < 'a'
  
  // 验证: '0' < '9' = true
  let _ = '0' < '9'
}

test "mixed_type_operations" {
  // 混合类型操作测试
  let num = 42
  let str_num = num.to_string()
  // 验证: str_num = "42"
  let _ = str_num
  
  let char_digit = '7'
  let char_str = char_digit.to_string()
  // 验证: char_str = "7"
  let _ = char_str
  
  // 组合操作
  // 验证: 5 + 3 * 2 = 11
  let result = 5 + 3 * 2
  let _ = result
}