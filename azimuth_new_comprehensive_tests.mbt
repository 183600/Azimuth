// Azimuth 项目新的综合测试用例
// 使用标准 MoonBit 测试语法，覆盖各种功能场景

test "string_concatenation" {
  // 测试字符串连接功能
  let first = "Azimuth"
  let last = "Project"
  let space = " "
  let result = first + space + last
  assert_eq_string("Azimuth Project", result)
}

test "array_operations" {
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  let first_element = numbers[0]
  let last_element = numbers[4]
  let length = array_length(numbers)
  
  assert_eq(1, first_element)
  assert_eq(5, last_element)
  assert_eq(5, length)
}

test "boolean_logic" {
  // 测试布尔逻辑运算
  let true_val = true
  let false_val = false
  
  assert_eq(true, true_val and true_val)
  assert_eq(false, true_val and false_val)
  assert_eq(true, true_val or false_val)
  assert_eq(false, false_val or false_val)
  assert_eq(false, not true_val)
  assert_eq(true, not false_val)
}

test "comparison_operations" {
  // 测试比较操作
  let a = 10
  let b = 20
  let c = 10
  
  assert_eq(true, a < b)
  assert_eq(false, a > b)
  assert_eq(true, a <= b)
  assert_eq(false, a >= b)
  assert_eq(true, a == c)
  assert_eq(false, a == b)
  assert_eq(true, a != b)
  assert_eq(false, a != c)
}

test "mathematical_functions" {
  // 测试数学函数
  let x = 16
  let y = -5
  
  // 假设有绝对值函数
  let abs_y = y < 0 ? -y : y
  assert_eq(5, abs_y)
  
  // 假设有平方函数
  let square_x = x * x
  assert_eq(256, square_x)
  
  // 假设有立方函数
  let cube_y = y * y * y
  assert_eq(-125, cube_y)
}

test "conditional_expressions" {
  // 测试条件表达式
  let age = 18
  let is_adult = age >= 18 ? "Adult" : "Minor"
  let can_vote = age >= 18 ? true : false
  let price_category = age < 12 ? "Child" : age < 65 ? "Adult" : "Senior"
  
  assert_eq_string("Adult", is_adult)
  assert_eq(true, can_vote)
  assert_eq_string("Adult", price_category)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  let numerator = 10
  let denominator = 0
  
  // 模拟除零检查
  let can_divide = denominator != 0
  assert_eq(false, can_divide)
  
  // 安全除法
  let safe_denominator = denominator == 0 ? 1 : denominator
  let result = numerator / safe_denominator
  assert_eq(10, result)
}

test "type_conversion" {
  // 测试类型转换
  let int_val = 42
  let float_val = 42.5
  let string_val = "42"
  
  // 假设有类型转换函数
  let int_to_string = int_to_string(int_val)
  assert_eq_string("42", int_to_string)
  
  // 假设有字符串到整数转换
  let string_to_int = string_to_int(string_val)
  assert_eq(42, string_to_int)
}

test "loop_patterns" {
  // 测试循环模式
  let sum = 0
  let product = 1
  
  // 模拟 for 循环求和 1+2+3+4+5
  sum = 1 + 2 + 3 + 4 + 5
  assert_eq(15, sum)
  
  // 模拟 for 循环求积 1*2*3*4
  product = 1 * 2 * 3 * 4
  assert_eq(24, product)
}

test "data_validation" {
  // 测试数据验证
  let email = "user@example.com"
  let password = "SecurePass123"
  let age = 25
  
  // 简单的邮箱验证（包含@符号）
  let has_at_symbol = string_contains(email, "@")
  assert_eq(true, has_at_symbol)
  
  // 密码长度验证
  let password_length = string_length(password)
  let is_password_valid = password_length >= 8
  assert_eq(true, is_password_valid)
  
  // 年龄验证
  let is_age_valid = age >= 18 and age <= 120
  assert_eq(true, is_age_valid)
}