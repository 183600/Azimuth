// Azimuth 库综合测试用例
// 使用标准 MoonBit 测试语法

test "subtract_functionality" {
  // 测试减法功能（通过加负数实现）
  assert_eq(5, add(10, -5))
  assert_eq(-3, add(2, -5))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-5, -5))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(-3, divide_with_ceil(-8, 3))
}

test "multiply_by_negative_one" {
  // 测试乘以-1的取反操作
  assert_eq(-5, multiply(5, -1))
  assert_eq(5, multiply(-5, -1))
  assert_eq(0, multiply(0, -1))
  assert_eq(100, multiply(-100, -1))
}

test "greet_with_special_characters" {
  // 测试特殊字符的问候功能
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, <>[]{}!", greet("<>[]{}"))
  assert_eq_string("Hello, 🌟🌙⭐!", greet("🌟🌙⭐"))
}

test "complex_nested_operations" {
  // 测试复杂嵌套运算
  let result = add(multiply(3, add(2, 4)), divide_with_ceil(multiply(5, 7), 3))
  assert_eq(36, result)  // 3*(2+4) + ceil(5*7/3) = 18 + 12 = 30
  
  let result2 = multiply(add(divide_with_ceil(25, 4), 3), 2)
  assert_eq(14, result2)  // (ceil(25/4)+3)*2 = (7+3)*2 = 20
}

test "boundary_value_analysis" {
  // 边界值分析测试
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(0, 1))
  assert_eq(0, multiply(0, -1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "mathematical_series_calculation" {
  // 数学级数计算测试
  // 计算等差数列前n项和：1+2+3+...+10 = 55
  let n = 10
  let sum = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(55, sum)
  
  // 计算等比数列前n项积：2^4 = 16
  let base = 2
  let power = 4
  let product = multiply(multiply(multiply(base, base), base), base)
  assert_eq(16, product)
}

test "real_world_financial_calculation" {
  // 实际金融计算场景
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简单利息计算（使用向上取整）
  let year1_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let year2_interest = divide_with_ceil(multiply(add(principal, year1_interest), rate_percent), 100)
  let year3_interest = divide_with_ceil(multiply(add(add(principal, year1_interest), year2_interest), rate_percent), 100)
  
  let total_amount = add(add(add(principal, year1_interest), year2_interest), year3_interest)
  assert_eq(1158, total_amount)
}

test "string_concatenation_with_greet" {
  // 字符串连接与问候函数结合测试
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
  
  // 测试数字字符串
  let number_str = "123"
  let number_greeting = greet(number_str)
  assert_eq_string("Hello, 123!", number_greeting)
}

test "error_resilience_test" {
  // 错误恢复能力测试
  // 测试除数为0的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试大数运算的稳定性
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(0, multiply(large_num, 0))
  assert_eq(1, divide_with_ceil(large_num, large_num))
}