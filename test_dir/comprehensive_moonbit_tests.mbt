// 综合MoonBit测试用例
// 使用标准的MoonBit测试语法，覆盖azimuth模块的核心功能

// 导入主模块中的函数
use azimuth

test "add_function_basic_tests" {
  // 基本加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(50, 50))
}

test "add_function_zero_handling" {
  // 零值处理测试
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(-10, add(-10, 0))
  assert_eq(-10, add(0, -10))
  assert_eq(0, add(0, 0))
}

test "add_function_overflow_protection" {
  // 溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  
  // 测试极值相加
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_tests" {
  // 基本乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(5, 0))
  assert_eq(25, multiply(5, 5))
  assert_eq(100, multiply(10, 10))
}

test "multiply_function_identity_elements" {
  // 单位元测试
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
  assert_eq(-7, multiply(-7, 1))
  assert_eq(-7, multiply(1, -7))
  
  // 零元测试
  assert_eq(0, multiply(123, 0))
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(-456, 0))
  assert_eq(0, multiply(0, -456))
}

test "multiply_function_negative_one" {
  // 负一乘法测试
  assert_eq(-5, multiply(5, -1))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(5, multiply(-5, -1))
  assert_eq(5, multiply(-1, -5))
  
  // 最小值特殊情况
  let min_val = -2147483648
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(-1, min_val))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  
  // 特殊字符测试
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello,   spaced!", greet("  spaced"))
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  let a = 12
  let b = 8
  let c = 5
  
  // 测试 (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(100, result1)
  
  // 测试 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(101, result2)
  
  // 测试分配律 a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "real_world_application_scenarios" {
  // 实际应用场景测试
  // 场景1：计算矩形面积和周长
  let length = 15
  let width = 10
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(150, area)
  assert_eq(50, perimeter)
  
  // 场景2：计算折扣价格
  let original_price = 300
  let discount_percentage = 20
  let discount_amount = multiply(original_price, discount_percentage) / 100
  let final_price = add(original_price, -discount_amount)
  
  assert_eq(240, final_price)
  
  // 场景3：计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let average = add(add(score1, score2), score3) / 3
  
  assert_eq(90, average)
}

test "assertion_functions_verification" {
  // 断言函数验证测试
  // 测试assert_true和assert_false
  assert_true(true)
  assert_false(false)
  
  // 测试assert_eq
  assert_eq(42, 42)
  assert_eq(0, 0)
  assert_eq(-100, -100)
  
  // 测试assert_eq_string
  assert_eq_string("test", "test")
  assert_eq_string("", "")
  assert_eq_string("Hello, World!", "Hello, World!")
}