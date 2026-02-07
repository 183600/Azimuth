test "enhanced_arithmetic_basic_operations" {
  // 增强算术基本运算测试
  assert_eq(10, add(7, 3))
  assert_eq(50, multiply(10, 5))
  assert_eq(20, subtract(30, 10))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "enhanced_string_greeting_functionality" {
  // 增强字符串问候功能测试
  assert_eq_string("Hello, Enhanced!", greet("Enhanced"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "enhanced_boundary_value_testing" {
  // 增强边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "enhanced_negative_number_operations" {
  // 增强负数运算测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(-1, divide_with_ceil(-5, 3))
}

test "enhanced_complex_calculation_scenario" {
  // 增强复杂计算场景测试
  let base = 100
  let multiplier = 3
  let offset = 25
  
  let step1 = multiply(base, multiplier)
  let step2 = add(step1, offset)
  let step3 = divide_with_ceil(step2, 10)
  
  assert_eq(300, step1)
  assert_eq(325, step2)
  assert_eq(33, step3)
}

test "enhanced_financial_calculation_example" {
  // 增强金融计算示例测试
  let principal = 10000
  let rate = 5
  
  let interest = divide_with_ceil(multiply(principal, rate), 100)
  let total = add(principal, interest)
  
  assert_eq(500, interest)
  assert_eq(10500, total)
}

test "enhanced_error_handling_verification" {
  // 增强错误处理验证测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "enhanced_mathematical_properties_validation" {
  // 增强数学性质验证测试
  let x = 15
  let y = 25
  
  // 验证交换律
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证单位元
  assert_eq(add(x, 0), x)
  assert_eq(multiply(x, 1), x)
}