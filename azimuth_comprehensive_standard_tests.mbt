// Azimuth 综合标准测试用例
// 覆盖核心功能的全面测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  // 基本加法
  assert_eq(15, add(7, 8))
  assert_eq(-5, add(-10, 5))
  assert_eq(0, add(100, -100))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  // 基本乘法
  assert_eq(42, multiply(6, 7))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(0, multiply(0, 999))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_comprehensive" {
  // 全面测试问候函数
  // 基本问候
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  
  // 特殊字符测试
  assert_eq_string("Hello, 测试123!", greet("测试123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_operations_chain" {
  // 测试数学运算链
  let a = 10
  let b = 20
  let c = 30
  
  // 复合运算：(a + b) * c
  let sum = add(a, b)
  let product = multiply(sum, c)
  assert_eq(900, product)
  
  // 另一个复合运算：a * b + c
  let product2 = multiply(a, b)
  let sum2 = add(product2, c)
  assert_eq(230, sum2)
}

test "real_world_calculation_scenario" {
  // 实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  // 计算商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let subtotal = add(total1, total2)
  
  // 计算最终总价（含运费）
  let final_total = add(subtotal, shipping)
  
  // 验证：199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
  assert_eq(717, final_total)
}

test "temperature_conversion_calculation" {
  // 温度转换计算（简化版）
  // 摄氏度转华氏度：F = C * 2 + 30（简化公式）
  
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = 0
  let freezing_f = add(multiply(freezing, 2), 30)
  assert_eq(30, freezing_f)
  
  // 测试沸点
  let boiling = 100
  let boiling_f = add(multiply(boiling, 2), 30)
  assert_eq(230, boiling_f)
}

test "statistical_calculations" {
  // 统计计算测试
  // 计算平均数
  let numbers_sum = add(add(add(85, 90), 75), 80)
  let average = numbers_sum / 4
  assert_eq(82, average)
  
  // 计算总和
  let scores = [100, 95, 90, 85, 80]
  let total_scores = add(add(add(add(100, 95), 90), 85), 80)
  assert_eq(450, total_scores)
  
  // 计算极差
  let max_score = 100
  let min_score = 80
  let range = add(max_score, -min_score)
  assert_eq(20, range)
}

test "assert_functions_validation" {
  // 测试断言函数
  // 测试成功断言
  assert_true(5 > 3)
  assert_false(5 < 3)
  assert_eq(10, add(3, 7))
  assert_eq_string("Hello, Test!", greet("Test"))
  
  // 测试边界条件断言
  let max_val = 2147483647
  let min_val = -2147483648
  assert_true(add(max_val, 1) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
}

test "complex_string_scenarios" {
  // 复杂字符串场景测试
  // 长字符串测试
  let long_name = "VeryLongNameWithManyCharacters"
  assert_eq_string("Hello, VeryLongNameWithManyCharacters!", greet(long_name))
  
  // 包含空格的字符串
  let spaced_name = "John Doe"
  assert_eq_string("Hello, John Doe!", greet(spaced_name))
  
  // 特殊符号字符串
  let symbols = "test@example.com"
  assert_eq_string("Hello, test@example.com!", greet(symbols))
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  assert_true(add(max_val, max_val) == max_val)
  assert_true(add(min_val, min_val) == min_val)
  
  // 测试乘法溢出保护
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, 2) == min_val)
  
  // 测试特殊值运算
  assert_true(multiply(min_val, -1) == min_val)
  assert_true(add(0, max_val) == max_val)
  assert_true(add(0, min_val) == min_val)
}