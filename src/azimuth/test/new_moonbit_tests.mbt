// 新增的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，不超过10个测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-3, @azimuth.add(5, -8))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-18, @azimuth.multiply(-6, 3))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数的交换律
  let c = -5
  let d = 9
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算三角形的面积：底 × 高 ÷ 2
  let base = 12
  let height = 8
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(48, triangle_area)
  
  // 计算等差数列的和：n × (首项 + 末项) ÷ 2
  let n = 10
  let first_term = 1
  let last_term = 10
  let arithmetic_sum = @azimuth.multiply(n, @azimuth.add(first_term, last_term)) / 2
  @azimuth.assert_eq(55, arithmetic_sum)
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景
  // 计算贷款月供（简化版）：本金 ÷ 期数 + 利息
  let principal = 120000
  let months = 12
  let monthly_rate = 1  // 1%月利率
  
  let monthly_principal = principal / months
  let first_month_interest = @azimuth.multiply(principal, monthly_rate) / 100
  let first_month_payment = @azimuth.add(monthly_principal, first_month_interest)
  
  @azimuth.assert_eq(10000, monthly_principal)
  @azimuth.assert_eq(1200, first_month_interest)
  @azimuth.assert_eq(11200, first_month_payment)
}

test "temperature_conversion_calculation" {
  // 测试温度转换计算
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  let celsius_temperatures = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  // 测试冰点
  let freezing_f = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  // 测试室温
  let room_f = @azimuth.add(@azimuth.multiply(20, 2), 30)
  @azimuth.assert_eq(70, room_f)
  
  // 测试沸点
  let boiling_f = @azimuth.add(@azimuth.multiply(100, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
}

test "assertion_function_validation" {
  // 测试断言函数的有效性
  // 测试 assert_true 和 assert_false
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(@azimuth.add(2, 3) == 5)
  @azimuth.assert_true(@azimuth.multiply(4, 5) == 20)
  
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_false(@azimuth.add(2, 3) == 6)
  @azimuth.assert_false(@azimuth.multiply(4, 5) == 19)
  
  // 测试 assert_eq 和 assert_eq_string
  @azimuth.assert_eq(42, @azimuth.add(20, 22))
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, min_val))
  
  // 测试特殊情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "performance_calculation_patterns" {
  // 测试性能计算模式
  // 计算平方和：1² + 2² + 3² + 4² + 5²
  let square1 = @azimuth.multiply(1, 1)
  let square2 = @azimuth.multiply(2, 2)
  let square3 = @azimuth.multiply(3, 3)
  let square4 = @azimuth.multiply(4, 4)
  let square5 = @azimuth.multiply(5, 5)
  
  let sum_of_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(square1, square2), square3), square4), square5)
  @azimuth.assert_eq(55, sum_of_squares)
  
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1
  let factorial = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial)
  
  // 计算幂运算：2^6 = 64
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)))
  @azimuth.assert_eq(64, power_of_2)
}