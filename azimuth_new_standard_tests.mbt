// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "advanced_mathematical_operations" {
  // 高级数学运算测试
  let base = 5
  let exponent = 3
  
  // 计算 5^3 = 125
  let power = @azimuth.multiply(@azimuth.multiply(base, base), base)
  @azimuth.assert_eq(125, power)
  
  // 计算平方和：a^2 + b^2
  let a = 3
  let b = 4
  let a_squared = @azimuth.multiply(a, a)
  let b_squared = @azimuth.multiply(b, b)
  let sum_of_squares = @azimuth.add(a_squared, b_squared)
  @azimuth.assert_eq(25, sum_of_squares)
}

test "financial_calculations" {
  // 金融计算测试
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简单利息计算：本金 + 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)
  
  // 分期付款计算：总价 / 期数
  let total_price = 3600
  let installments = 12
  let monthly_payment = total_price / installments
  @azimuth.assert_eq(300, monthly_payment)
}

test "geometry_calculations" {
  // 几何计算测试
  let length = 8
  let width = 6
  
  // 矩形面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)
  
  // 矩形周长
  let perimeter = @azimuth.add(@azimuth.multiply(length, 2), @azimuth.multiply(width, 2))
  @azimuth.assert_eq(28, perimeter)
  
  // 三角形面积（底×高÷2）
  let base = 10
  let height = 5
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(25, triangle_area)
}

test "temperature_conversion" {
  // 温度转换测试
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  let name = "Alice"
  let greeting = @azimuth.greet(name)
  @azimuth.assert_eq_string("Hello, Alice!", greeting)
  
  // 测试包含数字和特殊字符的字符串
  let complex_name = "User123@Example"
  let complex_greeting = @azimuth.greet(complex_name)
  @azimuth.assert_eq_string("Hello, User123@Example!", complex_greeting)
  
  // 测试包含中文字符的字符串
  let chinese_name = "张三"
  let chinese_greeting = @azimuth.greet(chinese_name)
  @azimuth.assert_eq_string("Hello, 张三!", chinese_greeting)
}

test "boundary_value_extreme" {
  // 极端边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 极值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "algorithm_implementation" {
  // 简单算法实现测试
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let factorial = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, factorial)
  
  // 计算斐波那契数列第6项（简化版）
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  @azimuth.assert_eq(8, fib6)
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 购物车计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let shipping = 20
  
  let total1 = @azimuth.multiply(item1_price, item1_qty)
  let total2 = @azimuth.multiply(item2_price, item2_qty)
  let subtotal = @azimuth.add(total1, total2)
  let final_total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(717, final_total)
  
  // 速度计算：距离 ÷ 时间
  let distance = 300
  let time = 5
  let speed = distance / time
  @azimuth.assert_eq(60, speed)
}

test "error_handling_validation" {
  // 错误处理和验证测试
  // 验证断言函数正常工作
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 验证溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 验证边界条件
  let min_val = -2147483648
  @azimuth.assert_true(@azimuth.add(min_val, -100) == min_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
}