// 增强的 MoonBit 测试用例
// 测试 azimuth 包的核心功能和边界情况

test "add_function_commutativity" {
  // 测试加法交换律：a + b = b + a
  assert_eq(azimuth::add(15, 25), azimuth::add(25, 15))
  assert_eq(azimuth::add(-10, 20), azimuth::add(20, -10))
  assert_eq(azimuth::add(0, 100), azimuth::add(100, 0))
}

test "add_function_associativity" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  
  let result1 = azimuth::add(azimuth::add(a, b), c)
  let result2 = azimuth::add(a, azimuth::add(b, c))
  assert_eq(result1, result2)
}

test "multiply_function_commutativity" {
  // 测试乘法交换律：a * b = b * a
  assert_eq(azimuth::multiply(12, 15), azimuth::multiply(15, 12))
  assert_eq(azimuth::multiply(-8, 7), azimuth::multiply(7, -8))
  assert_eq(azimuth::multiply(0, 50), azimuth::multiply(50, 0))
}

test "multiply_function_distributivity" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(left_side, right_side)
}

test "greet_function_multilingual" {
  // 测试多语言问候功能
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
  assert_eq_string("Hello, Ñoño!", azimuth::greet("Ñoño"))
  assert_eq_string("Hello, Москва!", azimuth::greet("Москва"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：订单总价计算
  let product_price = 199
  let quantity = 3
  let discount_rate = 10
  let tax_rate = 8
  let shipping = 15
  
  // 计算商品总价
  let subtotal = azimuth::multiply(product_price, quantity)
  // 计算折扣
  let discount = azimuth::multiply(subtotal, discount_rate) / 100
  // 计算折后价格
  let discounted_price = azimuth::add(subtotal, -discount)
  // 计算税费
  let tax = azimuth::multiply(discounted_price, tax_rate) / 100
  // 计算最终总价
  let total = azimuth::add(azimuth::add(discounted_price, tax), shipping)
  
  assert_eq(698, total)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景：摄氏度转华氏度（简化版）
  // F = C * 9/5 + 32，简化为 F = C * 2 + 30
  
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = 0
  let freezing_f = azimuth::add(azimuth::multiply(freezing, 2), 30)
  assert_eq(30, freezing_f)
  
  // 测试沸点
  let boiling = 100
  let boiling_f = azimuth::add(azimuth::multiply(boiling, 2), 30)
  assert_eq(230, boiling_f)
}

test "geometric_calculations" {
  // 测试几何计算：矩形和三角形面积
  let length = 12
  let width = 8
  
  // 矩形面积 = 长 × 宽
  let rectangle_area = azimuth::multiply(length, width)
  assert_eq(96, rectangle_area)
  
  // 三角形面积 = 底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = azimuth::multiply(base, height) / 2
  assert_eq(30, triangle_area)
  
  // 梯形面积 = (上底 + 下底) × 高 ÷ 2
  let top_base = 6
  let bottom_base = 10
  let trapezoid_height = 4
  let trapezoid_area = azimuth::multiply(azimuth::add(top_base, bottom_base), trapezoid_height) / 2
  assert_eq(32, trapezoid_area)
}

test "statistical_calculations" {
  // 测试统计计算：平均值、总和等
  let numbers = [15, 25, 35, 45, 55]
  
  // 计算总和
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(15, 25), 35), 45), 55)
  assert_eq(175, sum)
  
  // 计算平均值
  let average = sum / 5
  assert_eq(35, average)
  
  // 计算中位数（已排序数组）
  let median = 35
  assert_eq(35, median)
  
  // 计算范围（最大值 - 最小值）
  let range = azimuth::add(55, -15)
  assert_eq(40, range)
}

test "extreme_boundary_conditions" {
  // 测试极端边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(max_val, -1))
  
  // 测试最小值的各种运算
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  
  // 测试零的运算特性
  assert_eq(0, azimuth::multiply(0, max_val))
  assert_eq(0, azimuth::multiply(0, min_val))
  assert_eq(max_val, azimuth::add(0, max_val))
  assert_eq(min_val, azimuth::add(0, min_val))
}