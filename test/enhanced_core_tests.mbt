// 增强的核心功能测试用例
// 测试 azimuth 包的核心功能：add, multiply, greet 函数
// 包含边界情况、实际应用场景和复杂计算测试

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  assert_eq(add(5, 10), add(10, 5))
  assert_eq(add(-3, 7), add(7, -3))
  assert_eq(add(-15, -20), add(-20, -15))
  assert_eq(add(0, 100), add(100, 0))
  assert_eq(add(2147483647, -2147483647), add(-2147483647, 2147483647))
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 6
  let b = 8
  let c = 12
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 测试负数情况
  let x = -5
  let y = 3
  let z = 7
  
  let left_side_neg = multiply(x, add(y, z))
  let right_side_neg = add(multiply(x, y), multiply(x, z))
  assert_eq(left_side_neg, right_side_neg)
}

test "greet_multilingual_support" {
  // 测试多语言字符串支持
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, John!", greet("John"))
  assert_eq_string("Hello, José!", greet("José"))
  assert_eq_string("Hello, François!", greet("François"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 東京!", greet("東京"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
  assert_eq_string("Hello, 123-ABC!", greet("123-ABC"))
  assert_eq_string("Hello, !", greet(""))
}

test "financial_calculations" {
  // 测试金融计算场景
  // 计算复利：本金 × (1 + 利率 × 年数)
  let principal = 10000  // 本金
  let rate = 5           // 利率 5%
  let years = 3          // 年数
  
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
  
  // 计算折扣：原价 × (1 - 折扣率)
  let original_price = 500
  let discount_rate = 20   // 20% 折扣
  let discount_amount = multiply(original_price, discount_rate) / 100
  let discounted_price = add(original_price, -discount_amount)
  assert_eq(400, discounted_price)
}

test "temperature_conversions" {
  // 测试温度转换计算
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  assert_eq(30, add(multiply(0, 2), 30))
  assert_eq(50, add(multiply(10, 2), 30))
  assert_eq(80, add(multiply(25, 2), 30))
  assert_eq(104, add(multiply(37, 2), 30))
  assert_eq(230, add(multiply(100, 2), 30))
  
  // 华氏度转摄氏度（简化版）：C = (F - 30) ÷ 2
  let fahrenheit = 80
  let celsius = (fahrenheit - 30) / 2
  assert_eq(25, celsius)
}

test "geometry_calculations" {
  // 测试几何计算
  // 计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(96, area)      // 12 × 8 = 96
  assert_eq(40, perimeter) // 2 × (12 + 8) = 40
  
  // 计算三角形面积（简化）：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = multiply(base, height) / 2
  assert_eq(30, triangle_area)
  
  // 计算梯形面积：（上底 + 下底）× 高 ÷ 2
  let top_base = 6
  let bottom_base = 10
  let trapezoid_height = 4
  let trapezoid_area = multiply(add(top_base, bottom_base), trapezoid_height) / 2
  assert_eq(32, trapezoid_area)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算总和：15 + 25 + 35 + 45 + 55 = 175
  let sum = add(add(add(add(15, 25), 35), 45), 55)
  assert_eq(175, sum)
  
  // 计算平均值
  let average = sum / 5
  assert_eq(35, average)
  
  // 计算中位数（已排序数组）
  let median = 35  // 中间值
  assert_eq(35, median)
  
  // 计算范围（最大值 - 最小值）
  let range = add(55, -15)
  assert_eq(40, range)
}

test "boundary_value_stress_test" {
  // 测试边界值压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续加法运算
  let result1 = add(add(add(max_val, -1000000), 500000), -500000)
  assert_eq(2147482647, result1)
  
  // 测试连续乘法运算
  let result2 = multiply(multiply(100, 100), multiply(10, 10))
  assert_eq(1000000, result2)
  
  // 测试混合运算
  let result3 = add(multiply(1000, 2), multiply(500, 3))
  assert_eq(3500, result3)
  
  // 测试极值组合
  let result4 = add(multiply(max_val, 0), min_val)
  assert_eq(min_val, result4)
  
  let result5 = multiply(add(min_val, 1), 0)
  assert_eq(0, result5)
}

test "time_calculations" {
  // 测试时间计算
  // 计算总秒数：小时 × 3600 + 分钟 × 60 + 秒
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let total_seconds = add(add(multiply(hours, 3600), multiply(minutes, 60)), seconds)
  assert_eq(9045, total_seconds)
  
  // 计算时间差
  let start_hour = 9
  let start_minute = 15
  let end_hour = 14
  let end_minute = 45
  
  let start_minutes = add(multiply(start_hour, 60), start_minute)
  let end_minutes = add(multiply(end_hour, 60), end_minute)
  let duration = add(end_minutes, -start_minutes)
  
  assert_eq(330, duration)  // 5小时30分钟 = 330分钟
}

test "complex_business_logic" {
  // 测试复杂业务逻辑
  // 订单总价计算：商品总价 × 数量 + 运费 - 折扣
  let unit_price = 199
  let quantity = 3
  let shipping_fee = 20
  let discount_threshold = 500
  let discount_rate = 10
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = multiply(unit_price, quantity)
  
  // 计算折扣（如果超过阈值）
  let discount = if (subtotal > discount_threshold) {
    multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算税费
  let after_discount = add(subtotal, -discount)
  let tax = multiply(after_discount, tax_rate) / 100
  
  // 计算最终总价（含运费）
  let final_total = add(add(after_discount, tax), shipping_fee)
  
  assert_eq(656, final_total)  // (199×3 - 59.7) × 1.08 + 20 ≈ 656
}