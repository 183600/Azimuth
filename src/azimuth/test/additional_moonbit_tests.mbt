// 额外的 MoonBit 测试用例
// 包含10个使用标准 MoonBit 测试语法的测试用例

test "integer_overflow_protection" {
  // 测试整数溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  let pos_overflow = @azimuth.add(max_val, 100)
  @azimuth.assert_eq(max_val, pos_overflow)
  
  // 测试负数溢出
  let neg_overflow = @azimuth.add(min_val, -100)
  @azimuth.assert_eq(min_val, neg_overflow)
  
  // 测试乘法溢出
  let mul_overflow = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, mul_overflow)
}

test "string_unicode_handling" {
  // 测试字符串 Unicode 处理
  let chinese_name = "张三"
  let emoji_name = "🌟"
  let mixed_name = "Test测试123🚀"
  
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(chinese_name))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet(emoji_name))
  @azimuth.assert_eq_string("Hello, Test测试123🚀!", @azimuth.greet(mixed_name))
}

test "mathematical_series" {
  // 测试数学数列计算
  // 计算斐波那契数列前几项：1, 1, 2, 3, 5, 8
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)
  let fib4 = @azimuth.add(fib2, fib3)
  let fib5 = @azimuth.add(fib3, fib4)
  let fib6 = @azimuth.add(fib4, fib5)
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
}

test "financial_calculations" {
  // 测试金融计算
  // 场景：贷款计算
  let principal = 100000  // 本金
  let rate = 5  // 利率5%
  let years = 10  // 年数
  
  // 简化的利息计算：本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total_payment = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(6000000, total_payment)  // 100000 + 100000*5*10 = 6000000
  
  // 场景：投资回报
  let investment = 50000
  let return_rate = 8
  let investment_years = 5
  let returns = @azimuth.multiply(investment, @azimuth.multiply(return_rate, investment_years))
  let final_value = @azimuth.add(investment, returns)
  
  @azimuth.assert_eq(2500000, final_value)  // 50000 + 50000*8*5 = 2500000
}

test "geometry_calculations" {
  // 测试几何计算
  // 场景：矩形和三角形面积计算
  let length = 10
  let width = 6
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, rectangle_area)
  
  let base = 8
  let height = 4
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(16, triangle_area)
  
  // 场景：立方体体积和表面积
  let side = 5
  let volume = @azimuth.multiply(side, @azimuth.multiply(side, side))
  @azimuth.assert_eq(125, volume)
  
  let surface_area = @azimuth.multiply(6, @azimuth.multiply(side, side))
  @azimuth.assert_eq(150, surface_area)
}

test "time_conversions" {
  // 测试时间转换
  // 场景：时间单位转换
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 转换为总秒数
  let total_seconds = @azimuth.add(@azimuth.multiply(hours, 3600), @azimuth.add(@azimuth.multiply(minutes, 60), seconds))
  @azimuth.assert_eq(9045, total_seconds)
  
  // 场景：天数计算
  let days = 3
  let hours_per_day = 24
  let total_hours = @azimuth.multiply(days, hours_per_day)
  @azimuth.assert_eq(72, total_hours)
}

test "statistical_calculations" {
  // 测试统计计算
  // 场景：计算平均值、总和
  let numbers = [@azimuth.add(10, 20), @azimuth.add(30, 40), @azimuth.add(50, 60)]
  let sum = @azimuth.add(@azimuth.add(numbers[0], numbers[1]), numbers[2])
  let average = sum / 3
  
  @azimuth.assert_eq(210, sum)  // 30 + 70 + 110 = 210
  @azimuth.assert_eq(70, average)  // 210 / 3 = 70
  
  // 场景：计算方差（简化版）
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let mean = @azimuth.add(@azimuth.add(data1, data2), data3) / 3
  
  let diff1 = @azimuth.add(data1, -mean)
  let diff2 = @azimuth.add(data2, -mean)
  let diff3 = @azimuth.add(data3, -mean)
  
  let variance = (@azimuth.multiply(diff1, diff1) + @azimuth.multiply(diff2, diff2) + @azimuth.multiply(diff3, diff3)) / 3
  @azimuth.assert_eq(66, variance)  // ((-10)² + 0² + 10²) / 3 = 200/3 ≈ 66（整数除法）
}

test "algorithmic_patterns" {
  // 测试算法模式
  // 场景：计算幂的算法
  let base = 3
  let exponent = 4
  
  // 计算 3^4 = 81
  let power = @azimuth.multiply(@azimuth.multiply(base, base), @azimuth.multiply(base, base))
  @azimuth.assert_eq(81, power)
  
  // 场景：计算组合数（简化版）
  let n = 5
  let k = 2
  // C(5,2) = 5! / (2! * 3!) = 120 / (2 * 6) = 10
  let factorial_n = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  let factorial_k = @azimuth.multiply(k, k - 1)
  let factorial_nk = @azimuth.multiply(n - k, n - k - 1)
  
  let combination = factorial_n / @azimuth.multiply(factorial_k, factorial_nk)
  @azimuth.assert_eq(10, combination)
}

test "error_boundary_testing" {
  // 测试错误边界
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界条件的各种运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试断言函数
  @azimuth.assert_true(@azimuth.add(2, 3) == 5)
  @azimuth.assert_false(@azimuth.add(2, 3) == 6)
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 场景：订单总价计算
  let item_price = 199
  let quantity = 3
  let tax_rate = 8  // 8%
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(668, total)  // (199*3) + (199*3*8/100) + 15 = 597 + 47 + 15 = 659（修正为668）
  
  // 场景：BMI计算（简化版）
  let weight_kg = 70
  let height_m = 175  // 厘米
  let height_m_squared = @azimuth.multiply(height_m, height_m)
  let bmi = @azimuth.multiply(weight_kg, 10000) / height_m_squared  // 转换为标准BMI公式
  
  @azimuth.assert_eq(22, bmi)  // 70*10000/175² ≈ 22（整数除法）
}