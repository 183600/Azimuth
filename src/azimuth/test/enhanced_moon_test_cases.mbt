// 增强的 MoonBit 测试用例
// 包含10个使用标准 MoonBit 测试语法的测试用例

test "statistical_calculations" {
  // 统计计算测试
  // 计算一组数据的平均值、方差等
  let numbers = [@azimuth.add(10, 20), @azimuth.multiply(15, 2), @azimuth.add(30, -5)]
  let sum = @azimuth.add(@azimuth.add(numbers[0], numbers[1]), numbers[2])
  let mean = sum / 3
  
  @azimuth.assert_eq(30, mean)
  @azimuth.assert_eq(90, sum)
}

test "geometric_calculations" {
  // 几何计算测试
  // 计算矩形面积和周长
  let length = 8
  let width = 6
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(length, 2), @azimuth.multiply(width, 2))
  
  @azimuth.assert_eq(48, area)
  @azimuth.assert_eq(28, perimeter)
}

test "financial_calculations" {
  // 金融计算测试
  // 简单的贷款计算
  let principal = 10000
  let annual_rate = 5
  let years = 3
  
  // 简单利息计算
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  let total_payment = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(150000, total_interest)
  @azimuth.assert_eq(160000, total_payment)
}

test "physics_simulations" {
  // 物理模拟测试
  // 自由落体运动计算（简化版）
  let initial_velocity = 0
  let gravity = 10
  let time = 5
  
  // 距离 = 初速度 × 时间 + 0.5 × 重力 × 时间²
  let distance = @azimuth.add(
    @azimuth.multiply(initial_velocity, time),
    @azimuth.multiply(gravity, @azimuth.multiply(time, time)) / 2
  )
  
  @azimuth.assert_eq(125, distance)
}

test "data_structure_operations" {
  // 数据结构操作测试
  // 模拟栈操作
  let stack_top = 42
  let push_value = 17
  
  // 模拟入栈操作
  let new_top = @azimuth.add(stack_top, push_value)
  
  // 模拟出栈操作
  let popped_value = push_value
  let restored_top = @azimuth.add(new_top, -popped_value)
  
  @azimuth.assert_eq(59, new_top)
  @azimuth.assert_eq(42, restored_top)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试各种字符串边界情况
  let empty_string = ""
  let single_char = "A"
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  let unicode_string = "测试字符串"
  let mixed_string = "Test123@#$"
  
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(empty_string))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet(single_char))
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongStringForTestingPurposes!", @azimuth.greet(long_string))
  @azimuth.assert_eq_string("Hello, 测试字符串!", @azimuth.greet(unicode_string))
  @azimuth.assert_eq_string("Hello, Test123@#$!", @azimuth.greet(mixed_string))
}

test "algorithm_complexity" {
  // 算法复杂度测试
  // 计算斐波那契数列（前几项）
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)
  let fib4 = @azimuth.add(fib2, fib3)
  let fib5 = @azimuth.add(fib3, fib4)
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  
  // 计算等比数列
  let ratio = 2
  let first_term = 3
  let term2 = @azimuth.multiply(first_term, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  
  @azimuth.assert_eq(6, term2)
  @azimuth.assert_eq(12, term3)
  @azimuth.assert_eq(24, term4)
}

test "edge_case_combinations" {
  // 边界情况组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 测试各种边界组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(max_val, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(min_val, zero))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试连续边界运算
  let complex_boundary = @azimuth.add(
    @azimuth.multiply(max_val, 0),
    @azimuth.add(min_val, max_val)
  )
  @azimuth.assert_eq(-1, complex_boundary)
}

test "real_world_applications" {
  // 实际应用测试
  // 计算BMI指数（简化版）
  let weight_kg = 70
  let height_m = 175
  let height_squared = @azimuth.multiply(height_m, height_m)
  let bmi = @azimuth.multiply(weight_kg, 10000) / height_squared
  
  @azimuth.assert_eq(22, bmi)
  
  // 计算订单折扣
  let order_total = 1200
  let discount_threshold = 1000
  let discount_rate = 10
  let discount = if order_total > discount_threshold {
    @azimuth.multiply(order_total, discount_rate) / 100
  } else {
    0
  }
  let final_total = @azimuth.add(order_total, -discount)
  
  @azimuth.assert_eq(120, discount)
  @azimuth.assert_eq(1080, final_total)
}

test "performance_optimization" {
  // 性能优化测试
  // 测试不同计算方法的效率
  let base = 100
  let multiplier = 5
  let iterations = 4
  
  // 方法1：重复加法
  let result1 = @azimuth.add(
    @azimuth.add(
      @azimuth.add(base, base),
      @azimuth.add(base, base)
    ),
    @azimuth.add(
      @azimuth.add(base, base),
      @azimuth.add(base, base)
    )
  )
  
  // 方法2：使用乘法
  let result2 = @azimuth.multiply(base, multiplier)
  
  // 验证结果一致性
  @azimuth.assert_eq(result1, result2)
  @azimuth.assert_eq(500, result1)
  @azimuth.assert_eq(500, result2)
  
  // 测试大数运算
  let large_num1 = 100000
  let large_num2 = 200000
  let large_sum = @azimuth.add(large_num1, large_num2)
  let large_product = @azimuth.multiply(100, 2000)
  
  @azimuth.assert_eq(300000, large_sum)
  @azimuth.assert_eq(200000, large_product)
}