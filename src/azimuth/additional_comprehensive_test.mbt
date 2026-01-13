// 额外的综合测试用例
// 这些测试用例覆盖了更多的边界情况和实际应用场景

// 测试负数加法的各种情况
test "negative_addition_comprehensive" {
  // 测试两个负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  
  // 测试负数与正数相加
  @azimuth.assert_eq(-2, @azimuth.add(-10, 8))
  @azimuth.assert_eq(5, @azimuth.add(-3, 8))
  
  // 测试对称性
  @azimuth.assert_eq(@azimuth.add(-5, 10), @azimuth.add(10, -5))
}

// 测试负数乘法的各种情况
test "negative_multiplication_comprehensive" {
  // 测试两个负数相乘得正数
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  
  // 测试负数与正数相乘得负数
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  
  // 测试乘法分配律 (a * (b + c) = a * b + a * c)
  let a = 3
  let b = 4
  let c = 5
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

// 测试字符串拼接的边界情况
test "string_greet_edge_cases" {
  // 测试非常长的字符串
  let long_name = "ThisIsAVeryLongNameThatCouldPotentiallyCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, " + long_name + "!", result)
  
  // 测试包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
  
  // 测试包含反斜杠的字符串
  @azimuth.assert_eq_string("Hello, C:\\Users\\Test!", @azimuth.greet("C:\\Users\\Test"))
}

// 测试数学函数的组合使用
test "mathematical_function_combination" {
  // 测试多项式计算：2x² + 3x + 1，当 x = 5 时
  let x = 5
  let x_squared = @azimuth.multiply(x, x)  // x²
  let term1 = @azimuth.multiply(2, x_squared)  // 2x²
  let term2 = @azimuth.multiply(3, x)  // 3x
  let result = @azimuth.add(@azimuth.add(term1, term2), 1)  // 2x² + 3x + 1
  @azimuth.assert_eq(66, result)  // 2*25 + 3*5 + 1 = 50 + 15 + 1 = 66
  
  // 测试几何计算：计算梯形面积 (上底 + 下底) * 高 / 2
  let top_base = 5
  let bottom_base = 10
  let height = 4
  let area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), height) / 2
  @azimuth.assert_eq(30, area)  // (5 + 10) * 4 / 2 = 15 * 4 / 2 = 30
}

// 测试实际应用场景：温度转换
test "temperature_conversion_scenario" {
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  @azimuth.assert_eq(77, fahrenheit)  // 25 * 9/5 + 32 = 45 + 32 = 77
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let fahrenheit2 = 77
  let celsius2 = @azimuth.multiply(fahrenheit2 - 32, 5) / 9
  @azimuth.assert_eq(25, celsius2)  // (77 - 32) * 5/9 = 45 * 5/9 = 25
}

// 测试实际应用场景：购物计算
test "shopping_calculation_scenario" {
  // 计算购物总价：商品价格 × 数量 + 运费
  let item_price = 199  // 商品价格：1.99元（以分为单位）
  let quantity = 3      // 数量
  let shipping = 500    // 运费：5.00元（以分为单位）
  
  let subtotal = @azimuth.multiply(item_price, quantity)  // 小计
  let total = @azimuth.add(subtotal, shipping)            // 总价
  
  @azimuth.assert_eq(1097, total)  // 199 * 3 + 500 = 597 + 500 = 1097（10.97元）
  
  // 计算折扣后的价格：总价 × (100 - 折扣百分比) / 100
  let discount_percent = 10  // 10% 折扣
  let discounted_total = @azimuth.multiply(total, 100 - discount_percent) / 100
  @azimuth.assert_eq(987, discounted_total)  // 1097 * 90 / 100 = 987.3 ≈ 987（9.87元）
}

// 测试数据类型边界和特殊情况
test "data_type_boundary_conditions" {
  // 测试连续运算的边界情况
  let max_val = 2147483647
  
  // 测试达到最大值后继续运算
  let result1 = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result1)  // 应该饱和到最大值
  
  // 测试从最大值减法
  let result2 = max_val - 100
  @azimuth.assert_eq(2147483547, result2)
  
  // 测试最小值的特殊情况
  let min_val = -2147483648
  let result3 = @azimuth.add(min_val, 1)
  @azimuth.assert_eq(-2147483647, result3)
}

// 测试错误处理和异常情况
test "error_handling_and_edge_cases" {
  // 测试大数运算的稳定性
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  // 大数相加
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  
  // 大数相乘
  @azimuth.assert_eq(2000000000000, @azimuth.multiply(large_num1, large_num2))
  
  // 测试零的各种运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123456789))
  @azimuth.assert_eq(0, @azimuth.multiply(-123456789, 0))
  @azimuth.assert_eq(123456789, @azimuth.add(0, 123456789))
  @azimuth.assert_eq(-123456789, @azimuth.add(-123456789, 0))
}

// 测试性能相关的场景
test "performance_related_scenarios" {
  // 测试多次连续运算的结果一致性
  let base = 100
  let iterations = 10
  
  // 连续加法
  let mut sum = 0
  for i = 0; i < iterations; i = i + 1 {
    sum = @azimuth.add(sum, base)
  }
  @azimuth.assert_eq(1000, sum)  // 100 * 10 = 1000
  
  // 连续乘法（2的幂次方）
  let mut power = 1
  for i = 0; i < 10; i = i + 1 {
    power = @azimuth.multiply(power, 2)
  }
  @azimuth.assert_eq(1024, power)  // 2^10 = 1024
}

// 测试实际应用场景：时间计算
test "time_calculation_scenario" {
  // 计算总秒数：小时 × 3600 + 分钟 × 60 + 秒
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let hours_in_seconds = @azimuth.multiply(hours, 3600)
  let minutes_in_seconds = @azimuth.multiply(minutes, 60)
  let total_seconds = @azimuth.add(@azimuth.add(hours_in_seconds, minutes_in_seconds), seconds)
  
  @azimuth.assert_eq(9045, total_seconds)  // 2*3600 + 30*60 + 45 = 7200 + 1800 + 45 = 9045
  
  // 计算时间差（秒转换为时分秒）
  let total_seconds2 = 9045
  let calculated_hours = total_seconds2 / 3600
  let remaining_seconds = total_seconds2 % 3600
  let calculated_minutes = remaining_seconds / 60
  let calculated_seconds = remaining_seconds % 60
  
  @azimuth.assert_eq(hours, calculated_hours)
  @azimuth.assert_eq(minutes, calculated_minutes)
  @azimuth.assert_eq(seconds, calculated_seconds)
}