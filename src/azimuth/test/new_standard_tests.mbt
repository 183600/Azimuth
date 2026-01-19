// 新增的标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能，包括基本运算、边界条件和实际应用场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(-2, @azimuth.add(3, -5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 7
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -8
  @azimuth.assert_eq(@azimuth.add(a, c), @azimuth.add(c, a))
  @azimuth.assert_eq(@azimuth.multiply(a, c), @azimuth.multiply(c, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值边界测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "string_functionality_comprehensive" {
  // 测试字符串功能的全面情况
  // 基本字符串
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 特殊字符字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  
  // 边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算矩形的面积和周长
  let length = 15
  let width = 8
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(120, area)
  @azimuth.assert_eq(46, perimeter)
  
  // 场景2：计算二次方程的值 (ax² + bx + c)
  let x = 4
  let a = 2
  let b = 3
  let c = 5
  
  let x_squared = @azimuth.multiply(x, x)
  let ax_squared = @azimuth.multiply(a, x_squared)
  let bx = @azimuth.multiply(b, x)
  let result = @azimuth.add(@azimuth.add(ax_squared, bx), c)
  
  @azimuth.assert_eq(53, result)
}

test "real_world_financial_calculations" {
  // 测试实际金融计算场景
  // 场景1：简单利息计算
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)
  
  // 场景2：折扣计算
  let original_price = 299
  let discount_rate = 20  // 20%
  let tax_rate = 8  // 8%
  
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  @azimuth.assert_eq(259, final_price)
}

test "temperature_conversion_calculations" {
  // 测试温度转换计算
  // 简化的摄氏度转华氏度公式：F = C × 2 + 30
  
  // 常见温度点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  let room_celsius = 22
  let room_fahrenheit = @azimuth.add(@azimuth.multiply(room_celsius, 2), 30)
  @azimuth.assert_eq(74, room_fahrenheit)
  
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
  
  // 负温度
  let negative_celsius = -10
  let negative_fahrenheit = @azimuth.add(@azimuth.multiply(negative_celsius, 2), 30)
  @azimuth.assert_eq(10, negative_fahrenheit)
}

test "statistical_calculations" {
  // 测试统计计算
  // 数据集：15, 25, 35, 45, 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 25), 35), 45), 55)
  @azimuth.assert_eq(175, sum)
  
  // 计算平均值
  let count = 5
  let mean = sum / count
  @azimuth.assert_eq(35, mean)
  
  // 计算范围（最大值 - 最小值）
  let range = @azimuth.add(55, -15)
  @azimuth.assert_eq(40, range)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(15, -35)
  let diff2 = @azimuth.add(25, -35)
  let diff3 = @azimuth.add(35, -35)
  let diff4 = @azimuth.add(45, -35)
  let diff5 = @azimuth.add(55, -35)
  
  let squared_diff1 = @azimuth.multiply(diff1, diff1)
  let squared_diff2 = @azimuth.multiply(diff2, diff2)
  let squared_diff3 = @azimuth.multiply(diff3, diff3)
  let squared_diff4 = @azimuth.multiply(diff4, diff4)
  let squared_diff5 = @azimuth.multiply(diff5, diff5)
  
  let sum_squared_diffs = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(squared_diff1, squared_diff2), squared_diff3), squared_diff4), squared_diff5)
  let variance = sum_squared_diffs / count
  
  @azimuth.assert_eq(200, variance)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  // 基本断言测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("success", "success")
  
  // 边界条件验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
  
  // 验证特殊值处理
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.add(0, max_val) == max_val)
  @azimuth.assert_true(@azimuth.add(0, min_val) == min_val)
  @azimuth.assert_true(@azimuth.multiply(0, max_val) == 0)
  @azimuth.assert_true(@azimuth.multiply(0, min_val) == 0)
}

test "algorithm_implementation_tests" {
  // 测试算法实现
  // 计算1到n的和：n × (n + 1) / 2
  let n = 10
  let sum_1_to_n = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum_1_to_n)
  
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1
  let factorial_5 = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial_5)
  
  // 计算幂：2^6 = 2 × 2 × 2 × 2 × 2 × 2
  let power_2_6 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(2, 2))
  @azimuth.assert_eq(64, power_2_6)
  
  // 计算斐波那契数列的第6项（简化版）
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  
  @azimuth.assert_eq(8, fib6)
}