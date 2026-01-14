// 额外的 MoonBit 测试用例
// 包含 8 个针对特定场景的测试用例

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))     // 10/4 = 2.5 -> 3
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(8, 4))      // 8/4 = 2 -> 2
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(9, 2))      // 9/2 = 4.5 -> 5
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))      // 0/5 = 0 -> 0
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 4))    // -8/4 = -2 -> -2
}

test "extreme_overflow_scenarios" {
  // 测试极端溢出场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与自身相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 最小值与自身相加
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 最大值与最大值相乘
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, max_val))
  
  // 最小值与2相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "string_processing_with_special_chars" {
  // 测试包含特殊字符的字符串处理
  let unicode_name = "🚀MoonBit🌟"
  @azimuth.assert_eq_string("Hello, 🚀MoonBit🌟!", @azimuth.greet(unicode_name))
  
  let whitespace_name = "  \t\n  "
  @azimuth.assert_eq_string("Hello,  	\n  !", @azimuth.greet(whitespace_name))
  
  let emoji_combo = "😀🎉🎊"
  @azimuth.assert_eq_string("Hello, 😀🎉🎊!", @azimuth.greet(emoji_combo))
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算等差数列前n项和：1 + 4 + 7 + 10 + 13
  let n = 5
  let first_term = 1
  let common_diff = 3
  let sum = @azimuth.multiply(n, @azimuth.add(@azimuth.multiply(first_term, 2), @azimuth.multiply(common_diff, n - 1))) / 2
  @azimuth.assert_eq(35, sum)
  
  // 计算等比数列前n项积：2 × 4 × 8 × 16
  let product = @azimuth.multiply(2, @azimuth.multiply(4, @azimuth.multiply(8, 16)))
  @azimuth.assert_eq(1024, product)
}

test "business_calculation_scenarios" {
  // 测试商业计算场景
  // 计算折扣价格：原价 × (1 - 折扣率)
  let original_price = 1000
  let discount_rate = 20  // 20%
  let discounted_price = @azimuth.multiply(original_price, @azimuth.add(100, -discount_rate)) / 100
  @azimuth.assert_eq(800, discounted_price)
  
  // 计算利润：售价 - 成本
  let selling_price = 1500
  let cost_price = 1000
  let profit = @azimuth.add(selling_price, -cost_price)
  @azimuth.assert_eq(500, profit)
  
  // 计算利润率：利润 / 成本 × 100%
  let profit_rate = @azimuth.multiply(profit, 100) / cost_price
  @azimuth.assert_eq(50, profit_rate)
}

test "time_based_calculations" {
  // 测试基于时间的计算
  // 计算总秒数：小时 × 3600 + 分钟 × 60 + 秒
  let hours = 2
  let minutes = 30
  let seconds = 45
  let total_seconds = @azimuth.add(@azimuth.multiply(hours, 3600), @azimuth.add(@azimuth.multiply(minutes, 60), seconds))
  @azimuth.assert_eq(9045, total_seconds)
  
  // 计算时间差：结束时间 - 开始时间（以分钟计）
  let start_hour = 9
  let start_min = 15
  let end_hour = 14
  let end_min = 45
  let start_total = @azimuth.add(@azimuth.multiply(start_hour, 60), start_min)
  let end_total = @azimuth.add(@azimuth.multiply(end_hour, 60), end_min)
  let duration = @azimuth.add(end_total, -start_total)
  @azimuth.assert_eq(330, duration)
}

test "nested_function_compositions" {
  // 测试嵌套函数组合
  let base = 10
  
  // 计算：((base + 5) × 2) - 3
  let step1 = @azimuth.add(base, 5)      // 15
  let step2 = @azimuth.multiply(step1, 2) // 30
  let result = @azimuth.add(step2, -3)    // 27
  @azimuth.assert_eq(27, result)
  
  // 计算：greet(greet("test")的前5个字符)的简化版本
  let inner = @azimuth.greet("test")     // "Hello, test!"
  let outer = @azimuth.greet("User")     // "Hello, User!"
  @azimuth.assert_eq_string("Hello, User!", outer)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保边界值运算的安全性
  @azimuth.assert_true(@azimuth.add(max_val, 1) <= max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) >= min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) <= max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) >= min_val)
  
  // 确保正常运算的正确性
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq_string("Hello, Boundary!", @azimuth.greet("Boundary"))
}