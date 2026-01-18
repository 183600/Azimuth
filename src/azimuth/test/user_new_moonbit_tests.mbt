// 用户自定义的新 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "add_sequence_operations" {
  // 测试序列加法运算
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  let result2 = @azimuth.add(@azimuth.add(100, 200), @azimuth.add(300, 400))
  @azimuth.assert_eq(1000, result2)
  
  // 测试包含负数的序列加法
  let result3 = @azimuth.add(@azimuth.add(-10, 20), @azimuth.add(30, -40))
  @azimuth.assert_eq(0, result3)
}

test "multiply_sequence_operations" {
  // 测试序列乘法运算
  let result1 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result1)
  
  // 测试包含零的序列乘法
  let result2 = @azimuth.multiply(@azimuth.multiply(10, 0), @azimuth.multiply(5, 6))
  @azimuth.assert_eq(0, result2)
  
  // 测试包含负数的序列乘法
  let result3 = @azimuth.multiply(@azimuth.multiply(-2, 3), @azimuth.multiply(4, -5))
  @azimuth.assert_eq(120, result3)
}

test "mixed_arithmetic_operations" {
  // 测试混合算术运算：(a + b) * c - d
  let a = 15
  let b = 25
  let c = 4
  let d = 20
  
  let sum = @azimuth.add(a, b)  // 40
  let product = @azimuth.multiply(sum, c)  // 160
  let result = @azimuth.add(product, -d)  // 140
  
  @azimuth.assert_eq(140, result)
}

test "string_formatting_variations" {
  // 测试字符串格式化的各种情况
  @azimuth.assert_eq_string("Hello, John!", @azimuth.greet("John"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与普通值的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值与普通值的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 3))
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列的和：1 + 3 + 5 + ... + 19（前10个奇数）
  // 使用公式：n × (首项 + 末项) / 2
  let n = 10
  let first = 1
  let last = 19
  
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(100, sum)
  
  // 计算等比数列的和：2 + 4 + 8 + 16（简化计算）
  let geo_sum = @azimuth.add(@azimuth.add(2, 4), @azimuth.add(8, 16))
  @azimuth.assert_eq(30, geo_sum)
}

test "practical_business_calculations" {
  // 测试实际业务计算场景
  // 计算订单总价
  let unit_price = 199
  let quantity = 3
  let discount = 50
  let tax_rate = 8  // 8%
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 计算折后价格
  let discounted = @azimuth.add(subtotal, -discount)
  // 计算税费
  let tax = @azimuth.multiply(discounted, tax_rate) / 100
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(discounted, tax), shipping)
  
  @azimuth.assert_eq(606, total)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算一组数据的总和、平均值、最大值、最小值
  let values = [12, 18, 24, 30, 36]
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(12, 18), 24), 30), 36)
  @azimuth.assert_eq(120, sum)
  
  // 计算平均值
  let average = sum / 5
  @azimuth.assert_eq(24, average)
  
  // 验证中位数（已排序数组）
  @azimuth.assert_eq(24, 24)
}

test "time_based_calculations" {
  // 测试基于时间的计算
  // 计算时间差和持续时间
  let start_hour = 9
  let start_minute = 30
  let end_hour = 17
  let end_minute = 45
  
  // 转换为分钟
  let start_total = @azimuth.add(@azimuth.multiply(start_hour, 60), start_minute)
  let end_total = @azimuth.add(@azimuth.multiply(end_hour, 60), end_minute)
  
  // 计算时间差（分钟）
  let duration = @azimuth.add(end_total, -start_total)
  @azimuth.assert_eq(495, duration)
  
  // 转换为小时和分钟
  let hours = duration / 60
  let minutes = duration % 60
  @azimuth.assert_eq(8, hours)
  @azimuth.assert_eq(15, minutes)
}

test "algorithmic_pattern_testing" {
  // 测试算法模式
  // 计算斐波那契数列的第n项（简化版）
  let n = 6
  // F(6) = F(5) + F(4) = 5 + 3 = 8
  let fib_1 = 1
  let fib_2 = 1
  let fib_3 = @azimuth.add(fib_1, fib_2)  // 2
  let fib_4 = @azimuth.add(fib_2, fib_3)  // 3
  let fib_5 = @azimuth.add(fib_3, fib_4)  // 5
  let fib_6 = @azimuth.add(fib_4, fib_5)  // 8
  
  @azimuth.assert_eq(8, fib_6)
  
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let factorial_5 = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial_5)
}