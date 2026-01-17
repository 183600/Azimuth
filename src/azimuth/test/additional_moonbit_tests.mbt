// 额外的 MoonBit 测试用例
// 测试 azimuth 模块的高级功能

test "add_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  @azimuth.assert_eq(0, @azimuth.add(1500000000, -1500000000))
}

test "add_mixed_signs" {
  // 测试不同符号的数相加
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(-50, @azimuth.add(100, -150))
  @azimuth.assert_eq(50, @azimuth.add(-100, 150))
  @azimuth.assert_eq(-200, @azimuth.add(-100, -100))
}

test "multiply_large_numbers" {
  // 测试大数相乘
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
  @azimuth.assert_eq(0, @azimuth.multiply(1000000, 0))
}

test "multiply_boundary_combinations" {
  // 测试边界值组合的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_with_whitespace" {
  // 测试包含空白字符的字符串
  @azimuth.assert_eq_string("Hello,   World!", @azimuth.greet("  World"))
  @azimuth.assert_eq_string("Hello, World   !", @azimuth.greet("World  "))
  @azimuth.assert_eq_string("Hello,  World  !", @azimuth.greet(" World  "))
  @azimuth.assert_eq_string("Hello, 	 !", @azimuth.greet("	"))  // Tab字符
}

test "greet_with_unicode_emoji" {
  // 测试包含 Unicode emoji 的字符串
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, 👋!", @azimuth.greet("👋"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 🎉!", @azimuth.greet("🎉"))
}

test "sequential_operations" {
  // 测试连续运算
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  let result3 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result3)
}

test "extreme_value_operations" {
  // 测试极值运算的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 极值与接近极值的数相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 极值相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let a = 12
  let b = 8
  
  // 加法恒等式
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(b, @azimuth.add(0, b))
  
  // 乘法恒等式
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(b, @azimuth.multiply(1, b))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, b))
  
  // 乘法分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 5)), 
                     @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 5)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 圆的周长计算：2 × π × r (简化为 2 × 3 × r)
  let radius = 5
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(30, circumference)
  
  // 简单的贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(160000, total)  // 10000 × (1 + 5×3) = 10000 × 16 = 160000
  
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
}