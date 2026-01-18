// 用户新增的 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_negative_numbers" {
  // 测试加法函数处理负数的情况
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(-3, @azimuth.add(-8, 5))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
}

test "multiply_function_negative_combinations" {
  // 测试乘法函数处理负数组合的情况
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-20, @azimuth.multiply(4, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-10, 0))
}

test "greet_function_unicode_support" {
  // 测试 greet 函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 👋!", @azimuth.greet("👋"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let base = 10
  let multiplier = 3
  let addition = 5
  
  // 计算：(base * multiplier) + addition
  let step1 = @azimuth.multiply(base, multiplier)
  let result = @azimuth.add(step1, addition)
  
  @azimuth.assert_eq(35, result)
}

test "boundary_value_arithmetic" {
  // 测试边界值算术运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值与普通值的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
  
  // 测试边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", @azimuth.greet("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  @azimuth.assert_eq_string("Hello, abcdefghijklmnopqrstuvwxyz!", @azimuth.greet("abcdefghijklmnopqrstuvwxyz"))
  @azimuth.assert_eq_string("Hello, 0123456789!", @azimuth.greet("0123456789"))
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  let x = 42
  
  // 加法恒等：x + 0 = x
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等：x * 1 = x
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  
  // 乘法零性质：x * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, x))
}

test "practical_business_calculation" {
  // 测试实际业务计算：折扣计算
  let original_price = 1000
  let discount_percentage = 20
  let tax_rate = 8
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  // 验证结果：(1000 - 200) + 800 * 0.08 = 800 + 64 = 864
  @azimuth.assert_eq(864, final_price)
}

test "scientific_calculation_simulation" {
  // 测试科学计算模拟：简单物理公式
  let mass = 10
  let acceleration = 5
  let time = 3
  
  // 计算力：F = m * a
  let force = @azimuth.multiply(mass, acceleration)
  
  // 计算距离：s = 0.5 * a * t^2 (简化为整数计算)
  let time_squared = @azimuth.multiply(time, time)
  let distance = @azimuth.multiply(acceleration, time_squared) / 2
  
  // 验证结果
  @azimuth.assert_eq(50, force)  // 10 * 5 = 50
  @azimuth.assert_eq(22, distance)  // 5 * 9 / 2 = 22.5，整数除法为 22
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试边界值比较
  @azimuth.assert_true(max_val > 0)
  @azimuth.assert_true(min_val < 0)
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)  // 溢出保护
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)  // 溢出保护
}