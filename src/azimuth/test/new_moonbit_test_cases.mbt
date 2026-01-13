// 新的 MoonBit 测试用例
// 导入 azimuth 包中的函数
use azimuth

test "add_function_basic_arithmetic" {
  // 测试基本算术运算
  assert_eq(10, azimuth::add(3, 7))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-8, azimuth::add(-3, -5))
}

test "add_function_edge_cases" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "multiply_function_properties" {
  // 测试乘法性质
  // 交换律：a * b = b * a
  assert_eq(20, azimuth::multiply(4, 5))
  assert_eq(20, azimuth::multiply(5, 4))
  
  // 结合律测试：(a * b) * c = a * (b * c)
  let a = 2, b = 3, c = 4
  assert_eq(azimuth::multiply(azimuth::multiply(a, b), c), 
            azimuth::multiply(a, azimuth::multiply(b, c)))
}

test "multiply_function_special_cases" {
  // 测试特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 零乘法性质
  assert_eq(0, azimuth::multiply(0, 12345))
  assert_eq(0, azimuth::multiply(-6789, 0))
  
  // 单位元性质
  assert_eq(12345, azimuth::multiply(1, 12345))
  assert_eq(-6789, azimuth::multiply(-6789, 1))
  
  // 负一乘法
  assert_eq(-12345, azimuth::multiply(-1, 12345))
  assert_eq(6789, azimuth::multiply(-1, -6789))
  
  // 最小值特殊情况
  assert_eq(min_val, azimuth::multiply(min_val, -1))
}

test "greet_function_various_inputs" {
  // 测试各种输入情况
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  assert_eq_string("Hello,   spaced   !", azimuth::greet("  spaced  "))
}

test "greet_function_unicode_support" {
  // 测试Unicode支持
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  assert_eq_string("Hello, Café!", azimuth::greet("Café"))
  assert_eq_string("Hello, Ñoño!", azimuth::greet("Ñoño"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 圆的周长近似计算：2 * π * r，使用π≈3
  let radius = 5
  let circumference = azimuth::multiply(2, azimuth::multiply(3, radius))
  assert_eq(30, circumference)
  
  // 简单的利润计算：收入 - 成本
  let revenue = 1000
  let cost = 600
  let profit = azimuth::add(revenue, -cost)
  assert_eq(400, profit)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 等差数列前n项和：n * (a1 + an) / 2
  let n = 10
  let a1 = 1
  let an = 10
  let sum = azimuth::multiply(n, azimuth::add(a1, an)) / 2
  assert_eq(55, sum)  // 10 * (1 + 10) / 2 = 55
  
  // 等比数列前n项和（简化）：a1 * (1 - r^n) / (1 - r)，这里用简单值
  let a1_geo = 2
  let r = 3
  let n_geo = 4
  // 手动计算：2 + 6 + 18 + 54 = 80
  let term1 = a1_geo
  let term2 = azimuth::multiply(term1, r)
  let term3 = azimuth::multiply(term2, r)
  let term4 = azimuth::multiply(term3, r)
  let geo_sum = azimuth::add(azimuth::add(azimuth::add(term1, term2), term3), term4)
  assert_eq(80, geo_sum)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 购物折扣计算：原价 * (1 - 折扣率)
  let original_price = 200
  let discount_rate = 20  // 20%折扣
  let final_price = azimuth::multiply(original_price, azimuth::add(100, -discount_rate)) / 100
  assert_eq(160, final_price)  // 200 * 0.8 = 160
  
  // 税收计算：价格 * 税率
  let price = 500
  let tax_rate = 8  // 8%税率
  let tax = azimuth::multiply(price, tax_rate) / 100
  let total_price = azimuth::add(price, tax)
  assert_eq(540, total_price)  // 500 + 500*0.08 = 540
}

test "performance_and_stress_tests" {
  // 测试性能和压力场景
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  assert_eq(3000000, azimuth::add(large_num1, large_num2))
  assert_eq(2000000000000, azimuth::multiply(large_num1, large_num2))
  
  // 连续运算测试
  let result = azimuth::add(
    azimuth::multiply(10, 20),
    azimuth::multiply(30, 40)
  )
  assert_eq(1400, result)  // 200 + 1200 = 1400
  
  // 字符串处理性能
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  let greeting_result = azimuth::greet(long_string)
  assert_eq_string("Hello, " + long_string + "!", greeting_result)
}