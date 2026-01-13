// 新的 MoonBit 测试用例
// 测试 azimuth 包中的各种功能

test "add_overflow_edge_cases" {
  // 测试加法溢出的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::add(max_val, max_val))
  
  // 测试负数溢出
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::add(min_val, min_val))
  
  // 测试最大值与最小值相加
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "multiply_overflow_scenarios" {
  // 测试乘法溢出的各种场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘溢出
  assert_eq(max_val, azimuth::multiply(46341, 46341))
  assert_eq(min_val, azimuth::multiply(-46341, 46341))
  
  // 测试最小值乘法特殊情况
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  assert_eq(min_val, azimuth::multiply(min_val, 2))
  
  // 测试边界值组合
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(0, azimuth::multiply(min_val, 0))
}

test "greet_unicode_and_special_chars" {
  // 测试包含 Unicode 字符和特殊字符的问候
  assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
  assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  assert_eq_string("Hello, 张三!", azimuth::greet("张三"))
  assert_eq_string("Hello, ñoño!", azimuth::greet("ñoño"))
  assert_eq_string("Hello, Москва!", azimuth::greet("Москва"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  // 计算：(a + b) * (c - d)
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  
  let sum_ab = azimuth::add(a, b)  // 15
  let diff_cd = azimuth::add(c, -d)  // 12
  let result = azimuth::multiply(sum_ab, diff_cd)  // 15 * 12 = 180
  
  assert_eq(180, result)
  
  // 计算：a * b + c * d
  let product_ab = azimuth::multiply(a, b)  // 50
  let product_cd = azimuth::multiply(c, d)  // 160
  let sum_result = azimuth::add(product_ab, product_cd)  // 50 + 160 = 210
  
  assert_eq(210, sum_result)
}

test "financial_calculations" {
  // 测试金融计算场景
  // 计算复利：principal * (1 + rate * years)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  let rate_multiplier = azimuth::add(1, azimuth::multiply(rate, years))  // 1 + 5*3 = 16
  let total = azimuth::multiply(principal, rate_multiplier)  // 10000 * 16 = 160000
  
  assert_eq(160000, total)
  
  // 计算折扣价格：original_price * (1 - discount_rate)
  let original_price = 200
  let discount_rate = 20  // 20%
  let discount_multiplier = azimuth::add(1, -discount_rate)  // 1 - 20 = -19
  let discounted_price = azimuth::multiply(original_price, discount_multiplier)  // 200 * -19 = -3800
  
  assert_eq(-3800, discounted_price)
}

test "geometry_calculations" {
  // 测试几何计算
  // 计算圆的周长（近似值）：2 * π * r，使用 π ≈ 3
  let radius = 10
  let pi_approx = 3
  let circumference = azimuth::multiply(2, azimuth::multiply(pi_approx, radius))  // 2 * 3 * 10 = 60
  
  assert_eq(60, circumference)
  
  // 计算梯形面积：(上底 + 下底) * 高 / 2
  let top_base = 8
  let bottom_base = 12
  let height = 5
  
  let sum_bases = azimuth::add(top_base, bottom_base)  // 20
  let area_times_2 = azimuth::multiply(sum_bases, height)  // 20 * 5 = 100
  let area = area_times_2 / 2  // 50
  
  assert_eq(50, area)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试非常长的名字
  let very_long_name = "ThisIsAnExtremelyLongNameThatCouldPotentiallyCauseIssuesInSomeSystems"
  let result1 = azimuth::greet(very_long_name)
  assert_eq_string("Hello, " + very_long_name + "!", result1)
  
  // 测试包含各种特殊字符的名字
  let special_chars_name = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result2 = azimuth::greet(special_chars_name)
  assert_eq_string("Hello, " + special_chars_name + "!", result2)
  
  // 测试包含空格的名字
  let spaced_name = "   "
  let result3 = azimuth::greet(spaced_name)
  assert_eq_string("Hello,    !", result3)
}

test "sequence_operations" {
  // 测试序列操作
  // 计算等差数列前n项和：n/2 * (2a + (n-1)d)
  let n = 10
  let a = 1  // 首项
  let d = 2  // 公差
  
  let two_a = azimuth::multiply(2, a)  // 2
  let n_minus_1 = azimuth::add(n, -1)  // 9
  let term_in_parentheses = azimuth::add(two_a, azimuth::multiply(n_minus_1, d))  // 2 + 9*2 = 20
  let sum = azimuth::multiply(n / 2, term_in_parentheses)  // 5 * 20 = 100
  
  assert_eq(100, sum)
  
  // 计算等比数列前n项和（简化版）：a * (1 - r^n) / (1 - r)
  // 这里我们只计算前3项的和
  let first_term = 2
  let ratio = 3
  
  let term1 = first_term  // 2
  let term2 = azimuth::multiply(first_term, ratio)  // 6
  let term3 = azimuth::multiply(term2, ratio)  // 18
  let geo_sum = azimuth::add(azimuth::add(term1, term2), term3)  // 2 + 6 + 18 = 26
  
  assert_eq(26, geo_sum)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数的正确性
  azimuth::assert_true(true)
  azimuth::assert_false(false)
  azimuth::assert_eq(42, 42)
  azimuth::assert_eq_string("test", "test")
  
  // 测试边界值的运算结果
  assert_true(azimuth::add(max_val, 1) == max_val)
  assert_true(azimuth::add(min_val, -1) == min_val)
  assert_true(azimuth::multiply(max_val, 2) == max_val)
  assert_true(azimuth::multiply(min_val, -1) == min_val)
  
  // 测试正常运算
  assert_true(azimuth::add(100, 200) == 300)
  assert_true(azimuth::multiply(15, 20) == 300)
  assert_true(azimuth::greet("Test") == "Hello, Test!")
}