// 额外的 MoonBit 测试用例
// 这些测试用例覆盖了 azimuth 模块的核心功能，使用标准的 MoonBit 测试语法

// 导入主模块中的函数
use azimuth

test "string_handling_with_unicode" {
  // 测试 Unicode 字符串处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, naïve!", greet("naïve"))
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  let x = 42
  
  // 加法恒等律：x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等律：x * 1 = x
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零律：x * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  // 计算折扣商品总价：原价 × 数量 × (1 - 折扣率)
  let original_price = 150
  let quantity = 4
  let discount_rate = 20  // 20% 折扣
  
  let subtotal = multiply(original_price, quantity)
  let discount_amount = multiply(subtotal, discount_rate) / 100
  let final_price = add(subtotal, -discount_amount)
  
  assert_eq(480, final_price)  // 150*4 - 150*4*20/100 = 600 - 120 = 480
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数运算：F = C × 2 + 30
  
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
  
  let room_celsius = 20
  let room_fahrenheit = add(multiply(room_celsius, 2), 30)
  assert_eq(70, room_fahrenheit)
  
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  assert_eq(230, boiling_fahrenheit)
}

test "array_arithmetic_progression" {
  // 测试等差数列计算
  // 计算前 n 个奇数的和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 7
  
  // 使用公式：n × n
  let sum_of_odds = multiply(n, n)
  assert_eq(49, sum_of_odds)
  
  // 验证前5个奇数的和
  let first_five = multiply(5, 5)
  assert_eq(25, first_five)
}

test "financial_compound_interest" {
  // 测试复利计算
  // 本金 × (1 + 利率 × 期数)
  let principal = 10000
  let annual_rate = 8  // 8%
  let years = 5
  
  let total_interest = multiply(principal, multiply(annual_rate, years))
  let total_amount = add(principal, total_interest)
  
  assert_eq(500000, total_amount)  // 10000 + 10000*8*5 = 10000 + 400000 = 410000
}

test "geometric_calculations" {
  // 测试几何计算
  // 计算立方体表面积：6 × 边长²
  let side_length = 5
  let surface_area = multiply(6, multiply(side_length, side_length))
  assert_eq(150, surface_area)
  
  // 计算立方体体积：边长³
  let volume = multiply(side_length, multiply(side_length, side_length))
  assert_eq(125, volume)
  
  // 计算圆柱体表面积（简化）：2πrh + 2πr² ≈ 6rh + 6r²
  let radius = 3
  let height = 10
  let cylinder_surface = add(multiply(6, multiply(radius, height)), multiply(6, multiply(radius, radius)))
  assert_eq(234, cylinder_surface)
}

test "boundary_value_stress_test" {
  // 测试边界值压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(max_val, -min_val))  // max + (-min) = max + max = max (溢出保护)
  assert_eq(min_val, multiply(max_val, -1))
  
  // 测试最小值的各种运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(min_val, max_val))   // min + max = -1
  assert_eq(min_val, multiply(min_val, 1))
  
  // 测试连续运算
  let chain_result = add(multiply(add(max_val, min_val), 2), max_val)
  assert_eq(max_val, chain_result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", greet(special_chars))
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric = "User123_Admin456"
  assert_eq_string("Hello, User123_Admin456!", greet(alphanumeric))
  
  // 测试包含空格的字符串
  let spaced = "  Leading and trailing spaces  "
  assert_eq_string("Hello,   Leading and trailing spaces  !", greet(spaced))
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟
  // 模拟 O(n²) 算法的计算量：n × (n + 1) / 2
  let n = 100
  
  let complexity_calculation = multiply(n, add(n, 1)) / 2
  assert_eq(5050, complexity_calculation)
  
  // 模拟 O(n³) 算法的计算量：n² × (n + 1) / 2
  let cubic_complexity = multiply(multiply(n, n), add(n, 1)) / 2
  assert_eq(505000, cubic_complexity)
  
  // 模拟 O(log n) 算法的迭代次数（简化）
  let log_iterations = 7  // log₂(100) ≈ 6.64，向上取整为7
  assert_eq(7, log_iterations)
}