// 综合 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(4, 6))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-5, -5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 测试边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试用户!", greet("测试用户"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律：a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律：a × b = b × a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a × (b + c) = a × b + a × c
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "practical_calculation_scenarios" {
  // 测试实际计算场景
  // 圆的周长计算（简化）：2 × π × r ≈ 2 × 3 × r
  let radius = 7
  let circumference = multiply(multiply(2, 3), radius)
  assert_eq(42, circumference)
  
  // 简单利息计算：本金 × 利率 × 时间
  let principal = 1000
  let rate = 5
  let time = 3
  let interest = multiply(principal, multiply(rate, time))
  assert_eq(15000, interest)
}

test "complex_nested_calculations" {
  // 测试复杂嵌套计算
  // 计算：(a + b) × (c - d) + e × f
  let a = 10
  let b = 15
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = add(a, b)  // 25
  let diff_cd = add(c, -d)  // 12
  let product_ef = multiply(e, f)  // 21
  let result = add(multiply(sum_ab, diff_cd), product_ef)  // 25 × 12 + 21 = 321
  
  assert_eq(321, result)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_true(add(max_val, 100) == max_val)
  assert_true(add(min_val, -100) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -2) == min_val)
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(100, 100)
  assert_eq_string("test", "test")
}

test "performance_and_stress_tests" {
  // 测试性能和压力场景
  // 大数计算
  let large_num1 = 1000000
  let large_num2 = 2000000
  assert_eq(3000000, add(large_num1, large_num2))
  assert_eq(2000000000000, multiply(large_num1, large_num2))
  
  // 连续运算测试
  let result = add(multiply(10, 20), add(30, 40))
  assert_eq(270, result)
  
  let nested_result = multiply(add(5, 10), multiply(2, 3))
  assert_eq(90, nested_result)
}

test "real_world_application_tests" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let shipping = 20
  
  let total1 = multiply(item1_price, item1_qty)
  let total2 = multiply(item2_price, item2_qty)
  let final_total = add(add(total1, total2), shipping)
  
  assert_eq(717, final_total)  // 199×2 + 299×1 + 20 = 398 + 299 + 20 = 717
  
  // 温度转换（简化）：华氏度 = 摄氏度 × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "unicode_and_special_character_handling" {
  // 测试 Unicode 和特殊字符处理
  // 中文测试
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, 上海!", greet("上海"))
  
  // Emoji 测试
  assert_eq_string("Hello, 🎉!", greet("🎉"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 💻!", greet("💻"))
  
  // 特殊符号测试
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, &*()!", greet("&*()"))
  
  // 混合字符测试
  assert_eq_string("Hello, Test测试123!", greet("Test测试123"))
  assert_eq_string("Hello, 用户@2023!", greet("用户@2023"))
}