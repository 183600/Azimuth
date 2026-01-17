// 用户添加的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法

test "add_boundary_conditions" {
  // 测试加法边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(azimuth::add(max_val, 0), max_val)
  assert_eq(azimuth::add(max_val, -1), 2147483646)
  assert_eq(azimuth::add(max_val, 1), max_val)  // 溢出保护
  
  // 测试最小值边界
  assert_eq(azimuth::add(min_val, 0), min_val)
  assert_eq(azimuth::add(min_val, 1), -2147483647)
  assert_eq(azimuth::add(min_val, -1), min_val)  // 溢出保护
}

test "multiply_special_cases" {
  // 测试乘法特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0和1的乘法
  assert_eq(azimuth::multiply(12345, 0), 0)
  assert_eq(azimuth::multiply(0, 12345), 0)
  assert_eq(azimuth::multiply(12345, 1), 12345)
  assert_eq(azimuth::multiply(1, 12345), 12345)
  
  // 测试与-1的乘法
  assert_eq(azimuth::multiply(12345, -1), -12345)
  assert_eq(azimuth::multiply(-12345, -1), 12345)
  
  // 测试最小值与-1的特殊情况
  assert_eq(azimuth::multiply(min_val, -1), min_val)
}

test "greet_unicode_and_special_chars" {
  // 测试包含Unicode和特殊字符的问候
  assert_eq_string(azimuth::greet("🌍"), "Hello, 🌍!")
  assert_eq_string(azimuth::greet("测试"), "Hello, 测试!")
  assert_eq_string(azimuth::greet("emoji🚀"), "Hello, emoji🚀!")
  assert_eq_string(azimuth::greet("@user#123"), "Hello, @user#123!")
  assert_eq_string(azimuth::greet("Tab\tSeparated"), "Hello, Tab\tSeparated!")
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 圆面积计算：π × r² (简化为 3 × r²)
  let radius = 10
  let area = azimuth::multiply(3, azimuth::multiply(radius, radius))
  assert_eq(area, 300)
  
  // 温度转换：摄氏度转华氏度 (简化为 C × 2 + 30)
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(fahrenheit, 80)
  
  // 简单利息计算：本金 × 利率 × 时间
  let principal = 1000
  let rate = 5
  let time = 3
  let interest = azimuth::multiply(principal, azimuth::multiply(rate, time))
  assert_eq(interest, 15000)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 15
  let b = 25
  let c = 10
  
  // 加法交换律
  assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 分配律
  let left = azimuth::multiply(a, azimuth::add(b, c))
  let right = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(left, right)
  
  // 结合律
  let assoc1 = azimuth::add(azimuth::add(a, b), c)
  let assoc2 = azimuth::add(a, azimuth::add(b, c))
  assert_eq(assoc1, assoc2)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(azimuth::add(max_val, max_val), max_val)
  assert_eq(azimuth::add(min_val, min_val), min_val)
  assert_eq(azimuth::multiply(max_val, 2), max_val)
  assert_eq(azimuth::multiply(min_val, 2), min_val)
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "real_world_applications" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  
  let total1 = azimuth::multiply(item1_price, item1_qty)
  let total2 = azimuth::multiply(item2_price, item2_qty)
  let cart_total = azimuth::add(total1, total2)
  assert_eq(cart_total, 697)
  
  // 平均分计算
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let sum_scores = azimuth::add(azimuth::add(score1, score2), score3)
  let average = sum_scores / 3
  assert_eq(average, 90)
}

test "performance_and_large_numbers" {
  // 测试性能和大数处理
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  assert_eq(azimuth::add(large_num1, large_num2), 3000000)
  assert_eq(azimuth::multiply(large_num1, 100), 100000000)
  
  // 连续运算测试
  let result = azimuth::add(
    azimuth::multiply(100, 100),
    azimuth::multiply(200, 200)
  )
  assert_eq(result, 50000)
  
  // 嵌套运算测试
  let nested = azimuth::multiply(
    azimuth::add(10, 20),
    azimuth::add(30, 40)
  )
  assert_eq(nested, 2100)
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  // 空字符串和单字符
  assert_eq_string(azimuth::greet(""), "Hello, !")
  assert_eq_string(azimuth::greet("A"), "Hello, A!")
  
  // 长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  assert_eq_string(azimuth::greet(long_name), "Hello, " + long_name + "!")
  
  // 包含空格的字符串
  assert_eq_string(azimuth::greet("John Doe"), "Hello, John Doe!")
  assert_eq_string(azimuth::greet("  LeadingSpace"), "Hello,   LeadingSpace!")
  assert_eq_string(azimuth::greet("TrailingSpace  "), "Hello, TrailingSpace  !")
  
  // 混合字符类型
  assert_eq_string(azimuth::greet("User123_测试"), "Hello, User123_测试!")
}

test "comprehensive_integration_test" {
  // 综合集成测试
  let base_value = 100
  let multiplier = 5
  let increment = 25
  
  // 复杂计算：base_value × multiplier + increment
  let step1 = azimuth::multiply(base_value, multiplier)
  let result = azimuth::add(step1, increment)
  assert_eq(result, 525)
  
  // 字符串处理与数值计算结合
  let user_name = "Alice"
  let user_id = 12345
  let greeting = azimuth::greet(user_name)
  
  // 验证问候语格式
  assert_eq_string(greeting, "Hello, Alice!")
  
  // 模拟用户ID验证（简化）
  let id_valid = azimuth::multiply(user_id, 1) == user_id
  assert_true(id_valid)
}