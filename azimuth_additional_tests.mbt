// Azimuth 项目附加测试用例
// 使用标准 MoonBit 测试语法，补充现有测试覆盖的场景

test "boundary_values_zero_operations" {
  // 零值边界测试
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::multiply(0, 0), 0)
  assert_eq(azimuth::add(0, 42), 42)
  assert_eq(azimuth::multiply(0, 42), 0)
  assert_eq(azimuth::add(42, 0), 42)
  assert_eq(azimuth::multiply(42, 0), 0)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(azimuth::add(-10, -20), -30)
  assert_eq(azimuth::add(-15, 25), 10)
  assert_eq(azimuth::add(30, -45), -15)
  assert_eq(azimuth::multiply(-5, -5), 25)
  assert_eq(azimuth::multiply(-8, 3), -24)
  assert_eq(azimuth::multiply(7, -9), -63)
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(azimuth::add(1000000, 2000000), 3000000)
  assert_eq(azimuth::add(999999999, 1), 1000000000)
  assert_eq(azimuth::multiply(10000, 10000), 100000000)
  assert_eq(azimuth::multiply(50000, 2000), 100000000)
}

test "special_character_strings" {
  // 特殊字符字符串测试
  assert_eq(azimuth::greet("\n"), "Hello, \n!")
  assert_eq(azimuth::greet("\""), "Hello, \"!")
  assert_eq(azimuth::greet("'") , "Hello, '!")
  assert_eq(azimuth::greet("\\") , "Hello, \\\!")
  assert_eq(azimuth::greet("@#$%"), "Hello, @#$%!")
}

test "mathematical_formulas" {
  // 数学公式测试
  
  // 圆的面积: π * r² (使用 π ≈ 3.14，整数计算)
  let radius = 10
  let circle_area = azimuth::multiply(314, azimuth::multiply(radius, radius)) / 100
  assert_eq(314, circle_area)
  
  // 勾股定理: a² + b² = c²
  let a = 3
  let b = 4
  let c_squared = azimuth::add(azimuth::multiply(a, a), azimuth::multiply(b, b))
  assert_eq(25, c_squared)
  
  // 等差数列第n项: a + (n-1)*d
  let first_term = 2
  let difference = 3
  let n = 5
  let nth_term = azimuth::add(first_term, azimuth::multiply(azimuth::add(n, -1), difference))
  assert_eq(14, nth_term)
}

test "real_world_scenarios" {
  // 实际应用场景测试
  
  // 购物车总价计算
  let item1_price = 299
  let item2_price = 199
  let item3_price = 99
  let discount = 50
  let subtotal = azimuth::add(azimuth::add(item1_price, item2_price), item3_price)
  let total = azimuth::add(subtotal, -discount)
  assert_eq(547, total)
  
  // 平均分计算
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 96
  let sum_scores = azimuth::add(azimuth::add(azimuth::add(score1, score2), score3), score4)
  let average = sum_scores / 4
  assert_eq(87, average)
  
  // 速度计算: 距离 / 时间
  let distance = 300
  let time = 5
  let speed = distance / time
  assert_eq(60, speed)
}

test "mixed_operations" {
  // 混合运算测试
  
  // 复杂表达式: (a + b) * (c - d) + e
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 15
  
  let result = azimuth::add(
    azimuth::multiply(
      azimuth::add(a, b),
      azimuth::add(c, -d)
    ),
    e
  )
  assert_eq(175, result)
  
  // 嵌套运算: a * (b + (c * d))
  let result2 = azimuth::multiply(
    a,
    azimuth::add(
      b,
      azimuth::multiply(c, d)
    )
  )
  assert_eq(410, result2)
}

test "pattern_based_calculations" {
  // 模式化计算测试
  
  // 阶乘计算简化版: 5! = 5 * 4 * 3 * 2 * 1
  let fact_5 = azimuth::multiply(
    azimuth::multiply(
      azimuth::multiply(
        azimuth::multiply(5, 4),
        3
      ),
      2
    ),
    1
  )
  assert_eq(120, fact_5)
  
  // 幂运算简化版: 2^4 = 2 * 2 * 2 * 2
  let power_2_4 = azimuth::multiply(
    azimuth::multiply(
      azimuth::multiply(2, 2),
      2
    ),
    2
  )
  assert_eq(16, power_2_4)
  
  // 组合数 C(n,k) 简化计算: C(5,2) = 5! / (2! * 3!)
  let n_fact = 120  // 5!
  let k_fact = 2    // 2!
  let n_k_fact = 6  // 3!
  let combination = n_fact / azimuth::multiply(k_fact, n_k_fact)
  assert_eq(10, combination)
}

test "internationalization_tests" {
  // 国际化测试
  assert_eq(azimuth::greet("世界"), "Hello, 世界!")
  assert_eq(azimuth::greet("Мир"), "Hello, Мир!")
  assert_eq(azimuth::greet("العالم"), "Hello, العالم!")
  assert_eq(azimuth::greet("🌍"), "Hello, 🌍!")
  assert_eq(azimuth::greet("Düsseldorf"), "Hello, Düsseldorf!")
  assert_eq(azimuth::greet("São Paulo"), "Hello, São Paulo!")
}

test "edge_case_string_lengths" {
  // 边界字符串长度测试
  let single_char = "A"
  assert_eq(azimuth::greet(single_char), "Hello, A!")
  
  let long_name = "VeryLongNameThatExceedsNormalExpectations"
  assert_eq(azimuth::greet(long_name), "Hello, VeryLongNameThatExceedsNormalExpectations!")
  
  // 混合字符类型
  let mixed = "Test123测试!@#"
  assert_eq(azimuth::greet(mixed), "Hello, Test123测试!@#!")
}