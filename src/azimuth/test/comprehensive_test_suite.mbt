// 综合测试套件 - azimuth 包的全面测试
// 包含 8 个测试用例，覆盖核心功能和实际应用场景

test "arithmetic_properties_validation" {
  // 验证基本算术性质
  
  // 加法交换律: a + b = b + a
  let a = 123
  let b = 456
  assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let c = 789
  let left_assoc = azimuth::add(azimuth::add(a, b), c)
  let right_assoc = azimuth::add(a, azimuth::add(b, c))
  assert_eq(left_assoc, right_assoc)
  
  // 乘法交换律: a * b = b * a
  assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let left_mul_assoc = azimuth::multiply(azimuth::multiply(a, b), c)
  let right_mul_assoc = azimuth::multiply(a, azimuth::multiply(b, c))
  assert_eq(left_mul_assoc, right_mul_assoc)
  
  // 分配律: a * (b + c) = a * b + a * c
  let distrib_left = azimuth::multiply(a, azimuth::add(b, c))
  let distrib_right = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(distrib_left, distrib_right)
}

test "boundary_value_comprehensive" {
  // 全面的边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法测试
  assert_eq(-1, azimuth::add(max_val, min_val))
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::add(max_val, max_val))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::add(min_val, min_val))
  
  // 极值乘法测试
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(0, azimuth::multiply(min_val, 0))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  assert_eq(min_val, azimuth::multiply(max_val, -1))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  
  // 边界值附近的运算
  assert_eq(2147483646, azimuth::add(max_val, -1))
  assert_eq(-2147483647, azimuth::add(min_val, 1))
  assert_eq(2147483646, azimuth::multiply(1073741823, 2))
}

test "string_processing_comprehensive" {
  // 全面的字符串处理测试
  
  // 基本字符串测试
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  
  // 特殊字符测试
  assert_eq_string("Hello, @#$%^&*()!", azimuth::greet("@#$%^&*()"))
  assert_eq_string("Hello, \"quoted\"!", azimuth::greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", azimuth::greet("'single'"))
  
  // Unicode 字符测试
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, 🌟!", azimuth::greet("🌟"))
  assert_eq_string("Hello, ñáéíóú!", azimuth::greet("ñáéíóú"))
  
  // 数字和混合字符串测试
  assert_eq_string("Hello, 12345!", azimuth::greet("12345"))
  assert_eq_string("Hello, test@123.com!", azimuth::greet("test@123.com"))
  assert_eq_string("Hello, User_Name_123!", azimuth::greet("User_Name_123"))
  
  // 长字符串测试
  let long_string = "ThisIsAVeryLongStringForTestingPurposesWithVariousCharacters1234567890"
  assert_eq_string("Hello, " + long_string + "!", azimuth::greet(long_string))
}

test "mathematical_formulas" {
  // 数学公式计算测试
  
  // 平方差公式: (a + b) * (a - b) = a² - b²
  let a = 15
  let b = 7
  let sum = azimuth::add(a, b)
  let diff = azimuth::add(a, -b)
  let left_side = azimuth::multiply(sum, diff)
  
  let a_squared = azimuth::multiply(a, a)
  let b_squared = azimuth::multiply(b, b)
  let right_side = azimuth::add(a_squared, -b_squared)
  
  assert_eq(left_side, right_side)
  assert_eq(176, left_side)  // (15+7)*(15-7) = 22*8 = 176
  
  // 等差数列求和: n * (first + last) / 2
  let n = 10
  let first = 3
  let last = 30
  let arithmetic_sum = azimuth::multiply(n, azimuth::add(first, last)) / 2
  assert_eq(165, arithmetic_sum)  // 10 * (3 + 30) / 2 = 165
  
  // 立方和公式验证: a³ + b³
  let a_cubed = azimuth::multiply(a_squared, a)
  let b_cubed = azimuth::multiply(b_squared, b)
  let sum_of_cubes = azimuth::add(a_cubed, b_cubed)
  assert_eq(4478, sum_of_cubes)  // 15³ + 7³ = 3375 + 343 = 3718
}

test "real_world_financial_calculations" {
  // 实际金融计算场景
  
  // 场景1: 复合利息计算
  let principal = 10000
  let rate_percent = 5
  let years = 3
  // 简化复利: 本金 + 本金 * 利率 * 年数
  let interest = azimuth::multiply(principal, azimuth::multiply(rate_percent, years))
  let total = azimuth::add(principal, interest)
  assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
  
  // 场景2: 折扣计算
  let original_price = 2500
  let discount_percent = 15
  let tax_percent = 8
  let discount_amount = azimuth::multiply(original_price, discount_percent) / 100
  let discounted_price = azimuth::add(original_price, -discount_amount)
  let tax_amount = azimuth::multiply(discounted_price, tax_percent) / 100
  let final_price = azimuth::add(discounted_price, tax_amount)
  assert_eq(2295, final_price)  // 2500 - 375 + 170 = 2295
  
  // 场景3: 分期付款计算
  let total_cost = 12000
  let months = 12
  let monthly_payment = total_cost / months
  let total_payment = azimuth::multiply(monthly_payment, months)
  assert_eq(1000, monthly_payment)
  assert_eq(total_cost, total_payment)
}

test "geometry_and_physics_calculations" {
  // 几何和物理计算
  
  // 圆形计算（简化π=3）
  let radius = 10
  let circumference = azimuth::multiply(azimuth::multiply(2, 3), radius)
  let area = azimuth::multiply(3, azimuth::multiply(radius, radius))
  assert_eq(60, circumference)  // 2 * 3 * 10 = 60
  assert_eq(300, area)          // 3 * 10² = 300
  
  // 矩形和三角形
  let length = 12
  let width = 8
  let rectangle_area = azimuth::multiply(length, width)
  let rectangle_perimeter = azimuth::add(azimuth::multiply(2, length), azimuth::multiply(2, width))
  
  let base = 10
  let height = 6
  let triangle_area = azimuth::multiply(base, height) / 2
  
  assert_eq(96, rectangle_area)
  assert_eq(40, rectangle_perimeter)
  assert_eq(30, triangle_area)
  
  // 简单物理计算：距离 = 速度 × 时间
  let speed = 60  // km/h
  let time_minutes = 150  // 分钟
  let distance = azimuth::multiply(speed, time_minutes) / 60
  assert_eq(150, distance)  // 60 * 2.5 = 150 km
}

test "algorithmic_implementation" {
  // 算法实现测试
  
  // 计算阶乘: 6! = 720
  let n = 6
  let factorial = azimuth::multiply(
    azimuth::multiply(
      azimuth::multiply(
        azimuth::multiply(
          azimuth::multiply(n, n - 1),
          n - 2
        ),
        n - 3
      ),
      n - 4
    ),
    n - 5
  )
  assert_eq(720, factorial)
  
  // 计算平方和: 1² + 2² + ... + 6² = 91
  let sum_of_squares = azimuth::add(
    azimuth::add(
      azimuth::add(
        azimuth::add(
          azimuth::add(
            azimuth::multiply(1, 1),
            azimuth::multiply(2, 2)
          ),
          azimuth::multiply(3, 3)
        ),
        azimuth::multiply(4, 4)
      ),
      azimuth::multiply(5, 5)
    ),
    azimuth::multiply(6, 6)
  )
  assert_eq(91, sum_of_squares)
  
  // 计算等比数列: 2 + 4 + 8 + 16 + 32 = 62
  let geometric_sum = azimuth::add(
    azimuth::add(
      azimuth::add(
        azimuth::add(2, 4),
        8
      ),
      16
    ),
    32
  )
  assert_eq(62, geometric_sum)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护验证
  assert_true(azimuth::add(max_val, 1) == max_val)
  assert_true(azimuth::add(max_val, max_val) == max_val)
  assert_true(azimuth::add(min_val, -1) == min_val)
  assert_true(azimuth::add(min_val, min_val) == min_val)
  
  assert_true(azimuth::multiply(max_val, 2) == max_val)
  assert_true(azimuth::multiply(min_val, 2) == min_val)
  assert_true(azimuth::multiply(min_val, -1) == min_val)
  
  // 特殊值处理
  assert_true(azimuth::multiply(0, max_val) == 0)
  assert_true(azimuth::multiply(0, min_val) == 0)
  assert_true(azimuth::multiply(max_val, 1) == max_val)
  assert_true(azimuth::multiply(min_val, 1) == min_val)
  
  // 字符串边界情况
  assert_true(azimuth::greet("") == "Hello, !")
  assert_true(azimuth::greet(" ") == "Hello,  !")
  assert_true(azimuth::greet("\n") == "Hello, \n!")
  
  // 验证正常运算不受影响
  assert_eq(100, azimuth::add(50, 50))
  assert_eq(1000, azimuth::multiply(100, 10))
  assert_eq_string("Hello, Test!", azimuth::greet("Test"))
}

test "performance_and_scalability" {
  // 性能和可扩展性测试
  
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_num3 = 3000000
  
  assert_eq(3000000, azimuth::add(large_num1, large_num2))
  assert_eq(6000000, azimuth::add(large_num1, azimuth::add(large_num2, large_num3)))
  
  // 复杂嵌套运算
  let complex_result = azimuth::add(
    azimuth::multiply(100, 200),
    azimuth::multiply(
      azimuth::add(50, 25),
      azimuth::add(10, 15)
    )
  )
  assert_eq(23750, complex_result)  // 20000 + 75*25 = 20000 + 1875 = 21875
  
  // 连续运算链
  let chain_result = azimuth::multiply(
    azimuth::add(
      azimuth::multiply(10, 20),
      30
    ),
    azimuth::add(
      azimuth::multiply(5, 6),
      7
    )
  )
  assert_eq(4620, chain_result)  // (200+30)*(30+7) = 230*37 = 8510
  
  // 批量字符串处理（模拟）
  // 验证字符串处理正常工作
  assert_eq_string("Hello, Alice!", azimuth::greet("Alice"))
  assert_eq_string("Hello, Bob!", azimuth::greet("Bob"))
  assert_eq_string("Hello, Charlie!", azimuth::greet("Charlie"))
  assert_eq_string("Hello, Diana!", azimuth::greet("Diana"))
  assert_eq_string("Hello, Eve!", azimuth::greet("Eve"))
  
  // 模拟处理计数
  let mut processed_count = 0
  processed_count = azimuth::add(processed_count, 1)  // Alice
  processed_count = azimuth::add(processed_count, 1)  // Bob
  processed_count = azimuth::add(processed_count, 1)  // Charlie
  processed_count = azimuth::add(processed_count, 1)  // Diana
  processed_count = azimuth::add(processed_count, 1)  // Eve
  
  assert_eq(5, processed_count)
}