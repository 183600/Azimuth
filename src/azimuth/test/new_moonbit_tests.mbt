// 新增的 10 个 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试 azimuth 包的核心功能

test "sequential_operations" {
  // 测试连续运算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) + c
  let sum1 = azimuth::add(azimuth::add(a, b), c)
  // a + (b + c)
  let sum2 = azimuth::add(a, azimuth::add(b, c))
  
  // 验证加法结合律
  assert_eq(sum1, sum2)
  assert_eq(60, sum1)
  
  // (a * b) * c
  let product1 = azimuth::multiply(azimuth::multiply(a, b), c)
  // a * (b * c)
  let product2 = azimuth::multiply(a, azimuth::multiply(b, c))
  
  // 验证乘法结合律
  assert_eq(product1, product2)
  assert_eq(6000, product1)
}

test "mixed_operations" {
  // 测试混合运算
  let x = 15
  let y = 4
  let z = 6
  
  // (x + y) * z
  let result1 = azimuth::multiply(azimuth::add(x, y), z)
  // x * z + y * z
  let result2 = azimuth::add(azimuth::multiply(x, z), azimuth::multiply(y, z))
  
  // 验证分配律
  assert_eq(result1, result2)
  assert_eq(114, result1)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(max_val, -1))
  
  // 最小值运算
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  
  // 极值组合
  assert_eq(-1, azimuth::add(max_val, min_val))
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(0, azimuth::multiply(min_val, 0))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  
  // 空字符串
  assert_eq_string("Hello, !", azimuth::greet(""))
  
  // 单字符
  assert_eq_string("Hello, A!", azimuth::greet("A"))
  
  // 长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  assert_eq_string("Hello, ThisIsAVeryLongNameForTestingPurposes!", azimuth::greet(long_name))
  
  // 包含空格的字符串
  assert_eq_string("Hello, John Doe!", azimuth::greet("John Doe"))
  
  // 特殊字符和符号
  assert_eq_string("Hello, test@example.com!", azimuth::greet("test@example.com"))
  assert_eq_string("Hello, User_123!", azimuth::greet("User_123"))
  
  // Unicode字符
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
}

test "mathematical_formulas" {
  // 测试数学公式计算
  
  // 平方差公式: (a + b) * (a - b) = a² - b²
  let a = 10
  let b = 3
  
  let sum = azimuth::add(a, b)
  let diff = azimuth::add(a, -b)
  let left_side = azimuth::multiply(sum, diff)
  
  let a_squared = azimuth::multiply(a, a)
  let b_squared = azimuth::multiply(b, b)
  let right_side = azimuth::add(a_squared, -b_squared)
  
  assert_eq(left_side, right_side)
  assert_eq(91, left_side)
  
  // 立方和公式: a³ + b³ = (a + b) * (a² - a*b + b²)
  let a_cubed = azimuth::multiply(a_squared, a)
  let b_cubed = azimuth::multiply(b_squared, b)
  let sum_of_cubes = azimuth::add(a_cubed, b_cubed)
  
  let a_times_b = azimuth::multiply(a, b)
  let second_factor = azimuth::add(azimuth::add(a_squared, -a_times_b), b_squared)
  let formula_result = azimuth::multiply(sum, second_factor)
  
  assert_eq(sum_of_cubes, formula_result)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  
  // 场景1: 计算折扣价格
  let original_price = 1000
  let discount_rate = 20  // 20%折扣
  let tax_rate = 8        // 8%税率
  
  let discount_amount = azimuth::multiply(original_price, discount_rate) / 100
  let discounted_price = azimuth::add(original_price, -discount_amount)
  let tax_amount = azimuth::multiply(discounted_price, tax_rate) / 100
  let final_price = azimuth::add(discounted_price, tax_amount)
  
  assert_eq(864, final_price)  // 1000 - 200 + 64 = 864
  
  // 场景2: 计算平均分
  let math_score = 85
  let english_score = 78
  let science_score = 92
  let history_score = 88
  
  let total_score = azimuth::add(azimuth::add(azimuth::add(math_score, english_score), science_score), history_score)
  let average_score = total_score / 4
  
  assert_eq(85, average_score)
  
  // 场景3: 计算矩形周长和面积
  let length = 12
  let width = 8
  
  let perimeter = azimuth::add(azimuth::multiply(2, length), azimuth::multiply(2, width))
  let area = azimuth::multiply(length, width)
  
  assert_eq(40, perimeter)
  assert_eq(96, area)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  
  // 验证溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::add(max_val, max_val))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::add(min_val, min_val))
  
  // 乘法溢出保护
  assert_eq(max_val, azimuth::multiply(max_val, 2))
  assert_eq(min_val, azimuth::multiply(min_val, 2))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  
  // 特殊值处理
  assert_eq(0, azimuth::multiply(0, max_val))
  assert_eq(0, azimuth::multiply(0, min_val))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
}

test "performance_related_operations" {
  // 测试性能相关的运算
  
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  assert_eq(3000000, azimuth::add(large_num1, large_num2))
  assert_eq(2000000000000, azimuth::multiply(large_num1, large_num2))
  
  // 连续运算
  let base = 100
  let result = azimuth::multiply(azimuth::add(azimuth::multiply(base, 2), 50), 3)
  assert_eq(750, result)  // (100*2 + 50)*3 = 250*3 = 750
  
  // 嵌套运算
  let nested_result = azimuth::add(
    azimuth::multiply(10, 20),
    azimuth::multiply(azimuth::add(5, 3), 15)
  )
  assert_eq(320, nested_result)  // 200 + (8*15) = 200 + 120 = 320
}

test "conditional_logic_scenarios" {
  // 测试条件逻辑场景
  
  // 基于比较结果的条件运算
  let x = 25
  let y = 30
  let z = 15
  
  // 如果 x < y，则计算 x + z，否则计算 x * z
  let condition1 = x < y
  let result1 = if condition1 { azimuth::add(x, z) } else { azimuth::multiply(x, z) }
  assert_eq(40, result1)  // 25 < 30 为真，所以 25 + 15 = 40
  
  // 如果 y > z，则计算 y * z，否则计算 y + z
  let condition2 = y > z
  let result2 = if condition2 { azimuth::multiply(y, z) } else { azimuth::add(y, z) }
  assert_eq(450, result2)  // 30 > 15 为真，所以 30 * 15 = 450
  
  // 复杂条件
  let complex_condition = azimuth::add(x, y) > azimuth::multiply(z, 3)
  let complex_result = if complex_condition { azimuth::multiply(x, y) } else { azimuth::add(x, y) }
  assert_eq(750, complex_result)  // 25 + 30 > 15*3 (55 > 45) 为真，所以 25 * 30 = 750
}

test "data_conversion_and_formatting" {
  // 测试数据转换和格式化
  
  // 数字字符串处理
  let number_str = "2023"
  let year_greeting = azimuth::greet(number_str)
  assert_eq_string("Hello, 2023!", year_greeting)
  
  // 布尔值字符串处理
  let true_str = "true"
  let false_str = "false"
  
  assert_eq_string("Hello, true!", azimuth::greet(true_str))
  assert_eq_string("Hello, false!", azimuth::greet(false_str))
  
  // 特殊格式字符串
  let date_str = "2023-12-25"
  let time_str = "14:30:00"
  let phone_str = "+86-138-0013-8000"
  
  assert_eq_string("Hello, 2023-12-25!", azimuth::greet(date_str))
  assert_eq_string("Hello, 14:30:00!", azimuth::greet(time_str))
  assert_eq_string("Hello, +86-138-0013-8000!", azimuth::greet(phone_str))
  
  // 混合内容字符串
  let mixed_str = "User123_Admin"
  assert_eq_string("Hello, User123_Admin!", azimuth::greet(mixed_str))
}

test "algorithm_implementation" {
  // 测试简单算法实现
  
  // 计算等差数列的和: n * (a1 + an) / 2
  let n = 10
  let first_term = 1
  let last_term = 19
  
  let arithmetic_sum = azimuth::multiply(n, azimuth::add(first_term, last_term)) / 2
  assert_eq(100, arithmetic_sum)  // 10 * (1 + 19) / 2 = 100
  
  // 计算等比数列的和: a1 * (1 - r^n) / (1 - r)
  // 简化为整数计算: 1 + 2 + 4 + 8 + 16 = 31
  let geo_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(1, 2), 4), 8), 16)
  assert_eq(31, geo_sum)
  
  // 计算平方和: 1² + 2² + 3² + 4² + 5² = 55
  let square_sum = azimuth::add(
    azimuth::add(
      azimuth::add(
        azimuth::add(azimuth::multiply(1, 1), azimuth::multiply(2, 2)),
        azimuth::multiply(3, 3)
      ),
      azimuth::multiply(4, 4)
    ),
    azimuth::multiply(5, 5)
  )
  assert_eq(55, square_sum)
  
  // 计算阶乘: 5! = 5 * 4 * 3 * 2 * 1 = 120
  let factorial = azimuth::multiply(
    azimuth::multiply(
      azimuth::multiply(
        azimuth::multiply(5, 4),
        3
      ),
      2
    ),
    1
  )
  assert_eq(120, factorial)
}