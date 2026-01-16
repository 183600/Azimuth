// Azimuth 项目综合新测试用例
// 使用标准 MoonBit 测试语法，覆盖各种测试场景

test "basic_addition_properties" {
  // 基本加法属性测试
  assert_eq(azimuth::add(5, 3), 8)
  assert_eq(azimuth::add(0, 7), 7)
  assert_eq(azimuth::add(-2, 9), 7)
  assert_eq(azimuth::add(-5, -3), -8)
  assert_eq(azimuth::add(100, 200), 300)
}

test "basic_multiplication_properties" {
  // 基本乘法属性测试
  assert_eq(azimuth::multiply(4, 3), 12)
  assert_eq(azimuth::multiply(0, 10), 0)
  assert_eq(azimuth::multiply(-2, 5), -10)
  assert_eq(azimuth::multiply(-3, -4), 12)
  assert_eq(azimuth::multiply(7, 1), 7)
}

test "greet_function_variations" {
  // 问候函数变体测试
  assert_eq(azimuth::greet("World"), "Hello, World!")
  assert_eq(azimuth::greet("MoonBit"), "Hello, MoonBit!")
  assert_eq(azimuth::greet("测试123"), "Hello, 测试123!")
  assert_eq(azimuth::greet("user@domain"), "Hello, user@domain!")
  assert_eq(azimuth::greet("   "), "Hello,    !")
}

test "mathematical_formulas" {
  // 数学公式测试
  
  // 计算圆的面积 (近似): π * r²，使用 π ≈ 3
  let radius = 5
  let circle_area = azimuth::multiply(3, azimuth::multiply(radius, radius))
  assert_eq(75, circle_area)
  
  // 计算梯形面积: (上底 + 下底) * 高 / 2
  let top_base = 4
  let bottom_base = 8
  let height = 5
  let trapezoid_area = azimuth::multiply(azimuth::add(top_base, bottom_base), height) / 2
  assert_eq(30, trapezoid_area)
}

test "real_world_calculations" {
  // 实际世界计算测试
  
  // 购物计算: 商品价格 + 税费
  let item_price = 120
  let tax_rate = 10  // 10%
  let tax_amount = azimuth::multiply(item_price, tax_rate) / 100
  let total_cost = azimuth::add(item_price, tax_amount)
  assert_eq(132, total_cost)
  
  // 速度计算: 距离 / 时间 (简化为乘法)
  let distance = 300
  let time_hours = 5
  let speed = azimuth::multiply(distance, 1) / time_hours
  assert_eq(60, speed)
}

test "boundary_value_tests" {
  // 边界值测试
  
  // 大数运算
  assert_eq(azimuth::add(999999, 1), 1000000)
  assert_eq(azimuth::multiply(1000, 1000), 1000000)
  
  // 零值运算
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::multiply(0, 0), 0)
  
  // 单位运算
  assert_eq(azimuth::add(1, 0), 1)
  assert_eq(azimuth::multiply(1, 1), 1)
}

test "negative_number_operations" {
  // 负数运算测试
  
  // 负数加法
  assert_eq(azimuth::add(-10, -20), -30)
  assert_eq(azimuth::add(-15, 25), 10)
  assert_eq(azimuth::add(30, -45), -15)
  
  // 负数乘法
  assert_eq(azimuth::multiply(-6, 7), -42)
  assert_eq(azimuth::multiply(8, -9), -72)
  assert_eq(azimuth::multiply(-4, -5), 20)
}

test "complex_expression_evaluation" {
  // 复杂表达式求值测试
  
  // 表达式: (a * b) + (c * d) - e
  let a = 6
  let b = 4
  let c = 8
  let d = 3
  let e = 15
  
  let result = azimuth::add(
    azimuth::multiply(a, b),
    azimuth::add(azimuth::multiply(c, d), -e)
  )
  assert_eq(33, result)
  
  // 表达式: (x + y) * (z - w)
  let x = 10
  let y = 5
  let z = 20
  let w = 8
  
  let result2 = azimuth::multiply(
    azimuth::add(x, y),
    azimuth::add(z, -w)
  )
  assert_eq(120, result2)
}

test "algorithmic_patterns" {
  // 算法模式测试
  
  // 计算阶乘: 5! = 5 * 4 * 3 * 2 * 1 = 120
  let factorial_5 = azimuth::multiply(
    azimuth::multiply(
      azimuth::multiply(
        azimuth::multiply(5, 4), 3
      ), 2
    ), 1
  )
  assert_eq(120, factorial_5)
  
  // 计算幂: 2^4 = 2 * 2 * 2 * 2 = 16
  let power_2_4 = azimuth::multiply(
    azimuth::multiply(
      azimuth::multiply(2, 2), 2
    ), 2
  )
  assert_eq(16, power_2_4)
}

test "string_and_number_integration" {
  // 字符串和数字集成测试
  
  // 验证问候函数与数字输入的兼容性
  assert_eq(azimuth::greet("123"), "Hello, 123!")
  assert_eq(azimuth::greet("-456"), "Hello, -456!")
  assert_eq(azimuth::greet("3.14"), "Hello, 3.14!")
  
  // 特殊字符测试
  assert_eq(azimuth::greet("!@#$"), "Hello, !@#$!")
  assert_eq(azimuth::greet("\n\t"), "Hello, \n\t!")
}