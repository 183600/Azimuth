// 综合测试用例 - 测试 Azimuth 项目的核心功能
// 使用标准 MoonBit 测试语法

// 导入 azimuth 包以使用其功能
use azimuth

// 测试1: 边界值组合测试
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  assert_eq(-1, azimuth.add(max_val, min_val))
  assert_eq(min_val, azimuth.multiply(min_val, 1))
  assert_eq(min_val, azimuth.multiply(max_val, -1))
  
  // 测试接近边界的值
  assert_eq(max_val, azimuth.add(max_val - 1, 2))
  assert_eq(min_val, azimuth.add(min_val + 1, -2))
}

// 测试2: 数学运算性质验证
test "mathematical_properties" {
  // 测试加法交换律: a + b = b + a
  let a = 12345
  let b = 67890
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let c = 54321
  assert_eq(
    azimuth.add(azimuth.add(a, b), c),
    azimuth.add(a, azimuth.add(b, c))
  )
  
  // 测试乘法交换律: a * b = b * a
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  assert_eq(
    azimuth.multiply(a, azimuth.add(b, c)),
    azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  )
}

// 测试3: 字符串处理边界情况
test "string_processing_edge_cases" {
  // 测试空字符串
  assert_eq_string("Hello, !", azimuth.greet(""))
  
  // 测试单字符
  assert_eq_string("Hello, A!", azimuth.greet("A"))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, \n\t\r!", azimuth.greet("\n\t\r"))
  assert_eq_string("Hello, \"quotes\"!", azimuth.greet("\"quotes\""))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetingFunctionWithExtendedInput"
  assert_eq_string("Hello, " + long_string + "!", azimuth.greet(long_string))
}

// 测试4: 实际应用场景 - 银行利息计算
test "bank_interest_calculation" {
  // 本金
  let principal = 10000
  
  // 年利率 (5% 表示为 5)
  let rate = 5
  
  // 年数
  let years = 3
  
  // 简单利息计算: 本金 + (本金 × 利率 × 年数) / 100
  let interest = azimuth.multiply(principal, azimuth.multiply(rate, years)) / 100
  let total = azimuth.add(principal, interest)
  
  // 验证: 10000 + (10000 × 5 × 3) / 100 = 10000 + 1500 = 11500
  assert_eq(11500, total)
}

// 测试5: 实际应用场景 - 购物车总价计算
test "shopping_cart_total" {
  // 商品价格和数量
  let item1_price = 299
  let item1_quantity = 2
  
  let item2_price = 199
  let item2_quantity = 3
  
  let item3_price = 99
  let item3_quantity = 1
  
  // 计算每种商品的小计
  let item1_total = azimuth.multiply(item1_price, item1_quantity)
  let item2_total = azimuth.multiply(item2_price, item2_quantity)
  let item3_total = azimuth.multiply(item3_price, item3_quantity)
  
  // 计算购物车总价
  let cart_total = azimuth.add(
    azimuth.add(item1_total, item2_total),
    item3_total
  )
  
  // 验证: 299*2 + 199*3 + 99*1 = 598 + 597 + 99 = 1294
  assert_eq(1294, cart_total)
}

// 测试6: 温度转换计算
test "temperature_conversion" {
  // 摄氏度转华氏度 (简化整数版本): F = C * 9/5 + 32 ≈ C * 2 + 30
  
  // 冰点: 0°C = 32°F (简化计算: 0*2+30=30)
  let freezing_c = 0
  let freezing_f = azimuth.add(azimuth.multiply(freezing_c, 2), 30)
  assert_eq(30, freezing_f)
  
  // 室温: 20°C = 68°F (简化计算: 20*2+30=70)
  let room_c = 20
  let room_f = azimuth.add(azimuth.multiply(room_c, 2), 30)
  assert_eq(70, room_f)
  
  // 沸点: 100°C = 212°F (简化计算: 100*2+30=230)
  let boiling_c = 100
  let boiling_f = azimuth.add(azimuth.multiply(boiling_c, 2), 30)
  assert_eq(230, boiling_f)
}

// 测试7: 几何计算 - 面积和周长
test "geometry_calculations" {
  // 矩形面积和周长
  let length = 12
  let width = 8
  
  let area = azimuth.multiply(length, width)
  let perimeter = azimuth.add(
    azimuth.multiply(2, length),
    azimuth.multiply(2, width)
  )
  
  assert_eq(96, area)      // 12 * 8 = 96
  assert_eq(40, perimeter) // 2*12 + 2*8 = 40
  
  // 三角形面积 (底×高÷2)
  let base = 10
  let height = 6
  let triangle_area = azimuth.multiply(base, height) / 2
  assert_eq(30, triangle_area) // 10 * 6 / 2 = 30
}

// 测试8: 数列计算
test "sequence_calculations" {
  // 计算等差数列前n项和: 1 + 2 + 3 + ... + n = n*(n+1)/2
  let n = 10
  let sum = azimuth.multiply(n, azimuth.add(n, 1)) / 2
  assert_eq(55, sum) // 10*11/2 = 55
  
  // 计算等比数列前n项和: 2^0 + 2^1 + 2^2 + ... + 2^(n-1) = 2^n - 1
  // 这里我们手动计算前5项: 1 + 2 + 4 + 8 + 16 = 31
  let geo_sum = azimuth.add(
    azimuth.add(
      azimuth.add(1, 2),
      azimuth.add(4, 8)
    ),
    16
  )
  assert_eq(31, geo_sum)
}

// 测试9: 条件逻辑测试
test "conditional_logic_test" {
  let x = 15
  let y = 20
  let z = 25
  
  // 测试各种条件组合
  assert_true(x < y)
  assert_true(y <= z)
  assert_true(x < z)
  
  // 基于条件的计算
  let max_val = if x > y { x } else { y }
  max_val = if max_val > z { max_val } else { z }
  assert_eq(25, max_val)
  
  // 测试嵌套条件
  let result = if x > 0 {
    if y > 0 { azimuth.add(x, y) } else { x }
  } else {
    if y > 0 { y } else { 0 }
  }
  assert_eq(35, result) // 15 + 20 = 35
}

// 测试10: 错误处理和边界检查
test "error_handling_and_boundary_checks" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出处理
  assert_true(azimuth.add(max_val, 1) == max_val)
  assert_true(azimuth.add(min_val, -1) == min_val)
  assert_true(azimuth.multiply(max_val, 2) == max_val)
  assert_true(azimuth.multiply(min_val, -1) == min_val)
  
  // 验证特殊情况
  assert_eq(0, azimuth.multiply(0, max_val))
  assert_eq(0, azimuth.multiply(0, min_val))
  assert_eq(max_val, azimuth.multiply(max_val, 1))
  assert_eq(min_val, azimuth.multiply(min_val, 1))
  
  // 验证字符串处理的鲁棒性
  assert_eq_string("Hello, null!", azimuth.greet("null"))
  assert_eq_string("Hello, undefined!", azimuth.greet("undefined"))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
}