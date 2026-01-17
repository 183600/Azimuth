// Azimuth 项目综合测试用例
// 使用标准 MoonBit 测试语法，涵盖多种测试场景

test "add_function_comprehensive" {
  // 测试加法函数的多种情况
  assert_eq(azimuth::add(5, 3), 8)
  assert_eq(azimuth::add(-5, 3), -2)
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::add(100, 200), 300)
  assert_eq(azimuth::add(-10, -20), -30)
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的多种情况
  assert_eq(azimuth::multiply(4, 5), 20)
  assert_eq(azimuth::multiply(-4, 5), -20)
  assert_eq(azimuth::multiply(4, -5), -20)
  assert_eq(azimuth::multiply(-4, -5), 20)
  assert_eq(azimuth::multiply(0, 100), 0)
}

test "greet_function_comprehensive" {
  // 测试问候函数的多种情况
  assert_eq_string(azimuth::greet("World"), "Hello, World!")
  assert_eq_string(azimuth::greet("Alice"), "Hello, Alice!")
  assert_eq_string(azimuth::greet("张三"), "Hello, 张三!")
  assert_eq_string(azimuth::greet(""), "Hello, !")
  assert_eq_string(azimuth::greet("123"), "Hello, 123!")
}

test "mathematical_expressions" {
  // 测试数学表达式组合
  // (2 + 3) * 4 = 20
  let temp1 = azimuth::add(2, 3)
  let result1 = azimuth::multiply(temp1, 4)
  assert_eq(result1, 20)
  
  // 5 * (6 + 7) = 65
  let temp2 = azimuth::add(6, 7)
  let result2 = azimuth::multiply(5, temp2)
  assert_eq(result2, 65)
  
  // (8 * 2) + (3 * 4) = 28
  let temp3 = azimuth::multiply(8, 2)
  let temp4 = azimuth::multiply(3, 4)
  let result3 = azimuth::add(temp3, temp4)
  assert_eq(result3, 28)
}

test "boundary_values" {
  // 测试边界值
  assert_eq(azimuth::add(2147483647, 0), 2147483647)
  assert_eq(azimuth::add(-2147483648, 0), -2147483648)
  assert_eq(azimuth::multiply(1, 2147483647), 2147483647)
  assert_eq(azimuth::multiply(-1, 2147483647), -2147483647)
  assert_eq(azimuth::multiply(0, -2147483648), 0)
}

test "sequence_calculations" {
  // 测试序列计算
  // 斐波那契数列: 1, 1, 2, 3, 5, 8
  let fib1 = 1
  let fib2 = 1
  let fib3 = azimuth::add(fib1, fib2)  // 2
  let fib4 = azimuth::add(fib2, fib3)  // 3
  let fib5 = azimuth::add(fib3, fib4)  // 5
  let fib6 = azimuth::add(fib4, fib5)  // 8
  
  assert_eq(fib3, 2)
  assert_eq(fib4, 3)
  assert_eq(fib5, 5)
  assert_eq(fib6, 8)
}

test "business_calculations" {
  // 测试商业计算场景
  // 商品总价计算
  let price = 50
  let quantity = 3
  let subtotal = azimuth::multiply(price, quantity)
  assert_eq(subtotal, 150)
  
  // 折扣计算
  let discount = 20
  let final_price = azimuth::add(subtotal, -discount)
  assert_eq(final_price, 130)
  
  // 税费计算
  let tax_rate = 10  // 10%
  let tax = azimuth::multiply(final_price, tax_rate) / 100
  let total = azimuth::add(final_price, tax)
  assert_eq(total, 143)
}

test "geometry_calculations" {
  // 测试几何计算
  // 正方形面积
  let side = 5
  let area = azimuth::multiply(side, side)
  assert_eq(area, 25)
  
  // 矩形周长: 2 * (长 + 宽)
  let length = 8
  let width = 4
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  assert_eq(perimeter, 24)
  
  // 立方体体积
  let cube_side = 3
  let volume = azimuth::multiply(azimuth::multiply(cube_side, cube_side), cube_side)
  assert_eq(volume, 27)
}

test "time_calculations" {
  // 测试时间计算
  // 小时转分钟
  let hours = 3
  let minutes = azimuth::multiply(hours, 60)
  assert_eq(minutes, 180)
  
  // 天转小时
  let days = 2
  let total_hours = azimuth::multiply(days, 24)
  assert_eq(total_hours, 48)
  
  // 周转分钟
  let weeks = 1
  let week_minutes = azimuth::multiply(azimuth::multiply(weeks, 7), azimuth::multiply(24, 60))
  assert_eq(week_minutes, 10080)
}