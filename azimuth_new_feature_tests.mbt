// Azimuth 项目新增功能测试用例
// 使用标准 MoonBit 测试语法

test "add_large_numbers" {
  // 测试大数相加
  assert_eq(azimuth::add(999999, 1), 1000000)
  assert_eq(azimuth::add(500000, 500000), 1000000)
  assert_eq(azimuth::add(1000000, -1000000), 0)
}

test "multiply_by_negative_one" {
  // 测试乘以-1的性质
  assert_eq(azimuth::multiply(42, -1), -42)
  assert_eq(azimuth::multiply(-17, -1), 17)
  assert_eq(azimuth::multiply(0, -1), 0)
}

test "add_sequence_pattern" {
  // 测试序列模式相加
  // 斐波那契数列前几项: 1, 1, 2, 3, 5, 8
  let fib1 = 1
  let fib2 = 1
  let fib3 = azimuth::add(fib1, fib2)  // 2
  let fib4 = azimuth::add(fib2, fib3)  // 3
  let fib5 = azimuth::add(fib3, fib4)  // 5
  
  assert_eq(fib3, 2)
  assert_eq(fib4, 3)
  assert_eq(fib5, 5)
}

test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  assert_eq(azimuth::multiply(8, 2), 16)
  assert_eq(azimuth::multiply(16, 2), 32)
  assert_eq(azimuth::multiply(32, 2), 64)
  assert_eq(azimuth::multiply(64, 2), 128)
}

test "greet_with_special_names" {
  // 测试特殊名称的问候
  assert_eq(azimuth::greet("Alice"), "Hello, Alice!")
  assert_eq(azimuth::greet("Bob"), "Hello, Bob!")
  assert_eq(azimuth::greet("Charlie"), "Hello, Charlie!")
}

test "add_temperature_conversion" {
  // 测试温度转换计算
  // 摄氏度转华氏度: F = C * 9/5 + 32，简化为整数运算
  let celsius = 20
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 9) / 5, 32)
  assert_eq(68, fahrenheit)
  
  // 华氏度转摄氏度: C = (F - 32) * 5/9，简化为整数运算
  let fahrenheit2 = 100
  let celsius2 = azimuth::multiply(azimuth::add(fahrenheit2, -32), 5) / 9
  assert_eq(37, celsius2)
}

test "multiply_area_calculation" {
  // 测试面积计算
  // 正方形面积: 边长 × 边长
  let side = 7
  let square_area = azimuth::multiply(side, side)
  assert_eq(49, square_area)
  
  // 三角形面积: 底 × 高 ÷ 2，简化为整数运算
  let base = 10
  let height = 6
  let triangle_area = azimuth::multiply(base, height) / 2
  assert_eq(30, triangle_area)
}

test "add_time_calculations" {
  // 测试时间计算
  // 计算总秒数: 小时 × 3600 + 分钟 × 60 + 秒
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let total_seconds = azimuth::add(
    azimuth::add(azimuth::multiply(hours, 3600), azimuth::multiply(minutes, 60)),
    seconds
  )
  assert_eq(9045, total_seconds)
}

test "multiply_volume_calculation" {
  // 测试体积计算
  // 长方体体积: 长 × 宽 × 高
  let length = 5
  let width = 4
  let height = 3
  
  let base_area = azimuth::multiply(length, width)
  let volume = azimuth::multiply(base_area, height)
  assert_eq(60, volume)
}

test "complex_arithmetic_expression" {
  // 测试复合算术表达式
  // 计算: (a × b + c) × d - e
  let a = 3
  let b = 4
  let c = 5
  let d = 2
  let e = 10
  
  let step1 = azimuth::multiply(a, b)  // 12
  let step2 = azimuth::add(step1, c)   // 17
  let step3 = azimuth::multiply(step2, d)  // 34
  let result = azimuth::add(step3, -e)     // 24
  
  assert_eq(24, result)
}