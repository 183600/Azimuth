// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，提供额外的测试覆盖

test "boolean_logic_simulation" {
  // 模拟布尔逻辑运算（使用整数表示）
  // 1 表示 true，0 表示 false
  
  // AND 运算：只有两个操作数都为 1 时结果才为 1
  let and_result = azimuth::multiply(1, 1)  // 1 AND 1 = 1
  assert_eq(and_result, 1)
  
  let and_result2 = azimuth::multiply(1, 0)  // 1 AND 0 = 0
  assert_eq(and_result2, 0)
  
  // OR 运算：使用加法实现（只要有一个为 1 结果就大于 0）
  let or_result = azimuth::add(1, 0)  // 1 OR 0 = 1
  assert_eq(or_result, 1)
  
  let or_result2 = azimuth::add(0, 0)  // 0 OR 0 = 0
  assert_eq(or_result2, 0)
}

test "temperature_conversion" {
  // 温度转换测试
  
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = azimuth::multiply(celsius, 9) / 5 + 32
  assert_eq(fahrenheit, 77)
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let fahrenheit2 = 212
  let celsius2 = azimuth::add(fahrenheit2, -32) * 5 / 9
  assert_eq(celsius2, 100)
}

test "fibonacci_sequence_calculation" {
  // 斐波那契数列计算测试
  
  // F(0) = 0, F(1) = 1
  let fib0 = 0
  let fib1 = 1
  assert_eq(fib0, 0)
  assert_eq(fib1, 1)
  
  // F(2) = F(1) + F(0) = 1
  let fib2 = azimuth::add(fib1, fib0)
  assert_eq(fib2, 1)
  
  // F(3) = F(2) + F(1) = 2
  let fib3 = azimuth::add(fib2, fib1)
  assert_eq(fib3, 2)
  
  // F(4) = F(3) + F(2) = 3
  let fib4 = azimuth::add(fib3, fib2)
  assert_eq(fib4, 3)
}

test "area_calculations" {
  // 面积计算测试
  
  // 正方形面积：边长 × 边长
  let square_side = 5
  let square_area = azimuth::multiply(square_side, square_side)
  assert_eq(square_area, 25)
  
  // 矩形面积：长 × 宽
  let rectangle_length = 8
  let rectangle_width = 4
  let rectangle_area = azimuth::multiply(rectangle_length, rectangle_width)
  assert_eq(rectangle_area, 32)
  
  // 三角形面积：底 × 高 ÷ 2
  let triangle_base = 10
  let triangle_height = 6
  let triangle_area = azimuth::multiply(triangle_base, triangle_height) / 2
  assert_eq(triangle_area, 30)
}

test "time_conversion" {
  // 时间转换测试
  
  // 小时转分钟
  let hours = 3
  let minutes = azimuth::multiply(hours, 60)
  assert_eq(minutes, 180)
  
  // 分钟转秒
  let minutes2 = 15
  let seconds = azimuth::multiply(minutes2, 60)
  assert_eq(seconds, 900)
  
  // 天转小时
  let days = 2
  let hours2 = azimuth::multiply(days, 24)
  assert_eq(hours2, 48)
}

test "percentage_calculations" {
  // 百分比计算测试
  
  // 计算百分比：部分 / 总数 × 100
  let part = 25
  let total = 200
  let percentage = part * 100 / total
  assert_eq(percentage, 12)
  
  // 计算百分比值：总数 × 百分比 / 100
  let total2 = 500
  let percentage2 = 20
  let value = azimuth::multiply(total2, percentage2) / 100
  assert_eq(value, 100)
}

test "factorial_calculation" {
  // 阶乘计算测试
  
  // 5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let fact1 = azimuth::multiply(n, 4)  // 20
  let fact2 = azimuth::multiply(fact1, 3)  // 60
  let fact3 = azimuth::multiply(fact2, 2)  // 120
  let fact4 = azimuth::multiply(fact3, 1)  // 120
  assert_eq(fact4, 120)
  
  // 3! = 3 × 2 × 1 = 6
  let n2 = 3
  let fact5 = azimuth::multiply(n2, 2)  // 6
  let fact6 = azimuth::multiply(fact5, 1)  // 6
  assert_eq(fact6, 6)
}

test "geometric_progression" {
  // 等比数列测试
  
  // 首项为 2，公比为 3 的等比数列前几项
  let first_term = 2
  let ratio = 3
  
  // 第 1 项：2
  assert_eq(first_term, 2)
  
  // 第 2 项：2 × 3 = 6
  let second_term = azimuth::multiply(first_term, ratio)
  assert_eq(second_term, 6)
  
  // 第 3 项：6 × 3 = 18
  let third_term = azimuth::multiply(second_term, ratio)
  assert_eq(third_term, 18)
  
  // 第 4 项：18 × 3 = 54
  let fourth_term = azimuth::multiply(third_term, ratio)
  assert_eq(fourth_term, 54)
}

test "simple_interest_calculation" {
  // 简单利息计算测试
  
  // 利息 = 本金 × 利率 × 时间
  let principal = 1000
  let rate = 5  // 5%
  let time = 2  // 2年
  
  let interest = azimuth::multiply(azimuth::multiply(principal, rate), time) / 100
  assert_eq(interest, 100)
  
  // 总金额 = 本金 + 利息
  let total_amount = azimuth::add(principal, interest)
  assert_eq(total_amount, 1100)
}

test "volume_calculations" {
  // 体积计算测试
  
  // 正方体体积：边长 × 边长 × 边长
  let cube_side = 3
  let cube_volume = azimuth::multiply(azimuth::multiply(cube_side, cube_side), cube_side)
  assert_eq(cube_volume, 27)
  
  // 长方体体积：长 × 宽 × 高
  let length = 4
  let width = 5
  let height = 6
  let cuboid_volume = azimuth::multiply(azimuth::multiply(length, width), height)
  assert_eq(cuboid_volume, 120)
}