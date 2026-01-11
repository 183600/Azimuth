// 简单的新测试用例，不使用 @azimuth 前缀

// 测试1: 基础加法验证
///|
test "basic_addition_check" {
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
}

// 测试2: 基础乘法验证
///|
test "basic_multiplication_check" {
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(-5, 0))
  assert_eq(15, multiply(-3, -5))
}

// 测试3: 字符串拼接验证
///|
test "basic_greeting_check" {
  assert_eq("Hello, MoonBit!", greet("MoonBit"))
  assert_eq("Hello, !", greet(""))
}

// 测试4: 边界值测试
///|
test "boundary_value_test" {
  // 测试接近边界的值
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  
  // 测试乘法边界
  assert_eq(2147483647, multiply(46341, 46341))
}

// 测试5: 数学性质验证
///|
test "math_properties_test" {
  // 加法交换律
  assert_eq(add(5, 3), add(3, 5))
  
  // 乘法交换律
  assert_eq(multiply(4, 6), multiply(6, 4))
  
  // 加法单位元
  assert_eq(7, add(7, 0))
  assert_eq(7, add(0, 7))
  
  // 乘法单位元
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
}

// 测试6: 实际应用场景 - 简单购物计算
///|
test "simple_shopping_calculation" {
  let item_price = 25
  let quantity = 4
  let subtotal = multiply(item_price, quantity)
  assert_eq(100, subtotal)
  
  let tax = multiply(subtotal, 10)  // 假设税率是10%，用乘法表示
  assert_eq(1000, tax)  // 100 * 10 = 1000，表示100元的10%是10元
}

// 测试7: 温度转换计算
///|
test "temperature_conversion_test" {
  // 摄氏度到华氏度：F = C * 9/5 + 32
  // 我们用乘法避免除法：F * 5 = C * 9 + 160
  
  let celsius = 20
  let fahrenheit_times_5 = add(multiply(celsius, 9), 160)
  assert_eq(340, fahrenheit_times_5)  // 20 * 9 + 160 = 340，所以是68华氏度
}

// 测试8: 面积计算
///|
test "area_calculation_test" {
  // 矩形面积 = 长 × 宽
  let length = 12
  let width = 8
  let area = multiply(length, width)
  assert_eq(96, area)
  
  // 三角形面积 = 底 × 高 ÷ 2
  // 我们用乘法表示：面积 × 2 = 底 × 高
  let base = 10
  let height = 6
  let triangle_area_times_2 = multiply(base, height)
  assert_eq(60, triangle_area_times_2)  // 10 * 6 = 60，所以面积是30
}

// 测试9: 速度距离时间计算
///|
test "motion_calculation_test" {
  // 距离 = 速度 × 时间
  let speed = 60  // km/h
  let time = 2.5  // 小时，用乘法表示：距离 × 10 = 速度 × 时间 × 10
  
  let distance_times_10 = multiply(multiply(speed, 25), 10)
  assert_eq(15000, distance_times_10)  // 60 * 25 * 10 = 15000，所以是150km
}

// 测试10: 数列计算
///|
test "sequence_calculation_test" {
  // 等差数列：1, 3, 5, 7, 9 的和
  // 使用公式：和 = n * (首项 + 末项) / 2
  // 我们用乘法表示：和 * 2 = n * (首项 + 末项)
  
  let n = 5
  let first_term = 1
  let last_term = 9
  let sum_times_2 = multiply(n, add(first_term, last_term))
  assert_eq(50, sum_times_2)  // 5 * 10 = 50，所以和是25
}