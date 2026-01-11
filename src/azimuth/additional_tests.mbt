// 额外的 MoonBit 测试用例
// 包含对 add、multiply 和 greet 函数的综合测试

// 测试1: 加法函数的边界值测试
///|
test "add_function_boundary_values" {
  // 测试零值加法
  assert_eq(5, add(0, 5))
  assert_eq(-3, add(-3, 0))
  assert_eq(0, add(0, 0))
  
  // 测试正数相加
  assert_eq(10, add(3, 7))
  assert_eq(100, add(50, 50))
  
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(-100, add(-50, -50))
  
  // 测试正负数相加
  assert_eq(0, add(5, -5))
  assert_eq(-2, add(3, -5))
  assert_eq(2, add(-3, 5))
}

// 测试2: 乘法函数的边界值测试
///|
test "multiply_function_boundary_values" {
  // 测试零值乘法
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(-3, 0))
  assert_eq(0, multiply(0, 0))
  
  // 测试单位元乘法
  assert_eq(5, multiply(1, 5))
  assert_eq(-3, multiply(-3, 1))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(3, multiply(-1, -3))
  
  // 测试正数相乘
  assert_eq(15, multiply(3, 5))
  assert_eq(100, multiply(10, 10))
  
  // 测试负数相乘
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
}

// 测试3: greet 函数的字符串处理测试
///|
test "greet_function_string_processing" {
  // 测试基本问候
  assert_eq("Hello, World!", greet("World"))
  assert_eq("Hello, MoonBit!", greet("MoonBit"))
  
  // 测试空字符串
  assert_eq("Hello, !", greet(""))
  
  // 测试单字符
  assert_eq("Hello, A!", greet("A"))
  
  // 测试包含空格的字符串
  assert_eq("Hello, John Doe!", greet("John Doe"))
  
  // 测试包含数字的字符串
  assert_eq("Hello, 123!", greet("123"))
  
  // 测试包含特殊字符的字符串
  assert_eq("Hello, @#$%!", greet("@#$%"))
}

// 测试4: 加法溢出处理测试
///|
test "add_function_overflow_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(-1000000000, -1500000000))
  
  // 测试最小值特殊情况
  assert_eq(min_val, add(min_val, -100))
  assert_eq(-2147483647, add(min_val, 1))
}

// 测试5: 乘法溢出处理测试
///|
test "multiply_function_overflow_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, multiply(50000, 50000))
  assert_eq(max_val, multiply(100000, 30000))
  
  // 测试负数溢出
  assert_eq(min_val, multiply(-50000, 50000))
  assert_eq(min_val, multiply(100000, -30000))
  
  // 测试最小值特殊情况
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -2))
  assert_eq(min_val, multiply(2, min_val))
  assert_eq(min_val, multiply(-2, min_val))
}

// 测试6: 实际应用场景 - 购物车计算
///|
test "shopping_cart_calculation" {
  // 商品价格和数量
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let item3_price = 99
  let item3_quantity = 3
  
  // 计算各商品小计
  let item1_total = multiply(item1_price, item1_quantity)
  let item2_total = multiply(item2_price, item2_quantity)
  let item3_total = multiply(item3_price, item3_quantity)
  
  // 计算总价
  let subtotal = add(item1_total, item2_total)
  let total = add(subtotal, item3_total)
  
  // 验证计算结果
  assert_eq(398, item1_total)  // 199 * 2
  assert_eq(299, item2_total)  // 299 * 1
  assert_eq(297, item3_total)  // 99 * 3
  assert_eq(994, total)        // 398 + 299 + 297
}

// 测试7: 数学性质验证 - 结合律
///|
test "mathematical_associative_laws" {
  // 加法结合律：(a + b) + c = a + (b + c)
  let a = 15
  let b = 27
  let c = 42
  
  let left_side = add(add(a, b), c)
  let right_side = add(a, add(b, c))
  
  assert_eq(left_side, right_side)
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  let x = 7
  let y = 11
  let z = 13
  
  let left_side_mul = multiply(multiply(x, y), z)
  let right_side_mul = multiply(x, multiply(y, z))
  
  assert_eq(left_side_mul, right_side_mul)
}

// 测试8: 数学性质验证 - 交换律
///|
test "mathematical_commutative_laws" {
  // 加法交换律：a + b = b + a
  let a = 123
  let b = 456
  
  let sum_ab = add(a, b)
  let sum_ba = add(b, a)
  
  assert_eq(sum_ab, sum_ba)
  
  // 乘法交换律：a * b = b * a
  let x = 23
  let y = 47
  
  let product_xy = multiply(x, y)
  let product_yx = multiply(y, x)
  
  assert_eq(product_xy, product_yx)
}

// 测试9: 实际应用场景 - 温度转换
///|
test "temperature_conversion" {
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 为避免小数，我们使用整数运算：F = (C * 9 + 160) / 5
  // 验证：0°C = 32°F
  
  let celsius_0 = 0
  let fahrenheit_32 = 32
  
  // 计算 (0 * 9 + 160) / 5 = 160 / 5 = 32
  let numerator_0 = add(multiply(celsius_0, 9), 160)
  // 验证 32 * 5 = 160
  let verification_0 = multiply(fahrenheit_32, 5)
  assert_eq(numerator_0, verification_0)
  
  // 验证：100°C = 212°F
  let celsius_100 = 100
  let fahrenheit_212 = 212
  
  // 计算 (100 * 9 + 160) / 5 = 1060 / 5 = 212
  let numerator_100 = add(multiply(celsius_100, 9), 160)
  // 验证 212 * 5 = 1060
  let verification_100 = multiply(fahrenheit_212, 5)
  assert_eq(numerator_100, verification_100)
}

// 测试10: 综合应用 - 面积和周长计算
///|
test "area_and_perimeter_calculation" {
  // 矩形面积和周长计算
  let length = 15
  let width = 8
  
  // 面积 = 长 * 宽
  let area = multiply(length, width)
  
  // 周长 = 2 * (长 + 宽)
  let sum_sides = add(length, width)
  let perimeter = multiply(2, sum_sides)
  
  // 验证计算结果
  assert_eq(120, area)      // 15 * 8 = 120
  assert_eq(46, perimeter)  // 2 * (15 + 8) = 46
  
  // 三角形面积计算（使用海伦公式的简化版本）
  // 假设直角三角形，面积 = 底 * 高 / 2
  let base = 12
  let height = 9
  
  // 面积 = (底 * 高) / 2
  let triangle_area_times_2 = multiply(base, height)
  let triangle_area = 54  // 108 / 2
  
  // 验证：54 * 2 = 108
  assert_eq(108, triangle_area_times_2)
}