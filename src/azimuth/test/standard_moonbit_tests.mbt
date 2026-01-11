// 标准 MoonBit 测试用例
// 测试基本功能和边界情况

// 测试加法的基本功能
///|
test "basic_addition_functionality" {
  // 正常加法
  assert_eq(5, @azimuth.add(2, 3))
  assert_eq(0, @azimuth.add(-2, 2))
  assert_eq(-4, @azimuth.add(-2, -2))
  
  // 零值加法
  assert_eq(7, @azimuth.add(7, 0))
  assert_eq(-3, @azimuth.add(0, -3))
  assert_eq(0, @azimuth.add(0, 0))
}

// 测试乘法的基本功能
///|
test "basic_multiplication_functionality" {
  // 正常乘法
  assert_eq(6, @azimuth.multiply(2, 3))
  assert_eq(-6, @azimuth.multiply(-2, 3))
  assert_eq(6, @azimuth.multiply(-2, -3))
  
  // 零值乘法
  assert_eq(0, @azimuth.multiply(5, 0))
  assert_eq(0, @azimuth.multiply(0, -7))
  assert_eq(0, @azimuth.multiply(0, 0))
  
  // 单位元乘法
  assert_eq(8, @azimuth.multiply(8, 1))
  assert_eq(-9, @azimuth.multiply(1, -9))
}

// 测试字符串拼接功能
///|
test "string_greeting_functionality" {
  // 基本拼接
  assert_eq("Hello, Alice!", @azimuth.greet("Alice"))
  assert_eq("Hello, Bob!", @azimuth.greet("Bob"))
  
  // 特殊字符处理
  assert_eq("Hello, !", @azimuth.greet(""))
  assert_eq("Hello, 123!", @azimuth.greet("123"))
  assert_eq("Hello, !@#$!", @azimuth.greet("!@#$"))
}

// 测试加法边界值
///|
test "addition_boundary_values" {
  // 最大值边界
  let max_val = 2147483647
  assert_eq(max_val, @azimuth.add(max_val, 0))
  assert_eq(max_val, @azimuth.add(max_val, 1))
  assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 最小值边界
  let min_val = -2147483648
  assert_eq(min_val, @azimuth.add(min_val, 0))
  assert_eq(min_val, @azimuth.add(min_val, -1))
  assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 混合边界
  assert_eq(-1, @azimuth.add(max_val, min_val))
  assert_eq(0, @azimuth.add(max_val, -max_val))
}

// 测试乘法边界值
///|
test "multiplication_boundary_values" {
  // 最大值边界
  let max_val = 2147483647
  assert_eq(max_val, @azimuth.multiply(max_val, 1))
  assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  
  // 最小值边界
  let min_val = -2147483648
  assert_eq(min_val, @azimuth.multiply(min_val, 1))
  assert_eq(min_val, @azimuth.multiply(min_val, -1))
  assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 溢出测试
  assert_eq(max_val, @azimuth.multiply(46341, 46341))
  assert_eq(max_val, @azimuth.multiply(46342, 46342))
}

// 测试数学性质：交换律
///|
test "mathematical_commutative_property" {
  // 加法交换律
  assert_eq(@azimuth.add(10, 20), @azimuth.add(20, 10))
  assert_eq(@azimuth.add(-5, 15), @azimuth.add(15, -5))
  assert_eq(@azimuth.add(-10, -20), @azimuth.add(-20, -10))
  
  // 乘法交换律
  assert_eq(@azimuth.multiply(4, 7), @azimuth.multiply(7, 4))
  assert_eq(@azimuth.multiply(-3, 8), @azimuth.multiply(8, -3))
  assert_eq(@azimuth.multiply(-6, -9), @azimuth.multiply(-9, -6))
}

// 测试数学性质：结合律
///|
test "mathematical_associative_property" {
  // 加法结合律
  let a = 10, b = 20, c = 30
  assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  let x = -5, y = 15, z = -10
  assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
  
  // 乘法结合律
  assert_eq(@azimuth.multiply(@azimuth.multiply(2, 3), 4), @azimuth.multiply(2, @azimuth.multiply(3, 4)))
  assert_eq(@azimuth.multiply(@azimuth.multiply(-2, 3), -4), @azimuth.multiply(-2, @azimuth.multiply(3, -4)))
}

// 测试实际应用：简单计算
///|
test "practical_calculations" {
  // 矩形面积计算
  let length = 12, width = 8
  let area = @azimuth.multiply(length, width)
  assert_eq(96, area)
  
  // 矩形周长计算
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  assert_eq(40, perimeter)
  
  // 简单购物计算
  let price = 25, quantity = 4, tax = 10
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(subtotal, tax)
  assert_eq(110, total)
}

// 测试特殊情况处理
///|
test "special_cases_handling" {
  // 最小值加正数
  let min_val = -2147483648
  assert_eq(-2147483647, @azimuth.add(min_val, 1))
  assert_eq(-2147483646, @azimuth.add(min_val, 2))
  assert_eq(-2147483640, @azimuth.add(min_val, 8))
  
  // 最小值乘法
  assert_eq(min_val, @azimuth.multiply(min_val, 1))
  assert_eq(min_val, @azimuth.multiply(1, min_val))
  assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 字符串边界情况
  assert_eq("Hello, !", @azimuth.greet(""))
  assert_eq("Hello, A!", @azimuth.greet("A"))
  assert_eq("Hello, ThisIsAVeryLongString!", @azimuth.greet("ThisIsAVeryLongString"))
}