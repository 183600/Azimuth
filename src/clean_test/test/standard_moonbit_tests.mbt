test "basic_addition_functionality" {
  // 正常加法
  @clean_test.assert_eq(5, @clean_test.add(2, 3))
  @clean_test.assert_eq(0, @clean_test.add(-2, 2))
  @clean_test.assert_eq(-4, @clean_test.add(-2, -2))
  
  // 零值加法
  @clean_test.assert_eq(7, @clean_test.add(7, 0))
  @clean_test.assert_eq(-3, @clean_test.add(0, -3))
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
}

// 测试乘法的基本功能
///|
test "basic_multiplication_functionality" {
  // 正常乘法
  @clean_test.assert_eq(6, @clean_test.multiply(2, 3))
  @clean_test.assert_eq(-6, @clean_test.multiply(-2, 3))
  @clean_test.assert_eq(6, @clean_test.multiply(-2, -3))
  
  // 零值乘法
  @clean_test.assert_eq(0, @clean_test.multiply(5, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(0, -7))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 0))
  
  // 单位元乘法
  @clean_test.assert_eq(8, @clean_test.multiply(8, 1))
  @clean_test.assert_eq(-9, @clean_test.multiply(1, -9))
}

// 测试字符串拼接功能
///|
test "string_greeting_functionality" {
  // 基本拼接
  @clean_test.assert_eq_string("Hello, Alice!", @clean_test.greet("Alice"))
  @clean_test.assert_eq_string("Hello, Bob!", @clean_test.greet("Bob"))
  
  // 特殊字符处理
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(""))
  @clean_test.assert_eq_string("Hello, 123!", @clean_test.greet("123"))
  @clean_test.assert_eq_string("Hello, !@#$!", @clean_test.greet("!@#$"))
}

// 测试加法边界值
///|
test "addition_boundary_values" {
  // 最大值边界
  let max_val = 2147483647
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, 0))
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, 1))
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, max_val))
  
  // 最小值边界
  let min_val = -2147483648
  @clean_test.assert_eq(min_val, @clean_test.add(min_val, 0))
  @clean_test.assert_eq(min_val, @clean_test.add(min_val, -1))
  @clean_test.assert_eq(min_val, @clean_test.add(min_val, min_val))
  
  // 混合边界
  @clean_test.assert_eq(-1, @clean_test.add(max_val, min_val))
  @clean_test.assert_eq(0, @clean_test.add(max_val, -max_val))
}

// 测试乘法边界值
///|
test "multiplication_boundary_values" {
  // 最大值边界
  let max_val = 2147483647
  @clean_test.assert_eq(max_val, @clean_test.multiply(max_val, 1))
  @clean_test.assert_eq(-max_val, @clean_test.multiply(max_val, -1))
  
  // 最小值边界
  let min_val = -2147483648
  @clean_test.assert_eq(min_val, @clean_test.multiply(min_val, 1))
  @clean_test.assert_eq(min_val, @clean_test.multiply(min_val, -1))
  @clean_test.assert_eq(0, @clean_test.multiply(min_val, 0))
  
  // 溢出测试
  @clean_test.assert_eq(max_val, @clean_test.multiply(46341, 46341))
  @clean_test.assert_eq(max_val, @clean_test.multiply(46342, 46342))
}

// 测试数学性质：交换律
///|
test "mathematical_commutative_property" {
  // 加法交换律
  @clean_test.assert_eq(@clean_test.add(10, 20), @clean_test.add(20, 10))
  @clean_test.assert_eq(@clean_test.add(-5, 15), @clean_test.add(15, -5))
  @clean_test.assert_eq(@clean_test.add(-10, -20), @clean_test.add(-20, -10))
  
  // 乘法交换律
  @clean_test.assert_eq(@clean_test.multiply(4, 7), @clean_test.multiply(7, 4))
  @clean_test.assert_eq(@clean_test.multiply(-3, 8), @clean_test.multiply(8, -3))
  @clean_test.assert_eq(@clean_test.multiply(-6, -9), @clean_test.multiply(-9, -6))
}

// 测试数学性质：结合律
///|
test "mathematical_associative_property" {
  // 加法结合律
  let a = 10
  let b = 20
  let c = 30
  @clean_test.assert_eq(@clean_test.add(@clean_test.add(a, b), c), @clean_test.add(a, @clean_test.add(b, c)))
  
  let x = -5
  let y = 15
  let z = -10
  @clean_test.assert_eq(@clean_test.add(@clean_test.add(x, y), z), @clean_test.add(x, @clean_test.add(y, z)))
  
  // 乘法结合律
  @clean_test.assert_eq(@clean_test.multiply(@clean_test.multiply(2, 3), 4), @clean_test.multiply(2, @clean_test.multiply(3, 4)))
  @clean_test.assert_eq(@clean_test.multiply(@clean_test.multiply(-2, 3), -4), @clean_test.multiply(-2, @clean_test.multiply(3, -4)))
}

// 测试实际应用：简单计算
///|
test "practical_calculations" {
  // 矩形面积计算
  let length = 12
  let width = 8
  let area = @clean_test.multiply(length, width)
  @clean_test.assert_eq(96, area)
  
  // 矩形周长计算
  let perimeter = @clean_test.multiply(2, @clean_test.add(length, width))
  @clean_test.assert_eq(40, perimeter)
  
  // 简单购物计算
  let price = 25
  let quantity = 4
  let tax = 10
  let subtotal = @clean_test.multiply(price, quantity)
  let total = @clean_test.add(subtotal, tax)
  @clean_test.assert_eq(110, total)
}

// 测试特殊情况处理
///|
test "special_cases_handling" {
  // 最小值加正数
  let min_val = -2147483648
  @clean_test.assert_eq(-2147483647, @clean_test.add(min_val, 1))
  @clean_test.assert_eq(-2147483646, @clean_test.add(min_val, 2))
  @clean_test.assert_eq(-2147483640, @clean_test.add(min_val, 8))
  
  // 最小值乘法
  @clean_test.assert_eq(min_val, @clean_test.multiply(min_val, 1))
  @clean_test.assert_eq(min_val, @clean_test.multiply(1, min_val))
  @clean_test.assert_eq(0, @clean_test.multiply(min_val, 0))
  
  // 字符串边界情况
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(""))
  @clean_test.assert_eq_string("Hello, A!", @clean_test.greet("A"))
  @clean_test.assert_eq_string("Hello, ThisIsAVeryLongString!", @clean_test.greet("ThisIsAVeryLongString"))
}
