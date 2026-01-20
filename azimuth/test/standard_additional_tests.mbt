// 额外的标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_commutative_property" {
  // 测试加法交换律
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-15, 27), @azimuth.add(27, -15))
  @azimuth.assert_eq(@azimuth.add(-15, -27), @azimuth.add(-27, -15))
  @azimuth.assert_eq(@azimuth.add(0, 42), @azimuth.add(42, 0))
}

test "add_function_associative_property" {
  // 测试加法结合律（在安全范围内）
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(10, 20), 30), @azimuth.add(10, @azimuth.add(20, 30)))
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(-10, 20), 30), @azimuth.add(-10, @azimuth.add(20, 30)))
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(10, -20), 30), @azimuth.add(10, @azimuth.add(-20, 30)))
}

test "multiply_function_commutative_property" {
  // 测试乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  @azimuth.assert_eq(@azimuth.multiply(-6, 7), @azimuth.multiply(7, -6))
  @azimuth.assert_eq(@azimuth.multiply(-6, -7), @azimuth.multiply(-7, -6))
  @azimuth.assert_eq(@azimuth.multiply(0, 42), @azimuth.multiply(42, 0))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律（在安全范围内）
  @azimuth.assert_eq(@azimuth.multiply(5, @azimuth.add(3, 4)), @azimuth.add(@azimuth.multiply(5, 3), @azimuth.multiply(5, 4)))
  @azimuth.assert_eq(@azimuth.multiply(-5, @azimuth.add(3, 4)), @azimuth.add(@azimuth.multiply(-5, 3), @azimuth.multiply(-5, 4)))
}

test "greet_function_with_special_names" {
  // 测试 greet 函数的特殊名称处理
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, C++!", @azimuth.greet("C++"))
  @azimuth.assert_eq_string("Hello, JavaScript!", @azimuth.greet("JavaScript"))
  @azimuth.assert_eq_string("Hello, 42!", @azimuth.greet("42"))
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(13, 2))
  @azimuth.assert_eq(11, @azimuth.divide_with_ceil(21, 2))
}

test "divide_with_ceil_zero_dividend" {
  // 测试被除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
}

test "complex_combined_operations" {
  // 测试复杂的组合运算
  let x = 12
  let y = 5
  let z = 3
  
  // 复杂表达式
  @azimuth.assert_eq(51, @azimuth.add(@azimuth.multiply(x, y), @azimuth.divide_with_ceil(@azimuth.multiply(z, 7), 2)))
  @azimuth.assert_eq(27, @azimuth.multiply(@azimuth.add(@azimuth.divide_with_ceil(x, y), 2), z))
  @azimuth.assert_eq(8, @azimuth.divide_with_ceil(@azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(y, z)), @azimuth.multiply(x, 2)))
}