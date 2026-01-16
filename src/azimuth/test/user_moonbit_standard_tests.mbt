// 用户自定义 MoonBit 标准测试用例
// 覆盖 azimuth 模块的主要功能

test "add_function_basic_tests" {
  // 测试基本加法功能
  assert_eq(10, @azimuth.add(3, 7))
  assert_eq(0, @azimuth.add(-5, 5))
  assert_eq(-8, @azimuth.add(-3, -5))
  assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic_tests" {
  // 测试基本乘法功能
  assert_eq(21, @azimuth.multiply(3, 7))
  assert_eq(-25, @azimuth.multiply(-5, 5))
  assert_eq(15, @azimuth.multiply(-3, -5))
  assert_eq(0, @azimuth.multiply(10, 0))
}

test "greet_function_tests" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, Azimuth!", @azimuth.greet("Azimuth"))
}

test "assert_true_function_tests" {
  // 测试断言真值功能
  @azimuth.assert_true(true)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(10 > 5)
  @azimuth.assert_true(-3 < 0)
}

test "assert_false_function_tests" {
  // 测试断言假值功能
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 2)
  @azimuth.assert_false(10 < 5)
  @azimuth.assert_false(-3 > 0)
}

test "divide_with_ceil_function_tests" {
  // 测试向上取整除法功能
  assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "complex_calculation_tests" {
  // 测试复合计算
  let a = 10
  let b = 5
  let c = 2
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  assert_eq(30, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  assert_eq(52, result2)
}

test "edge_case_boundary_tests" {
  // 测试边界情况
  assert_eq(1, @azimuth.add(1, 0))
  assert_eq(-1, @azimuth.add(0, -1))
  assert_eq(0, @azimuth.multiply(0, 12345))
  assert_eq(1, @azimuth.multiply(1, 1))
}

test "mathematical_properties_tests" {
  // 测试数学性质
  let x = 15
  let y = 25
  
  // 加法交换律
  assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  let z = 5
  assert_eq(@azimuth.multiply(x, @azimuth.add(y, z)), 
            @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z)))
}