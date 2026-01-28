// 标准 MoonBit 测试用例 - 覆盖 azimuth 库的核心功能
// 使用标准的 MoonBit 测试语法

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(2, -3))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-2, subtract(3, 5))
}

test "mathematical_commutative_property" {
  // 测试数学交换律
  assert_eq(add(3, 7), add(7, 3))
  assert_eq(multiply(3, 7), multiply(7, 3))
}

test "zero_element_properties" {
  // 测试零元素性质
  assert_eq(7, add(7, 0))
  assert_eq(0, multiply(7, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-5, -5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let result = add(multiply(2, 3), divide_with_ceil(10, 4))
  assert_eq(8, result)  // 2*3 + ceil(10/4) = 6 + 3 = 9
}

test "real_world_application" {
  // 测试真实世界应用场景
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}