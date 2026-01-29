// 标准 MoonBit 测试用例 - 覆盖 azimuth 核心功能
// 测试文件：standard_azimuth_core_tests.mbt

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-4, multiply(-2, 2))
}

test "greeting_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "ceil_division_basic" {
  // 测试向上取整除法基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "ceil_division_zero_handling" {
  // 测试除零处理
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "subtraction_functionality" {
  // 测试减法功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "complex_arithmetic_combination" {
  // 测试复杂算术组合
  let result = add(multiply(2, 3), divide_with_ceil(10, 4))
  assert_eq(9, result)  // 2*3 + ceil(10/4) = 6 + 3 = 9
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "edge_case_scenarios" {
  // 测试边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq("Hello, a!", greet("a"))
}

test "business_logic_packaging" {
  // 测试业务逻辑：包装计算
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}