// Azimuth 标准测试用例 - 10个高质量测试用例
// 使用标准 MoonBit 测试语法

test "add_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(15, -5))
}

test "multiply_basic_functionality" {
  // 测试乘法函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "greet_string_functionality" {
  // 测试问候函数的字符串处理
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试向上取整除法的正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

test "subtract_basic_functionality" {
  // 测试减法函数的基本功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
  assert_eq(0, subtract(5, 5))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = divide_with_ceil(add(10, 15), 4)
  assert_eq(7, result2)  // ceil(25/4) = 7
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和边缘情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, multiply(10, 10))
  assert_eq(1000, add(500, 500))
}

test "real_world_application_scenarios" {
  // 测试真实世界应用场景
  // 包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(30, total_capacity)
}