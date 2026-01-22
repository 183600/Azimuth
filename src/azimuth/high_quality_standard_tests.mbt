// 高质量标准 MoonBit 测试用例 - 简化版本
// 针对 azimuth 库核心功能的基本测试

test "basic_addition_test" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "basic_multiplication_test" {
  // 基本乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-4, multiply(-2, 2))
}

test "basic_greet_test" {
  // 基本问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "basic_divide_with_ceil_test" {
  // 基本向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties_test" {
  // 数学性质测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "edge_cases_test" {
  // 边界情况测试
  assert_eq(0, multiply(0, 0))
  assert_eq(123, add(123, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "practical_application_test" {
  // 实际应用测试：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "string_processing_test" {
  // 字符串处理测试
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_test" {
  // 复杂计算测试
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "negative_number_test" {
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-5, add(-2, -3))
}