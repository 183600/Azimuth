// 标准 MoonBit 测试用例 - 为 azimuth 库添加的高质量测试
// 包含10个标准测试用例，覆盖基本功能和实际应用场景

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(7, 3))
  assert_eq(-10, add(-7, -3))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
  assert_eq(25, multiply(-5, -5))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "divide_with_ceil_ceil_behavior" {
  // 测试向上取整除法的取整行为
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
  assert_eq(add(-5, 10), add(10, -5))
  assert_eq(multiply(-4, 6), multiply(6, -4))
}

test "zero_element_properties" {
  // 测试零元素性质
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "complex_arithmetic_operations" {
  // 测试复杂算术运算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = divide_with_ceil(add(10, 15), 5)
  assert_eq(5, result2)  // ceil(25/5) = 5
}

test "real_world_packaging_scenario" {
  // 真实世界场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(30, total_capacity)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(15, multiply(-3, -5))
}

test "edge_case_boundary_conditions" {
  // 测试边界条件和边缘情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq_string("Hello, !", greet(""))
}