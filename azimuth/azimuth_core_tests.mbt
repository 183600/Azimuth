// Azimuth 核心功能测试用例
// 标准 MoonBit 测试语法，包含10个高质量测试用例

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
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "ceil_division_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "ceil_division_zero_handling" {
  // 测试除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let result = add(multiply(3, 4), divide_with_ceil(10, 3))
  assert_eq(16, result)  // 3*4 + ceil(10/3) = 12 + 4 = 16
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "edge_case_values" {
  // 测试边界值
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(1000, multiply(100, 10))
  assert_eq(100, add(50, 50))
}

test "real_world_packaging_problem" {
  // 真实世界包装问题
  let items = 25
  let capacity = 6
  let boxes_needed = divide_with_ceil(items, capacity)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}