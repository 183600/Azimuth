// 用户自定义的 MoonBit 测试用例
// 包含不超过10个标准测试用例

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(-5, -5))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
}

test "add_with_large_numbers" {
  // 测试大数加法
  assert_eq(2000000000, add(1000000000, 1000000000))
  assert_eq(-2000000000, add(-1000000000, -1000000000))
}

test "multiply_with_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}