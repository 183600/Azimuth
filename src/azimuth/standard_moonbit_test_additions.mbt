// 新增的 MoonBit 标准测试用例
// 测试核心功能：add, multiply, greet 等函数

test "add_basic_functionality" {
  // 测试基本的加法功能
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(-1, azimuth::add(5, -6))
  azimuth::assert_eq(100, azimuth::add(50, 50))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_test" {
  // 测试问候函数
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
}

test "add_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "complex_calculations" {
  // 测试复合计算
  // 计算 (2 + 3) * 4 = 20
  let sum = add(2, 3)
  let result = multiply(sum, 4)
  assert_eq(20, result)
  
  // 计算 10 * (5 + 3) = 80
  let sum2 = add(5, 3)
  let result2 = multiply(10, sum2)
  assert_eq(80, result2)
}

test "string_processing_variations" {
  // 测试各种字符串处理情况
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, emoji🚀!", greet("emoji🚀"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律 a * (b + c) = a * b + a * c
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}