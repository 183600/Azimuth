// 标准 MoonBit 测试用例 - 为 azimuth 项目添加
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "multiply_function_properties" {
  // 测试乘法函数特性
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  assert_eq(add(3, 7), add(7, 3))
  assert_eq(multiply(3, 7), multiply(7, 3))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let base = 5
  let doubled = multiply(base, 2)
  let incremented = add(doubled, 3)
  let result = divide_with_ceil(incremented, 4)
  assert_eq(4, result)  // ceil((5*2 + 3)/4) = ceil(13/4) = 4
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "edge_cases_and_boundaries" {
  // 测试边界情况和边缘条件
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
}