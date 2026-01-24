// 标准 MoonBit 测试用例 - azimuth 库
// 包含 8 个高质量测试用例，涵盖核心功能

test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(2, -3))
  assert_eq(10, add(-5, 15))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
  assert_eq(25, multiply(-5, -5))
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "divide_with_ceil_basic" {
  // 测试基本向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法交换律
  assert_eq(add(5, 7), add(7, 5))
  // 乘法交换律
  assert_eq(multiply(5, 7), multiply(7, 5))
  // 加法恒等式
  assert_eq(7, add(7, 0))
  // 乘法恒等式
  assert_eq(0, multiply(7, 0))
}

test "complex_calculation" {
  // 测试复杂计算场景
  let result = add(multiply(3, 4), divide_with_ceil(10, 4))
  assert_eq(15, result)  // 12 + 3 = 15
  
  let business_calc = divide_with_ceil(multiply(8, 5), 12)
  assert_eq(4, business_calc)  // ceil(40/12) = 4
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "edge_cases_and_boundaries" {
  // 测试边界情况和边缘条件
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  
  // 负数测试
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-3, divide_with_ceil(10, -3))
}