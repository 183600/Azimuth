// 标准 MoonBit 测试套件
// 包含10个高质量的测试用例，测试 azimuth 库的核心功能

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_standard_cases" {
  // 测试向上取整除法的标准情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 8
  let b = 12
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_properties_identity" {
  // 测试数学单位元性质
  assert_eq(7, add(7, 0))
  assert_eq(7, multiply(7, 1))
  assert_eq(0, multiply(7, 0))
}

test "combined_operations_simple" {
  // 测试简单的组合运算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "real_world_application_packaging" {
  // 实际应用：包装计算
  let total_items = 23
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(23/5) = 5
}

test "string_greeting_with_special_characters" {
  // 测试特殊字符的问候
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}