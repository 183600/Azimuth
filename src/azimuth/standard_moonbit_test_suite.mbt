// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的简洁测试

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

test "basic_greet_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试基本向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative_laws" {
  // 验证加法和乘法交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_identity_properties" {
  // 验证单位元性质
  assert_eq(5, add(5, 0))
  assert_eq(7, multiply(7, 1))
  assert_eq(0, multiply(0, 100))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "practical_packaging_calculation" {
  // 实际应用：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "unicode_greeting_support" {
  // 测试 Unicode 字符串问候支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}