// 标准 MoonBit 测试套件
// 包含基本的测试用例，验证核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-2, -3), -5)
}

test "add_with_zero" {
  // 测试与0相加的情况
  assert_eq(add(10, 0), 10)
  assert_eq(add(0, 10), 10)
  assert_eq(add(0, 0), 0)
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(add(5, -5), 0)
  assert_eq(add(-3, -5), -8)
  assert_eq(add(3, -5), -2)
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(multiply(2, 3), 6)
  assert_eq(multiply(5, 0), 0)
  assert_eq(multiply(1, 1), 1)
}

test "multiply_with_negative_numbers" {
  // 测试负数乘法
  assert_eq(multiply(2, -3), -6)
  assert_eq(multiply(-2, -3), 6)
  assert_eq(multiply(-5, 0), 0)
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string(greet("测试"), "Hello, 测试!")
  assert_eq_string(greet("User123"), "Hello, User123!")
  assert_eq_string(greet("@#$"), "Hello, @#$!")
}

test "boundary_values_add" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(add(max_val, 1), max_val)
  assert_eq(add(min_val, -1), min_val)
  assert_eq(add(max_val, min_val), -1)
}

test "boundary_values_multiply" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(multiply(max_val, 0), 0)
  assert_eq(multiply(min_val, 0), 0)
  assert_eq(multiply(min_val, -1), min_val)
}