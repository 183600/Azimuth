// Azimuth 新增标准测试用例
// 使用标准 MoonBit 测试语法，包含5个高质量测试用例
// 覆盖各种场景、边界情况和实际应用

test "add_function_basic" {
  // 基本加法功能测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
}

test "multiply_function_basic" {
  // 基本乘法功能测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "divide_with_ceil_basic" {
  // 向上取整除法基本测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_basic" {
  // 基本问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "subtract_function_basic" {
  // 基本减法功能测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}