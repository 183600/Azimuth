// 标准 MoonBit 测试套件
// 针对 azimuth 库的核心功能测试

test "add_function_basic_test" {
  // 基本加法功能测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "multiply_function_basic_test" {
  // 基本乘法功能测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(0, 10))
  assert_eq(-15, multiply(-3, 5))
}

test "greet_function_basic_test" {
  // 基本问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive_test" {
  // 正数向上取整除法测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative_test" {
  // 负数向上取整除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-11, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "zero_operations_test" {
  // 零值运算测试
  assert_eq(0, multiply(7, 0))
  assert_eq(7, add(7, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "large_number_test" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "combined_operations_test" {
  // 组合运算测试
  let result = add(multiply(2, 4), multiply(3, 5))
  assert_eq(23, result)  // 2*4 + 3*5 = 8 + 15 = 23
}

test "practical_packaging_test" {
  // 实际包装计算测试
  let total_items = 28
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(28/6) = 5
}

test "unicode_greeting_test" {
  // Unicode 字符问候测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}