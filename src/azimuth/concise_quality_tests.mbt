// 简洁高质量 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能

test "add_basic_functionality" {
  // 基础加法功能测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "multiply_basic_functionality" {
  // 基础乘法功能测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(7, 0))
  assert_eq(-15, multiply(-3, 5))
}

test "greet_basic_functionality" {
  // 基础问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive_numbers" {
  // 正数向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative_numbers" {
  // 负数向上取整除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "zero_handling_operations" {
  // 零值处理测试
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "combined_arithmetic_operations" {
  // 组合算术运算测试
  let result = add(multiply(2, 4), multiply(3, 5))
  assert_eq(23, result)  // 2*4 + 3*5 = 8 + 15 = 23
}

test "practical_packaging_calculation" {
  // 实际包装计算场景
  let total_items = 17
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)  // ceil(17/5) = 4
}

test "unicode_greeting_functionality" {
  // Unicode 字符问候测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, áéíóú!", greet("áéíóú"))
}