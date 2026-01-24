// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(8, -3))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-2, divide_with_ceil(-4, 2))
}

test "zero_operations" {
  // 测试与零相关的运算
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "large_number_operations" {
  // 测试大数运算
  assert_eq(1000000, add(500000, 500000))
  assert_eq(10000, multiply(100, 100))
  assert_eq(334, divide_with_ceil(1000, 3))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = divide_with_ceil(add(multiply(3, 4), multiply(2, 5)), 3)
  assert_eq(8, result2)  // ceil((12 + 10) / 3) = ceil(22/3) = 8
}

test "practical_packaging" {
  // 实际应用场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "greeting_with_special_characters" {
  // 测试特殊字符的问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
}