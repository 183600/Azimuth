// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含 10 个简洁的测试用例，覆盖基本功能和边界情况

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 7))
  assert_eq(-12, multiply(-3, 4))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive" {
  // 测试正数的向上取整除法
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "zero_operations" {
  // 测试与零相关的运算
  assert_eq(0, multiply(7, 0))
  assert_eq(7, add(7, 0))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "mathematical_properties" {
  // 测试基本数学性质
  let a = 6
  let b = 9
  assert_eq(add(a, b), add(b, a))  // 加法交换律
  assert_eq(multiply(a, b), multiply(b, a))  // 乘法交换律
}

test "combined_operations" {
  // 测试组合运算
  let result = add(multiply(2, 4), multiply(3, 5))
  assert_eq(23, result)  // 2*4 + 3*5 = 8 + 15 = 23
}

test "practical_application" {
  // 实际应用场景：计算包装需求
  let total_items = 17
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)  // ceil(17/5) = 4
}

test "unicode_support" {
  // 测试 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
}