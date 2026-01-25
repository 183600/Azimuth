// 标准 MoonBit 测试用例 - 10个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic" {
  // 测试基本加法功能
  @builtin.assert_eq(5, add(2, 3))
  @builtin.assert_eq(0, add(0, 0))
  @builtin.assert_eq(-5, add(-2, -3))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  @builtin.assert_eq(6, multiply(2, 3))
  @builtin.assert_eq(0, multiply(0, 5))
  @builtin.assert_eq(-6, multiply(-2, 3))
}

test "greet_function_basic" {
  // 测试基本问候功能
  @builtin.assert_eq("Hello, World!", greet("World"))
  @builtin.assert_eq("Hello, MoonBit!", greet("MoonBit"))
}

test "divide_with_ceil_positive" {
  // 测试正数向上取整除法
  @builtin.assert_eq(4, divide_with_ceil(10, 3))
  @builtin.assert_eq(3, divide_with_ceil(9, 3))
  @builtin.assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative" {
  // 测试负数向上取整除法
  @builtin.assert_eq(-3, divide_with_ceil(-10, 3))
  @builtin.assert_eq(4, divide_with_ceil(-10, -3))
  @builtin.assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative" {
  // 测试数学交换律
  @builtin.assert_eq(add(7, 13), add(13, 7))
  @builtin.assert_eq(multiply(7, 13), multiply(13, 7))
}

test "zero_element_properties" {
  // 测试零元素性质
  @builtin.assert_eq(5, add(5, 0))
  @builtin.assert_eq(0, multiply(5, 0))
}

test "complex_arithmetic" {
  // 测试复杂算术运算
  let result = add(multiply(2, 3), multiply(4, 5))
  @builtin.assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "packaging_calculation" {
  // 测试包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  @builtin.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "unicode_greeting" {
  // 测试Unicode字符支持
  @builtin.assert_eq("Hello, 世界!", greet("世界"))
  @builtin.assert_eq("Hello, 🚀!", greet("🚀"))
}