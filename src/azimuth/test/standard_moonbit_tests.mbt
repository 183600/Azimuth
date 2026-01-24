// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含 10 个简洁的测试用例，覆盖基本功能和边界情况

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 7))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_positive" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-8, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "zero_operations" {
  // 测试与零相关的运算
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
}

test "mathematical_properties" {
  // 测试基本数学性质
  let a = 6
  let b = 9
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))  // 加法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))  // 乘法交换律
}

test "combined_operations" {
  // 测试组合运算
  let result = @azimuth.add(@azimuth.multiply(2, 4), @azimuth.multiply(3, 5))
  @azimuth.assert_eq(23, result)  // 2*4 + 3*5 = 8 + 15 = 23
}

test "practical_application" {
  // 实际应用场景：计算包装需求
  let total_items = 17
  let items_per_box = 5
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(4, boxes_needed)  // ceil(17/5) = 4
}

test "unicode_support" {
  // 测试 Unicode 支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
}