// Azimuth 核心功能测试用例
// 包含基本功能的简洁测试，使用标准 MoonBit 测试语法

test "essential_add_function" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
}

test "essential_multiply_function" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-8, @azimuth.multiply(-2, 4))
}

test "essential_greet_function" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "essential_divide_with_ceil_positive" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
}

test "essential_divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "essential_zero_operations" {
  // 测试与零相关的运算
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "essential_combined_operations" {
  // 测试组合运算
  let result = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "essential_practical_packaging" {
  // 实际应用场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "essential_greeting_unicode" {
  // 测试 Unicode 字符的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "essential_mathematical_properties" {
  // 测试基本数学性质
  let a = 7
  let b = 13
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}