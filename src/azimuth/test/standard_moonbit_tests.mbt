// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-4, @azimuth.multiply(-2, 2))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "add_commutative_property" {
  // 测试加法交换律
  let a = 7
  let b = 13
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
}

test "multiply_commutative_property" {
  // 测试乘法交换律
  let x = 6
  let y = 8
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
}

test "negative_number_division" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-4, 2))
}

test "greet_special_characters" {
  // 测试特殊字符的问候
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "combined_operations" {
  // 测试组合运算
  let result = @azimuth.add(@azimuth.multiply(3, 4), @azimuth.multiply(2, 5))
  @azimuth.assert_eq(22, result)  // 3*4 + 2*5 = 12 + 10 = 22
}

test "practical_packaging_calculation" {
  // 实际应用：包装计算
  let total_items = 17
  let items_per_box = 5
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(4, boxes_needed)  // ceil(17/5) = 4
}