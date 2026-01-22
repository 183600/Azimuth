// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_addition_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
}

test "basic_multiplication_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
  @azimuth.assert_eq(-8, @azimuth.multiply(-2, 4))
}

test "greeting_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
}

test "divide_with_ceil_basic" {
  // 测试带向上取整的除法基本功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的带向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-8, -3))
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(5, 8), @azimuth.add(8, 5))
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(4, 7), @azimuth.multiply(7, 4))
}

test "zero_element_tests" {
  // 测试零元素性质
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
}

test "complex_calculation" {
  // 测试复杂计算
  let step1 = @azimuth.multiply(3, 4)
  let step2 = @azimuth.multiply(2, 5)
  let result = @azimuth.add(step1, step2)
  @azimuth.assert_eq(22, result)  // 3*4 + 2*5 = 12 + 10 = 22
}

test "packaging_calculation" {
  // 测试实际应用场景：包装计算
  let total_items = 17
  let items_per_package = 5
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  @azimuth.assert_eq(4, packages_needed)  // ceil(17/5) = 4
}

test "unicode_support" {
  // 测试 Unicode 支持
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}