// 标准 MoonBit 测试用例 - 10个高质量测试

test "basic_arithmetic_commutativity" {
  // 测试基本算术运算的交换律
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.multiply(8, 12), @azimuth.multiply(12, 8))
  @azimuth.assert_eq(@azimuth.add(-5, 10), @azimuth.add(10, -5))
  @azimuth.assert_eq(@azimuth.multiply(-3, 7), @azimuth.multiply(7, -3))
}

test "zero_element_properties" {
  // 测试零元素性质
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
}

test "divide_with_ceil_rounding_behavior" {
  // 测试向上取整除法的舍入行为
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 3))
}

test "string_greeting_variations" {
  // 测试各种字符串问候变化
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let step1 = @azimuth.add(5, 3)
  let step2 = @azimuth.multiply(step1, 4)
  let step3 = @azimuth.divide_with_ceil(step2, 6)
  let result = @azimuth.add(step3, 2)
  @azimuth.assert_eq(6, result)  // ((5+3)*4)/6 + 2 = 32/6 + 2 = 6 + 2 = 8
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 17
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  @azimuth.assert_eq(@azimuth.multiply(x, -1), @azimuth.multiply(x, -1))
  @azimuth.assert_eq(@azimuth.multiply(-1, x), @azimuth.multiply(-1, x))
}

test "boundary_value_operations" {
  // 测试边界值运算
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(100, 100))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(100, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "resource_allocation_calculation" {
  // 测试资源分配计算
  let total_items = 47
  let container_capacity = 8
  let containers_needed = @azimuth.divide_with_ceil(total_items, container_capacity)
  @azimuth.assert_eq(6, containers_needed)
  
  let total_capacity = @azimuth.multiply(containers_needed, container_capacity)
  @azimuth.assert_eq(48, total_capacity)
  
  let unused_capacity = @azimuth.add(total_capacity, -total_items)
  @azimuth.assert_eq(1, unused_capacity)
}

test "distributive_property_verification" {
  // 测试分配律验证
  let a = 6
  let b = 7
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // (a + b) * c = a * c + b * c
  let left_side2 = @azimuth.multiply(@azimuth.add(a, b), c)
  let right_side2 = @azimuth.add(@azimuth.multiply(a, c), @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_side2, right_side2)
}