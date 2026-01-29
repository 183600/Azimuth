// 新的增强测试用例
// 使用标准 MoonBit 测试语法编写的测试用例

test "new_arithmetic_operations_test" {
  // 算术运算测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  // 使用加法和负数代替减法
  @azimuth.assert_eq(4, @azimuth.add(10, -6))
}

test "new_string_processing_test" {
  // 字符串处理测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "new_business_calculation_test" {
  // 业务计算测试
  let items = 47
  let capacity = 12
  let boxes_needed = @azimuth.divide_with_ceil(items, capacity)
  @azimuth.assert_eq(4, boxes_needed)
  
  let total_capacity = @azimuth.multiply(boxes_needed, capacity)
  @azimuth.assert_eq(48, total_capacity)
  
  // 使用加法和负数代替减法
  let remaining = @azimuth.add(total_capacity, -items)
  @azimuth.assert_eq(1, remaining)
}

test "new_error_handling_test" {
  // 错误处理测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(10, 0))
}

test "new_complex_calculation_test" {
  // 复杂计算测试
  let base = 15
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  let step1 = @azimuth.multiply(base, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(17, result)  // ceil(15*3/4) + 5 = 12 + 5 = 17
}