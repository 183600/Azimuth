// 标准 MoonBit 测试用例
// 为 Azimuth 项目提供高质量的核心功能测试

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(5, @azimuth.add(-8, 13))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "greeting_functionality" {
  // 问候功能测试
  @azimuth.assert_eq_string("Hello, Azimuth!", @azimuth.greet("Azimuth"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "ceil_division_basic" {
  // 向上取整除法基本测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "negative_number_handling" {
  // 负数处理测试
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-5, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "practical_packaging_scenario" {
  // 实际包装场景测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(items, capacity_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, capacity_per_box)
  @azimuth.assert_eq(48, total_capacity)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 3
  let divisor = 4
  let addition = 7
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(19, result)  // ceil(15*3/4) + 7 = 12 + 7 = 19
}

test "edge_case_boundary_values" {
  // 边界值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(99, 100))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(10000, 100))
  @azimuth.assert_eq(101, @azimuth.divide_with_ceil(10001, 100))
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
}