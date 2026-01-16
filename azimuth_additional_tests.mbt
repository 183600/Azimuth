// Azimuth 项目附加测试用例
// 使用标准 MoonBit 测试语法，补充更多边界情况和实际应用场景

test "add_function_zero_boundary" {
  // 测试加法零边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
}

test "multiply_function_zero_boundary" {
  // 测试乘法零边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 456))
  @azimuth.assert_eq(0, @azimuth.multiply(-789, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -789))
}

test "multiply_function_identity_boundary" {
  // 测试乘法单位元边界情况
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(1, -17))
}

test "add_function_negative_numbers" {
  // 测试负数加法的各种组合
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-10, 10))
  @azimuth.assert_eq(-5, @azimuth.add(-15, 10))
  @azimuth.assert_eq(5, @azimuth.add(15, -10))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
}

test "multiply_function_negative_numbers" {
  // 测试负数乘法的各种组合
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-50, @azimuth.multiply(-5, 10))
  @azimuth.assert_eq(-50, @azimuth.multiply(10, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(0, @azimuth.multiply(-10, 0))
}

test "sequential_operations" {
  // 测试连续运算
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  let result3 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result3)
}

test "mixed_operations" {
  // 测试混合运算
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(45, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(53, result2)
  
  // a + b * c
  let result3 = @azimuth.add(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(25, result3)
}

test "practical_calculation_examples" {
  // 测试实际计算例子
  
  // 简单利息计算: 本金 * 利率 + 本金
  let principal = 1000
  let rate = 5
  let interest = @azimuth.multiply(principal, rate) / 100
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(1050, total)
  
  // 矩形周长: 2 * (长 + 宽)
  let length = 8
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
  
  // 梯形面积: (上底 + 下底) * 高 / 2
  let top = 6
  let bottom = 10
  let height = 4
  let area = @azimuth.multiply(@azimuth.add(top, bottom), height) / 2
  @azimuth.assert_eq(32, area)
}

test "boundary_value_testing" {
  // 测试边界值
  let small_positive = 1
  let small_negative = -1
  let medium_number = 100
  
  // 最小正数测试
  @azimuth.assert_eq(2, @azimuth.add(small_positive, small_positive))
  @azimuth.assert_eq(1, @azimuth.multiply(small_positive, small_positive))
  
  // 最大负数测试
  @azimuth.assert_eq(-2, @azimuth.add(small_negative, small_negative))
  @azimuth.assert_eq(1, @azimuth.multiply(small_negative, small_negative))
  
  // 混合边界测试
  @azimuth.assert_eq(0, @azimuth.add(small_positive, small_negative))
  @azimuth.assert_eq(-1, @azimuth.multiply(small_positive, small_negative))
}