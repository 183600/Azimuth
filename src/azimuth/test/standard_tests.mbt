// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，覆盖主要功能和边界情况

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
}

test "add_zero_handling" {
  // 测试零值处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.add(1000000000, 1500000000))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
}

test "multiply_special_values" {
  // 测试特殊值处理
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "greet_edge_cases" {
  // 测试问候功能边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 8
  let width = 6
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  @azimuth.assert_eq(48, area)
  @azimuth.assert_eq(28, perimeter)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律简化测试
  let left_side = @azimuth.multiply(a, @azimuth.add(b, 2))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 2))
  @azimuth.assert_eq(left_side, right_side)
}