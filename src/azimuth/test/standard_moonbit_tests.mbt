// MoonBit 标准测试用例
// 为 Azimuth 项目添加的新测试用例

test "basic_arithmetic_commutativity" {
  // 测试基本算术交换律
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "identity_elements_properties" {
  // 测试单位元性质
  let x = 42
  
  // 加法单位元: x + 0 = x
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法单位元: x * 1 = x
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  
  // 乘法零元: x * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, x))
}

test "distributive_property_validation" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  // 空字符串测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符测试
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 包含数字的字符串
  @azimuth.assert_eq_string("Hello, Test123!", @azimuth.greet("Test123"))
}

test "boundary_value_arithmetic" {
  // 测试边界值算术运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加法溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 最小值减法溢出测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 最大值乘法溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值特殊乘法测试
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列前n项和: 1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
  
  // 计算等比数列前n项和: 1 + 2 + 4 + 8 + 16 = 31
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 4), 8), 16)
  @azimuth.assert_eq(31, geo_sum)
  
  // 计算平方和: 1² + 2² + 3² + 4² + 5² = 55
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4)), @azimuth.multiply(5, 5))
  @azimuth.assert_eq(55, square_sum)
}

test "real_world_application_calculations" {
  // 测试实际应用计算
  // 场景1: 计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(96, area)
  @azimuth.assert_eq(40, perimeter)
  
  // 场景2: 简单利息计算
  let principal = 10000
  let rate = 5
  let time = 3
  
  let simple_interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time)) / 100
  let total_amount = @azimuth.add(principal, simple_interest)
  
  @azimuth.assert_eq(1500, simple_interest)
  @azimuth.assert_eq(11500, total_amount)
}

test "complex_nested_calculations" {
  // 测试复杂嵌套计算
  // 计算: (a + b) * (c - d) + e * f
  let a = 10
  let b = 15
  let c = 20
  let d = 8
  let e = 6
  let f = 7
  
  let sum_ab = @azimuth.add(a, b)  // 25
  let diff_cd = @azimuth.add(c, -d)  // 12
  let product_ef = @azimuth.multiply(e, f)  // 42
  
  let result = @azimuth.add(@azimuth.multiply(sum_ab, diff_cd), product_ef)  // 25 * 12 + 42 = 342
  
  @azimuth.assert_eq(342, result)
  
  // 计算: a² + b² - 2*a*b (实际上是 (a-b)²)
  let a_squared = @azimuth.multiply(a, a)
  let b_squared = @azimuth.multiply(b, b)
  let two_ab = @azimuth.multiply(@azimuth.multiply(2, a), b)
  
  let square_diff = @azimuth.add(@azimuth.add(a_squared, b_squared), -two_ab)
  @azimuth.assert_eq(25, square_diff)  // (10-15)² = 25
}