// 用户添加的 MoonBit 测试用例 - 覆盖核心功能

// 测试加法交换律
test "add_commutative_property" {
  let a = 123
  let b = 456
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  @azimuth.assert_eq(result1, result2)
}

// 测试乘法结合律
test "multiply_associative_property" {
  let a = 5
  let b = 10
  let c = 15
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result1, result2)
}

// 测试分配律
test "distributive_property" {
  let a = 7
  let b = 8
  let c = 9
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

// 测试字符串处理特殊字符
test "greet_special_characters" {
  let special_chars = "!@#$%^&*()"
  let result = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", result)
}

// 测试温度转换场景
test "temperature_conversion" {
  // 摄氏度转华氏度：F = C * 2 + 30 (简化版)
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}

// 测试银行利息计算
test "bank_interest_calculation" {
  let principal = 10000
  let rate = 5
  let years = 3
  // 简化利息计算：本金 + 本金 * 利率 * 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)
}

// 测试数组索引计算
test "array_index_calculation" {
  let row = 3
  let col = 4
  let width = 10
  // 二维数组转一维数组的索引：index = row * width + col
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(34, index)
}

// 测试阶乘计算
test "factorial_calculation" {
  // 计算5的阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let result = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, result)
}

// 测试几何面积计算
test "geometry_area_calculation" {
  // 计算矩形面积
  let length = 12
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

// 测试边界值组合
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试最大值与负一相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值与零相加
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}