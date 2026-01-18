// 用户标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含10个基础测试用例

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(@azimuth.add(2, 3), 5)
  @azimuth.assert_eq(@azimuth.add(-2, 2), 0)
  @azimuth.assert_eq(@azimuth.add(-2, -3), -5)
}

test "add_zero_handling" {
  // 测试零值处理
  @azimuth.assert_eq(@azimuth.add(5, 0), 5)
  @azimuth.assert_eq(@azimuth.add(0, 5), 5)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(@azimuth.multiply(2, 3), 6)
  @azimuth.assert_eq(@azimuth.multiply(2, -3), -6)
  @azimuth.assert_eq(@azimuth.multiply(-2, -3), 6)
}

test "multiply_identity_elements" {
  // 测试乘法单位元
  @azimuth.assert_eq(@azimuth.multiply(5, 1), 5)
  @azimuth.assert_eq(@azimuth.multiply(1, 5), 5)
  @azimuth.assert_eq(@azimuth.multiply(5, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, 5), 0)
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string(@azimuth.greet("World"), "Hello, World!")
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
}

test "greet_edge_cases" {
  // 测试问候函数边界情况
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet("A"), "Hello, A!")
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与0的运算
  @azimuth.assert_eq(@azimuth.add(max_val, 0), max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, 0), min_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 0), 0)
}

test "practical_calculation" {
  // 实际计算场景：计算矩形面积
  let length = 8
  let width = 6
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(area, 48)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(perimeter, 28)
}

test "complex_expression" {
  // 复杂表达式测试
  let a = 10
  let b = 5
  let c = 3
  
  // 计算 (a + b) * c
  let result = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(result, 45)
  
  // 计算 a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(result2, 53)
}