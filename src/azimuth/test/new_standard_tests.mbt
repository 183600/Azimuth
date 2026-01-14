// 新的标准 MoonBit 测试用例
// 覆盖 azimuth 项目的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-7, @azimuth.add(-4, -3))
}

test "addition_with_zero" {
  // 测试与0相加的特殊情况
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "addition_boundary_values" {
  // 测试加法的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(16, @azimuth.multiply(-4, -4))
}

test "multiplication_with_zero_and_one" {
  // 测试乘法中的0和1特殊情况
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 456))
  @azimuth.assert_eq(789, @azimuth.multiply(789, 1))
  @azimuth.assert_eq(789, @azimuth.multiply(1, 789))
}

test "multiplication_boundary_values" {
  // 测试乘法的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, user@123!", @azimuth.greet("user@123"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
}

test "assert_functions_verification" {
  // 测试断言函数本身
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("success", "success")
}