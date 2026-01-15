// Azimuth 项目新增 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试项目核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "greet_function_basic" {
  // 测试基本的 greet 函数
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试特殊乘法情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 10
  let width = 5
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(30, perimeter)
}

test "string_processing_with_special_chars" {
  // 测试包含特殊字符的字符串处理
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
}

test "financial_calculation_simple" {
  // 测试简单金融计算：简化版复利
  let principal = 1000
  let rate = 5
  let years = 2
  
  // 简化复利：本金 × (1 + 利率 × 年数)
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(11000, total)  // 1000 × (1 + 5×2) = 11000
}

test "error_handling_validation" {
  // 测试错误处理和验证
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 测试边界条件
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(0, max_val) == 0)
}

test "real_world_application" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  // 计算总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(612, total)  // 199×3 + 15 = 612
}