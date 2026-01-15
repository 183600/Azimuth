// 新增的标准 MoonBit 测试用例
// 测试 azimuth 包中的核心功能

test "integer_arithmetic_operations" {
  // 测试整数算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-2, @azimuth.add(5, -7))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
}

test "boundary_value_multiplication" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, -1))
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, 特殊字符@#$!", @azimuth.greet("特殊字符@#$"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "real_world_shopping_cart" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  let total = @azimuth.add(subtotal1, subtotal2)
  
  @azimuth.assert_eq(697, total)
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化版）：F = C * 2 + 30
  
  // 冰点
  @azimuth.assert_eq(30, @azimuth.add(@azimuth.multiply(0, 2), 30))
  
  // 室温
  @azimuth.assert_eq(70, @azimuth.add(@azimuth.multiply(20, 2), 30))
  
  // 沸点
  @azimuth.assert_eq(230, @azimuth.add(@azimuth.multiply(100, 2), 30))
}

test "simple_calculator_expressions" {
  // 测试简单计算器表达式
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let expr1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(45, expr1)
  
  // a * b + c
  let expr2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(53, expr2)
}

test "assertion_functions_test" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 6
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(28, perimeter)
}