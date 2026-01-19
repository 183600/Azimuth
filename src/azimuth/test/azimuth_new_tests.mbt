// 新增的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

// 测试基本加法运算
test "add_basic_functionality" {
  let result = azimuth.add(10, 20)
  @assertion.assert_eq(30, result)
}

// 测试乘法运算的边界情况
test "multiply_boundary_conditions" {
  // 测试零值
  @assertion.assert_eq(0, azimuth.multiply(0, 12345))
  @assertion.assert_eq(0, azimuth.multiply(12345, 0))
  
  // 测试单位值
  @assertion.assert_eq(12345, azimuth.multiply(1, 12345))
  @assertion.assert_eq(12345, azimuth.multiply(12345, 1))
  
  // 测试负值
  @assertion.assert_eq(-12345, azimuth.multiply(-1, 12345))
  @assertion.assert_eq(12345, azimuth.multiply(-1, -12345))
}

// 测试字符串拼接功能
test "greet_functionality" {
  @assertion.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))
  @assertion.assert_eq_string("Hello, 测试123!", azimuth.greet("测试123"))
}

// 测试向上取整除法
test "divide_with_ceil_function" {
  // 测试整除情况
  @assertion.assert_eq(5, azimuth.divide_with_ceil(10, 2))
  @assertion.assert_eq(3, azimuth.divide_with_ceil(9, 3))
  
  // 测试需要向上取整的情况
  @assertion.assert_eq(6, azimuth.divide_with_ceil(11, 2))
  @assertion.assert_eq(4, azimuth.divide_with_ceil(10, 3))
  
  // 测试负数情况
  @assertion.assert_eq(-4, azimuth.divide_with_ceil(-10, 3))
}

// 测试断言函数
test "assertion_functions" {
  // 测试布尔断言
  @assertion.assert_true(true)
  @assertion.assert_false(false)
  
  // 测试整数断言
  @assertion.assert_eq(42, 42)
  
  // 测试字符串断言
  @assertion.assert_eq_string("test", "test")
}

// 测试复合运算场景
test "complex_calculation_scenario" {
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  @assertion.assert_eq(26, perimeter)
}

// 测试溢出保护
test "overflow_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @assertion.assert_eq(max_val, azimuth.add(max_val, 1))
  @assertion.assert_eq(min_val, azimuth.add(min_val, -1))
  
  // 测试乘法溢出保护
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 2))
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))
}

// 测试实际应用场景
test "real_world_application" {
  // 模拟购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  // 计算商品总价
  let subtotal = azimuth.multiply(item_price, quantity)
  // 计算最终总价（含运费）
  let total = azimuth.add(subtotal, shipping)
  
  @assertion.assert_eq(612, total)  // 199*3 + 15 = 597 + 15 = 612
}

// 测试边界值组合
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @assertion.assert_eq(-1, azimuth.add(max_val, min_val))
  @assertion.assert_eq(0, azimuth.multiply(max_val, 0))
  @assertion.assert_eq(0, azimuth.multiply(min_val, 0))
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 1))
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 1))
}