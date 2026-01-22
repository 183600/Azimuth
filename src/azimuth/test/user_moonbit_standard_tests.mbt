// 用户定义的标准 MoonBit 测试用例
// 包含10个全面的测试用例，覆盖主要功能

test "standard_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(@azimuth.add(3, 7), 10)
  @azimuth.assert_eq(@azimuth.add(-5, 5), 0)
  @azimuth.assert_eq(@azimuth.add(-3, -5), -8)
  @azimuth.assert_eq(@azimuth.add(50, 50), 100)
}

test "standard_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(@azimuth.multiply(3, 5), 15)
  @azimuth.assert_eq(@azimuth.multiply(-3, 4), -12)
  @azimuth.assert_eq(@azimuth.multiply(8, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(5, 5), 25)
}

test "standard_greet_functionality" {
  // 测试字符串拼接功能
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet("用户123"), "Hello, 用户123!")
}

test "standard_boundary_values" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(@azimuth.add(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1), min_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1), min_val)
}

test "standard_mathematical_properties" {
  // 测试数学性质
  let a = 6
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 2)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 2)))
}

test "standard_complex_calculations" {
  // 测试复杂计算场景
  // 计算矩形周长：2 × (长 + 宽)
  let length = 8
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, area)
}

test "standard_assertion_functions" {
  // 测试断言函数
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("success", "success")
  
  // 测试条件断言
  let x = 10
  let y = 20
  @azimuth.assert_eq(true, x < y)
  @azimuth.assert_eq(0, 1)(x > y)
}

test "standard_real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let total = @azimuth.add(@azimuth.multiply(item_price, quantity), shipping)
  @azimuth.assert_eq(612, total)
  
  // 温度转换（简化版）：摄氏度转华氏度
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}

test "standard_error_handling" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  
  // 测试溢出保护
  @azimuth.assert_eq(@azimuth.add(max_val, 100, true) == max_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2, true) == max_val)
  
  // 测试零值处理
  @azimuth.assert_eq(@azimuth.multiply(0, max_val), 0)
  @azimuth.assert_eq(@azimuth.add(0, max_val), max_val)
}

test "standard_performance_calculations" {
  // 测试性能相关的计算
  // 计算等差数列和：1 + 2 + 3 + ... + 10
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
  
  // 计算等比数列和：2 + 4 + 8 + 16
  let geo_sum = @azimuth.add(2, @azimuth.add(4, @azimuth.add(8, 16)))
  @azimuth.assert_eq(30, geo_sum)
}