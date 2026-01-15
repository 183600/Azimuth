// 新的 MoonBit 测试用例
// 测试 azimuth 项目的核心功能

// 测试基本的数学运算
test "add_function_basic_operations" {
  // 测试基本加法运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

// 测试乘法运算
test "multiply_function_basic_operations" {
  // 测试基本乘法运算
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(16, @azimuth.multiply(4, 4))
}

// 测试字符串拼接功能
test "greet_function_various_inputs" {
  // 测试不同类型的输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
}

// 测试边界值处理
test "boundary_value_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试零值
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

// 测试复杂计算场景
test "complex_calculation_scenarios" {
  // 测试矩形面积计算
  let length = 12
  let width = 8
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 测试周长计算
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 测试连续运算
  let result = @azimuth.add(@azimuth.multiply(3, 4), @azimuth.multiply(5, 6))
  @azimuth.assert_eq(42, result)
}

// 测试实际应用场景
test "real_world_scenarios" {
  // 测试购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(595, cart_total)
  
  // 测试温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}

// 测试错误处理和溢出保护
test "overflow_and_error_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊值处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(-1, min_val))
}

// 测试字符串边界情况
test "string_edge_cases" {
  // 测试特殊字符
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, 用户@123!", @azimuth.greet("用户@123"))
  
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongNameForTestingPurposes!", @azimuth.greet(long_name))
}

// 测试数学性质
test "mathematical_properties" {
  let a = 7
  let b = 11
  let c = 5
  
  // 测试加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试分配律
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

// 测试性能相关计算
test "performance_calculations" {
  // 测试大量数据的计算
  let base = 1000
  let count = 100
  
  // 计算 1+2+...+100 = 5050
  let sum = @azimuth.multiply(count, @azimuth.add(count, 1)) / 2
  @azimuth.assert_eq(5050, sum)
  
  // 计算 base * sum
  let total = @azimuth.multiply(base, sum)
  @azimuth.assert_eq(5050000, total)
  
  // 测试连续运算
  let result = @azimuth.add(
    @azimuth.multiply(100, 100),
    @azimuth.add(@azimuth.multiply(200, 200), @azimuth.multiply(300, 300))
  )
  @azimuth.assert_eq(140000, result)
}