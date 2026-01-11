// 标准 MoonBit 测试套件 - 包含10个全面的测试用例

// 测试1: 基础加法功能
///|
test "basic_addition_functionality" {
  assert_eq(@azimuth.add(2, 3), 5)
  assert_eq(@azimuth.add(-2, 3), 1)
  assert_eq(@azimuth.add(0, 5), 5)
  assert_eq(@azimuth.add(5, 0), 5)
  assert_eq(@azimuth.add(-3, -2), -5)
}

// 测试2: 基础乘法功能
///|
test "basic_multiplication_functionality" {
  assert_eq(@azimuth.multiply(2, 3), 6)
  assert_eq(@azimuth.multiply(-2, 3), -6)
  assert_eq(@azimuth.multiply(2, -3), -6)
  assert_eq(@azimuth.multiply(-2, -3), 6)
  assert_eq(@azimuth.multiply(0, 5), 0)
  assert_eq(@azimuth.multiply(5, 0), 0)
}

// 测试3: 边界值测试 - 最大值
///|
test "boundary_values_max" {
  let max_val = 2147483647
  assert_eq(@azimuth.add(max_val, 0), max_val)
  assert_eq(@azimuth.add(0, max_val), max_val)
  assert_eq(@azimuth.multiply(max_val, 1), max_val)
  assert_eq(@azimuth.multiply(1, max_val), max_val)
}

// 测试4: 边界值测试 - 最小值
///|
test "boundary_values_min" {
  let min_val = -2147483648
  assert_eq(@azimuth.add(min_val, 0), min_val)
  assert_eq(@azimuth.add(0, min_val), min_val)
  assert_eq(@azimuth.multiply(min_val, 1), min_val)
  assert_eq(@azimuth.multiply(1, min_val), min_val)
}

// 测试5: 溢出处理测试
///|
test "overflow_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(@azimuth.add(max_val, 1), max_val)
  assert_eq(@azimuth.add(1000000000, 1500000000), max_val)
  
  // 负数溢出测试
  assert_eq(@azimuth.add(min_val, -1), min_val)
  assert_eq(@azimuth.add(-1000000000, -1500000000), min_val)
  
  // 乘法溢出测试
  assert_eq(@azimuth.multiply(min_val, -1), min_val)
  assert_eq(@azimuth.multiply(min_val, 2), min_val)
}

// 测试6: 数学性质测试
///|
test "mathematical_properties" {
  // 加法恒等元
  assert_eq(@azimuth.add(5, 0), 5)
  assert_eq(@azimuth.add(0, 5), 5)
  assert_eq(@azimuth.add(-3, 0), -3)
  
  // 乘法恒等元
  assert_eq(@azimuth.multiply(5, 1), 5)
  assert_eq(@azimuth.multiply(1, 5), 5)
  assert_eq(@azimuth.multiply(-3, 1), -3)
  
  // 乘法零元
  assert_eq(@azimuth.multiply(5, 0), 0)
  assert_eq(@azimuth.multiply(0, 5), 0)
  assert_eq(@azimuth.multiply(-3, 0), 0)
}

// 测试7: 字符串处理测试
///|
test "string_processing" {
  assert_eq(@azimuth.greet("World"), "Hello, World!")
  assert_eq(@azimuth.greet(""), "Hello, !")
  assert_eq(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  assert_eq(@azimuth.greet("123"), "Hello, 123!")
}

// 测试8: 实际应用场景测试
///|
test "practical_scenarios" {
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 50
  let total = @azimuth.add(@azimuth.multiply(item_price, quantity), shipping)
  assert_eq(total, 647)
  
  // 温度转换（简化版）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9), 160) / 5
  assert_eq(fahrenheit, 77)
}

// 测试9: 特殊值组合测试
///|
test "special_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的组合
  assert_eq(@azimuth.add(max_val, min_val), -1)
  assert_eq(@azimuth.multiply(max_val, 0), 0)
  assert_eq(@azimuth.multiply(min_val, 0), 0)
  
  // -1的特殊处理
  assert_eq(@azimuth.multiply(-1, 1), -1)
  assert_eq(@azimuth.multiply(1, -1), -1)
  assert_eq(@azimuth.multiply(-1, -1), 1)
}

// 测试10: 性能和稳定性测试
///|
test "performance_and_stability" {
  // 连续运算测试
  let mut result = 0
  result = @azimuth.add(result, 1)
  result = @azimuth.add(result, 2)
  result = @azimuth.add(result, 3)
  assert_eq(result, 6)
  
  result = @azimuth.multiply(result, 2)
  assert_eq(result, 12)
  
  // 嵌套运算测试
  let complex_result = @azimuth.add(
    @azimuth.multiply(2, 3),
    @azimuth.multiply(4, 5)
  )
  assert_eq(complex_result, 26)
  
  // 字符串拼接稳定性
  let long_name = "VeryLongNameThatShouldNotCauseAnyIssuesWhenProcessed"
  assert_eq(@azimuth.greet(long_name), "Hello, " + long_name + "!")
}