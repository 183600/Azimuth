// 额外的标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-3, @azimuth.add(5, -8))
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-24, @azimuth.multiply(4, -6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "string_concatenation_basic" {
  // 测试基本字符串连接功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 17
  let b = 23
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数的交换律
  let x = -15
  let y = 8
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
}

test "edge_case_zero_operations" {
  // 测试零值边界情况
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
  @azimuth.assert_eq(100, @azimuth.add(0, 100))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  
  @azimuth.assert_eq(-100, @azimuth.multiply(-100, 1))
  @azimuth.assert_eq(100, @azimuth.multiply(-100, -1))
}

test "negative_number_calculations" {
  // 测试负数计算
  @azimuth.assert_eq(-25, @azimuth.add(-10, -15))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(-5, @azimuth.add(10, -15))
  
  @azimuth.assert_eq(150, @azimuth.multiply(-10, -15))
  @azimuth.assert_eq(-150, @azimuth.multiply(-10, 15))
  @azimuth.assert_eq(-150, @azimuth.multiply(10, -15))
}

test "large_number_handling" {
  // 测试大数处理
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(2000000, @azimuth.multiply(large_num1, 2))
  
  // 测试与边界值的大数运算
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1000000))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
}

test "overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "real_world_application" {
  // 测试实际应用场景：计算折扣价格
  let original_price = 299
  let discount_percentage = 20
  let quantity = 3
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  // 计算折扣后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  // 计算总价
  let total_price = @azimuth.multiply(discounted_price, quantity)
  
  @azimuth.assert_eq(717, total_price)  // (299 - 60) * 3 = 239 * 3 = 717
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  let one = 1
  let neg_one = -1
  
  // 最大值与特殊值的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, neg_one))
  
  // 最小值与特殊值的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, neg_one))
  
  // 特殊值之间的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

test "error_handling_validation" {
  // 测试错误处理验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证断言函数正常工作
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 验证溢出处理正确性
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 验证基本运算正确性
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_true(@azimuth.multiply(4, 6) == 24)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
}