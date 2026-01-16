// 额外的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能和边界情况

test "add_complex_scenarios" {
  // 测试复杂加法场景
  @azimuth.assert_eq(100, @azimuth.add(45, 55))
  @azimuth.assert_eq(-100, @azimuth.add(-45, -55))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(50, @azimuth.add(-20, 70))
}

test "multiply_complex_scenarios" {
  // 测试复杂乘法场景
  @azimuth.assert_eq(1000, @azimuth.multiply(25, 40))
  @azimuth.assert_eq(-1000, @azimuth.multiply(-25, 40))
  @azimuth.assert_eq(1000, @azimuth.multiply(-25, -40))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, John_Doe!", @azimuth.greet("John_Doe"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "mathematical_expressions" {
  // 测试数学表达式计算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let sum_ab = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum_ab, c)
  @azimuth.assert_eq(900, result)
  
  // a * b + c
  let product_ab = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product_ab, c)
  @azimuth.assert_eq(230, result2)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与普通值的运算
  @azimuth.assert_eq(2147483647, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(max_val, 2))
  
  // 最小值与普通值的运算
  @azimuth.assert_eq(-2147483648, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(min_val, 2))
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "sequential_operations" {
  // 测试连续运算
  let initial = 100
  
  // 连续加法
  let result1 = @azimuth.add(@azimuth.add(initial, 50), 25)
  @azimuth.assert_eq(175, result1)
  
  // 连续乘法
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(24, result2)
  
  // 混合运算
  let result3 = @azimuth.add(@azimuth.multiply(10, 5), @azimuth.multiply(3, 4))
  @azimuth.assert_eq(62, result3)
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(697, cart_total)
  
  // 计算折扣价格（使用整数运算模拟9折）
  let discount_rate = 10
  let discount_amount = @azimuth.multiply(cart_total, discount_rate) / 100
  let final_price = @azimuth.add(cart_total, -discount_amount)
  
  @azimuth.assert_eq(627, final_price)
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串
  let long_name = "VeryLongNameThatExceedsNormalLength"
  let long_greeting = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, VeryLongNameThatExceedsNormalLength!", long_greeting)
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

test "error_handling_validation" {
  // 测试错误处理验证
  // 验证断言函数正常工作
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 验证溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  let overflow_add = @azimuth.add(max_val, 1)
  @azimuth.assert_true(overflow_add == max_val)
  
  let underflow_add = @azimuth.add(min_val, -1)
  @azimuth.assert_true(underflow_add == min_val)
  
  let overflow_mul = @azimuth.multiply(max_val, 2)
  @azimuth.assert_true(overflow_mul == max_val)
  
  let special_mul = @azimuth.multiply(min_val, -1)
  @azimuth.assert_true(special_mul == min_val)
}

test "performance_related_operations" {
  // 测试性能相关操作
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000000, @azimuth.multiply(1000, 1000000))
  
  // 负数大数运算
  @azimuth.assert_eq(-2000000, @azimuth.add(-1000000, -1000000))
  @azimuth.assert_eq(-1000000000, @azimuth.multiply(-1000, 1000000))
  
  // 混合符号大数运算
  @azimuth.assert_eq(0, @azimuth.add(1000000, -1000000))
  @azimuth.assert_eq(-1000000000, @azimuth.multiply(1000, -1000000))
}