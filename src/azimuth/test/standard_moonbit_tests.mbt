// 新增的 MoonBit 标准测试用例
// 测试 azimuth 模块的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 18
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "real_world_calculation_scenario" {
  // 测试实际应用场景：计算折扣价格
  let original_price = 200
  let discount_percentage = 15
  let quantity = 3
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  // 计算折扣后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  // 计算总价
  let total_price = @azimuth.multiply(discounted_price, quantity)
  
  @azimuth.assert_eq(170, discounted_price)  // 200 - 30 = 170
  @azimuth.assert_eq(510, total_price)      // 170 × 3 = 510
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  // 计算：(a + b) * c - d
  let a = 8
  let b = 12
  let c = 5
  let d = 20
  
  let sum_ab = @azimuth.add(a, b)           // 20
  let product = @azimuth.multiply(sum_ab, c) // 100
  let result = @azimuth.add(product, -d)     // 80
  
  @azimuth.assert_eq(80, result)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列前n项和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 7
  
  // 使用公式：n²
  let formula_result = @azimuth.multiply(n, n)
  
  // 手动计算验证
  let manual_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 3), 5), 7), 9), 11), 13)
  
  @azimuth.assert_eq(formula_result, manual_sum)
  @azimuth.assert_eq(49, formula_result)  // 7² = 49
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "@#$%^&*()"
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet(special_chars))
  
  // 测试包含空格的字符串
  let spaced_string = "  Hello  World  "
  @azimuth.assert_eq_string("Hello,   Hello  World  !", @azimuth.greet(spaced_string))
  
  // 测试包含数字的字符串
  let numeric_string = "12345"
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet(numeric_string))
}

test "comprehensive_application_test" {
  // 综合应用测试：模拟简单的库存管理系统
  let initial_stock = 100
  let received_stock = 50
  let sold_items = 30
  let returned_items = 5
  let damaged_items = 3
  
  // 计算最终库存
  let total_received = @azimuth.add(initial_stock, received_stock)     // 150
  let total_sold = @azimuth.add(sold_items, -returned_items)           // 25
  let final_stock = @azimuth.add(@azimuth.add(total_received, -total_sold), -damaged_items) // 122
  
  @azimuth.assert_eq(122, final_stock)
  
  // 计算库存价值（假设每个物品价值10元）
  let item_value = 10
  let total_value = @azimuth.multiply(final_stock, item_value)
  @azimuth.assert_eq(1220, total_value)
}