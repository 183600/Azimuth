// 新的综合测试用例 - Azimuth 项目
// 使用标准的 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "string_processing_functionality" {
  // 测试字符串处理功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
}

test "real_world_calculation_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), shipping)
  
  @azimuth.assert_eq(717, cart_total)
}

test "complex_nested_calculation" {
  // 测试复杂嵌套计算：((a + b) × c) - (d × e)
  let a = 10
  let b = 15
  let c = 4
  let d = 8
  let e = 6
  
  let sum_ab = @azimuth.add(a, b)
  let product1 = @azimuth.multiply(sum_ab, c)
  let product2 = @azimuth.multiply(d, e)
  let result = @azimuth.add(product1, -product2)
  
  @azimuth.assert_eq(52, result)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  let special_chars = "@#$%^&*()_+-=[]{}|;':\",./<>?"
  let unicode_name = "张三李四王五"
  
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  @azimuth.assert_eq_string("Hello, " + special_chars + "!", @azimuth.greet(special_chars))
  @azimuth.assert_eq_string("Hello, " + unicode_name + "!", @azimuth.greet(unicode_name))
}

test "overflow_protection" {
  // 测试溢出保护机制
  let max_val = 2147483647
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(50000, 50000))
}

test "sequential_operations" {
  // 测试连续运算
  // 计算：(1 + 2 + 3 + 4 + 5) × 2
  let sum1 = @azimuth.add(1, 2)
  let sum2 = @azimuth.add(sum1, 3)
  let sum3 = @azimuth.add(sum2, 4)
  let sum4 = @azimuth.add(sum3, 5)
  let result = @azimuth.multiply(sum4, 2)
  
  @azimuth.assert_eq(30, result)
}

test "conditional_calculation" {
  // 测试条件计算场景
  let base_value = 100
  let multiplier = 3
  let is_premium = true
  
  // 如果是高级用户，应用额外折扣
  let final_value = if is_premium {
    @azimuth.multiply(base_value, multiplier)
  } else {
    base_value
  }
  
  @azimuth.assert_eq(300, final_value)
}