// 高质量 MoonBit 测试用例集合
// 覆盖 azimuth 库的核心功能和边界情况

// 测试1: 基本算术运算验证
test "basic_arithmetic_operations" {
  // 测试加法基本功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  
  // 测试乘法基本功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

// 测试2: 向上取整除法的边界情况
test "divide_with_ceil_boundary_cases" {
  // 测试正数向上取整
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 测试负数向上取整
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  
  // 测试除数为零的安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

// 测试3: 字符串问候功能的多语言支持
test "greeting_function_multilingual" {
  // 测试基本英文问候
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 测试中文问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 中国!", @azimuth.greet("中国"))
  
  // 测试特殊字符和Emoji
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

// 测试4: 数学性质验证 - 交换律和结合律
test "mathematical_properties_verification" {
  let a = 7
  let b = 13
  let c = 5
  
  // 验证加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 验证乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
}

// 测试5: 实际业务场景 - 购物车计算
test "shopping_cart_calculation" {
  let item1_price = 29
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  let shipping_fee = 12
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 计算最终总价
  let total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(58, item1_total)  // 29 * 2 = 58
  @azimuth.assert_eq(45, item2_total)  // 15 * 3 = 45
  @azimuth.assert_eq(103, subtotal)    // 58 + 45 = 103
  @azimuth.assert_eq(115, total)       // 103 + 12 = 115
}

// 测试6: 包装计算场景
test "packaging_calculation_scenario" {
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 计算包装成本
  let cost_per_box = 3
  let total_cost = @azimuth.multiply(boxes_needed, cost_per_box)
  
  @azimuth.assert_eq(15, total_cost)   // 5 * 3 = 15
}

// 测试7: 零值运算测试
test "zero_value_operations" {
  // 测试与零的加法
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-3, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 测试与零的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  
  // 测试零的除法
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

// 测试8: 大数运算测试
test "large_number_operations" {
  let big_num1 = 1000000
  let big_num2 = 500000
  
  // 大数加法
  @azimuth.assert_eq(1500000, @azimuth.add(big_num1, big_num2))
  
  // 大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 大数除法向上取整
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
}

// 测试9: 复杂表达式计算
test "complex_expression_calculation" {
  let x = 10
  let y = 15
  let z = 8
  
  // 计算 (x + y) * z
  let result1 = @azimuth.multiply(@azimuth.add(x, y), z)
  @azimuth.assert_eq(200, result1)  // (10 + 15) * 8 = 25 * 8 = 200
  
  // 计算 x * y + z
  let result2 = @azimuth.add(@azimuth.multiply(x, y), z)
  @azimuth.assert_eq(158, result2)  // 10 * 15 + 8 = 150 + 8 = 158
  
  // 计算组合表达式
  let complex_result = @azimuth.add(
    @azimuth.multiply(x, @azimuth.divide_with_ceil(y, z)),
    @azimuth.multiply(y, @azimuth.divide_with_ceil(x, y))
  )
  @azimuth.assert_eq(22, complex_result)  // 10*ceil(15/8) + 15*ceil(10/15) = 10*2 + 15*1 = 20 + 15 = 35
}

// 测试10: 边界值和错误处理
test "boundary_values_and_error_handling" {
  // 测试极端值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值运算
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 测试空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符处理
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
}