// 标准 MoonBit 测试用例
// 覆盖核心功能和各种测试场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let a = 15
  let b = 7
  
  // 加法测试
  let sum_result = @azimuth.add(a, b)
  @azimuth.assert_eq(22, sum_result)
  
  // 乘法测试
  let product_result = @azimuth.multiply(a, b)
  @azimuth.assert_eq(105, product_result)
  
  // 组合运算测试
  let complex_result = @azimuth.add(@azimuth.multiply(a, 2), @azimuth.multiply(b, 3))
  @azimuth.assert_eq(51, complex_result)
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  // 基本问候
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  
  // 空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 数字字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  
  // 特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  let z = 5
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, z)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  )
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "negative_number_operations" {
  // 测试负数运算
  let neg_a = -15
  let neg_b = -7
  let pos_a = 15
  let pos_b = 7
  
  // 负数相加
  @azimuth.assert_eq(-22, @azimuth.add(neg_a, neg_b))
  
  // 负数相乘
  @azimuth.assert_eq(105, @azimuth.multiply(neg_a, neg_b))
  
  // 正负数混合运算
  @azimuth.assert_eq(-8, @azimuth.add(neg_a, pos_b))
  @azimuth.assert_eq(-105, @azimuth.multiply(neg_a, pos_b))
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let discount = 50
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  
  @azimuth.assert_eq(607, total)
  
  // 温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}

test "complex_formula_evaluation" {
  // 测试复杂公式评估
  // 计算二次方程 ax² + bx + c
  let x = 6
  let a = 2
  let b = 3
  let c = 5
  
  let x_squared = @azimuth.multiply(x, x)
  let ax_squared = @azimuth.multiply(a, x_squared)
  let bx = @azimuth.multiply(b, x)
  let result = @azimuth.add(@azimuth.add(ax_squared, bx), c)
  
  @azimuth.assert_eq(89, result)
}

test "sequential_operations" {
  // 测试连续运算
  // 连续加法
  let sum_chain = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4)
  @azimuth.assert_eq(10, sum_chain)
  
  // 连续乘法
  let product_chain = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 3), 4), 5)
  @azimuth.assert_eq(120, product_chain)
  
  // 混合连续运算
  let mixed_chain = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, mixed_chain)
}

test "error_handling_validation" {
  // 测试错误处理验证
  // 验证断言函数正常工作
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 验证溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 验证零值处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
}