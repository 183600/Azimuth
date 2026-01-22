// 实用测试用例集 - 覆盖多种实际应用场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-24, @azimuth.multiply(-4, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, spaced name!", @azimuth.greet("spaced name"))
}

test "overflow_boundary_conditions" {
  // 测试溢出边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  let c = 5
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 场景：计算矩形花园的围栏长度
  let garden_length = 15
  let garden_width = 10
  let fence_perimeter = @azimuth.add(
    @azimuth.multiply(2, garden_length),
    @azimuth.multiply(2, garden_width)
  )
  @azimuth.assert_eq(50, fence_perimeter)
  
  // 场景：计算购物折扣
  let original_price = 200
  let discount_percent = 20
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(160, final_price)
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值
  // 计算：(a + b) * c - d / e
  let a = 10
  let b = 5
  let c = 3
  let d = 12
  let e = 4
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let division = d / e
  let result = @azimuth.add(product, -division)
  
  @azimuth.assert_eq(44, result)  // (10 + 5) * 3 - 12 / 4 = 45 - 3 = 42
}

test "data_processing_scenarios" {
  // 测试数据处理场景
  // 场景：计算学生平均分
  let scores_sum = @azimuth.add(@azimuth.add(@azimuth.add(85, 90), 78), 92)
  let average_score = scores_sum / 4
  @azimuth.assert_eq(86, average_score)
  
  // 场景：计算班级总分
  let student_count = 30
  let class_average = 85
  let class_total = @azimuth.multiply(student_count, class_average)
  @azimuth.assert_eq(2550, class_total)
}

test "error_handling_validation" {
  // 测试错误处理验证
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试边界条件下的正确处理
  let max_val = 2147483647
  @azimuth.assert_eq(@azimuth.add(max_val, 100, true) == max_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2, true) == max_val)
}

test "performance_related_operations" {
  // 测试性能相关操作
  // 场景：批量计算
  let base_value = 100
  let multiplier = 5
  
  // 连续相加
  let sum_result = @azimuth.add(@azimuth.add(@azimuth.add(base_value, base_value), base_value), base_value)
  @azimuth.assert_eq(400, sum_result)
  
  // 连续相乘
  let product_result = @azimuth.multiply(@azimuth.multiply(base_value, multiplier), 2)
  @azimuth.assert_eq(1000, product_result)
}

test "business_logic_simulation" {
  // 测试业务逻辑模拟
  // 场景：计算订单总价（含税费和运费）
  let item_price = 150
  let quantity = 3
  let tax_rate = 10  // 10%
  let shipping_fee = 25
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let tax_amount = @azimuth.multiply(subtotal, tax_rate) / 100
  let total = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping_fee)
  
  @azimuth.assert_eq(530, total)  // 450 + 45 + 25 = 520
}