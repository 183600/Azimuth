// 额外的标准 MoonBit 测试用例
// 测试一些边界情况和数学性质

test "divide_with_ceil_precision_tests" {
  // 测试 divide_with_ceil 函数的精确性
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(100, 100))
  
  // 测试接近边界的除法
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483647, 1))
  @azimuth.assert_eq(1073741824, @azimuth.divide_with_ceil(2147483647, 2))
  
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-3, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-2, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
}

test "mathematical_associative_laws" {
  // 测试加法结合律
  let a = 10
  let b = 20
  let c = 30
  
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 测试乘法结合律
  let left_mult = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mult = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mult, right_mult)
}

test "mathematical_distributive_law" {
  // 测试乘法对加法的分配律
  let a = 5
  let b = 7
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "real_world_inventory_calculation" {
  // 实际应用：库存计算场景
  let boxes_per_pallet = 24
  let items_per_box = 12
  let total_items_needed = 350
  
  // 计算需要的箱子数
  let boxes_needed = @azimuth.divide_with_ceil(total_items_needed, items_per_box)
  
  // 计算需要的托盘数
  let pallets_needed = @azimuth.divide_with_ceil(boxes_needed, boxes_per_pallet)
  
  @azimuth.assert_eq(30, boxes_needed)  // ceil(350/12) = 30
  @azimuth.assert_eq(2, pallets_needed)  // ceil(30/24) = 2
}

test "string_concatenation_properties" {
  // 测试字符串连接的性质
  let name1 = "Alice"
  let name2 = "Bob"
  
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, Alice Bob!", @azimuth.greet("Alice Bob"))
  
  // 测试数字字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 8
  let discount_per_item = -5
  let shipping_fee = 15
  
  // 计算商品原价
  let original_total = @azimuth.multiply(unit_price, quantity)
  
  // 计算折扣
  let total_discount = @azimuth.multiply(discount_per_item, quantity)
  
  // 计算最终价格
  let final_price = @azimuth.add(@azimuth.add(original_total, total_discount), shipping_fee)
  
  @azimuth.assert_eq(360, original_total)  // 45 * 8
  @azimuth.assert_eq(-40, total_discount)  // -5 * 8
  @azimuth.assert_eq(335, final_price)  // 360 - 40 + 15
}

test "boundary_value_analysis" {
  // 边界值分析测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  @azimuth.assert_eq(max_int - 1, @azimuth.add(max_int - 2, 1))
  @azimuth.assert_eq(min_int + 1, @azimuth.add(min_int + 2, -1))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
  @azimuth.assert_eq(max_int, @azimuth.multiply(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, 1))
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  // 除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  
  // 极值除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2147483647, 2147483647))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-2147483648, -2147483648))
}

test "performance_related_operations" {
  // 性能相关的操作测试
  // 连续运算
  let mut result = 1
  result = @azimuth.add(result, 1)  // 2
  result = @azimuth.multiply(result, 2)  // 4
  result = @azimuth.add(result, 1)  // 5
  result = @azimuth.multiply(result, 2)  // 10
  result = @azimuth.add(result, 1)  // 11
  result = @azimuth.multiply(result, 2)  // 22
  
  @azimuth.assert_eq(22, result)
  
  // 嵌套运算
  let nested = @azimuth.multiply(
    @azimuth.add(10, 5),
    @azimuth.add(3, 2)
  )
  @azimuth.assert_eq(75, nested)  // (10+5) * (3+2) = 15 * 5 = 75
}

test "data_type_consistency" {
  // 数据类型一致性测试
  // 确保所有函数返回预期的类型
  let add_result = @azimuth.add(5, 3)
  let multiply_result = @azimuth.multiply(5, 3)
  let greet_result = @azimuth.greet("Test")
  let divide_result = @azimuth.divide_with_ceil(10, 3)
  
  @azimuth.assert_eq(8, add_result)
  @azimuth.assert_eq(15, multiply_result)
  @azimuth.assert_eq_string("Hello, Test!", greet_result)
  @azimuth.assert_eq(4, divide_result)
  
  // 测试类型一致性 - 确保相同输入产生相同输出
  @azimuth.assert_eq(add_result, @azimuth.add(5, 3))
  @azimuth.assert_eq(multiply_result, @azimuth.multiply(5, 3))
  @azimuth.assert_eq_string(greet_result, @azimuth.greet("Test"))
  @azimuth.assert_eq(divide_result, @azimuth.divide_with_ceil(10, 3))
}