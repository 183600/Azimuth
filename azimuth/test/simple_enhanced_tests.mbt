// 简单的 MoonBit 增强测试用例
// 专注于边界条件、实际应用场景和高级功能测试

test "integer_overflow_boundary_conditions" {
  // 测试整数溢出的边界条件
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近最大值的加法
  @azimuth.assert_eq(max_int, @azimuth.add(max_int - 1, 1))
  @azimuth.assert_eq(max_int, @azimuth.add(max_int - 100, 100))
  
  // 测试接近最小值的减法（通过加负数实现）
  @azimuth.assert_eq(min_int, @azimuth.add(min_int + 1, -1))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int + 100, -100))
}

test "real_world_inventory_management" {
  // 实际库存管理场景测试
  let initial_stock = 150
  let incoming_shipment = 75
  let sold_items = 43
  let returned_items = 5
  
  // 计算最终库存
  let total_received = @azimuth.add(initial_stock, incoming_shipment)
  let net_sold = @azimuth.add(sold_items, -returned_items)
  let final_stock = @azimuth.add(total_received, -net_sold)
  
  @azimuth.assert_eq(187, final_stock)
  
  // 计算需要的存储箱数量（每箱最多装25个）
  let boxes_needed = @azimuth.divide_with_ceil(final_stock, 25)
  @azimuth.assert_eq(8, boxes_needed)  // ceil(187/25) = 8
}

test "string_processing_internationalization" {
  // 国际化字符串处理测试
  // 测试亚洲姓名
  let asian_name1 = "张三"
  let asian_name2 = "李四"
  let greeting1 = @azimuth.greet(asian_name1)
  let greeting2 = @azimuth.greet(asian_name2)
  @azimuth.assert_eq_string("Hello, 张三!", greeting1)
  @azimuth.assert_eq_string("Hello, 李四!", greeting2)
  
  // 测试欧洲姓名
  let european_name1 = "Jean"
  let european_name2 = "François"
  let greeting3 = @azimuth.greet(european_name1)
  let greeting4 = @azimuth.greet(european_name2)
  @azimuth.assert_eq_string("Hello, Jean!", greeting3)
  @azimuth.assert_eq_string("Hello, François!", greeting4)
  
  // 测试混合姓名
  let mixed_name1 = "José"
  let mixed_name2 = "张伟"
  let greeting5 = @azimuth.greet(mixed_name1)
  let greeting6 = @azimuth.greet(mixed_name2)
  @azimuth.assert_eq_string("Hello, José!", greeting5)
  @azimuth.assert_eq_string("Hello, 张伟!", greeting6)
}

test "mathematical_sequence_calculations" {
  // 数学序列计算测试
  // 斐波那契数列前几项的计算
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  let fib7 = @azimuth.add(fib5, fib6)  // 13
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
  @azimuth.assert_eq(13, fib7)
  
  // 阶乘计算
  let fact3 = @azimuth.multiply(3, 2)  // 6
  let fact4 = @azimuth.multiply(fact3, 3)  // 18
  let fact5 = @azimuth.multiply(fact4, 4)  // 72
  
  @azimuth.assert_eq(6, fact3)
  @azimuth.assert_eq(18, fact4)
  @azimuth.assert_eq(72, fact5)
}

test "business_financial_calculations" {
  // 业务财务计算测试
  let unit_price = 199
  let quantity = 17
  let discount_rate = 15  // 15% 折扣
  let tax_rate = 8        // 8% 税率
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算折扣金额
  let discount_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, discount_rate), 100)
  
  // 计算折后价格
  let discounted_price = @azimuth.add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(discounted_price, tax_rate), 100)
  
  // 计算总价
  let total_price = @azimuth.add(discounted_price, tax_amount)
  
  @azimuth.assert_eq(3383, subtotal)
  @azimuth.assert_eq(508, discount_amount)  // ceil(3383 * 15 / 100)
  @azimuth.assert_eq(2875, discounted_price)
  @azimuth.assert_eq(231, tax_amount)      // ceil(2875 * 8 / 100)
  @azimuth.assert_eq(3106, total_price)
}

test "performance_large_number_operations" {
  // 大数操作性能测试
  // 测试大数加法
  let mut sum = 0
  sum = @azimuth.add(sum, 100000)
  sum = @azimuth.add(sum, 250000)
  sum = @azimuth.add(sum, 500000)
  sum = @azimuth.add(sum, 750000)
  sum = @azimuth.add(sum, 1000000)
  @azimuth.assert_eq(2600000, sum)
  
  // 测试大数乘法
  let large_num1 = 10000
  let large_num2 = 10000
  let large_product = @azimuth.multiply(large_num1, large_num2)
  @azimuth.assert_eq(100000000, large_product)
  
  // 测试大数除法向上取整
  let large_dividend = 1000000
  let large_divisor = 37
  let large_div_result = @azimuth.divide_with_ceil(large_dividend, large_divisor)
  @azimuth.assert_eq(27028, large_div_result)  // ceil(1000000/37)
}

test "edge_case_special_values" {
  // 特殊值边界情况测试
  // 测试零值的各种组合
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1000))
  
  // 测试单位值的各种组合
  @azimuth.assert_eq(2, @azimuth.add(1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(2, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  
  // 测试负单位值
  @azimuth.assert_eq(-2, @azimuth.add(-1, -1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
}

test "complex_data_processing_pipeline" {
  // 复杂数据处理流水线测试
  // 模拟数据处理流程：收集 -> 处理 -> 分组 -> 汇总
  
  // 阶段1：数据收集
  let mut total = 0
  total = @azimuth.add(total, 15)
  total = @azimuth.add(total, 23)
  total = @azimuth.add(total, 7)
  total = @azimuth.add(total, 42)
  total = @azimuth.add(total, 18)
  total = @azimuth.add(total, 33)
  total = @azimuth.add(total, 27)
  total = @azimuth.add(total, 11)
  @azimuth.assert_eq(176, total)
  
  // 阶段2：数据处理 - 计算平均值（向上取整）
  let count = 8
  let average = @azimuth.divide_with_ceil(total, count)
  @azimuth.assert_eq(22, average)  // ceil(176/8)
  
  // 阶段3：数据分组 - 按平均值分组
  let above_average = @azimuth.divide_with_ceil(@azimuth.multiply(42, 33), average)
  let below_average = @azimuth.divide_with_ceil(@azimuth.multiply(15, 7), average)
  
  // 阶段4：数据汇总
  let processing_result = @azimuth.add(above_average, below_average)
  @azimuth.assert_eq(88, processing_result)
}

test "unicode_and_emoji_support" {
  // Unicode和表情符号支持测试
  // 测试表情符号
  let emoji1 = "🚀"
  let emoji2 = "🌙"
  let greeting1 = @azimuth.greet(emoji1)
  let greeting2 = @azimuth.greet(emoji2)
  @azimuth.assert_eq_string("Hello, 🚀!", greeting1)
  @azimuth.assert_eq_string("Hello, 🌙!", greeting2)
  
  // 测试Unicode符号
  let symbol1 = "α"
  let symbol2 = "β"
  let greeting3 = @azimuth.greet(symbol1)
  let greeting4 = @azimuth.greet(symbol2)
  @azimuth.assert_eq_string("Hello, α!", greeting3)
  @azimuth.assert_eq_string("Hello, β!", greeting4)
  
  // 测试混合内容
  let mixed1 = "Hello🌍"
  let mixed2 = "测试🧪"
  let greeting5 = @azimuth.greet(mixed1)
  let greeting6 = @azimuth.greet(mixed2)
  @azimuth.assert_eq_string("Hello, Hello🌍!", greeting5)
  @azimuth.assert_eq_string("Hello, 测试🧪!", greeting6)
}