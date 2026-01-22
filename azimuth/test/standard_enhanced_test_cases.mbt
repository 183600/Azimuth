// 标准 MoonBit 增强测试用例
// 包含8个高质量测试用例，覆盖各种边界情况和实际应用场景

test "mathematical_associativity_laws" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 15
  let b = 25
  let c = 35
  
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 测试乘法结合律: (a * b) * c = a * (b * c)
  let left_mult = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mult = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mult, right_mult)
  
  // 测试混合运算结合律
  let mixed1 = @azimuth.add(@azimuth.multiply(a, b), c)
  let mixed2 = @azimuth.multiply(a, @azimuth.add(b, c))
  // 注意：这些不应该相等，只是测试函数组合的稳定性
  @azimuth.assert_eq(410, mixed1)  // 15*25 + 35 = 375 + 35 = 410
  @azimuth.assert_eq(900, mixed2)  // 15 * (25 + 35) = 15 * 60 = 900
}

test "string_processing_performance" {
  // 测试长字符串处理
  let very_long_name = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ"
  let result = @azimuth.greet(very_long_name)
  @azimuth.assert_eq_string("Hello, " + very_long_name + "!", result)
  
  // 测试包含空格的长字符串
  let long_spaced_name = "John Michael Alexander Robertson Junior"
  let spaced_result = @azimuth.greet(long_spaced_name)
  @azimuth.assert_eq_string("Hello, " + long_spaced_name + "!", spaced_result)
  
  // 测试包含特殊字符的长字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_result = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, " + special_chars + "!", special_result)
}

test "divide_with_ceil_comprehensive_boundary" {
  // 测试向上取整除法的全面边界情况
  
  // 测试相同数字
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-5, -5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(5, -5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-5, 5))
  
  // 测试1的除法
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(100, 1))
  @azimuth.assert_eq(-100, @azimuth.divide_with_ceil(-100, 1))
  @azimuth.assert_eq(-100, @azimuth.divide_with_ceil(100, -1))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(-100, -1))
  
  // 测试接近整数的除法
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(99, 50))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(100, 50))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(101, 50))
  
  // 测试大数除法
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(1000000, 1000))
  @azimuth.assert_eq(1001, @azimuth.divide_with_ceil(1001000, 1000))
}

test "complex_function_composition" {
  // 测试复杂的函数组合场景
  
  // 场景1：计算多项式值 f(x) = ax² + bx + c
  let x = 5
  let a = 2
  let b = 3
  let c = 7
  
  let x_squared = @azimuth.multiply(x, x)  // x² = 25
  let ax_squared = @azimuth.multiply(a, x_squared)  // ax² = 50
  let bx = @azimuth.multiply(b, x)  // bx = 15
  let polynomial_result = @azimuth.add(@azimuth.add(ax_squared, bx), c)  // ax² + bx + c = 72
  @azimuth.assert_eq(72, polynomial_result)
  
  // 场景2：计算平均值的向上取整
  let values_sum = @azimuth.add(@azimuth.add(15, 25), 35)  // 总和 = 75
  let count = 3
  let average_ceil = @azimuth.divide_with_ceil(values_sum, count)  // ceil(75/3) = 25
  @azimuth.assert_eq(25, average_ceil)
  
  // 场景3：计算折扣后的价格向上取整
  let original_price = 199
  let discount_percentage = 20  // 20%折扣
  let discount_amount = @azimuth.divide_with_ceil(@azimuth.multiply(original_price, discount_percentage), 100)
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(159, final_price)  // 199 - ceil(199*20/100) = 199 - 40 = 159
}

test "practical_business_calculations" {
  // 测试实际业务计算场景
  
  // 场景1：计算订单总价（含税）
  let item_price = 299
  let quantity = 3
  let subtotal = @azimuth.multiply(item_price, quantity)  // 897
  let tax_rate = 8  // 8%税率
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)  // ceil(897*8/100) = 72
  let total_with_tax = @azimuth.add(subtotal, tax_amount)  // 969
  @azimuth.assert_eq(969, total_with_tax)
  
  // 场景2：计算需要的包装箱数量
  let total_items = 157
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(14, boxes_needed)  // ceil(157/12) = 14
  
  // 场景3：计算项目完成时间（向上取整到整天）
  let total_hours = 67
  let hours_per_day = 8
  let days_needed = @azimuth.divide_with_ceil(total_hours, hours_per_day)
  @azimuth.assert_eq(9, days_needed)  // ceil(67/8) = 9
  
  // 场景4：批量个性化问候
  let greeting1 = @azimuth.greet("Alice")
  let greeting2 = @azimuth.greet("Bob")
  let greeting3 = @azimuth.greet("Charlie")
  @azimuth.assert_eq_string("Hello, Alice!", greeting1)
  @azimuth.assert_eq_string("Hello, Bob!", greeting2)
  @azimuth.assert_eq_string("Hello, Charlie!", greeting3)
}

test "extreme_input_combinations" {
  // 测试极端输入组合的处理
  
  // 测试最大值和最小值的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法极端组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 乘法极端组合
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(-1, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, max_val))
  
  // 除法极端组合
  @azimuth.assert_eq(max_val, @azimuth.divide_with_ceil(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.divide_with_ceil(min_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.divide_with_ceil(max_val, -1))
  @azimuth.assert_eq(-min_val, @azimuth.divide_with_ceil(min_val, -1))
}

test "sequential_function_calls_stability" {
  // 测试连续函数调用的稳定性
  
  // 连续加法序列
  let mut sum_result = 0
  sum_result = @azimuth.add(sum_result, 100)
  sum_result = @azimuth.add(sum_result, 200)
  sum_result = @azimuth.add(sum_result, 300)
  sum_result = @azimuth.add(sum_result, 400)
  sum_result = @azimuth.add(sum_result, 500)
  @azimuth.assert_eq(1500, sum_result)
  
  // 连续乘法序列
  let mut product_result = 1
  product_result = @azimuth.multiply(product_result, 2)
  product_result = @azimuth.multiply(product_result, 3)
  product_result = @azimuth.multiply(product_result, 4)
  product_result = @azimuth.multiply(product_result, 5)
  @azimuth.assert_eq(120, product_result)
  
  // 混合连续运算序列
  let mut mixed_result = 10
  mixed_result = @azimuth.add(mixed_result, 5)      // 15
  mixed_result = @azimuth.multiply(mixed_result, 2) // 30
  mixed_result = @azimuth.add(mixed_result, -10)    // 20
  mixed_result = @azimuth.divide_with_ceil(mixed_result, 3) // 7
  mixed_result = @azimuth.multiply(mixed_result, 4) // 28
  mixed_result = @azimuth.add(mixed_result, 2)      // 30
  @azimuth.assert_eq(30, mixed_result)
  
  // 连续字符串处理
  let greeting1 = @azimuth.greet("User1")
  let greeting2 = @azimuth.greet("User2")
  let greeting3 = @azimuth.greet("User3")
  @azimuth.assert_eq_string("Hello, User1!", greeting1)
  @azimuth.assert_eq_string("Hello, User2!", greeting2)
  @azimuth.assert_eq_string("Hello, User3!", greeting3)
}

test "data_type_boundary_behaviors" {
  // 测试数据类型边界行为
  
  // 测试零值的各种运算
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  
  // 测试单位值的各种运算
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(99, 99))
  
  // 测试负数单位值
  @azimuth.assert_eq(-1, @azimuth.add(-1, 0))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -1))
  
  // 测试接近边界的值
  let max_val = 2147483647
  let min_val = -2147483648
  let near_max = 2147483646
  let near_min = -2147483647
  
  @azimuth.assert_eq(max_val, @azimuth.add(near_max, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(near_min, -1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(near_max, near_max))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(near_min, near_min))
}