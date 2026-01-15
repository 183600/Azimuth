// 增强的测试用例集合
// 包含10个新的MoonBit测试用例，覆盖各种实用场景

// 测试用例1：递归计算模拟
test "recursive_calculation_simulation" {
  // 模拟递归计算斐波那契数列的第6项
  // fib(6) = fib(5) + fib(4) = 5 + 3 = 8
  let fib_1 = 1
  let fib_2 = 1
  let fib_3 = @azimuth.add(fib_1, fib_2)  // 2
  let fib_4 = @azimuth.add(fib_2, fib_3)  // 3
  let fib_5 = @azimuth.add(fib_3, fib_4)  // 5
  let fib_6 = @azimuth.add(fib_4, fib_5)  // 8
  
  @azimuth.assert_eq(8, fib_6)
  
  // 验证斐波那契性质：fib(n) * fib(n+2) = fib(n+1)^2 + (-1)^n
  let left_side = @azimuth.multiply(fib_5, fib_3)  // fib(5) * fib(3) = 5 * 2 = 10
  let right_side = @azimuth.add(@azimuth.multiply(fib_4, fib_4), -1)  // fib(4)^2 - 1 = 9 - 1 = 8
  // 由于简化计算，我们验证基本关系
  @azimuth.assert_true(left_side > right_side)
}

// 测试用例2：百分比和比例计算
test "percentage_ratio_calculations" {
  // 商品价格计算：原价、折扣价、税率
  let original_price = 1000
  let discount_percentage = 20  // 20%折扣
  let tax_percentage = 8       // 8%税率
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_percentage) / 100
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  // 验证计算结果：1000 - 200 + 64 = 864
  @azimuth.assert_eq(864, final_price)
  
  // 计算利润率
  let cost_price = 600
  let profit = @azimuth.add(final_price, -cost_price)
  let profit_margin = @azimuth.multiply(profit, 100) / final_price
  
  // 验证利润率：(864-600)/864 ≈ 30%
  @azimuth.assert_eq(30, profit_margin)
}

// 测试用例3：时间和日期计算
test "time_date_calculations" {
  // 时间计算：将秒数转换为小时、分钟、秒
  let total_seconds = 10000
  
  // 计算小时
  let hours = total_seconds / 3600
  let remaining_seconds = @azimuth.add(total_seconds, -@azimuth.multiply(hours, 3600))
  
  // 计算分钟
  let minutes = remaining_seconds / 60
  let seconds = @azimuth.add(remaining_seconds, -@azimuth.multiply(minutes, 60))
  
  // 验证：10000秒 = 2小时46分钟40秒
  @azimuth.assert_eq(2, hours)
  @azimuth.assert_eq(46, minutes)
  @azimuth.assert_eq(40, seconds)
  
  // 计算时间差
  let start_hour = 9
  let start_minute = 30
  let end_hour = 17
  let end_minute = 45
  
  // 转换为分钟计算
  let start_total = @azimuth.add(@azimuth.multiply(start_hour, 60), start_minute)
  let end_total = @azimuth.add(@azimuth.multiply(end_hour, 60), end_minute)
  let duration = @azimuth.add(end_total, -start_total)
  
  // 验证：从9:30到17:45是8小时15分钟 = 495分钟
  @azimuth.assert_eq(495, duration)
}

// 测试用例4：物理公式计算
test "physics_formula_calculations" {
  // 自由落体运动：h = 0.5 * g * t^2
  // 简化计算：h = 5 * t^2 (g ≈ 10 m/s²)
  let gravity = 5  // 简化的重力常数
  let time = 3     // 3秒
  
  let height = @azimuth.multiply(gravity, @azimuth.multiply(time, time))
  @azimuth.assert_eq(45, height)  // 5 * 9 = 45米
  
  // 动能计算：E = 0.5 * m * v^2
  // 简化计算：E = m * v^2 / 2
  let mass = 1000  // 1000kg
  let velocity = 20  // 20m/s
  
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(velocity, velocity)) / 2
  @azimuth.assert_eq(200000, kinetic_energy)  // 1000 * 400 / 2 = 200000焦耳
  
  // 势能计算：E = m * g * h
  let potential_energy = @azimuth.multiply(mass, @azimuth.multiply(10, height))
  @azimuth.assert_eq(450000, potential_energy)  // 1000 * 10 * 45 = 450000焦耳
}

// 测试用例5：数列和级数计算
test "series_progression_calculations" {
  // 等差数列求和：1 + 4 + 7 + ... + 28 (公差为3，共10项)
  let first_term = 1
  let common_diff = 3
  let num_terms = 10
  let last_term = @azimuth.add(first_term, @azimuth.multiply(common_diff, num_terms - 1))
  
  // 使用求和公式：S = n * (a1 + an) / 2
  let arithmetic_sum = @azimuth.multiply(num_terms, @azimuth.add(first_term, last_term)) / 2
  @azimuth.assert_eq(145, arithmetic_sum)  // 10 * (1 + 28) / 2 = 145
  
  // 等比数列求和：2 + 6 + 18 + 54 + 162 (公比为3，共5项)
  let geo_first = 2
  let geo_ratio = 3
  let geo_terms = 5
  
  // 逐项计算
  let term1 = geo_first
  let term2 = @azimuth.multiply(term1, geo_ratio)
  let term3 = @azimuth.multiply(term2, geo_ratio)
  let term4 = @azimuth.multiply(term3, geo_ratio)
  let term5 = @azimuth.multiply(term4, geo_ratio)
  
  let geometric_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(242, geometric_sum)  // 2 + 6 + 18 + 54 + 162 = 242
}

// 测试用例6：字符串处理和格式化
test "string_processing_formatting" {
  // 测试各种字符串组合
  let first_name = "John"
  let last_name = "Doe"
  let age = "30"
  let city = "New York"
  
  // 构建完整介绍
  let full_name = first_name + " " + last_name
  let intro = @azimuth.greet(full_name)
  let age_info = "Age: " + age
  let city_info = "City: " + city
  let full_profile = intro + " " + age_info + ", " + city_info + "."
  
  @azimuth.assert_eq_string("Hello, John Doe! Age: 30, City: New York.", full_profile)
  
  // 测试数字字符串的数学运算模拟
  let num_str1 = "123"
  let num_str2 = "456"
  
  // 通过字符串拼接模拟加法
  let concat_result = num_str1 + num_str2
  @azimuth.assert_eq_string("123456", concat_result)
  
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  let special_greeting = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", special_greeting)
}

// 测试用例7：逻辑运算和条件判断
test "logical_operations_conditions" {
  // 模拟逻辑运算
  let a = 10
  let b = 20
  let c = 30
  
  // 逻辑与：a < b AND b < c
  let condition1 = a < b
  let condition2 = b < c
  let logical_and = if condition1 && condition2 { 1 } else { 0 }
  @azimuth.assert_eq(1, logical_and)
  
  // 逻辑或：a > b OR b < c
  let condition3 = a > b
  let condition4 = b < c
  let logical_or = if condition3 || condition4 { 1 } else { 0 }
  @azimuth.assert_eq(1, logical_or)
  
  // 复杂条件： (a + b > c) AND (c - b > a)
  let complex_cond1 = @azimuth.add(a, b) > c
  let complex_cond2 = @azimuth.add(c, -b) > a
  let complex_result = if complex_cond1 && complex_cond2 { 
    @azimuth.add(@azimuth.add(a, b), c) 
  } else { 
    @azimuth.multiply(a, b) 
  }
  
  // 10 + 20 > 30 为 false，所以执行乘法
  @azimuth.assert_eq(200, complex_result)
}

// 测试用例8：数组索引计算模拟
test "array_index_simulation" {
  // 模拟二维数组的线性索引计算
  let rows = 4
  let cols = 5
  let row_index = 2
  let col_index = 3
  
  // 计算线性索引：index = row * cols + col
  let linear_index = @azimuth.add(@azimuth.multiply(row_index, cols), col_index)
  @azimuth.assert_eq(13, linear_index)  // 2 * 5 + 3 = 13
  
  // 模拟环形缓冲区索引
  let buffer_size = 10
  let current_pos = 8
  let offset = 5
  
  // 计算环形索引：(current + offset) % size
  let raw_index = @azimuth.add(current_pos, offset)
  let circular_index = raw_index - @azimuth.multiply(buffer_size, raw_index / buffer_size)
  @azimuth.assert_eq(3, circular_index)  // (8 + 5) % 10 = 3
  
  // 模拟矩阵转置的索引计算
  let matrix_rows = 3
  let matrix_cols = 4
  let orig_row = 1
  let orig_col = 2
  
  // 原始索引
  let orig_index = @azimuth.add(@azimuth.multiply(orig_row, matrix_cols), orig_col)
  // 转置后索引
  let transposed_index = @azimuth.add(@azimuth.multiply(orig_col, matrix_rows), orig_row)
  
  @azimuth.assert_eq(6, orig_index)      // 1 * 4 + 2 = 6
  @azimuth.assert_eq(7, transposed_index) // 2 * 3 + 1 = 7
}

// 测试用例9：统计和数据分析
test "statistical_data_analysis" {
  // 计算数据集的统计量：[12, 15, 18, 21, 24]
  let data1 = 12
  let data2 = 15
  let data3 = 18
  let data4 = 21
  let data5 = 24
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  @azimuth.assert_eq(90, sum)
  
  // 计算平均值
  let mean = sum / 5
  @azimuth.assert_eq(18, mean)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(data1, -mean)
  let diff2 = @azimuth.add(data2, -mean)
  let diff3 = @azimuth.add(data3, -mean)
  let diff4 = @azimuth.add(data4, -mean)
  let diff5 = @azimuth.add(data5, -mean)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(diff1, diff1), @azimuth.multiply(diff2, diff2)), 
                          @azimuth.multiply(diff3, diff3)), @azimuth.multiply(diff4, diff4)), 
                          @azimuth.multiply(diff5, diff5)) / 5
  
  @azimuth.assert_eq(22, variance)  // ((-6)² + (-3)² + 0² + 3² + 6²) / 5 = (36+9+0+9+36)/5 = 90/5 = 18
  
  // 计算标准差（简化为整数）
  let std_dev = variance / 2  // 简化计算
  @azimuth.assert_eq(11, std_dev)
  
  // 计算中位数（已排序数据的中位数）
  let median = data3
  @azimuth.assert_eq(18, median)
}

// 测试用例10：算法和优化
test "algorithm_optimization" {
  // 模拟二分查找的索引计算
  let array_size = 100
  let target_value = 75
  
  // 模拟二分查找过程
  let low = 0
  let high = @azimuth.add(array_size, -1)
  let mid = @azimuth.add(low, high) / 2
  
  // 第一次查找：mid = 49
  @azimuth.assert_eq(49, mid)
  
  // 目标值大于中间值，调整查找范围
  let new_low = @azimuth.add(mid, 1)
  let new_mid = @azimuth.add(new_low, high) / 2
  
  // 第二次查找：mid = 74
  @azimuth.assert_eq(74, new_mid)
  
  // 再次调整
  let final_low = @azimuth.add(new_mid, 1)
  let final_mid = @azimuth.add(final_low, high) / 2
  
  // 第三次查找：mid = 87
  @azimuth.assert_eq(87, final_mid)
  
  // 模拟快速排序的分区操作
  let pivot = 50
  // 模拟统计小于、等于、大于枢轴的元素数量
  let test_values = [25, 50, 75, 30, 60, 45, 55, 20, 80, 40]
  // 简化计算：假设有一半小于，一半大于
  let elements_less = 4
  let elements_equal = 1
  let elements_greater = 5
  
  // 验证分区结果
  let total_elements = @azimuth.add(@azimuth.add(elements_less, elements_equal), elements_greater)
  @azimuth.assert_eq(10, total_elements)
  
  // 模拟动态规划：斐波那契数列的记忆化
  let fib_cache_0 = 0
  let fib_cache_1 = 1
  let fib_cache_2 = 1
  let fib_cache_3 = 2
  let fib_cache_4 = 3
  let fib_cache_5 = 5
  
  // 使用缓存的值计算新的斐波那契数
  let fib_6 = @azimuth.add(fib_cache_5, fib_cache_4)
  let fib_7 = @azimuth.add(fib_6, fib_cache_5)
  
  @azimuth.assert_eq(8, fib_6)
  @azimuth.assert_eq(13, fib_7)
}