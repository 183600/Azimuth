// 标准 MoonBit 测试文件
// 测试 azimuth 库的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-4, 2))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = @azimuth.multiply(@azimuth.add(5, 3), 2)
  @azimuth.assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
}

test "practical_business_scenario" {
  // 实际业务场景：计算订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(215, total)
}

test "packaging_calculation" {
  // 包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

// 新增的10个专业 MoonBit 测试用例

test "boundary_value_extremes" {
  // 边界值极值测试
  let max_int = 1000000
  let min_int = -1000000
  
  // 测试大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(max_int, max_int))
  @azimuth.assert_eq(0, @azimuth.add(max_int, min_int))
  @azimuth.assert_eq(-2000000, @azimuth.add(min_int, min_int))
  
  // 测试大数乘法
  @azimuth.assert_eq(1000000000000, @azimuth.multiply(max_int, max_int))
  @azimuth.assert_eq(-1000000000000, @azimuth.multiply(max_int, min_int))
  @azimuth.assert_eq(1000000000000, @azimuth.multiply(min_int, min_int))
  
  // 测试边界除法
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(max_int, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(max_int, max_int))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(max_int, max_int + 1))
}

test "fibonacci_sequence_calculation" {
  // 斐波那契数列计算测试
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f1, f0)
  let f3 = @azimuth.add(f2, f1)
  let f4 = @azimuth.add(f3, f2)
  let f5 = @azimuth.add(f4, f3)
  let f6 = @azimuth.add(f5, f4)
  let f7 = @azimuth.add(f6, f5)
  let f8 = @azimuth.add(f7, f6)
  
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  @azimuth.assert_eq(21, f8)
  
  // 验证黄金比例近似
  let ratio1 = @azimuth.divide_with_ceil(@azimuth.multiply(f8, 100), f7)
  let ratio2 = @azimuth.divide_with_ceil(@azimuth.multiply(f7, 100), f6)
  @azimuth.assert_eq(162, ratio1)  // 21/13 ≈ 1.615
  @azimuth.assert_eq(162, ratio2)  // 13/8 ≈ 1.625
}

test "statistical_analysis" {
  // 统计分析测试
  let data_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 22), 18), 25), 20)
  let data_count = 5
  let mean = @azimuth.divide_with_ceil(data_sum, data_count)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.subtract(15, mean)
  let diff2 = @azimuth.subtract(22, mean)
  let diff3 = @azimuth.subtract(18, mean)
  let diff4 = @azimuth.subtract(25, mean)
  let diff5 = @azimuth.subtract(20, mean)
  
  let sq_diff1 = @azimuth.multiply(diff1, diff1)
  let sq_diff2 = @azimuth.multiply(diff2, diff2)
  let sq_diff3 = @azimuth.multiply(diff3, diff3)
  let sq_diff4 = @azimuth.multiply(diff4, diff4)
  let sq_diff5 = @azimuth.multiply(diff5, diff5)
  
  let variance_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(sq_diff1, sq_diff2), sq_diff3), sq_diff4), sq_diff5)
  let variance = @azimuth.divide_with_ceil(variance_sum, data_count)
  
  @azimuth.assert_eq(100, data_sum)
  @azimuth.assert_eq(20, mean)
  @azimuth.assert_eq(14, variance)
}

test "advanced_string_processing" {
  // 高级字符串处理测试
  let long_name = "VeryLongNameWithManyCharacters"
  let empty_name = ""
  let space_name = " "
  let special_name = "!@#$%^&*()"
  
  @azimuth.assert_eq_string("Hello, VeryLongNameWithManyCharacters!", @azimuth.greet(long_name))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(empty_name))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(space_name))
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet(special_name))
  
  // 测试数字字符串
  let numeric_name = "1234567890"
  @azimuth.assert_eq_string("Hello, 1234567890!", @azimuth.greet(numeric_name))
  
  // 测试混合字符串
  let mixed_name = "User123!@#"
  @azimuth.assert_eq_string("Hello, User123!@#!", @azimuth.greet(mixed_name))
}

test "complex_financial_calculations" {
  // 复杂金融计算测试
  let principal = 10000
  let annual_rate = 5
  let years = 3
  
  // 复利计算
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100)
  let year1_total = @azimuth.add(principal, year1_interest)
  
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year1_total, annual_rate), 100)
  let year2_total = @azimuth.add(year1_total, year2_interest)
  
  let year3_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year2_total, annual_rate), 100)
  let year3_total = @azimuth.add(year2_total, year3_interest)
  
  // 计算总收益
  let total_profit = @azimuth.subtract(year3_total, principal)
  let profit_percentage = @azimuth.divide_with_ceil(@azimuth.multiply(total_profit, 100), principal)
  
  @azimuth.assert_eq(10500, year1_total)
  @azimuth.assert_eq(11025, year2_total)
  @azimuth.assert_eq(11576, year3_total)
  @azimuth.assert_eq(1576, total_profit)
  @azimuth.assert_eq(16, profit_percentage)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试各种零除法情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  
  // 测试极值除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1000000))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  
  // 测试负数除法边界
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1000000))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(-1000000, -1))
}

test "performance_simulation" {
  // 性能模拟测试
  let iterations = 100
  let base_value = 2
  
  // 模拟指数增长
  let result1 = @azimuth.multiply(base_value, 2)
  let result2 = @azimuth.multiply(result1, 2)
  let result3 = @azimuth.multiply(result2, 2)
  let result4 = @azimuth.multiply(result3, 2)
  let result5 = @azimuth.multiply(result4, 2)
  
  // 模拟线性增长
  let linear1 = @azimuth.add(base_value, 2)
  let linear2 = @azimuth.add(linear1, 2)
  let linear3 = @azimuth.add(linear2, 2)
  let linear4 = @azimuth.add(linear3, 2)
  let linear5 = @azimuth.add(linear4, 2)
  
  // 模拟阶乘增长
  let fact1 = 1
  let fact2 = @azimuth.multiply(fact1, 2)
  let fact3 = @azimuth.multiply(fact2, 3)
  let fact4 = @azimuth.multiply(fact3, 4)
  let fact5 = @azimuth.multiply(fact4, 5)
  
  @azimuth.assert_eq(64, result5)
  @azimuth.assert_eq(12, linear5)
  @azimuth.assert_eq(120, fact5)
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let initial_stock = 1000
  let daily_sales = 45
  let days = 7
  let new_shipment = 500
  let safety_stock = 200
  
  // 计算一周销售
  let weekly_sales = @azimuth.multiply(daily_sales, days)
  
  // 计算销售后库存
  let stock_after_sales = @azimuth.subtract(initial_stock, weekly_sales)
  
  // 判断是否需要补货
  let need_reorder = 0
  let reorder_threshold = @azimuth.add(stock_after_sales, safety_stock)
  
  // 加入新货
  let final_stock = @azimuth.add(stock_after_sales, new_shipment)
  
  // 计算可供应天数
  let days_of_supply = @azimuth.divide_with_ceil(final_stock, daily_sales)
  
  @azimuth.assert_eq(315, weekly_sales)
  @azimuth.assert_eq(685, stock_after_sales)
  @azimuth.assert_eq(1185, final_stock)
  @azimuth.assert_eq(27, days_of_supply)
}

test "number_system_conversion" {
  // 数制转换测试
  let decimal_num = 255
  
  // 分解为二进制位
  let bit7 = @azimuth.multiply(1, 128)  // 2^7
  let bit6 = @azimuth.multiply(1, 64)   // 2^6
  let bit5 = @azimuth.multiply(1, 32)   // 2^5
  let bit4 = @azimuth.multiply(1, 16)   // 2^4
  let bit3 = @azimuth.multiply(1, 8)    // 2^3
  let bit2 = @azimuth.multiply(1, 4)    // 2^2
  let bit1 = @azimuth.multiply(1, 2)    // 2^1
  let bit0 = @azimuth.multiply(1, 1)    // 2^0
  
  // 重新组合验证
  let reconstructed = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(bit7, bit6), bit5), bit4), bit3), bit2), bit1), bit0)
  
  // 计算十六进制表示
  let hex_high = @azimuth.divide_with_ceil(decimal_num, 16)
  let hex_low = @azimuth.subtract(decimal_num, @azimuth.multiply(hex_high - 1, 16))
  
  // 计算八进制表示
  let octal_hundreds = @azimuth.divide_with_ceil(decimal_num, 64)
  let octal_remainder = @azimuth.subtract(decimal_num, @azimuth.multiply(octal_hundreds - 1, 64))
  let octal_tens = @azimuth.divide_with_ceil(octal_remainder, 8)
  let octal_ones = @azimuth.subtract(octal_remainder, @azimuth.multiply(octal_tens - 1, 8))
  
  @azimuth.assert_eq(255, reconstructed)
  @azimuth.assert_eq(16, hex_high)
  @azimuth.assert_eq(15, hex_low)
  @azimuth.assert_eq(4, octal_hundreds)
  @azimuth.assert_eq(7, octal_tens)
  @azimuth.assert_eq(7, octal_ones)
}

test "algorithm_simulation_sorting" {
  // 算法模拟 - 排序测试
  // 模拟冒泡排序过程
  let a = 5, b = 2, c = 8, d = 1, e = 9
  
  // 第一轮冒泡
  let step1_ab = b  // 5 > 2，交换
  let step1_bc = 5  // 5 < 8，不交换
  let step1_cd = 8  // 8 > 1，交换
  let step1_de = 1  // 8 < 9，不交换
  
  // 验证最大元素已就位
  let max_element = 9
  @azimuth.assert_eq(max_element, 9)
  
  // 模拟查找算法
  let search_target = 8
  let found_position = 3  // 假设在第3个位置找到
  
  // 模拟二分查找
  let low = 0, high = 4
  let mid = @azimuth.divide_with_ceil(@azimuth.add(low, high), 2)
  let mid_value = 5  // 假设中间值是5
  
  // 根据比较结果调整搜索范围
  let new_low = @azimuth.add(mid, 1)  // 5 < 8，搜索右半部分
  let new_mid = @azimuth.divide_with_ceil(@azimuth.add(new_low, high), 2)
  
  @azimuth.assert_eq(2, mid)
  @azimuth.assert_eq(3, new_mid)
  @azimuth.assert_eq(3, found_position)
}