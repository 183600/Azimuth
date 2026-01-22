// Enhanced MoonBit 测试用例
// 为 Azimuth 项目新增的10个标准测试用例

test "add_function_comprehensive_boundary_test" {
  // 全面测试 add 函数的边界条件和特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负数溢出边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试零值特性
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(123, @azimuth.add(0, 123))
  @azimuth.assert_eq(-456, @azimuth.add(-456, 0))
  
  // 测试对称性
  @azimuth.assert_eq(@azimuth.add(100, 200), @azimuth.add(200, 100))
  @azimuth.assert_eq(@azimuth.add(-100, 200), @azimuth.add(200, -100))
}

test "multiply_function_mathematical_properties" {
  // 测试 multiply 函数的数学性质
  // 测试乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(7, 13), @azimuth.multiply(13, 7))
  @azimuth.assert_eq(@azimuth.multiply(-8, 15), @azimuth.multiply(15, -8))
  @azimuth.assert_eq(@azimuth.multiply(-12, -9), @azimuth.multiply(-9, -12))
  
  // 测试乘法结合律：(a * b) * c = a * (b * c)
  let a = 3
  let b = 4
  let c = 5
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 测试分配律：a * (b + c) = a * b + a * c
  @azimuth.assert_eq(@azimuth.multiply(3, @azimuth.add(4, 5)), @azimuth.add(@azimuth.multiply(3, 4), @azimuth.multiply(3, 5)))
  
  // 测试特殊值
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-1, 12345))
  
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "greet_function_string_processing_comprehensive" {
  // 全面测试 greet 函数的字符串处理能力
  // 测试基本字符串
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 测试空字符串和单字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试数字字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
  
  // 测试特殊字符
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,  Leading and trailing  !", @azimuth.greet(" Leading and trailing  "))
}

test "complex_computation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算多项式值 ax² + bx + c
  let x = 5
  let a = 2
  let b = 3
  let c = 7
  let x_squared = @azimuth.multiply(x, x)
  let ax_squared = @azimuth.multiply(a, x_squared)
  let bx = @azimuth.multiply(b, x)
  let polynomial_result = @azimuth.add(@azimuth.add(ax_squared, bx), c)
  @azimuth.assert_eq(72, polynomial_result)  // 2*25 + 3*5 + 7 = 50 + 15 + 7 = 72
  
  // 场景2：计算圆的面积和周长（简化π=3）
  let radius = 8
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)  // 2πr
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))      // πr²
  @azimuth.assert_eq(48, circumference)  // 2 * 3 * 8 = 48
  @azimuth.assert_eq(192, area)          // 3 * 8 * 8 = 192
  
  // 场景3：计算平均数
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(85, 92), 78), 96), 89)
  let average = sum / 5
  @azimuth.assert_eq(440, sum)      // 85 + 92 + 78 + 96 + 89 = 440
  @azimuth.assert_eq(88, average)   // 440 / 5 = 88
}

test "practical_business_calculations" {
  // 测试实际业务计算场景
  // 场景1：购物车总价计算
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 3
  let item3_price = 99
  let item3_qty = 1
  let shipping_fee = 15
  let tax_rate = 8
  
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  let subtotal3 = @azimuth.multiply(item3_price, item3_qty)
  let items_total = @azimuth.add(@azimuth.add(subtotal1, subtotal2), subtotal3)
  let tax_amount = @azimuth.multiply(items_total, tax_rate) / 100
  let final_total = @azimuth.add(@azimuth.add(items_total, tax_amount), shipping_fee)
  
  @azimuth.assert_eq(1294, items_total)  // 299*2 + 199*3 + 99*1 = 598 + 597 + 99 = 1294
  @azimuth.assert_eq(103, tax_amount)    // 1294 * 8 / 100 = 103.52 ≈ 103
  @azimuth.assert_eq(1412, final_total)  // 1294 + 103 + 15 = 1412
  
  // 场景2：贷款利息计算
  let principal = 10000
  let monthly_rate = 5
  let months = 12
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(monthly_rate, months)) / 100
  let total_payment = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(6000, total_interest)  // 10000 * 5 * 12 / 100 = 6000
  @azimuth.assert_eq(16000, total_payment)  // 10000 + 6000 = 16000
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(@azimuth.add(max_val, 1, true) == max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1, true) == min_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2, true) == max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1, true) == min_val)
  
  // 测试断言函数
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试除零保护（通过 divide_with_ceil 函数）
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(9, 2))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-9, 2))
  
  // 测试特殊数学运算
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
  
  // 测试最小值特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "algorithm_implementation_tests" {
  // 测试基本算法实现
  // 算法1：计算阶乘 n! = n × (n-1) × ... × 1
  let n = 5
  let factorial = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, factorial)  // 5! = 120
  
  // 算法2：计算幂运算 2^10 = 1024
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2), 2), 
                             @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2), 2), 2))
  @azimuth.assert_eq(1024, power_of_2)
  
  // 算法3：计算等差数列和 1 + 3 + 5 + ... + 19 (前10个奇数)
  let first_term = 1
  let last_term = 19
  let num_terms = 10
  let arithmetic_sum = @azimuth.multiply(num_terms, @azimuth.add(first_term, last_term)) / 2
  @azimuth.assert_eq(100, arithmetic_sum)  // 10 * (1 + 19) / 2 = 100
  
  // 算法4：计算等比数列和 2 + 4 + 8 + 16 + 32 (前5项)
  let ratio = 2
  let terms = 5
  let geo_sum = @azimuth.multiply(2, @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(1, ratio), ratio), ratio), ratio))
  @azimuth.assert_eq(62, geo_sum)  // 2 + 4 + 8 + 16 + 32 = 62
}

test "data_conversion_and_formatting" {
  // 测试数据转换和格式化场景
  // 场景1：模拟数字到字符串的转换
  let year_str = "2023"
  let month_str = "12"
  let day_str = "25"
  let formatted_date = year_str + "-" + month_str + "-" + day_str
  @azimuth.assert_eq_string("2023-12-25", formatted_date)
  
  // 场景2：模拟货币格式化
  let amount_str = "1234"
  let formatted_currency = "$" + amount_str + ".00"
  @azimuth.assert_eq_string("$1234.00", formatted_currency)
  
  // 场景3：模拟百分比格式
  let percentage_str = "75"
  let formatted_percentage = percentage_str + "%"
  @azimuth.assert_eq_string("75%", formatted_percentage)
  
  // 测试 greet 函数作为格式化工具
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
  @azimuth.assert_eq_string("Hello, $1234.00!", @azimuth.greet("$1234.00"))
  @azimuth.assert_eq_string("Hello, 75%!", @azimuth.greet("75%"))
}

test "performance_and_optimization_tests" {
  // 测试性能相关的计算和优化场景
  // 场景1：批量计算优化
  let base_value = 100
  let iterations = 10
  
  // 使用公式优化：n × (n + 1) / 2 计算连续和
  let optimized_sum = @azimuth.multiply(base_value, @azimuth.multiply(iterations, @azimuth.add(iterations, 1)) / 2)
  @azimuth.assert_eq(5500, optimized_sum)  // 100 * 10 * 11 / 2 = 5500
  
  // 场景2：幂运算优化
  // 计算 x^8 可以通过 (x^2)^4 优化
  let x = 3
  let x_squared = @azimuth.multiply(x, x)
  let x_to_4th = @azimuth.multiply(x_squared, x_squared)
  let x_to_8th = @azimuth.multiply(x_to_4th, x_to_4th)
  @azimuth.assert_eq(6561, x_to_8th)  // 3^8 = 6561
  
  // 场景3：重复计算缓存
  let common_factor = @azimuth.multiply(7, 13)  // 91
  let result1 = @azimuth.multiply(common_factor, 5)   // 455
  let result2 = @azimuth.multiply(common_factor, 8)   // 728
  let result3 = @azimuth.multiply(common_factor, 12)  // 1092
  
  @azimuth.assert_eq(455, result1)
  @azimuth.assert_eq(728, result2)
  @azimuth.assert_eq(1092, result3)
  
  // 场景4：大数运算优化
  let large_num = 1000000
  let optimized_large_mult = @azimuth.multiply(large_num, 1234)
  @azimuth.assert_eq(1234000000, optimized_large_mult)
}

test "complex_business_logic_simulation" {
  // 测试复杂业务逻辑模拟
  // 场景1：库存管理系统
  let initial_stock = 1000
  let sales_day1 = 150
  let sales_day2 = 200
  let sales_day3 = 175
  let restock_day2 = 300
  let restock_day3 = 500
  
  // 计算每日库存变化
  let stock_after_day1 = @azimuth.add(initial_stock, -sales_day1)
  let stock_after_day2_before_restock = @azimuth.add(stock_after_day1, -sales_day2)
  let stock_after_day2_after_restock = @azimuth.add(stock_after_day2_before_restock, restock_day2)
  let stock_after_day3_before_restock = @azimuth.add(stock_after_day2_after_restock, -sales_day3)
  let final_stock = @azimuth.add(stock_after_day3_before_restock, restock_day3)
  
  @azimuth.assert_eq(850, stock_after_day1)           // 1000 - 150 = 850
  @azimuth.assert_eq(650, stock_after_day2_before_restock)  // 850 - 200 = 650
  @azimuth.assert_eq(950, stock_after_day2_after_restock)  // 650 + 300 = 950
  @azimuth.assert_eq(775, stock_after_day3_before_restock)  // 950 - 175 = 775
  @azimuth.assert_eq(1275, final_stock)               // 775 + 500 = 1275
  
  // 场景2：会员积分系统
  let base_points = 100
  let purchase_amount = 500
  let tier_multiplier = 2
  let bonus_threshold = 1000
  let bonus_points = 200
  
  let regular_points = @azimuth.multiply(purchase_amount, base_points) / 100
  let tier_points = @azimuth.multiply(regular_points, tier_multiplier)
  let total_points = if (regular_points > bonus_threshold) {
    @azimuth.add(tier_points, bonus_points)
  } else {
    tier_points
  }
  
  @azimuth.assert_eq(500, regular_points)  // 500 * 100 / 100 = 500
  @azimuth.assert_eq(1000, tier_points)    // 500 * 2 = 1000
  @azimuth.assert_eq(1000, total_points)   // 500 <= 1000，所以不加奖励积分
  
  // 场景3：折扣计算系统
  let original_price = 1000
  let quantity = 5
  let bulk_discount_threshold = 3
  let bulk_discount_rate = 10
  let vip_discount_rate = 5
  
  let subtotal = @azimuth.multiply(original_price, quantity)
  let bulk_discount = if (quantity > bulk_discount_threshold) {
    @azimuth.multiply(subtotal, bulk_discount_rate) / 100
  } else {
    0
  }
  
  let vip_discount = @azimuth.multiply(subtotal, vip_discount_rate) / 100
  let final_price = @azimuth.add(subtotal, @azimuth.add(-bulk_discount, -vip_discount))
  
  @azimuth.assert_eq(5000, subtotal)      // 1000 * 5 = 5000
  @azimuth.assert_eq(500, bulk_discount)  // 5000 * 10 / 100 = 500
  @azimuth.assert_eq(250, vip_discount)   // 5000 * 5 / 100 = 250
  @azimuth.assert_eq(4250, final_price)   // 5000 - 500 - 250 = 4250
}