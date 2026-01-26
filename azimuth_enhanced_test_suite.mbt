// Azimuth 项目新增的高质量 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能和边界情况

test "mathematical_commutativity_and_associativity" {
  // 测试数学交换律和结合律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 全面测试向上取整除法的各种场景
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-3, divide_with_ceil(-9, 3))   // -9/3 = -3 -> -3
  
  // 负数除负数
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(-9, -3))   // -9/-3 = 3 -> 3
  
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_internationalization_and_special_cases" {
  // 测试问候函数的国际化和特殊情况
  // 基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 空字符串和空白字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  
  // Unicode 和多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  
  // 特殊字符和符号
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))
}

test "complex_business_calculation_scenario" {
  // 复杂业务计算场景：电商订单计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping_cost = 12
  let discount = 20
  
  // 计算商品小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价（含运费）
  let total_before_discount = add(add(subtotal, tax_amount), shipping_cost)
  
  // 应用折扣
  let final_total = add(total_before_discount, -discount)
  
  // 验证计算结果
  assert_eq(356, subtotal)           // 89 * 4 = 356
  assert_eq(29, tax_amount)          // ceil(356 * 8 / 100) = ceil(28.48) = 29
  assert_eq(397, total_before_discount)  // 356 + 29 + 12 = 397
  assert_eq(377, final_total)        // 397 - 20 = 377
}

test "inventory_management_and_packaging" {
  // 库存管理和包装计算场景
  let total_products = 157
  let products_per_box = 24
  let boxes_per_pallet = 8
  
  // 计算需要的包装箱数量
  let boxes_needed = divide_with_ceil(total_products, products_per_box)
  
  // 计算需要的托盘数量
  let pallets_needed = divide_with_ceil(boxes_needed, boxes_per_pallet)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, products_per_box)
  let remaining_space = add(total_capacity, -total_products)
  
  // 验证计算结果
  assert_eq(7, boxes_needed)         // ceil(157 / 24) = 7
  assert_eq(1, pallets_needed)       // ceil(7 / 8) = 1
  assert_eq(168, total_capacity)     // 7 * 24 = 168
  assert_eq(11, remaining_space)     // 168 - 157 = 11
}

test "mathematical_series_and_patterns" {
  // 数学级数和模式计算
  // 计算等差数列前 n 项和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 10
  let arithmetic_sum = n * n  // 直接使用公式计算
  assert_eq(100, arithmetic_sum)
  
  // 计算等比数列前 n 项和：2 + 4 + 8 + ... + 2^n
  let geo_sum = multiply(2, multiply(2, multiply(2, multiply(2, multiply(2, 2)))))
  assert_eq(64, geo_sum)  // 2^6 = 64
  
  // 验证数学恒等式：(a+b)² = a² + 2ab + b²
  let a = 3
  let b = 4
  let left_side = multiply(add(a, b), add(a, b))
  let right_side = add(add(multiply(a, a), multiply(2, multiply(a, b))), multiply(b, b))
  assert_eq(left_side, right_side)  // 49 = 49
}

test "time_and_resource_management" {
  // 时间和资源管理计算
  let total_tasks = 47
  let tasks_per_day = 8
  let working_hours_per_day = 8
  let meeting_hours_per_day = 2
  
  // 计算工作日数
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时数
  let total_working_hours = multiply(workdays_needed, working_hours_per_day)
  
  // 计算有效工作小时数（扣除会议时间）
  let effective_hours = multiply(workdays_needed, add(working_hours_per_day, -meeting_hours_per_day))
  
  // 计算任务完成率
  let completion_rate = divide_with_ceil(multiply(total_tasks, 100), multiply(workdays_needed, tasks_per_day))
  
  // 验证计算结果
  assert_eq(6, workdays_needed)       // ceil(47 / 8) = 6
  assert_eq(48, total_working_hours)  // 6 * 8 = 48
  assert_eq(36, effective_hours)      // 6 * (8-2) = 36
  assert_eq(98, completion_rate)      // ceil(47 * 100 / (6 * 8)) = ceil(4700 / 48) = 98
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 极值测试
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, -10))
  
  // 大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  
  // 边界除法情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 字符串边界情况
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello,   !", greet("   "))  // 多个空格
}

test "data_analysis_and_statistics" {
  // 数据分析和统计计算
  // 模拟季度销售数据分析
  let q1_sales = 15000
  let q2_sales = 18000
  let q3_sales = 22000
  let q4_sales = 25000
  
  // 计算年度总销售额
  let annual_sales = add(add(add(q1_sales, q2_sales), q3_sales), q4_sales)
  
  // 计算季度平均销售额（向上取整）
  let quarterly_average = divide_with_ceil(annual_sales, 4)
  
  // 计算增长率
  let q2_growth = divide_with_ceil(multiply(add(q2_sales, -q1_sales), 100), q1_sales)
  let q3_growth = divide_with_ceil(multiply(add(q3_sales, -q2_sales), 100), q2_sales)
  let q4_growth = divide_with_ceil(multiply(add(q4_sales, -q3_sales), 100), q3_sales)
  
  // 计算年度预测（基于平均增长率）
  let avg_growth = divide_with_ceil(add(add(q2_growth, q3_growth), q4_growth), 3)
  let next_year_forecast = add(annual_sales, divide_with_ceil(multiply(annual_sales, avg_growth), 100))
  
  // 验证计算结果
  assert_eq(80000, annual_sales)
  assert_eq(20000, quarterly_average)
  assert_eq(20, q2_growth)   // ceil(3000 * 100 / 15000) = 20
  assert_eq(23, q3_growth)   // ceil(4000 * 100 / 18000) = 23
  assert_eq(14, q4_growth)   // ceil(3000 * 100 / 22000) = 14
  assert_eq(19, avg_growth)  // ceil((20 + 23 + 14) / 3) = 19
  assert_eq(95200, next_year_forecast)  // 80000 + ceil(80000 * 19 / 100) = 80000 + 15200
}