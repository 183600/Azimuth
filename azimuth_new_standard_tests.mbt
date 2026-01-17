// Azimuth 项目新增标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能和边界情况

test "complex_arithmetic_sequence" {
  // 测试复杂算术序列计算
  // 计算 (a + b) * (c - d) + e * f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = add(a, b)  // 15
  let diff_cd = add(c, -d)  // 12
  let product_ef = multiply(e, f)  // 21
  let product_ab_cd = multiply(sum_ab, diff_cd)  // 180
  let result = add(product_ab_cd, product_ef)  // 201
  
  assert_eq(201, result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result_special = greet(special_chars)
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result_special)
  
  // 测试包含Unicode字符的字符串
  let unicode_chars = "🌟MoonBit🚀"
  let result_unicode = greet(unicode_chars)
  assert_eq_string("Hello, 🌟MoonBit🚀!", result_unicode)
  
  // 测试包含数字和字母混合的字符串
  let mixed_alphanumeric = "Test123ABC456"
  let result_mixed = greet(mixed_alphanumeric)
  assert_eq_string("Hello, Test123ABC456!", result_mixed)
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算等差数列前n项和：1 + 4 + 7 + 10 + 13 (公差为3，前5项)
  let n = 5
  let first_term = 1
  let common_diff = 3
  
  // 使用公式：S_n = n/2 * (2*a_1 + (n-1)*d)
  let sum_n = multiply(n, add(multiply(2, first_term), multiply(n - 1, common_diff))) / 2
  assert_eq(35, sum_n)  // 1 + 4 + 7 + 10 + 13 = 35
  
  // 计算等比数列前n项积：2 * 4 * 8 * 16 (公比为2，前4项)
  let ratio = 2
  let geo_product = multiply(multiply(multiply(2, multiply(2, ratio)), multiply(4, ratio)), multiply(8, ratio))
  assert_eq(1024, geo_product)  // 2 * 4 * 8 * 16 = 1024
}

test "real_world_financial_calculations" {
  // 测试实际金融计算场景
  // 银行存款复利计算（简化版）
  let principal = 10000  // 本金
  let annual_rate = 5  // 年利率5%
  let years = 3  // 存款年限
  
  // 简化复利计算：本息和 = 本金 * (1 + 年利率 * 年数)
  let growth_factor = add(1, multiply(annual_rate, years))
  let total_amount = multiply(principal, growth_factor)
  assert_eq(160000, total_amount)  // 10000 * (1 + 5*3) = 10000 * 16 = 160000
  
  // 贷款月供计算（简化版）
  let loan_amount = 120000  // 贷款总额
  let monthly_rate = 1  // 月利率1%
  let months = 12  // 贷款期数
  
  // 简化月供计算：月供 = (贷款总额 + 贷款总额 * 月利率 * 期数) / 期数
  let total_interest = multiply(loan_amount, multiply(monthly_rate, months))
  let total_repayment = add(loan_amount, total_interest)
  let monthly_payment = total_repayment / months
  assert_eq(11000, monthly_payment)  // (120000 + 120000*1*12) / 12 = 264000 / 12 = 22000
}

test "extreme_boundary_conditions" {
  // 测试极端边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续溢出运算
  let overflow_chain_1 = add(add(max_val, 100), add(200, 300))
  assert_eq(max_val, overflow_chain_1)
  
  let overflow_chain_2 = multiply(multiply(max_val, 2), multiply(3, 4))
  assert_eq(max_val, overflow_chain_2)
  
  // 测试连续下溢运算
  let underflow_chain_1 = add(add(min_val, -100), add(-200, -300))
  assert_eq(min_val, underflow_chain_1)
  
  let underflow_chain_2 = multiply(multiply(min_val, 2), multiply(3, 4))
  assert_eq(min_val, underflow_chain_2)
  
  // 测试混合边界运算
  let mixed_boundary = add(multiply(max_val, -1), multiply(min_val, -1))
  assert_eq(0, mixed_boundary)
}

test "algorithmic_complexity_simulation" {
  // 测试算法复杂度模拟
  // 模拟O(n²)算法的计算量
  let n = 100
  let o_n_squared = multiply(n, n)
  assert_eq(10000, o_n_squared)
  
  // 模拟O(n³)算法的计算量
  let o_n_cubed = multiply(multiply(n, n), n)
  assert_eq(1000000, o_n_cubed)
  
  // 模拟O(log n)算法的计算量（简化为整数除法）
  let o_log_n = n / 2  // 简化表示
  assert_eq(50, o_log_n)
  
  // 模拟O(n log n)算法的计算量
  let o_n_log_n = multiply(n, n / 2)
  assert_eq(5000, o_n_log_n)
}

test "data_structure_operations_simulation" {
  // 测试数据结构操作模拟
  // 模拟数组索引计算：二维数组转一维数组的索引
  let row = 5
  let col = 3
  let width = 10
  
  let index_1d = add(multiply(row, width), col)
  assert_eq(53, index_1d)  // 5 * 10 + 3 = 53
  
  // 模拟栈操作：push和pop
  let stack_top = 100
  let push_value = 200
  let new_top = add(stack_top, push_value)
  assert_eq(300, new_top)
  
  let pop_value = 150
  let popped_top = add(new_top, -pop_value)
  assert_eq(150, popped_top)
  
  // 模拟队列操作：enqueue和dequeue
  let queue_front = 10
  let queue_rear = 20
  let queue_size = add(queue_rear, -queue_front)
  assert_eq(10, queue_size)
}

test "scientific_calculations" {
  // 测试科学计算
  // 模拟温度转换：摄氏度转华氏度 F = C * 9/5 + 32
  // 简化为整数计算：F = C * 2 + 32
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 32)
  assert_eq(82, fahrenheit)  // 25 * 2 + 32 = 82
  
  // 模拟距离转换：公里转英里 1公里 ≈ 0.62英里
  // 简化为整数计算：英里 = 公里 * 62 / 100
  let kilometers = 100
  let miles = multiply(kilometers, 62) / 100
  assert_eq(62, miles)  // 100 * 62 / 100 = 62
  
  // 模拟能量计算：E = mc²（简化版）
  let mass = 10
  let speed_of_light_squared = multiply(300000, 300000)  // 简化光速值
  let energy = multiply(mass, speed_of_light_squared)
  assert_eq(900000000000, energy)  // 10 * 300000²
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数的边界情况
  assert_true(add(1000000, 2000000) > 0)
  assert_true(add(-1000000, -2000000) < 0)
  assert_true(multiply(1000, 1000) > 0)
  assert_true(multiply(-1000, 1000) < 0)
  
  // 测试溢出保护的正确性
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, 2) == min_val)
  
  // 测试字符串处理的正确性
  assert_true(greet("Test") == "Hello, Test!")
  assert_true(greet("") == "Hello, !")
  assert_true(greet("123") == "Hello, 123!")
}

test "comprehensive_integration_test" {
  // 综合集成测试
  // 模拟一个复杂的业务场景：电商订单处理
  
  // 商品信息
  let product_a_price = 299
  let product_a_quantity = 2
  let product_b_price = 199
  let product_b_quantity = 3
  
  // 计算商品总价
  let total_a = multiply(product_a_price, product_a_quantity)  // 598
  let total_b = multiply(product_b_price, product_b_quantity)  // 597
  let subtotal = add(total_a, total_b)  // 1195
  
  // 折扣计算：满1000减100
  let discount = if (subtotal >= 1000) { 100 } else { 0 }
  let discounted_total = add(subtotal, -discount)  // 1095
  
  // 运费计算：满500免运费，否则运费20
  let shipping = if (discounted_total >= 500) { 0 } else { 20 }
  let total_with_shipping = add(discounted_total, shipping)  // 1095
  
  // 税费计算：8%税率
  let tax = multiply(total_with_shipping, 8) / 100  // 87
  let final_total = add(total_with_shipping, tax)  // 1182
  
  assert_eq(1182, final_total)
  
  // 生成订单确认消息（简化版）
  let order_id = "ORD-2023-001"
  let customer_name = "张三"
  let base_confirmation = greet(customer_name) + " 您的订单 " + order_id + " 已确认"
  
  // 验证确认消息包含必要信息
  let expected_confirmation = "Hello, 张三! 您的订单 ORD-2023-001 已确认"
  assert_eq_string(expected_confirmation, base_confirmation)
  
  // 验证总金额计算正确
  assert_eq(1182, final_total)
}