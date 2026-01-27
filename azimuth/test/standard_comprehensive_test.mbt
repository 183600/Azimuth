// 标准 MoonBit 测试用例 - 集合测试
// 专注于核心功能的全面测试覆盖

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(15, @azimuth.subtract(10, -5))
  @azimuth.assert_eq(-15, @azimuth.subtract(-10, 5))
  @azimuth.assert_eq(0, @azimuth.subtract(0, 0))
  @azimuth.assert_eq(2147483647, @azimuth.subtract(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.subtract(-2147483648, 0))
}

test "divide_with_ceil_precision_edge_cases" {
  // 测试向上取整除法的精度边缘情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-3, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(3, -2))
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(999, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1000))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1000))
}

test "greet_function_whitespace_and_special_chars" {
  // 测试问候函数的空白字符和特殊字符处理
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \t\n\r!", @azimuth.greet("\t\n\r"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  @azimuth.assert_eq_string("Hello, 'single'!", @azimuth.greet("'single'"))
  @azimuth.assert_eq_string("Hello, \\backslash\\!", @azimuth.greet("\\backslash\\"))
  @azimuth.assert_eq_string("Hello, []{}()!", @azimuth.greet("[]{}()"))
  @azimuth.assert_eq_string("Hello, @#$%^&*!", @azimuth.greet("@#$%^&*"))
  @azimuth.assert_eq_string("Hello, emoji🎉!", @azimuth.greet("emoji🎉"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let rate1 = 5  // 第一年利率5%
  let rate2 = 3  // 第二年利率3%
  let rate3 = 4  // 第三年利率4%
  
  // 计算三年复利
  let year1_end = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate1), 100))
  let year2_end = @azimuth.add(year1_end, @azimuth.divide_with_ceil(@azimuth.multiply(year1_end, rate2), 100))
  let year3_end = @azimuth.add(year2_end, @azimuth.divide_with_ceil(@azimuth.multiply(year2_end, rate3), 100))
  
  @azimuth.assert_eq(10500, year1_end)  // 10000 + 5%
  @azimuth.assert_eq(10815, year2_end)  // 10500 + 3%
  @azimuth.assert_eq(11247, year3_end)  // 10815 + 4%
  
  // 计算月供（36个月分期）
  let monthly_payment = @azimuth.divide_with_ceil(year3_end, 36)
  @azimuth.assert_eq(313, monthly_payment)  // ceil(11247/36)
}

test "inventory_management_optimization" {
  // 库存管理优化场景
  let product_a = 127
  let product_b = 89
  let product_c = 156
  let box_capacity = 24
  
  // 计算每种产品需要的箱子数量
  let boxes_a = @azimuth.divide_with_ceil(product_a, box_capacity)
  let boxes_b = @azimuth.divide_with_ceil(product_b, box_capacity)
  let boxes_c = @azimuth.divide_with_ceil(product_c, box_capacity)
  
  @azimuth.assert_eq(6, boxes_a)  // ceil(127/24)
  @azimuth.assert_eq(4, boxes_b)  // ceil(89/24)
  @azimuth.assert_eq(7, boxes_c)  // ceil(156/24)
  
  // 计算总箱子数和总容量
  let total_boxes = @azimuth.add(@azimuth.add(boxes_a, boxes_b), boxes_c)
  let total_capacity = @azimuth.multiply(total_boxes, box_capacity)
  let total_products = @azimuth.add(@azimuth.add(product_a, product_b), product_c)
  
  @azimuth.assert_eq(17, total_boxes)
  @azimuth.assert_eq(408, total_capacity)
  @azimuth.assert_eq(372, total_products)
  
  // 计算空间利用率
  let utilization = @azimuth.divide_with_ceil(@azimuth.multiply(total_products, 100), total_capacity)
  @azimuth.assert_eq(92, utilization)  // ceil(37200/408) = 92%
}

test "mathematical_sequence_calculation" {
  // 数学序列计算场景
  // 斐波那契式序列（简化版）
  let n1 = 1
  let n2 = 1
  let n3 = @azimuth.add(n1, n2)  // 2
  let n4 = @azimuth.add(n2, n3)  // 3
  let n5 = @azimuth.add(n3, n4)  // 5
  let n6 = @azimuth.add(n4, n5)  // 8
  let n7 = @azimuth.add(n5, n6)  // 13
  let n8 = @azimuth.add(n6, n7)  // 21
  
  @azimuth.assert_eq(2, n3)
  @azimuth.assert_eq(3, n4)
  @azimuth.assert_eq(5, n5)
  @azimuth.assert_eq(8, n6)
  @azimuth.assert_eq(13, n7)
  @azimuth.assert_eq(21, n8)
  
  // 计算序列的乘积和
  let sequence_sum = @azimuth.add(@azimuth.add(@azimuth.add(n5, n6), n7), n8)
  let sequence_product = @azimuth.multiply(@azimuth.multiply(n5, n6), n7)
  
  @azimuth.assert_eq(47, sequence_sum)  // 5 + 8 + 13 + 21
  @azimuth.assert_eq(520, sequence_product)  // 5 * 8 * 13
}

test "resource_allocation_fair_distribution" {
  // 资源分配公平分布场景
  let total_budget = 10000
  let team_members = 7
  let min_bonus = 500
  
  // 计算平均分配
  let avg_share = @azimuth.divide_with_ceil(total_budget, team_members)
  @azimuth.assert_eq(1429, avg_share)  // ceil(10000/7)
  
  // 计算总分配金额
  let total_allocated = @azimuth.multiply(avg_share, team_members)
  @azimuth.assert_eq(10003, total_allocated)
  
  // 计算剩余/超出的金额
  let excess = @azimuth.subtract(total_allocated, total_budget)
  @azimuth.assert_eq(3, excess)
  
  // 验证每个成员都能获得最低奖金
  @azimuth.assert_true(avg_share >= min_bonus)
  
  // 计算实际可用奖金（减去最低奖金后的余额）
  let remaining_budget = @azimuth.subtract(total_budget, @azimuth.multiply(min_bonus, team_members))
  let additional_bonus = @azimuth.divide_with_ceil(remaining_budget, team_members)
  @azimuth.assert_eq(929, additional_bonus)  // ceil(6500/7)
}

test "error_handling_boundary_conditions" {
  // 错误处理边界条件测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试加法边界
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, max_int))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, min_int))
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, -1))
  
  // 测试减法边界
  @azimuth.assert_eq(max_int, @azimuth.subtract(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.subtract(min_int, 0))
  @azimuth.assert_eq(max_int, @azimuth.subtract(max_int, min_int))
  
  // 测试乘法边界
  @azimuth.assert_eq(max_int, @azimuth.multiply(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, 1))
  @azimuth.assert_eq(max_int, @azimuth.multiply(max_int, -1))
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, -1))
  
  // 测试除法边界
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(min_int, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(max_int, max_int))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(min_int, min_int))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：电商订单处理
  
  // 订单基本信息
  let items_count = 15
  let unit_price = 199
  let shipping_cost = 25
  let tax_rate = 8  // 8%
  let discount = -50
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(items_count, unit_price)
  @azimuth.assert_eq(2985, subtotal)
  
  // 应用折扣
  let discounted_total = @azimuth.add(subtotal, discount)
  @azimuth.assert_eq(2935, discounted_total)
  
  // 计算税费
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(discounted_total, tax_rate), 100)
  @azimuth.assert_eq(235, tax_amount)
  
  // 加上运费和税费
  let total_with_shipping = @azimuth.add(discounted_total, shipping_cost)
  let final_total = @azimuth.add(total_with_shipping, tax_amount)
  @azimuth.assert_eq(3195, final_total)
  
  // 计算包装需求（每箱最多5件）
  let boxes_needed = @azimuth.divide_with_ceil(items_count, 5)
  @azimuth.assert_eq(3, boxes_needed)
  
  // 生成客户问候
  let customer_name = "张三"
  let greeting = @azimuth.greet(customer_name)
  @azimuth.assert_eq_string("Hello, 张三!", greeting)
  
  // 计算预期交付天数（每箱处理1天）
  let delivery_days = @azimuth.add(boxes_needed, 2)  // 基础处理2天 + 包装天数
  @azimuth.assert_eq(5, delivery_days)
}