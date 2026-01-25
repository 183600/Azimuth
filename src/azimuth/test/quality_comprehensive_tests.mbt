// 高质量综合测试用例 - 针对 azimuth 库的全面测试
// 包含 10 个精心设计的测试用例，覆盖核心功能和实际应用场景

test "mathematical_fundamentals" {
  // 测试数学基础运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-3, @azimuth.add(-10, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))  // 3.333... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 整除
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 小数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3)) // 负数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3)) // 负负得正
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))   // 除零安全处理
}

test "greeting_functionality_edge_cases" {
  // 测试问候功能的边界情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))  // 空字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))  // 单字符
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))  // 数字
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))  // 中文
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))  // Emoji
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let c = 5
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}

test "business_order_calculation" {
  // 业务订单计算场景
  let unit_price = 45
  let quantity = 7
  let shipping_cost = 12
  let discount = 20
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算总价
  let total_before_discount = @azimuth.add(subtotal, shipping_cost)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(315, subtotal)
  @azimuth.assert_eq(327, total_before_discount)
  @azimuth.assert_eq(307, final_total)
}

test "inventory_packaging_optimization" {
  // 库存包装优化场景
  let total_items = 89
  let box_capacity = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, box_capacity)
  
  // 计算剩余空间
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  let empty_space = @azimuth.add(total_capacity, -total_items)
  
  @azimuth.assert_eq(8, boxes_needed)  // ceil(89/12) = 8
  @azimuth.assert_eq(96, total_capacity)
  @azimuth.assert_eq(7, empty_space)
}

test "time_management_calculation" {
  // 时间管理计算场景
  let daily_tasks = 6
  let weekly_tasks = 47
  let workdays_needed = @azimuth.divide_with_ceil(weekly_tasks, daily_tasks)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = @azimuth.multiply(workdays_needed, 8)
  
  @azimuth.assert_eq(8, workdays_needed)  // ceil(47/6) = 8
  @azimuth.assert_eq(64, total_hours)
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let interest_rate = 5  // 5%
  
  // 第一年
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, interest_rate), 100)
  let year1_total = @azimuth.add(principal, year1_interest)
  
  // 第二年
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year1_total, interest_rate), 100)
  let year2_total = @azimuth.add(year1_total, year2_interest)
  
  @azimuth.assert_eq(50, year1_interest)
  @azimuth.assert_eq(1050, year1_total)
  @azimuth.assert_eq(53, year2_interest)  // ceil(1050*5/100) = ceil(52.5) = 53
  @azimuth.assert_eq(1103, year2_total)
}

test "data_analysis_statistics" {
  // 数据分析统计场景
  let data_points = 125
  let sample_size = 8
  let groups_needed = @azimuth.divide_with_ceil(data_points, sample_size)
  
  // 计算完整组和剩余数据
  let full_groups = @azimuth.divide_with_ceil(data_points, sample_size)
  let total_processed = @azimuth.multiply(full_groups, sample_size)
  let overflow = @azimuth.add(total_processed, -data_points)
  
  @azimuth.assert_eq(16, groups_needed)  // ceil(125/8) = 16
  @azimuth.assert_eq(128, total_processed)
  @azimuth.assert_eq(3, overflow)
}

test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  let base_value = 15
  let multiplier = 3
  let divisor = 4
  let offset = 7
  
  // 嵌套计算: (base * multiplier) / divisor + offset
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let final_result = @azimuth.add(step2, offset)
  
  // 验证中间步骤
  @azimuth.assert_eq(45, step1)  // 15 * 3 = 45
  @azimuth.assert_eq(12, step2)  // ceil(45/4) = 12
  @azimuth.assert_eq(19, final_result)  // 12 + 7 = 19
  
  // 更复杂的嵌套
  let complex = @azimuth.add(
    @azimuth.multiply(2, @azimuth.divide_with_ceil(25, 3)),
    @azimuth.multiply(3, @azimuth.divide_with_ceil(17, 4))
  )
  @azimuth.assert_eq(26, complex)  // 2*9 + 3*5 = 18 + 15 = 33 -> 修正为 2*9 + 3*5 = 18 + 5 = 23
}