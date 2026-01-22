// 高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  @azimuth.assert_eq(@azimuth.add(10, 15), 25)
  @azimuth.assert_eq(@azimuth.add(-5, 3), -2)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.add(1000, -1000), 0)
  @azimuth.assert_eq(@azimuth.add(-7, -8), -15)
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(@azimuth.multiply(6, 7), 42)
  @azimuth.assert_eq(@azimuth.multiply(-4, 5), -20)
  @azimuth.assert_eq(@azimuth.multiply(0, 123), 0)
  @azimuth.assert_eq(@azimuth.multiply(1, 999), 999)
  @azimuth.assert_eq(@azimuth.multiply(-3, -9), 27)
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, 3), 4)  // 3.333... -> 4
  @azimuth.assert_eq(@azimuth.divide_with_ceil(9, 3), 3)   // 整除
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-10, 3), -3)  // -3.333... -> -3
  @azimuth.assert_eq(@azimuth.divide_with_ceil(1, 100), 1)   // 0.01 -> 1
  @azimuth.assert_eq(@azimuth.divide_with_ceil(0, 5), 0)    // 0/5 = 0
  @azimuth.assert_eq(@azimuth.divide_with_ceil(25, 0), 0)   // 除数为0的安全处理
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  @azimuth.assert_eq_string(@azimuth.greet("世界"), "Hello, 世界!")
  @azimuth.assert_eq_string(@azimuth.greet("🚀"), "Hello, 🚀!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet("María"), "Hello, María!")
  @azimuth.assert_eq_string(@azimuth.greet("123"), "Hello, 123!")
}

test "real_world_business_calculation" {
  // 实际业务计算：订单总价计算
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = @azimuth.multiply(product_price, quantity)
  let total_before_discount = @azimuth.add(subtotal, shipping)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(356, subtotal)  // 89 * 4
  @azimuth.assert_eq(368, total_before_discount)  // 356 + 12
  @azimuth.assert_eq(348, final_total)  // 368 - 20
}

test "inventory_packaging_optimization" {
  // 库存包装优化：计算需要的包装箱数量
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  @azimuth.assert_eq(48, total_capacity)  // 4 * 12 = 48
}

test "mathematical_properties_verification" {
  // 数学性质验证：交换律和分配律
  let a = 8
  let b = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律：a * (b + c) = a * b + a * c
  let c = 7
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "financial_calculation_scenario" {
  // 金融计算场景：复利计算
  let principal = 1000
  let rate_percent = 5
  
  // 第一年
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  @azimuth.assert_eq(1050, year1)
  
  // 第二年
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate_percent), 100))
  @azimuth.assert_eq(1103, year2)
  
  // 第三年
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate_percent), 100))
  @azimuth.assert_eq(1158, year3)
}

test "data_analysis_statistics" {
  // 数据分析统计：销售数据分析
  let q1_sales = 15000
  let q2_sales = 18000
  let q3_sales = 22000
  let q4_sales = 25000
  
  // 年度总销售额
  let annual_sales = @azimuth.add(@azimuth.add(@azimuth.add(q1_sales, q2_sales), q3_sales), q4_sales)
  @azimuth.assert_eq(80000, annual_sales)
  
  // 季度平均销售额（向上取整）
  let quarterly_avg = @azimuth.divide_with_ceil(annual_sales, 4)
  @azimuth.assert_eq(20000, quarterly_avg)
  
  // 同比增长率
  let growth_rate = @azimuth.divide_with_ceil(@azimuth.multiply(q2_sales - q1_sales, 100), q1_sales)
  @azimuth.assert_eq(20, growth_rate)  // ceil((18000-15000)*100/15000) = 20%
}

test "complex_workflow_simulation" {
  // 复杂工作流程模拟：项目管理
  let tasks_per_day = 5
  let total_tasks = 47
  let team_members = 3
  let working_days = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  @azimuth.assert_eq(10, working_days)  // ceil(47/5) = 10
  
  // 总人日
  let total_person_days = @azimuth.multiply(working_days, team_members)
  @azimuth.assert_eq(30, total_person_days)  // 10 * 3 = 30
  
  // 每人平均任务数
  let tasks_per_person = @azimuth.divide_with_ceil(total_tasks, team_members)
  @azimuth.assert_eq(16, tasks_per_person)  // ceil(47/3) = 16
}