// Azimuth 项目增强测试用例
// 包含8个高质量的 MoonBit 测试用例，覆盖更复杂的业务场景和边界情况

test "complex_business_calculations" {
  // 复杂业务计算测试：项目资源规划
  let team_size = 15
  let sprint_duration = 2
  let tasks_per_person_per_day = 3
  let total_tasks = 180
  
  // 计算团队每日处理能力
  let daily_capacity = multiply(team_size, tasks_per_person_per_day)
  
  // 计算冲刺处理能力
  let sprint_capacity = multiply(daily_capacity, sprint_duration)
  
  // 计算需要的冲刺数
  let required_sprints = divide_with_ceil(total_tasks, sprint_capacity)
  
  // 验证计算结果
  assert_eq(45, daily_capacity)      // 15 * 3 = 45
  assert_eq(90, sprint_capacity)     // 45 * 2 = 90
  assert_eq(2, required_sprints)     // ceil(180/90) = 2
}

test "extreme_value_operations" {
  // 极值运算测试：处理非常大的数值
  let large_number = 1000000
  let another_large = 999999
  
  // 大数运算测试
  assert_eq(1999999, add(large_number, another_large))
  assert_eq(999999000000, multiply(large_number, another_large))
  assert_eq(1, subtract(large_number, another_large))
  
  // 大数除法测试
  assert_eq(1000, divide_with_ceil(large_number, 1000))
  assert_eq(2, divide_with_ceil(another_large, 500000))
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符处理测试
  assert_eq_string("Hello, 🌟MoonBit🚀!", greet("🌟MoonBit🚀"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, 数学公式∑∏∫!", greet("数学公式∑∏∫"))
}

test "multi_step_calculation_pipeline" {
  // 多步骤计算管道测试
  let initial_value = 100
  
  // 步骤1：增加50%
  let step1 = add(initial_value, divide_with_ceil(initial_value, 2))
  
  // 步骤2：乘以3
  let step2 = multiply(step1, 3)
  
  // 步骤3：减去100
  let step3 = subtract(step2, 100)
  
  // 步骤4：除以4（向上取整）
  let step4 = divide_with_ceil(step3, 4)
  
  // 验证最终结果
  assert_eq(150, step1)    // 100 + 50 = 150
  assert_eq(450, step2)    // 150 * 3 = 450
  assert_eq(350, step3)    // 450 - 100 = 350
  assert_eq(88, step4)     // ceil(350/4) = 88
}

test "inventory_optimization_scenario" {
  // 库存优化场景测试
  let total_products = 1234
  let small_box_capacity = 25
  let large_box_capacity = 100
  let cost_per_small_box = 5
  let cost_per_large_box = 15
  let budget = 200
  
  // 计算使用大箱子的数量
  let large_boxes_needed = divide_with_ceil(total_products, large_box_capacity)
  
  // 计算使用小箱子的数量
  let small_boxes_needed = divide_with_ceil(total_products, small_box_capacity)
  
  // 计算两种方案的成本
  let large_box_cost = multiply(large_boxes_needed, cost_per_large_box)
  let small_box_cost = multiply(small_boxes_needed, cost_per_small_box)
  
  // 验证计算结果
  assert_eq(13, large_boxes_needed)     // ceil(1234/100) = 13
  assert_eq(50, small_boxes_needed)     // ceil(1234/25) = 50
  assert_eq(195, large_box_cost)        // 13 * 15 = 195
  assert_eq(250, small_box_cost)        // 50 * 5 = 250
  
  // 验证预算约束
  assert_true(large_box_cost <= budget)
  assert_false(small_box_cost <= budget)
}

test "error_boundary_comprehensive" {
  // 错误边界综合测试
  // 各种除零情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  assert_eq(0, divide_with_ceil(-999999, 0))
  
  // 极小值运算
  assert_eq(0, multiply(0, -999999))
  assert_eq(0, add(0, 0))
  assert_eq(0, subtract(0, 0))
  
  // 单位运算
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "financial_calculation_precision" {
  // 金融计算精度测试
  let principal = 10000
  let interest_rate = 5  // 5%
  let years = 3
  
  // 计算简单利息
  let yearly_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let total_interest = multiply(yearly_interest, years)
  let final_amount = add(principal, total_interest)
  
  // 计算分期付款
  let total_months = multiply(years, 12)
  let monthly_payment = divide_with_ceil(final_amount, total_months)
  
  // 验证计算结果
  assert_eq(500, yearly_interest)       // ceil(10000 * 5 / 100) = 500
  assert_eq(1500, total_interest)       // 500 * 3 = 1500
  assert_eq(11500, final_amount)        // 10000 + 1500 = 11500
  assert_eq(36, total_months)           // 3 * 12 = 36
  assert_eq(320, monthly_payment)       // ceil(11500/36) = 320
}

test "string_edge_cases_and_validation" {
  // 字符串边界情况和验证测试
  // 空白字符处理
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
  assert_eq_string("Hello,  !", greet(" "))
  
  // 特殊字符组合
  assert_eq_string("Hello, @#$%^&*()_+-=[]{}|;':\",./<>?!", greet("@#$%^&*()_+-=[]{}|;':\",./<>?"))
  assert_eq_string("Hello, `~!", greet("`~"))
  
  // 数字和字母混合
  assert_eq_string("Hello, test123ABC!", greet("test123ABC"))
  assert_eq_string("Hello, 2024-12-25!", greet("2024-12-25"))
  
  // 长字符串测试
  assert_eq_string("Hello, This is a very long string with multiple words and punctuation!", greet("This is a very long string with multiple words and punctuation!"))
}