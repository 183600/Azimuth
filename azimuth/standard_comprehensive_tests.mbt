// 标准MoonBit测试用例 - 为azimuth项目添加的高质量测试
// 包含8个精心设计的测试用例，覆盖各种场景和边界情况

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
  assert_eq(-10, subtract(-5, 5))
}

test "complex_inventory_calculation" {
  // 复杂库存计算场景
  let initial_stock = 150
  let sold_items = 47
  let returned_items = 8
  let new_stock = 25
  
  // 计算最终库存
  let final_stock = subtract(add(subtract(initial_stock, sold_items), returned_items), new_stock)
  assert_eq(136, final_stock)
}

test "pricing_strategy_calculation" {
  // 定价策略计算测试
  let cost_price = 80
  let desired_margin = 30  // 30%
  let competitor_price = 115
  
  // 计算建议售价（成本+期望利润）
  let suggested_price = add(cost_price, divide_with_ceil(multiply(cost_price, desired_margin), 100))
  assert_eq(104, suggested_price)
  
  // 与竞争对手价格比较
  let price_difference = subtract(competitor_price, suggested_price)
  assert_eq(11, price_difference)
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  let project_duration = 12  // 月
  let team_size = 8
  let budget_per_person = 5000
  let contingency_budget = 15000
  
  // 计算基础预算
  let base_budget = multiply(project_duration, multiply(team_size, budget_per_person))
  assert_eq(480000, base_budget)
  
  // 计算总预算（包含应急预算）
  let total_budget = add(base_budget, contingency_budget)
  assert_eq(495000, total_budget)
  
  // 计算月均预算
  let monthly_budget = divide_with_ceil(total_budget, project_duration)
  assert_eq(41250, monthly_budget)
}

test "mathematical_sequence_operations" {
  // 数学序列操作测试
  // 等差数列：首项3，公差2，前5项和
  let first_term = 3
  let common_diff = 2
  let terms_count = 5
  
  // 等差数列求和公式：n/2 * (2a + (n-1)d)
  let sequence_sum = divide_with_ceil(
    multiply(terms_count, add(multiply(2, first_term), multiply(subtract(terms_count, 1), common_diff))),
    2
  )
  assert_eq(35, sequence_sum)  // 3+5+7+9+11 = 35
}

test "time_management_calculation" {
  // 时间管理计算测试
  let work_hours_per_day = 8
  let meeting_hours_per_day = 2
  let break_hours_per_day = 1
  let focused_work_hours = subtract(work_hours_per_day, add(meeting_hours_per_day, break_hours_per_day))
  assert_eq(5, focused_work_hours)
  
  // 计算周有效工作时间
  let work_days_per_week = 5
  let weekly_focused_hours = multiply(focused_work_hours, work_days_per_week)
  assert_eq(25, weekly_focused_hours)
  
  // 计算完成项目所需周数
  let project_hours = 120
  let weeks_needed = divide_with_ceil(project_hours, weekly_focused_hours)
  assert_eq(5, weeks_needed)
}

test "error_handling_edge_cases_new" {
  // 错误处理边缘情况测试
  // 测试极值运算
  assert_eq(2147483647, add(2147483646, 1))  // 接近Int最大值
  assert_eq(-2147483648, subtract(-2147483647, 1))  // 接近Int最小值
  
  // 测试除零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  assert_eq(2000000000, add(1000000000, 1000000000))
  assert_eq(1000000000000000000, multiply(1000000000, 1000000000))
}

test "multilingual_greeting_validation" {
  // 多语言问候验证测试
  let languages = ["中文", "English", "Español", "Français", "Deutsch", "日本語", "한국어", "Русский"]
  
  // 测试各种语言的问候
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
  assert_eq_string("Hello, Deutsch!", greet("Deutsch"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
}