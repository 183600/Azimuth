// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含基本功能、边界情况和实际应用场景的测试

test "subtract_function_basic" {
  // 测试基本的减法功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(-10, subtract(-5, 5))
}

test "boolean_assertions" {
  // 测试布尔断言函数
  assert_true(true)
  assert_false(false)
  assert_true(5 > 3)
  assert_false(3 > 5)
}

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  let a = 15
  let b = 7
  let c = 3
  
  // 测试 (a - b) * c
  let result1 = multiply(subtract(a, b), c)
  assert_eq(24, result1)  // (15 - 7) * 3 = 8 * 3 = 24
  
  // 测试 a + b - c
  let result2 = subtract(add(a, b), c)
  assert_eq(19, result2)  // 15 + 7 - 3 = 22 - 3 = 19
}

test "financial_calculations" {
  // 金融计算场景测试
  let income = 5000
  let expenses = 3200
  let savings_rate = 20  // 20%
  
  // 计算净收入
  let net_income = subtract(income, expenses)
  
  // 计算储蓄金额
  let savings_amount = divide_with_ceil(multiply(net_income, savings_rate), 100)
  
  // 计算可支配收入
  let disposable_income = subtract(net_income, savings_amount)
  
  assert_eq(1800, net_income)
  assert_eq(360, savings_amount)
  assert_eq(1440, disposable_income)
}

test "inventory_flow_calculation" {
  // 库存流转计算
  let initial_stock = 500
  let incoming = 150
  let outgoing = 220
  let safety_stock = 100
  
  // 计算当前库存
  let current_stock = add(subtract(initial_stock, outgoing), incoming)
  
  // 计算是否需要补货
  let needs_reorder = current_stock < safety_stock
  
  // 计算建议补货量（假设每次补货200件）
  let suggested_reorder = if needs_reorder { 200 } else { 0 }
  
  assert_eq(430, current_stock)
  assert_false(needs_reorder)
  assert_eq(0, suggested_reorder)
}

test "temperature_conversion" {
  // 温度转换计算（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  
  // 验证转换结果
  assert_true(fahrenheit > 70)
  assert_false(fahrenheit < 60)
  
  // 反向转换验证
  let back_to_celsius = subtract(multiply(fahrenheit, 5), 160) / 9
  assert_eq(celsius, back_to_celsius)
}

test "complex_business_metrics" {
  // 复杂业务指标计算
  let revenue = 100000
  let cost_of_goods = 60000
  let operating_expenses = 25000
  let tax_rate = 25
  
  // 计算毛利润
  let gross_profit = subtract(revenue, cost_of_goods)
  
  // 计算营业利润
  let operating_profit = subtract(gross_profit, operating_expenses)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(operating_profit, tax_rate), 100)
  
  // 计算净利润
  let net_profit = subtract(operating_profit, tax_amount)
  
  assert_eq(40000, gross_profit)
  assert_eq(15000, operating_profit)
  assert_eq(3750, tax_amount)
  assert_eq(11250, net_profit)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let contingency_rate = 10  // 10%
  
  // 计算已分配预算
  let allocated_budget = add(project_a_cost, project_b_cost)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, allocated_budget)
  
  // 计算应急资金
  let contingency_fund = divide_with_ceil(multiply(total_budget, contingency_rate), 100)
  
  // 验证预算分配是否合理
  let is_within_budget = remaining_budget >= contingency_fund
  
  assert_eq(6300, allocated_budget)
  assert_eq(3700, remaining_budget)
  assert_eq(1000, contingency_fund)
  assert_true(is_within_budget)
}

test "performance_metrics_calculation" {
  // 性能指标计算
  let total_tasks = 150
  let completed_tasks = 120
  let target_efficiency = 80  // 80%
  
  // 计算完成率
  let completion_rate = multiply(completed_tasks, 100) / total_tasks
  
  // 计算是否达到目标
  let meets_target = completion_rate >= target_efficiency
  
  // 计算剩余任务数
  let remaining_tasks = subtract(total_tasks, completed_tasks)
  
  // 计算预计完成天数（假设每天完成5个任务）
  let days_to_complete = divide_with_ceil(remaining_tasks, 5)
  
  assert_eq(80, completion_rate)
  assert_true(meets_target)
  assert_eq(30, remaining_tasks)
  assert_eq(6, days_to_complete)
}