// Azimuth 标准测试用例 - 新增
// 包含10个高质量的标准 MoonBit 测试用例

test "arithmetic_sequence_operations" {
  // 测试算术序列操作
  let start = 5
  let difference = 3
  let terms = 4
  
  // 计算等差数列的和
  let term1 = start
  let term2 = add(start, difference)
  let term3 = add(term2, difference)
  let term4 = add(term3, difference)
  
  let sum = add(add(add(term1, term2), term3), term4)
  assert_eq(38, sum)  // 5 + 8 + 11 + 14 = 38
}

test "resource_allocation_fair_distribution" {
  // 测试资源公平分配
  let total_resources = 100
  let participants = 9
  
  // 使用向上取整确保每个参与者至少获得基本份额
  let base_share = divide_with_ceil(total_resources, participants)
  assert_eq(12, base_share)  // ceil(100/9) = 12
  
  // 验证总分配量
  let total_allocated = multiply(base_share, participants)
  assert_eq(108, total_allocated)
  
  // 计算剩余资源
  let remaining = subtract(total_allocated, total_resources)
  assert_eq(8, remaining)
}

test "string_pattern_validation" {
  // 测试字符串模式验证
  let valid_names = ["Alice", "Bob", "Charlie", "Diana"]
  let test_name = "Alice"
  
  // 验证名称是否在有效列表中（使用字符串比较）
  assert_eq_string("Hello, Alice!", greet(test_name))
  assert_eq_string("Hello, Alice123!", greet("Alice123"))
  assert_eq_string("Hello, Alice_Bob!", greet("Alice_Bob"))
  assert_eq_string("Hello, Alice@Example!", greet("Alice@Example"))
}

test "time_calculation_scenario" {
  // 测试时间计算场景
  let total_minutes = 150
  let minutes_per_hour = 60
  
  // 计算小时数和剩余分钟数
  let hours = divide_with_ceil(total_minutes, minutes_per_hour)
  assert_eq(3, hours)  // ceil(150/60) = 3
  
  // 计算总分钟数（基于计算的小时数）
  let calculated_minutes = multiply(hours, minutes_per_hour)
  assert_eq(180, calculated_minutes)
  
  // 计算额外分钟数
  let extra_minutes = subtract(calculated_minutes, total_minutes)
  assert_eq(30, extra_minutes)
}

test "inventory_replenishment_calculation" {
  // 测试库存补充计算
  let current_stock = 45
  let min_required = 100
  let package_size = 12
  
  // 计算需要补充的数量
  let needed = subtract(min_required, current_stock)
  assert_eq(55, needed)
  
  // 计算需要订购的包数
  let packages_to_order = divide_with_ceil(needed, package_size)
  assert_eq(5, packages_to_order)  // ceil(55/12) = 5
  
  // 验证订购后的总库存
  let total_received = multiply(packages_to_order, package_size)
  let final_stock = add(current_stock, total_received)
  assert_eq(105, final_stock)
}

test "geometric_progression_calculation" {
  // 测试等比数列计算
  let first_term = 2
  let ratio = 3
  let terms = 4
  
  // 计算等比数列的各项
  let term1 = first_term
  let term2 = multiply(first_term, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  
  // 计算总和
  let sum = add(add(add(term1, term2), term3), term4)
  assert_eq(80, sum)  // 2 + 6 + 18 + 54 = 80
}

test "budget_planning_scenario" {
  // 测试预算规划场景
  let total_budget = 5000
  let departments = 3
  let contingency_percentage = 10
  
  // 计算每个部门的基本预算
  let base_budget_per_dept = divide_with_ceil(total_budget, departments)
  assert_eq(1667, base_budget_per_dept)  // ceil(5000/3) = 1667
  
  // 计算总基本预算
  let total_base_budget = multiply(base_budget_per_dept, departments)
  assert_eq(5001, total_base_budget)
  
  // 计算应急资金（简化计算）
  let contingency_fund = divide_with_ceil(multiply(total_base_budget, contingency_percentage), 100)
  assert_eq(501, contingency_fund)  // ceil(5001*10/100) = 501
  
  // 计算总需求
  let total_required = add(total_base_budget, contingency_fund)
  assert_eq(5502, total_required)
}

test "complex_nested_calculations" {
  // 测试复杂嵌套计算
  let base = 10
  let multiplier = 3
  let divisor = 4
  let offset = 7
  
  // 嵌套计算：ceil((base * multiplier + offset) / divisor)
  let step1 = multiply(base, multiplier)
  let step2 = add(step1, offset)
  let result = divide_with_ceil(step2, divisor)
  
  assert_eq(11, result)  // ceil((10*3 + 7) / 4) = ceil(37/4) = 10
}

test "data_transmission_calculation" {
  // 测试数据传输计算
  let data_size_mb = 250
  let bandwidth_mbps = 15
  let overhead_percentage = 20
  
  // 计算基础传输时间
  let base_time_seconds = divide_with_ceil(data_size_mb, bandwidth_mbps)
  assert_eq(17, base_time_seconds)  // ceil(250/15) = 17
  
  // 计算开销时间
  let overhead_time = divide_with_ceil(multiply(base_time_seconds, overhead_percentage), 100)
  assert_eq(4, overhead_time)  // ceil(17*20/100) = 4
  
  // 计算总传输时间
  let total_time = add(base_time_seconds, overhead_time)
  assert_eq(21, total_time)
}

test "temperature_conversion_validation" {
  // 测试温度转换验证（使用整数运算模拟）
  let celsius = 25
  
  // 简化的摄氏度到华氏度转换：F = (C * 9/5) + 32
  let step1 = multiply(celsius, 9)
  let step2 = divide_with_ceil(step1, 5)
  let fahrenheit = add(step2, 32)
  
  assert_eq(77, fahrenheit)  // ceil(25*9/5) + 32 = 45 + 32 = 77
  
  // 验证转换回摄氏度（简化）
  let back_to_celsius_step1 = subtract(fahrenheit, 32)
  let back_to_celsius_step2 = multiply(back_to_celsius_step1, 5)
  let back_to_celsius = divide_with_ceil(back_to_celsius_step2, 9)
  
  assert_eq(25, back_to_celsius)  // ceil((77-32)*5/9) = 25
}