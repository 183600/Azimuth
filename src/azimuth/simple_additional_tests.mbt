// 简单的额外测试用例 - 针对 azimuth 库
// 包含基础功能测试和实际应用场景

test "simple_arithmetic_sequence" {
  // 测试简单的等差数列求和
  let term1 = 5
  let term2 = 8
  let term3 = 11
  let term4 = 14
  let term5 = 17
  let term6 = 20
  
  let sum = add(add(add(add(add(term1, term2), term3), term4), term5), term6)
  
  assert_eq(75, sum)
}

test "simple_budget_calculation" {
  // 测试简单的预算计算
  let income = 5000
  let rent = 1500
  let food = 800
  let utilities = 200
  let entertainment = 300
  
  let total_expenses = add(add(add(rent, food), utilities), entertainment)
  let remaining = add(income, -total_expenses)
  
  assert_eq(2800, total_expenses)
  assert_eq(2200, remaining)
}

test "simple_time_calculation" {
  // 测试简单的时间计算
  let start_time = 9
  let work_hours = 8
  let break_hours = 1
  
  let end_time = add(start_time, work_hours)
  let total_hours = add(work_hours, break_hours)
  
  assert_eq(17, end_time)
  assert_eq(9, total_hours)
}

test "simple_packaging_calculation" {
  // 测试简单的包装计算
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "simple_growth_calculation" {
  // 测试简单的增长计算
  let initial_value = 1000
  let growth_rate = 15
  
  let growth_amount = divide_with_ceil(multiply(initial_value, growth_rate), 100)
  let final_value = add(initial_value, growth_amount)
  
  assert_eq(150, growth_amount)
  assert_eq(1150, final_value)
}

test "simple_greeting_multiple" {
  // 测试简单的多种问候
  let greeting1 = greet("Alice")
  let greeting2 = greet("Bob")
  let greeting3 = greet("")
  
  assert_eq_string("Hello, Alice!", greeting1)
  assert_eq_string("Hello, Bob!", greeting2)
  assert_eq_string("Hello, !", greeting3)
}

test "simple_area_calculation" {
  // 测试简单的面积计算
  let length = 10
  let width = 6
  let height = 4
  
  let floor_area = multiply(length, width)
  let wall_area = multiply(2, multiply(add(length, width), height))
  let total_area = add(floor_area, wall_area)
  
  assert_eq(60, floor_area)
  assert_eq(128, wall_area)
  assert_eq(188, total_area)
}

test "simple_division_rounding" {
  // 测试简单的除法取整
  let value1 = 25
  let divisor1 = 6
  let value2 = 100
  let divisor2 = 9
  let value3 = 7
  let divisor3 = 3
  
  let result1 = divide_with_ceil(value1, divisor1)
  let result2 = divide_with_ceil(value2, divisor2)
  let result3 = divide_with_ceil(value3, divisor3)
  
  assert_eq(5, result1)
  assert_eq(12, result2)
  assert_eq(3, result3)
}