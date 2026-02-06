// 额外的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能和边界情况

test "subtract_functionality" {
  // 测试减法功能
  @azimuth.assert_eq(5, @azimuth.subtract(8, 3))
  @azimuth.assert_eq(-5, @azimuth.subtract(3, 8))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(0, -10))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
}

test "complex_arithmetic_sequence" {
  // 测试复杂算术序列
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(900, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(230, result2)
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 简单利息计算
  let interest = @azimuth.multiply(principal, rate)
  let total_interest = @azimuth.multiply(interest, years)
  let total_amount = @azimuth.add(principal, @azimuth.divide_with_ceil(total_interest, 100))
  
  @azimuth.assert_eq(1150, total_amount)
}

test "inventory_management" {
  // 测试库存管理场景
  let initial_stock = 100
  let sold = 25
  let received = 15
  
  let remaining = @azimuth.subtract(initial_stock, sold)
  let final_stock = @azimuth.add(remaining, received)
  
  @azimuth.assert_eq(75, remaining)
  @azimuth.assert_eq(90, final_stock)
}

test "packaging_optimization" {
  // 测试包装优化场景
  let total_items = 47
  let box_capacity = 12
  
  let boxes_needed = @azimuth.divide_with_ceil(total_items, box_capacity)
  let last_box_items = @azimuth.subtract(total_items, @azimuth.multiply(boxes_needed - 1, box_capacity))
  
  @azimuth.assert_eq(4, boxes_needed)
  @azimuth.assert_eq(11, last_box_items)
}

test "temperature_conversion" {
  // 测试温度转换场景
  let celsius = 25
  
  // 摄氏度转华氏度: F = C * 9/5 + 32
  let fahrenheit_step1 = @azimuth.multiply(celsius, 9)
  let fahrenheit_step2 = @azimuth.divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit = @azimuth.add(fahrenheit_step2, 32)
  
  @azimuth.assert_eq(77, fahrenheit)
}

test "project_timeline" {
  // 测试项目时间线计算
  let total_tasks = 45
  let team_members = 5
  let work_days = 10
  
  let tasks_per_member = @azimuth.divide_with_ceil(total_tasks, team_members)
  let tasks_per_day = @azimuth.divide_with_ceil(tasks_per_member, work_days)
  
  @azimuth.assert_eq(9, tasks_per_member)
  @azimuth.assert_eq(1, tasks_per_day)
}

test "resource_allocation" {
  // 测试资源分配场景
  let total_budget = 1000
  let department1 = 300
  let department2 = 250
  let department3 = 200
  
  let allocated = @azimuth.add(@azimuth.add(department1, department2), department3)
  let remaining = @azimuth.subtract(total_budget, allocated)
  let reserve_percentage = @azimuth.divide_with_ceil(@azimuth.multiply(remaining, 100), total_budget)
  
  @azimuth.assert_eq(750, allocated)
  @azimuth.assert_eq(250, remaining)
  @azimuth.assert_eq(25, reserve_percentage)
}

test "string_greeting_variations" {
  // 测试字符串问候的各种变体
  @azimuth.assert_eq_string("Hello, John!", @azimuth.greet("John"))
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}