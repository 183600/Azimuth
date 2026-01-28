// Azimuth 项目的新增综合测试用例
// 使用标准 MoonBit 测试语法编写，覆盖各种实际应用场景

test "basic_financial_calculation" {
  // 基本财务计算
  let income = 5000
  let expenses = 2800
  
  // 计算剩余预算
  let remaining = add(income, -expenses)
  
  assert_eq(2200, remaining)
}

test "inventory_management" {
  // 库存管理测试
  let initial_stock = 150
  let daily_sales = 12
  let days = 7
  
  // 计算总销售量
  let total_sold = multiply(daily_sales, days)
  
  // 计算最终库存
  let final_stock = add(initial_stock, -total_sold)
  
  assert_eq(84, total_sold)
  assert_eq(66, final_stock)
}

test "project_timeline" {
  // 项目时间线计算
  let tasks_per_day = 5
  let total_tasks = 47
  
  // 计算工作日数
  let workdays = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(10, workdays)
}

test "energy_consumption" {
  // 能源消耗计算
  let daily_consumption = 120
  let days_in_month = 30
  
  // 计算月度消耗
  let monthly_consumption = multiply(daily_consumption, days_in_month)
  
  assert_eq(3600, monthly_consumption)
}

test "fibonacci_sequence" {
  // 斐波那契数列
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
}

test "shipping_cost" {
  // 运输成本计算
  let total_weight = 65
  let base_rate_per_kg = 5
  
  // 计算基础成本
  let base_cost = multiply(total_weight, base_rate_per_kg)
  
  assert_eq(325, base_cost)
}

test "data_compression" {
  // 数据压缩计算
  let original_size = 1000
  let compression_ratio = 70
  
  // 计算压缩后大小
  let compressed_size = multiply(original_size, compression_ratio) / 100
  
  assert_eq(700, compressed_size)
}

test "network_bandwidth" {
  // 网络带宽计算
  let file_size_mb = 850
  let network_speed_mbps = 50
  
  // 计算传输时间
  let transfer_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  
  assert_eq(17, transfer_time)
}

test "educational_grading" {
  // 教育评分系统
  let exam_score = 87
  let assignment_score = 85
  
  // 计算最终成绩
  let final_grade = add(exam_score, assignment_score) / 2
  
  assert_eq(86, final_grade)
}

test "greeting_functionality" {
  // 问候功能测试
  let name1 = "World"
  let name2 = "MoonBit"
  
  // 测试问候函数
  let greeting1 = greet(name1)
  let greeting2 = greet(name2)
  
  assert_eq_string("Hello, World!", greeting1)
  assert_eq_string("Hello, MoonBit!", greeting2)
}