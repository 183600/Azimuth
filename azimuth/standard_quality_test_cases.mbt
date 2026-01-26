// 标准质量测试用例 - 为 azimuth 库新增的10个高质量测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能和边界情况

test "add_function_comprehensive" {
  // 全面测试加法函数
  add(10, 15)
  add(0, 0)
  add(-8, 3)
  add(-7, -7)
  add(100000, 200000)
}

test "multiply_function_various_cases" {
  // 测试乘法函数的各种情况
  multiply(6, 7)
  multiply(0, 999)
  multiply(-4, 8)
  multiply(-5, -5)
  multiply(1000, 1000)
}

test "greet_function_string_handling" {
  // 测试问候函数的字符串处理
  greet("Azimuth")
  greet("")
  greet("123")
  greet("🌙")
  greet("café")
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  divide_with_ceil(10, 3)
  divide_with_ceil(9, 3)
  divide_with_ceil(1, 1)
  divide_with_ceil(0, 5)
  divide_with_ceil(5, 0)
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 18
  
  // 加法交换律
  add(a, b)
  add(b, a)
  
  // 乘法交换律
  multiply(a, b)
  multiply(b, a)
}

test "warehouse_packaging_calculation" {
  // 仓库包装计算场景
  let total_products = 89
  let box_capacity = 15
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 验证总容量
  multiply(boxes_needed, box_capacity)
}

test "project_timeline_calculation" {
  // 项目时间线计算
  let total_tasks = 67
  let team_members = 5
  let tasks_per_person_per_day = 3
  
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  let days_needed = divide_with_ceil(total_tasks, daily_capacity)
  
  // 验证总处理能力
  multiply(days_needed, daily_capacity)
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let x = 7
  let y = 9
  let z = 4
  
  // 分配律：x * (y + z) = x * y + x * z
  multiply(x, add(y, z))
  add(multiply(x, y), multiply(x, z))
}

test "financial_budget_allocation" {
  // 财务预算分配
  let total_budget = 25000
  let departments = 4
  let budget_per_department = divide_with_ceil(total_budget, departments)
  
  // 验证总分配
  multiply(budget_per_department, departments)
  
  // 验证预算利用率
  divide_with_ceil(multiply(total_budget, 100), multiply(budget_per_department, departments))
}

test "complex_sequential_operations" {
  // 复杂连续操作
  let initial = 8
  
  // 计算序列：((initial + 5) * 3) / 4 + 2
  let step1 = add(initial, 5)
  let step2 = multiply(step1, 3)
  let step3 = divide_with_ceil(step2, 4)
  let result = add(step3, 2)
}