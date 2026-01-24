// Azimuth 增强核心功能测试
// 高质量的 MoonBit 测试用例，覆盖核心功能和边界情况

test "add_function_comprehensive" {
  // 全面测试加法功能
  // 基本加法
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  
  // 边界情况
  assert_eq(1000000, add(999999, 1))
  assert_eq(-1000000, add(-999999, -1))
  assert_eq(0, add(2147483647, -2147483647))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  // 基本乘法
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  
  // 边界情况
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(2147483647, multiply(46340, 46340))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 零除数安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  // 基本 ASCII
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // Unicode 字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_business_calculations" {
  // 实际业务计算场景
  // 订单计算
  let unit_price = 89
  let quantity = 6
  let shipping_cost = 25
  let discount = 50
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(534, subtotal)
  assert_eq(559, total_before_discount)
  assert_eq(509, final_total)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let remaining_budget = add(total_budget, -add(project_a_cost, project_b_cost))
  
  // 计算可以支持的小项目数量
  let small_project_cost = 800
  let small_projects_count = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(3700, remaining_budget)
  assert_eq(5, small_projects_count)
}

test "time_and_scheduling_calculations" {
  // 时间和调度计算
  let total_tasks = 47
  let tasks_per_day = 8
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时（每天8小时）
  let total_work_hours = multiply(work_days_needed, 8)
  
  // 计算工作周数（每周5个工作日）
  let work_weeks_needed = divide_with_ceil(work_days_needed, 5)
  
  assert_eq(6, work_days_needed)
  assert_eq(48, total_work_hours)
  assert_eq(2, work_weeks_needed)
}

test "data_analysis_statistics" {
  // 数据分析统计测试
  // 计算总和（手动累加）
  let sum = add(add(add(add(add(add(add(add(add(15, 22), 18), 25), 20), 16), 23), 19), 21), 17)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, 10)
  
  // 计算最大值和最小值的差
  let max_value = 25
  let min_value = 15
  let range = add(max_value, -min_value)
  
  assert_eq(196, sum)
  assert_eq(20, average)
  assert_eq(10, range)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试极端输入值
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  
  // 测试负数运算
  assert_eq(-100, add(-50, -50))
  assert_eq(100, multiply(-10, -10))
  assert_eq(-3, divide_with_ceil(-10, 3))
}