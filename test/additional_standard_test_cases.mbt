// 额外的标准 MoonBit 测试用例
// 为 azimuth 库添加的高质量测试覆盖

test "add_function_with_zero" {
  // 测试加法函数与零的特殊情况
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(-5, add(0, -5))
}

test "multiply_function_with_one" {
  // 测试乘法函数与一的特殊情况
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
  assert_eq(1, multiply(1, 1))
  assert_eq(-7, multiply(-7, 1))
  assert_eq(-7, multiply(1, -7))
}

test "greet_function_with_spaces" {
  // 测试问候函数的空格处理
  assert_eq_string("Hello,  World !", greet(" World "))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, a b c!", greet("a b c"))
}

test "divide_with_ceil_exact_division" {
  // 测试向上取整除法的精确整除情况
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(7, divide_with_ceil(21, 3))
  assert_eq(8, divide_with_ceil(64, 8))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "divide_with_ceil_small_remainders" {
  // 测试向上取整除法的小余数情况
  assert_eq(6, divide_with_ceil(11, 2))
  assert_eq(8, divide_with_ceil(22, 3))
  assert_eq(9, divide_with_ceil(65, 8))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 场景：计算活动安排
  let guests = 35
  let tables = 8
  let chairs_per_table = 4
  
  // 计算总座位数
  let total_chairs = multiply(tables, chairs_per_table)
  
  // 计算是否需要更多桌子
  let additional_tables = divide_with_ceil(guests, chairs_per_table)
  
  // 验证计算结果
  assert_eq(32, total_chairs)
  assert_eq(9, additional_tables)
}

test "mathematical_commutativity_extended" {
  // 扩展测试数学交换律
  let a = 15
  let b = 23
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(38, add(a, b))
  assert_eq(345, multiply(a, b))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(-2, add(5, -7))
  
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
}

test "divide_with_ceil_negative_scenarios" {
  // 测试向上取整除法的负数场景
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "resource_planning_calculation" {
  // 测试资源规划计算
  // 场景：计算项目资源需求
  let tasks = 47
  let workers = 6
  let tasks_per_worker_per_day = 2
  
  // 计算每天完成的总任务数
  let daily_tasks = multiply(workers, tasks_per_worker_per_day)
  
  // 计算需要的天数
  let days_needed = divide_with_ceil(tasks, daily_tasks)
  
  // 验证计算结果
  assert_eq(12, daily_tasks)
  assert_eq(4, days_needed)
  
  // 验证总处理能力
  let total_capacity = multiply(days_needed, daily_tasks)
  assert_eq(48, total_capacity)
}