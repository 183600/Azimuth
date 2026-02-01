// Azimuth 标准质量测试用例
// 包含8个高质量的标准 MoonBit 测试用例，覆盖主要功能和边界情况

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(8, 7))
  assert_eq(-3, add(-10, 7))
  assert_eq(0, add(5, -5))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-24, multiply(-3, 8))
  assert_eq(0, multiply(12, 0))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(15, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(3, divide_with_ceil(10, -3))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 23
  let y = 17
  
  // 加法交换律和结合律
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(add(x, y), 5), add(x, add(y, 5)))
  
  // 乘法交换律和结合律
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(multiply(multiply(x, y), 2), multiply(x, multiply(y, 2)))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：仓库存储优化
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 验证空间利用率
  let utilization = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(94, utilization)  // floor(15700/168) = 93%
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(5, subtract(5, 0))
  
  // 单位值测试
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 大数测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  
  // 极值运算测试
  let max_safe = 1000000
  let min_safe = -1000000
  
  assert_eq(max_safe, add(max_safe, 0))
  assert_eq(min_safe, add(min_safe, 0))
  assert_eq(0, multiply(max_safe, 0))
  assert_eq(0, multiply(min_safe, 0))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_members = 8
  let tasks_per_member = 6
  let hours_per_task = 4
  let working_days = 15
  let hours_per_day = 8
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_member)
  assert_eq(48, total_tasks)
  
  // 计算总工时
  let total_hours = multiply(total_tasks, hours_per_task)
  assert_eq(192, total_hours)
  
  // 计算每日可完成工时
  let daily_capacity = multiply(team_members, hours_per_day)
  assert_eq(64, daily_capacity)
  
  // 计算需要的最少工作日
  let days_needed = divide_with_ceil(total_hours, daily_capacity)
  assert_eq(3, days_needed)
  
  // 验证项目在给定时间内可完成
  assert_true(days_needed <= working_days)
  
  // 计算剩余缓冲时间
  let buffer_days = subtract(working_days, days_needed)
  assert_eq(12, buffer_days)
}