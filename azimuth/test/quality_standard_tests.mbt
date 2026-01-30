// 质量标准测试用例 - 10个高质量的 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_comprehensive" {
  // 基础算术综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(0, 0))
  assert_eq(24, multiply(6, 4))
  assert_eq(-18, multiply(-3, 6))
  assert_eq(0, multiply(0, 123))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-2, divide_with_ceil(-7, 4))
}

test "string_greeting_international" {
  // 国际化字符串问候测试
  assert_eq_string("Hello, Global!", greet("Global"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 减法性质
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  let total_packages = 58
  let truck_capacity = 12
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(60, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(2, remaining_space)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-10, 14))
  assert_eq(21, multiply(-7, -3))
  assert_eq(-24, multiply(-8, 3))
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(2, divide_with_ceil(7, -4))
}

test "edge_boundary_conditions" {
  // 边界条件测试
  // 极小值
  assert_eq(1, add(1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(999, 1000))
  assert_eq(1000, divide_with_ceil(1000, 1))
  assert_eq(2, divide_with_ceil(1001, 1000))
  
  // 零值处理
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "complex_workflow_pipeline" {
  // 复杂工作流流水线测试
  // 场景：生产计划计算
  let daily_production = 25
  let workers = 6
  let days_per_week = 5
  let weekly_target = 150
  
  // 计算每日产能
  let daily_capacity = multiply(workers, 4)
  assert_eq(24, daily_capacity)
  
  // 计算周产能
  let weekly_capacity = multiply(daily_capacity, days_per_week)
  assert_eq(120, weekly_capacity)
  
  // 计算需要的额外天数
  let remaining = subtract(weekly_target, weekly_capacity)
  let extra_days = divide_with_ceil(remaining, daily_capacity)
  assert_eq(2, extra_days)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let departments = 3
  let min_allocation = divide_with_ceil(total_budget, departments)
  
  assert_eq(1667, min_allocation)
  
  // 验证分配总额
  let total_allocated = multiply(min_allocation, departments)
  assert_eq(5001, total_allocated)
  
  // 验证每个部门获得足够资金
  assert_true(min_allocation >= divide_with_ceil(total_budget, departments))
  
  // 计算超额分配
  let over_allocation = subtract(total_allocated, total_budget)
  assert_eq(1, over_allocation)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值运算测试
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(0, multiply(large_num, 0))
  assert_eq(large_num, add(large_num, 0))
  
  // 字符串边界测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, x!", greet("x"))
  assert_eq_string("Hello,   !", greet("   "))
}