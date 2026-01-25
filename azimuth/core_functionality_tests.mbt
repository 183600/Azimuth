// 核心功能测试用例 - 专注于边界情况和实际应用场景
// 使用标准的 MoonBit 测试语法

test "add_function_boundary_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, add(1, -1))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_zero_properties" {
  // 测试乘法函数的零性质
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-1, 12345))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000, divide_with_ceil(1999, 2))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_business_calculation" {
  // 复杂业务计算测试：项目资源规划
  let team_size = 12
  let hours_per_day = 8
  let project_days = 15
  let total_hours = multiply(team_size, multiply(hours_per_day, project_days))
  
  let budget_per_hour = 50
  let total_budget = multiply(total_hours, budget_per_hour)
  
  let payment_limit = 10000
  let payments_needed = divide_with_ceil(total_budget, payment_limit)
  
  assert_eq(1440, total_hours)
  assert_eq(72000, total_budget)
  assert_eq(8, payments_needed)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "error_handling_edge_cases" {
  // 错误处理边缘情况测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_items = 157
  let container_capacity = 24
  let containers_needed = divide_with_ceil(total_items, container_capacity)
  
  assert_eq(7, containers_needed)
  
  // 验证总容量
  let total_capacity = multiply(containers_needed, container_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(11, remaining_space)
}

test "sequential_operations_chain" {
  // 连续操作链测试
  let initial = 5
  let step1 = add(initial, multiply(2, 3))
  let step2 = divide_with_ceil(step1, 4)
  let step3 = multiply(step2, add(1, 2))
  let result = divide_with_ceil(step3, 5)
  
  assert_eq(2, result)
}

test "negative_numbers_comprehensive" {
  // 负数综合测试
  assert_eq(-10, add(-5, -5))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}