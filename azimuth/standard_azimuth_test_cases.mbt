// Azimuth 项目标准测试用例
// 包含10个高质量的 MoonBit 测试用例

test "add_function_various_scenarios" {
  // 测试加法函数的各种场景
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(1000, add(500, 500))
  assert_eq(1, add(2147483647, -2147483646))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合场景
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(0, multiply(0, 12345))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, multiply(-1, -1))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, empty!", greet(""))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算项目资源需求
  let team_size = 12
  let hours_per_day = 8
  let project_days = 15
  let total_hours = multiply(team_size, multiply(hours_per_day, project_days))
  assert_eq(1440, total_hours)
  
  // 计算需要的冲刺次数（每冲刺200小时）
  let sprint_capacity = 200
  let sprints_needed = divide_with_ceil(total_hours, sprint_capacity)
  assert_eq(8, sprints_needed)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 17
  let y = 23
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 5)),
    add(multiply(x, y), multiply(x, 5))
  )
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_items = 157
  let container_capacity = 25
  let containers_needed = divide_with_ceil(total_items, container_capacity)
  assert_eq(7, containers_needed)
  
  // 验证总容量
  let total_capacity = multiply(containers_needed, container_capacity)
  assert_eq(175, total_capacity)
  
  // 验证剩余空间
  let unused_space = subtract(total_capacity, total_items)
  assert_eq(18, unused_space)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试各种边界和错误情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, space!", greet("space"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
  assert_eq_string("Hello, MixedCase!", greet("MixedCase"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let data_size = 1000
  
  // 模拟O(n)算法
  let linear_operations = multiply(data_size, 3)
  assert_eq(3000, linear_operations)
  
  // 模拟O(log n)算法（简化）
  let log_operations = divide_with_ceil(data_size, 100)
  assert_eq(10, log_operations)
  
  // 模拟O(n²)算法（简化）
  let quadratic_operations = divide_with_ceil(multiply(data_size, data_size), 100000)
  assert_eq(10, quadratic_operations)
}

// 辅助函数
fn subtract(a : Int, b : Int) -> Int {
  a - b
}