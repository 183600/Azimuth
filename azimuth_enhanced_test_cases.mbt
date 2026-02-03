// Azimuth 增强测试用例
// 使用标准 MoonBit 测试语法

test "azimuth_core_mathematical_operations" {
  // 核心数学运算测试
  assert_eq(42, add(20, 22))
  assert_eq(144, multiply(12, 12))
  assert_eq(8, divide_with_ceil(63, 8))
  assert_eq(-5, subtract(10, 15))
}

test "azimuth_boundary_value_testing" {
  // 边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(999, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 999))
  assert_eq(0, divide_with_ceil(999, 0))
}

test "azimuth_negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-5, -5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-2, divide_with_ceil(-15, 8))
  assert_eq(2, divide_with_ceil(-15, -8))
}

test "azimuth_string_processing_edge_cases" {
  // 字符串处理边界情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "azimuth_business_logic_inventory" {
  // 业务逻辑：库存管理测试
  let total_products = 157
  let package_capacity = 24
  let packages_needed = divide_with_ceil(total_products, package_capacity)
  
  assert_eq(7, packages_needed)
  
  let total_capacity = multiply(packages_needed, package_capacity)
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "azimuth_complex_calculation_workflow" {
  // 复杂计算工作流测试
  let team_size = 8
  let tasks_per_member = 7
  let total_tasks = multiply(team_size, tasks_per_member)
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(56, total_tasks)
  assert_eq(3, sprints_needed)
}

test "azimuth_mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 17
  let y = 23
  
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

test "azimuth_resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 95
  let departments = 4
  let min_per_department = divide_with_ceil(total_resources, departments)
  
  assert_eq(24, min_per_department)
  
  let allocated_total = multiply(min_per_department, departments)
  let utilization = divide_with_ceil(multiply(total_resources, 100), allocated_total)
  assert_eq(99, utilization)
}

test "azimuth_error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零情况
  assert_eq(0, divide_with_ceil(50, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}