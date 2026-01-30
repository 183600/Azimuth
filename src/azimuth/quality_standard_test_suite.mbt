// 高质量标准 MoonBit 测试套件
// 针对 azimuth 库核心功能的综合测试

test "basic_arithmetic_comprehensive" {
  // 基础算术运算综合测试
  assert_eq(10, add(3, 7))
  assert_eq(-4, add(-10, 6))
  assert_eq(0, add(0, 0))
  
  assert_eq(21, multiply(3, 7))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(0, multiply(0, 123))
  
  assert_eq(-3, subtract(5, 8))
  assert_eq(15, subtract(20, 5))
  assert_eq(0, subtract(10, 10))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(
    multiply(a, add(b, 5)),
    add(multiply(a, b), multiply(a, 5))
  )
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let total_products = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(1, remaining_space)
}

test "string_processing_international_support" {
  // 字符串处理国际化支持测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let addition = 9
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(18, result)  // ceil(15*4/7) + 9 = 9 + 9 = 18
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 15000
  let departments = 4
  
  // 使用向上取整确保覆盖所有情况
  let max_allocation = divide_with_ceil(total_budget, departments)
  assert_eq(3750, max_allocation)  // ceil(15000/4) = 3750
  
  // 验证总分配
  let total_allocated = multiply(max_allocation, departments)
  assert_eq(15000, total_allocated)
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1000000, multiply(1000, 1000))
}

test "real_world_project_management" {
  // 真实世界项目管理测试
  let team_members = 6
  let tasks_per_person = 5
  let working_days = 8
  let total_tasks = 150
  
  // 计算团队处理能力
  let daily_capacity = multiply(team_members, tasks_per_person)
  let weekly_capacity = multiply(daily_capacity, working_days)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(total_tasks, weekly_capacity)
  assert_eq(1, weeks_needed)  // ceil(150/(6*5*8)) = 1
  
  // 验证处理能力
  assert_eq(240, weekly_capacity)  // 6*5*8 = 240
}

test "performance_large_scale_operations" {
  // 大规模运算性能测试
  let large_number = 10000
  let iterations = 100
  
  let sum_result = add(large_number, multiply(large_number, iterations))
  let product_result = multiply(large_number, iterations)
  let division_result = divide_with_ceil(product_result, large_number)
  
  assert_eq(1010000, sum_result)  // 10000 + 10000*100 = 1010000
  assert_eq(1000000, product_result)  // 10000*100 = 1000000
  assert_eq(100, division_result)  // ceil(1000000/10000) = 100
}