// Quality MoonBit Test Cases - 高质量标准测试用例
// 为 azimuth 库添加10个标准 MoonBit 测试用例

// 导入必要的包
test "basic_arithmetic_comprehensive" {
  // 综合测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(42, multiply(6, 7))
  assert_eq(-1, subtract(5, 6))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties_validation" {
  // 验证数学性质和恒等式
  let x = 12
  let y = 8
  
  // 加法交换律和结合律
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(add(x, y), 3), add(x, add(y, 3)))
  
  // 乘法交换律和分配律
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
  
  // 零元素性质
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
}

test "business_logistics_calculation" {
  // 业务物流计算场景
  let total_packages = 127
  let truck_capacity = 15
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  assert_eq(9, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(135, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(8, remaining_space)
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值边界测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "string_processing_international" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：计算项目资源需求
  let team_size = 6
  let tasks_per_person = 8
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 20
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(48, total_tasks)
  assert_eq(3, sprints_needed)
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, tasks_per_sprint)
  let buffer_capacity = subtract(total_capacity, total_tasks)
  assert_eq(12, buffer_capacity)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 25000
  let departments = 4
  let base_allocation = divide_with_ceil(total_budget, departments)
  
  assert_eq(6250, base_allocation)
  
  // 验证分配结果
  let total_allocated = multiply(base_allocation, departments)
  let overflow = subtract(total_allocated, total_budget)
  assert_eq(0, overflow)  // 25000/4 = 6250，没有溢出
  
  // 测试不均匀分配
  let special_department = 3
  let remaining_budget = subtract(total_budget, base_allocation)
  let special_allocation = divide_with_ceil(remaining_budget, special_department)
  assert_eq(6250, special_allocation)
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let attendees = 156
  let tables = 12
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(16, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(160, total_seats)
  
  // 验证座位充足性
  let extra_seats = subtract(total_seats, attendees)
  assert_eq(4, extra_seats)
  assert_true(total_seats >= attendees)
}

test "performance_large_scale_computation" {
  // 大规模计算性能测试
  let large_number = 100000
  let iterations = 100
  
  // 大数运算测试
  let sum_result = add(large_number, large_number)
  let product_result = multiply(large_number, 100)
  let division_result = divide_with_ceil(product_result, large_number)
  
  assert_eq(200000, sum_result)
  assert_eq(10000000, product_result)
  assert_eq(100, division_result)
  
  // 连续运算测试
  let pipeline_result = divide_with_ceil(
    multiply(add(large_number, 50000), 2),
    iterations
  )
  assert_eq(3000, pipeline_result)
}

test "comprehensive_function_integration" {
  // 综合功能集成测试
  // 场景：多步骤业务计算
  let initial_value = 100
  let growth_rate = 15
  let periods = 3
  let capacity_limit = 500
  
  // 计算增长
  let growth = multiply(initial_value, growth_rate)
  let growth_divided = divide_with_ceil(growth, 100)
  let new_value = add(initial_value, growth_divided)
  
  assert_eq(115, new_value)
  
  // 多期增长计算
  let period2_value = add(new_value, divide_with_ceil(multiply(new_value, growth_rate), 100))
  let period3_value = add(period2_value, divide_with_ceil(multiply(period2_value, growth_rate), 100))
  
  assert_eq(132, period2_value)
  assert_eq(152, period3_value)
  
  // 容量规划
  let required_units = divide_with_ceil(period3_value, capacity_limit)
  assert_eq(1, required_units)
  
  // 最终验证
  let final_capacity = multiply(required_units, capacity_limit)
  let remaining_capacity = subtract(final_capacity, period3_value)
  assert_eq(348, remaining_capacity)
}