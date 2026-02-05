// 用户综合测试用例 - 10个高质量 MoonBit 测试用例
// 覆盖 azimuth 库的各种功能和场景

test "basic_arithmetic_operations_comprehensive" {
  // 基本算术运算综合测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-1, @azimuth.add(5, -6))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
}

test "boundary_values_and_extreme_cases" {
  // 边界值和极值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1)) // 溢出保护
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1)) // 下溢保护
  
  // 边界值乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  let z = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), z), @azimuth.multiply(x, @azimuth.multiply(y, z)))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, z)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  )
}

test "business_logistics_packaging_scenario" {
  // 业务物流包装场景测试
  let total_products = 157
  let box_capacity = 24
  let cost_per_box = 15
  let total_budget = 200
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  @azimuth.assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(11, remaining_space)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(boxes_needed, cost_per_box)
  @azimuth.assert_eq(105, total_cost)
  
  // 验证预算充足
  @azimuth.assert_eq(1, if total_cost <= total_budget { 1 } else { 0 })
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 边界除法测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  
  // 零值运算测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(123, @azimuth.add(0, 123))
  @azimuth.assert_eq(123, @azimuth.add(123, 0))
}

test "performance_and_large_numbers" {
  // 性能和大数测试
  let large_number = 100000
  
  // 大数运算测试
  @azimuth.assert_eq(200000, @azimuth.add(large_number, large_number))
  @azimuth.assert_eq(10000000, @azimuth.multiply(large_number, 100))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_number, large_number))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(large_number + 1, large_number))
  
  // 连续运算性能测试
  let mut result = 1
  result = @azimuth.multiply(result, 2)  // 2
  result = @azimuth.multiply(result, 3)  // 6
  result = @azimuth.multiply(result, 4)  // 24
  result = @azimuth.multiply(result, 5)  // 120
  @azimuth.assert_eq(120, result)
  
  // 复杂计算链测试
  let chain_result = @azimuth.add(
    @azimuth.multiply(10, 20),
    @azimuth.divide_with_ceil(@azimuth.add(15, 25), 4)
  )
  @azimuth.assert_eq(210, chain_result)  // 200 + ceil(40/4) = 200 + 10 = 210
}

test "internationalization_and_unicode_support" {
  // 国际化和Unicode支持测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, São Paulo!", @azimuth.greet("São Paulo"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, 👋!", @azimuth.greet("👋"))
  
  // 特殊字符测试
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet("!@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：计算项目成本和资源需求
  let team_size = 8
  let hourly_rate = 50
  let hours_per_day = 8
  let project_days = 15
  let overhead_percentage = 20
  
  // 计算总工时
  let total_hours = @azimuth.multiply(team_size, @azimuth.multiply(hours_per_day, project_days))
  @azimuth.assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  
  // 计算基础成本
  let base_cost = @azimuth.multiply(total_hours, hourly_rate)
  @azimuth.assert_eq(48000, base_cost)  // 960 * 50 = 48000
  
  // 计算管理费用
  let overhead_cost = @azimuth.divide_with_ceil(
    @azimuth.multiply(base_cost, overhead_percentage), 
    100
  )
  @azimuth.assert_eq(9600, overhead_cost)  // ceil(48000 * 20 / 100) = 9600
  
  // 计算总成本
  let total_cost = @azimuth.add(base_cost, overhead_cost)
  @azimuth.assert_eq(57600, total_cost)  // 48000 + 9600 = 57600
  
  // 计算每阶段预算（分5个阶段）
  let phases = 5
  let budget_per_phase = @azimuth.divide_with_ceil(total_cost, phases)
  @azimuth.assert_eq(11520, budget_per_phase)  // ceil(57600/5) = 11520
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 1000
  let projects = 3
  let min_resources_per_project = 100
  let priority_project_bonus = 200
  
  // 基础资源分配
  let base_resources_per_project = @azimuth.divide_with_ceil(
    @azimuth.subtract(total_resources, priority_project_bonus), 
    projects
  )
  @azimuth.assert_eq(267, base_resources_per_project)  // ceil(800/3) = 267
  
  // 验证总分配资源
  let total_allocated = @azimuth.add(
    @azimuth.multiply(base_resources_per_project, projects),
    priority_project_bonus
  )
  @azimuth.assert_eq(1001, total_allocated)  // 267*3 + 200 = 1001
  
  // 验证每个项目至少获得最小资源
  @azimuth.assert_eq(1, if base_resources_per_project >= min_resources_per_project { 1 } else { 0 })
  
  // 计算资源利用率
  let utilization = @azimuth.multiply(
    @azimuth.divide_with_ceil(total_resources, total_allocated),
    100
  )
  @azimuth.assert_eq(100, utilization)  // ceil(1000/1001) * 100 = 100
}

test "real_world_event_planning_comprehensive" {
  // 真实世界活动规划综合测试
  let attendees = 250
  let tables_available = 25
  let chairs_per_table = 10
  let cost_per_chair = 15
  let total_budget = 5000
  let staff_needed_per_20_attendees = 3
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(25, tables_needed)
  
  // 检查桌子是否足够
  @azimuth.assert_eq(1, if tables_needed <= tables_available { 1 } else { 0 })
  
  // 计算总座位数
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(250, total_chairs)
  
  // 计算剩余座位
  let empty_chairs = @azimuth.subtract(total_chairs, attendees)
  @azimuth.assert_eq(0, empty_chairs)
  
  // 计算椅子总成本
  let chair_cost = @azimuth.multiply(total_chairs, cost_per_chair)
  @azimuth.assert_eq(3750, chair_cost)
  
  // 计算需要的员工数量
  let staff_needed = @azimuth.divide_with_ceil(
    @azimuth.multiply(attendees, staff_needed_per_20_attendees), 
    20
  )
  @azimuth.assert_eq(38, staff_needed)  // ceil(250*3/20) = 38
  
  // 计算员工成本（假设每人100元）
  let staff_cost = @azimuth.multiply(staff_needed, 100)
  @azimuth.assert_eq(3800, staff_cost)
  
  // 计算总成本
  let total_cost = @azimuth.add(chair_cost, staff_cost)
  @azimuth.assert_eq(7550, total_cost)
  
  // 计算预算差额
  let budget_difference = @azimuth.subtract(total_cost, total_budget)
  @azimuth.assert_eq(2550, budget_difference)
  
  // 计算人均成本
  let cost_per_attendee = @azimuth.divide_with_ceil(total_cost, attendees)
  @azimuth.assert_eq(31, cost_per_attendee)  // ceil(7550/250) = 31
}