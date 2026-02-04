test "enhanced_basic_arithmetic_operations" {
  // 增强基础算术操作测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-24, @azimuth.multiply(-6, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(10, 0))
  
  @azimuth.assert_eq(3, @azimuth.subtract(10, 7))
  @azimuth.assert_eq(-3, @azimuth.subtract(7, 10))
}

test "enhanced_divide_with_ceil_comprehensive" {
  // 增强向上取整除法综合测试
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "enhanced_greet_function_international" {
  // 增强问候函数国际化测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "enhanced_mathematical_properties_validation" {
  // 增强数学性质验证测试
  let a = 15
  let b = 9
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律验证
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, 3))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3))
  @azimuth.assert_eq(distributive_left, distributive_right)
}

test "enhanced_business_packaging_optimization" {
  // 增强业务包装优化测试
  let total_products = 87
  let box_capacity = 20
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  @azimuth.assert_eq(5, boxes_needed)
  
  // 计算总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(100, total_capacity)
  
  // 计算剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(13, remaining_space)
}

test "enhanced_complex_calculation_pipeline" {
  // 增强复杂计算流水线测试
  // 场景：项目资源计算
  let team_members = 8
  let tasks_per_member = 6
  let sprint_capacity = 25
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  @azimuth.assert_eq(48, total_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  @azimuth.assert_eq(2, sprints_needed)
  
  // 计算总工作量
  let total_work = @azimuth.multiply(sprints_needed, sprint_capacity)
  @azimuth.assert_eq(50, total_work)
}

test "enhanced_error_handling_robustness" {
  // 增强错误处理健壮性测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值处理
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1000000, 1000000))
}

test "enhanced_resource_allocation_scenario" {
  // 增强资源分配场景测试
  let total_resources = 100
  let projects = 3
  let min_resources_per_project = @azimuth.divide_with_ceil(total_resources, projects)
  
  @azimuth.assert_eq(34, min_resources_per_project)
  
  let total_allocated = @azimuth.multiply(min_resources_per_project, projects)
  @azimuth.assert_eq(102, total_allocated)
  
  let resource_overhead = @azimuth.subtract(total_allocated, total_resources)
  @azimuth.assert_eq(2, resource_overhead)
}

test "enhanced_performance_benchmark_operations" {
  // 增强性能基准操作测试
  let large_number = 10000
  
  let sum_result = @azimuth.add(large_number, large_number)
  let product_result = @azimuth.multiply(large_number, 100)
  let division_result = @azimuth.divide_with_ceil(product_result, large_number)
  
  @azimuth.assert_eq(20000, sum_result)
  @azimuth.assert_eq(1000000, product_result)
  @azimuth.assert_eq(100, division_result)
}

test "enhanced_real_world_application_comprehensive" {
  // 增强真实世界应用综合测试
  // 场景：活动安排和资源管理
  let participants = 87
  let chairs_per_table = 10
  let budget_per_table = 50
  
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, total_chairs)
  
  let empty_chairs = @azimuth.subtract(total_chairs, participants)
  @azimuth.assert_eq(3, empty_chairs)
  
  let total_budget = @azimuth.multiply(tables_needed, budget_per_table)
  @azimuth.assert_eq(450, total_budget)
  
  let budget_per_person = @azimuth.divide_with_ceil(total_budget, participants)
  @azimuth.assert_eq(6, budget_per_person)
}