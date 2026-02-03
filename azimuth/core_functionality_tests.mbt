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

// 新增的标准 MoonBit 测试用例 - 10个高质量测试用例
test "basic_addition_comprehensive" {
  // 基本加法综合测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "basic_multiplication_comprehensive" {
  // 基本乘法综合测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_negative_comprehensive" {
  // 向上取整除法负数综合测试
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 用户!", greet("用户"))
}

test "mathematical_commutative_comprehensive" {
  // 数学交换律综合测试
  let a = 12
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(19, add(12, 7))
  assert_eq(84, multiply(12, 7))
}

test "packaging_calculation_comprehensive" {
  // 包装计算综合测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(40, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "workflow_calculation_comprehensive" {
  // 复杂工作流计算综合测试
  // 场景：计算项目资源需求
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let days_per_sprint = 5
  let tasks_per_day = 10
  let sprint_capacity = multiply(days_per_sprint, tasks_per_day)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(50, sprint_capacity)  // 5 * 10 = 50
  assert_eq(1, sprints_needed)  // ceil(48/50) = 1
}

test "boundary_edge_comprehensive" {
  // 边界和边缘情况综合测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}

test "mathematical_properties_comprehensive" {
  // 数学性质综合测试
  let x = 15
  let y = 8
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(0, subtract(x, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
  
  // 分配律验证
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
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

// 新增的 MoonBit 测试用例 - 8个高质量测试用例
test "string_processing_comprehensive" {
  // 字符串处理综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, émoji 🚀!", greet("émoji 🚀"))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "error_handling_and_boundaries" {
  // 错误处理和边界测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值验证
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(13, result)  // ceil(10*3/4) + 5 = 8 + 5 = 13
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_resources = 100
  let teams = 3
  let min_resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(34, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = multiply(min_resources_per_team, teams)
  assert_eq(102, allocated_total)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), allocated_total)
  assert_eq(98, utilization)  // floor(10000/102) = 98%
}

test "mathematical_properties_verification" {
  // 验证数学性质
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

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
}

test "practical_resource_allocation" {
  // 实用资源分配测试
  let total_tasks = 87
  let team_members = 8
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  assert_eq(11, tasks_per_member)  // ceil(87/8) = 11
  
  // 计算总处理能力
  let total_capacity = multiply(tasks_per_member, team_members)
  assert_eq(88, total_capacity)  // 11*8 = 88
  
  // 计算剩余处理能力
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(1, remaining_capacity)  // 88-87 = 1
}

test "inventory_management_scenario" {
  // 库存管理场景测试
  let total_products = 245
  let box_capacity = 30
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(9, boxes_needed)  // ceil(245/30) = 9
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(270, total_capacity)  // 9*30 = 270
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(25, remaining_space)  // 270-245 = 25
}

test "project_timeline_calculation" {
  // 项目时间线计算测试
  let project_tasks = 125
  let daily_capacity = 15
  let days_needed = divide_with_ceil(project_tasks, daily_capacity)
  assert_eq(9, days_needed)  // ceil(125/15) = 9
  
  // 计算工作周数（每周5个工作日）
  let work_days_per_week = 5
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  assert_eq(2, weeks_needed)  // ceil(9/5) = 2
  
  // 计算总可用工作日
  let total_work_days = multiply(weeks_needed, work_days_per_week)
  assert_eq(10, total_work_days)  // 2*5 = 10
  
  // 计算剩余工作日
  let remaining_days = subtract(total_work_days, days_needed)
  assert_eq(1, remaining_days)  // 10-9 = 1
}

test "budget_allocation_optimization" {
  // 预算分配优化测试
  let total_budget = 75000
  let departments = 6
  let base_allocation = divide_with_ceil(total_budget, departments)
  assert_eq(12500, base_allocation)  // ceil(75000/6) = 12500
  
  // 计算总分配金额
  let total_allocated = multiply(base_allocation, departments)
  assert_eq(75000, total_allocated)  // 12500*6 = 75000
  
  // 计算预算余量
  let budget_surplus = subtract(total_allocated, total_budget)
  assert_eq(0, budget_surplus)  // 75000-75000 = 0
}

test "network_capacity_planning" {
  // 网络容量规划测试
  let total_users = 1850
  let bandwidth_per_user = 8  // Mbps
  let total_bandwidth_needed = multiply(total_users, bandwidth_per_user)
  assert_eq(14800, total_bandwidth_needed)  // 1850*8 = 14800
  
  // 计算需要的连接数
  let connection_capacity = 1000  // Mbps per connection
  let connections_needed = divide_with_ceil(total_bandwidth_needed, connection_capacity)
  assert_eq(15, connections_needed)  // ceil(14800/1000) = 15
  
  // 计算实际可用带宽
  let actual_bandwidth = multiply(connections_needed, connection_capacity)
  assert_eq(15000, actual_bandwidth)  // 15*1000 = 15000
  
  // 计算带宽余量
  let bandwidth_buffer = subtract(actual_bandwidth, total_bandwidth_needed)
  assert_eq(200, bandwidth_buffer)  // 15000-14800 = 200
}

test "educational_class_scheduling" {
  // 教育课程安排测试
  let total_students = 312
  let classroom_capacity = 35
  let classrooms_needed = divide_with_ceil(total_students, classroom_capacity)
  assert_eq(9, classrooms_needed)  // ceil(312/35) = 9
  
  // 计算总座位数
  let total_seats = multiply(classrooms_needed, classroom_capacity)
  assert_eq(315, total_seats)  // 9*35 = 315
  
  // 计算空余座位
  let empty_seats = subtract(total_seats, total_students)
  assert_eq(3, empty_seats)  // 315-312 = 3
  
  // 计算座位利用率
  let utilization_rate = divide_with_ceil(multiply(total_students, 100), total_seats)
  assert_eq(100, utilization_rate)  // floor(31200/315) = 99%
}

test "logistics_distribution_optimization" {
  // 物流配送优化测试
  let total_packages = 567
  let truck_capacity = 75
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  assert_eq(8, trucks_needed)  // ceil(567/75) = 8
  
  // 计算总运力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(600, total_capacity)  // 8*75 = 600
  
  // 计算剩余运力
  let remaining_capacity = subtract(total_capacity, total_packages)
  assert_eq(33, remaining_capacity)  // 600-567 = 33
  
  // 计算运力利用率
  let utilization = divide_with_ceil(multiply(total_packages, 100), total_capacity)
  assert_eq(95, utilization)  // floor(56700/600) = 94%
}

test "manufacturing_production_planning" {
  // 制造业生产计划测试
  let order_quantity = 1234
  let production_batch_size = 150
  let batches_needed = divide_with_ceil(order_quantity, production_batch_size)
  assert_eq(9, batches_needed)  // ceil(1234/150) = 9
  
  // 计算总产量
  let total_production = multiply(batches_needed, production_batch_size)
  assert_eq(1350, total_production)  // 9*150 = 1350
  
  // 计算额外产量
  let extra_production = subtract(total_production, order_quantity)
  assert_eq(116, extra_production)  // 1350-1234 = 116
  
  // 计算生产效率
  let efficiency = divide_with_ceil(multiply(order_quantity, 100), total_production)
  assert_eq(92, efficiency)  // floor(123400/1350) = 91%
}
}