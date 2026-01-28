// 新的标准 MoonBit 测试用例 - 10个高质量测试用例
// 为 azimuth 库提供额外的测试覆盖

test "add_function_comprehensive" {
  // 全面测试加法函数的各种情况
  assert_eq(15, add(8, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-15, add(-8, -7))
  assert_eq(1, add(-8, 9))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(-1, add(-1000000, 999999))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数的各种情况
  assert_eq(56, multiply(7, 8))
  assert_eq(0, multiply(0, 12345))
  assert_eq(1, multiply(1, 12345))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(12345, multiply(-1, -12345))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🌙⭐!", greet("🌙⭐"))
}

test "divide_with_ceil_business_scenarios" {
  // 测试向上取整除法的业务逻辑场景
  // 场景1：计算需要的运输车辆数量
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 场景2：计算需要的教室数量
  let students = 87
  let capacity_per_classroom = 25
  let classrooms_needed = divide_with_ceil(students, capacity_per_classroom)
  assert_eq(4, classrooms_needed)
  
  // 验证总容量
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(50, total_capacity)
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目完成时间和资源需求
  let tasks = 47
  let people = 6
  let tasks_per_person_per_day = 3
  
  let daily_capacity = multiply(people, tasks_per_person_per_day)
  let days_needed = divide_with_ceil(tasks, daily_capacity)
  assert_eq(3, days_needed)  // ceil(47/(6*3)) = 3
  
  // 验证总处理能力
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(54, total_capacity)
  
  // 计算资源利用率
  let utilization = divide_with_ceil(multiply(tasks, 100), total_capacity)
  assert_eq(87, utilization)  // ceil(4700/54) = 87%
}

test "mathematical_properties_verification" {
  // 验证数学性质和恒等式
  let x = 24
  let y = 18
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 5)),
    add(multiply(x, y), multiply(x, 5))
  )
  
  // 结合律验证
  assert_eq(
    add(x, add(y, 7)),
    add(add(x, y), 7)
  )
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 25000
  let departments = 8
  
  // 使用向上取整确保覆盖所有情况
  let max_share = divide_with_ceil(total_budget, departments)
  assert_eq(3125, max_share)  // ceil(25000/8) = 3125
  
  // 验证总分配
  let total_allocated = multiply(max_share, departments)
  assert_eq(25000, total_allocated)
  
  // 测试不均匀分配
  let priority_departments = 3
  let priority_budget = divide_with_ceil(multiply(total_budget, 60), 100)  // 60%给优先部门
  assert_eq(15000, priority_budget)
  
  let remaining_departments = subtract(departments, priority_departments)
  let remaining_budget = subtract(total_budget, priority_budget)
  let regular_share = divide_with_ceil(remaining_budget, remaining_departments)
  assert_eq(3333, regular_share)  // ceil(10000/5)
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(2, divide_with_ceil(101, 100))
  
  // 大数测试
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
}

test "string_processing_edge_cases" {
  // 字符串处理的边缘情况测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quotes\"!", greet("\"quotes\""))
  assert_eq_string("Hello, 'apostrophe'!", greet("'apostrophe'"))
}

test "real_world_application_scenarios" {
  // 真实世界应用场景测试
  // 场景1：活动安排
  let participants = 127
  let chairs_per_table = 10
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(13, tables_needed)  // ceil(127/10) = 13
  
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(130, total_seats)
  
  // 场景2：库存管理
  let product_quantity = 245
  let box_capacity = 18
  let boxes_needed = divide_with_ceil(product_quantity, box_capacity)
  assert_eq(14, boxes_needed)  // ceil(245/18) = 14
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  let remaining_space = subtract(total_capacity, product_quantity)
  assert_eq(7, remaining_space)
  
  // 场景3：时间计算
  let total_minutes = 487
  let minutes_per_hour = 60
  let hours_needed = divide_with_ceil(total_minutes, minutes_per_hour)
  assert_eq(9, hours_needed)  // ceil(487/60) = 9
  
  let actual_hours = divide_with_ceil(total_minutes, minutes_per_hour)
  let remaining_minutes = subtract(total_minutes, multiply(actual_hours - 1, minutes_per_hour))
  assert_eq(7, remaining_minutes)  // 487 - 8*60 = 7
}