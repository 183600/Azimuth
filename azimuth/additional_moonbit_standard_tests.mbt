// 额外的 MoonBit 标准测试用例
// 为 azimuth 库添加的高质量测试，不超过10个测试用例

// 测试加法函数的多种场景
test "add_function_various_scenarios" {
  // 正数加法
  assert_eq(10, add(7, 3))
  // 零的加法
  assert_eq(7, add(7, 0))
  // 负数加法
  assert_eq(-10, add(-7, -3))
  // 正负数混合
  assert_eq(4, add(-7, 11))
  // 大数加法
  assert_eq(2000000, add(1000000, 1000000))
}

// 测试乘法函数的边界条件
test "multiply_function_boundary_conditions" {
  // 零乘法
  assert_eq(0, multiply(0, 12345))
  // 单位乘法
  assert_eq(12345, multiply(1, 12345))
  // 负数乘法
  assert_eq(-12345, multiply(-1, 12345))
  // 负负得正
  assert_eq(12345, multiply(-1, -12345))
  // 大数乘法
  assert_eq(1000000, multiply(1000, 1000))
}

// 测试问候函数的国际化支持
test "greet_function_internationalization" {
  // 中文字符
  assert_eq_string("Hello, 北京!", greet("北京"))
  // 俄文字符
  assert_eq_string("Hello, Москва!", greet("Москва"))
  // 法语字符
  assert_eq_string("Hello, café!", greet("café"))
  // 表情符号
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  // 混合字符
  assert_eq_string("Hello, MoonBit🚀!", greet("MoonBit🚀"))
}

// 测试向上取整除法的实际应用场景
test "divide_with_ceil_practical_applications" {
  // 场景1：计算需要的运输车辆
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 场景2：计算需要的教室数量
  let students = 87
  let capacity_per_classroom = 30
  let classrooms_needed = divide_with_ceil(students, capacity_per_classroom)
  assert_eq(3, classrooms_needed)
  
  // 场景3：计算需要的页面数量
  let items = 123
  let items_per_page = 20
  let pages_needed = divide_with_ceil(items, items_per_page)
  assert_eq(7, pages_needed)
}

// 测试复杂计算流水线
test "complex_calculation_pipeline" {
  // 场景：计算项目完成时间和资源需求
  let tasks = 35
  let team_members = 4
  let working_days = 5
  let tasks_per_person_per_day = 2
  
  // 计算团队日产能
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  
  // 计算需要的天数
  let days_needed = divide_with_ceil(tasks, daily_capacity)
  assert_eq(5, days_needed)  // ceil(35/(4*2)) = 5
  
  // 计算总工作日
  let total_working_days = multiply(days_needed, working_days)
  assert_eq(25, total_working_days)
  
  // 计算总处理能力
  let total_capacity = multiply(daily_capacity, days_needed)
  assert_eq(40, total_capacity)
}

// 测试数学性质和恒等式
test "mathematical_properties_and_identities" {
  let x = 42
  let y = 17
  let z = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, z)),
    add(multiply(x, y), multiply(x, z))
  )
}

// 测试资源分配优化
test "resource_allocation_optimization" {
  // 场景：预算分配
  let total_budget = 15000
  let departments = 4
  
  // 使用向上取整确保每个部门都有足够资金
  let max_budget_per_dept = divide_with_ceil(total_budget, departments)
  assert_eq(3750, max_budget_per_dept)  // ceil(15000/4) = 3750
  
  // 验证总分配
  let total_allocated = multiply(max_budget_per_dept, departments)
  assert_eq(15000, total_allocated)
  
  // 场景：人员分配
  let total_people = 23
  let teams = 5
  let people_per_team = divide_with_ceil(total_people, teams)
  assert_eq(5, people_per_team)  // ceil(23/5) = 5
  
  // 验证总容量
  let total_capacity = multiply(people_per_team, teams)
  assert_eq(25, total_capacity)
}

// 测试错误处理和边界条件
test "error_handling_and_boundary_conditions" {
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数测试
  assert_eq(1000000, divide_with_ceil(999999, 1))
  assert_eq(1, divide_with_ceil(999999, 999999))
  assert_eq(2, divide_with_ceil(1000000, 999999))
}

// 测试字符串处理和特殊字符
test "string_processing_and_special_characters" {
  // 空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 单个字符
  assert_eq_string("Hello, a!", greet("a"))
  
  // 数字字符串
  assert_eq_string("Hello, 12345!", greet("12345"))
  
  // 特殊符号
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  
  // 空白字符
  assert_eq_string("Hello,   !", greet("   "))
  
  // 混合大小写
  assert_eq_string("Hello, MixedCASE!", greet("MixedCASE"))
  
  // 换行和制表符
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
}

// 测试真实世界计算场景
test "real_world_calculation_scenarios" {
  // 场景1：活动安排
  let participants = 87
  let chairs_per_table = 8
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 场景2：项目管理
  let project_tasks = 47
  let team_members = 6
  let tasks_per_person = divide_with_ceil(project_tasks, team_members)
  assert_eq(8, tasks_per_person)  // ceil(47/6) = 8
  
  let total_task_capacity = multiply(tasks_per_person, team_members)
  assert_eq(48, total_task_capacity)
  
  // 场景3：库存管理
  let total_items = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(7, boxes_needed)  // ceil(157/24) = 7
  
  let total_box_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_box_capacity)
}