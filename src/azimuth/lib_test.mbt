// Azimuth 库的标准 MoonBit 测试用例
// 测试核心功能：add, multiply, subtract, greet, divide_with_ceil

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "divide_with_ceil_positive_numbers" {
  // 测试向上取整除法的正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "subtract_function_basic_cases" {
  // 测试减法函数的基本情况
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_scenario" {
  // 测试业务包装场景
  let total_items = 25
  let items_per_package = 6
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "complex_workflow_calculation" {
  // 测试复杂工作流计算
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和极值
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

test "zero_value_arithmetic_operations" {
  // 测试零值在算术运算中的行为
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-3, add(0, -3))
  
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, multiply(-50, 0))
  
  assert_eq(5, subtract(5, 0))
  assert_eq(-10, subtract(0, 10))
  assert_eq(0, subtract(0, 0))
  
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "extreme_value_calculations" {
  // 测试极值计算
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试大数加法
  assert_eq(max_int, add(max_int - 1, 1))
  assert_eq(min_int, add(min_int + 1, -1))
  
  // 测试大数乘法
  assert_eq(4611686014132420609, multiply(2147483647, 2147483647))
  
  // 测试极值除法
  assert_eq(1, divide_with_ceil(max_int, max_int))
  assert_eq(1, divide_with_ceil(min_int, min_int))
}

test "string_boundary_and_special_cases" {
  // 测试字符串边界和特殊情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, emoji😊!", greet("emoji😊"))
  assert_eq_string("Hello, a_very_long_string_that_tests_length_handling!", greet("a_very_long_string_that_tests_length_handling"))
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  let y = 17
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(y, add(0, y))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(y, multiply(1, y))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(0, subtract(y, y))
  
  // 分配律测试
  assert_eq(multiply(x, add(y, 1)), add(multiply(x, y), multiply(x, 1)))
}

test "division_edge_cases_and_sign_behavior" {
  // 测试除法边界情况和符号行为
  // 正数除法
  assert_eq(2, divide_with_ceil(5, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  
  // 负数除法
  assert_eq(-2, divide_with_ceil(-5, 2))
  assert_eq(-2, divide_with_ceil(5, -2))
  assert_eq(3, divide_with_ceil(-5, -2))
  
  // 混合符号除法
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-1, divide_with_ceil(1, -2))
  assert_eq(1, divide_with_ceil(-1, -2))
  
  // 零除数
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  // 场景：库存管理和订单处理
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 计算剩余空间
  let total_space = multiply(boxes_needed, box_capacity)
  let empty_space = subtract(total_space, total_products)
  assert_eq(11, empty_space)
  
  // 场景：预算分配
  let total_budget = 10000
  let departments = 3
  let base_budget = divide_with_ceil(total_budget, departments)
  let allocated_total = multiply(base_budget, departments)
  
  assert_eq(3334, base_budget)
  assert_eq(10002, allocated_total)
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  // 测试空字符串和特殊输入
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, null!", greet("null"))
  assert_eq_string("Hello, undefined!", greet("undefined"))
  
  // 测试数值边界
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试大数组合运算
  let big_num1 = 1000000
  let big_num2 = 2000000
  let big_result = add(multiply(big_num1, 2), big_num2)
  assert_eq(4000000, big_result)
}

test "performance_and_stress_testing" {
  // 测试性能和压力情况
  // 连续运算测试
  let result1 = add(add(add(1, 2), 3), 4)
  assert_eq(10, result1)
  
  let result2 = multiply(multiply(multiply(2, 3), 4), 5)
  assert_eq(120, result2)
  
  // 嵌套运算测试
  let complex_result = add(multiply(10, 5), subtract(20, 8))
  assert_eq(62, complex_result)
  
  // 字符串处理压力测试
  let long_name = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  let expected_greeting = "Hello, " + long_name + "!"
  assert_eq_string(expected_greeting, greet(long_name))
}

// 新增的增强测试用例 - 9个高质量测试
test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体计算结果
  assert_eq(42, add(15, 27))
  assert_eq(405, multiply(15, 27))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法的综合场景测试
  // 正数场景
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数场景
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "string_processing_internationalization" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "business_logistics_calculation" {
  // 物流业务计算测试
  let total_products = 158
  let pallet_capacity = 25
  let pallets_needed = divide_with_ceil(total_products, pallet_capacity)
  
  assert_eq(7, pallets_needed)
  
  // 验证总容量
  let total_capacity = multiply(pallets_needed, pallet_capacity)
  assert_eq(175, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(17, remaining_space)
}

test "mathematical_distributive_law" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 8
  let b = 6
  let c = 4
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
  assert_eq(80, left_side)  // 8 * (6 + 4) = 80
}

test "financial_budget_planning" {
  // 财务预算规划测试
  let project_budget = 50000
  let team_members = 8
  let monthly_cost_per_member = 3500
  let months_needed = divide_with_ceil(project_budget, multiply(team_members, monthly_cost_per_member))
  
  assert_eq(2, months_needed)
  
  // 验证总月成本
  let total_monthly_cost = multiply(team_members, monthly_cost_per_member)
  assert_eq(28000, total_monthly_cost)
  
  // 验证项目总成本
  let total_project_cost = multiply(total_monthly_cost, months_needed)
  assert_eq(56000, total_project_cost)
}

test "complex_workflow_sequential_operations" {
  // 复杂工作流顺序操作测试
  let initial_value = 12
  
  // 操作序列: ((initial * 3) + 15) / 5
  let step1 = multiply(initial_value, 3)
  let step2 = add(step1, 15)
  let final_result = divide_with_ceil(step2, 5)
  
  assert_eq(11, final_result)  // ceil((12*3 + 15)/5) = ceil(51/5) = 11
}

test "error_handling_edge_cases" {
  // 错误处理边界情况测试
  // 除零情况的全面测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  
  // 极值运算测试
  assert_eq(0, multiply(0, 1000000))
  assert_eq(0, multiply(0, -1000000))
  assert_eq(1000000, add(1000000, 0))
  assert_eq(-1000000, add(-1000000, 0))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_tasks = 95
  let team_size = 6
  let working_days = 5
  let tasks_per_day_per_person = 3
  
  // 计算团队日处理能力
  let daily_capacity = multiply(team_size, tasks_per_day_per_person)
  
  // 计算团队周处理能力
  let weekly_capacity = multiply(daily_capacity, working_days)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(total_tasks, weekly_capacity)
  
  assert_eq(18, daily_capacity)   // 6 * 3 = 18
  assert_eq(90, weekly_capacity)  // 18 * 5 = 90
  assert_eq(2, weeks_needed)      // ceil(95/90) = 2
}

// 新增的10个标准 MoonBit 测试用例
test "enhanced_arithmetic_operations" {
  // 增强算术运算测试
  assert_eq(7, add(3, 4))
  assert_eq(12, multiply(3, 4))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(2, subtract(10, 8))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "boundary_conditions_comprehensive" {
  // 边界条件综合测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, multiply(1000, 1000))
}

test "negative_numbers_enhanced" {
  // 负数处理增强测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(1, divide_with_ceil(-3, -4))
  assert_eq(-8, subtract(5, 13))
}

test "international_string_processing" {
  // 国际化字符串处理测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "inventory_management_system" {
  // 库存管理系统测试
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

test "calculation_workflow_pipeline" {
  // 计算工作流流水线测试
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

test "resource_optimization_algorithm" {
  // 资源优化算法测试
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

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值验证
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
  
  // 边界减法
  assert_eq(0, subtract(100, 100))
  assert_eq(-100, subtract(0, 100))
}

test "project_management_scenario" {
  // 项目管理场景测试
  let participants = 87
  let chairs_per_table = 10
  let hourly_rate = 50
  let hours_per_day = 8
  let project_days = 5
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 计算可用座位数
  let available_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(available_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 计算项目总工时
  let total_hours = multiply(participants, multiply(hours_per_day, project_days))
  assert_eq(3480, total_hours)  // 87 * 8 * 5 = 3480
  
  // 计算项目总成本
  let total_cost = multiply(total_hours, hourly_rate)
  assert_eq(174000, total_cost)  // 3480 * 50 = 174000
}