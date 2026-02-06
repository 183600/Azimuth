// Azimuth 标准测试用例 - 10个高质量 MoonBit 测试
// 测试 azimuth 库的核心功能和边界情况

test "mathematical_operations_combination" {
  // 测试数学运算的组合使用
  let a = 15
  let b = 4
  let c = 7
  
  // 复杂表达式测试
  let result1 = add(multiply(a, b), subtract(c, 2))
  assert_eq(65, result1)  // 15*4 + (7-2) = 60 + 5 = 65
  
  let result2 = divide_with_ceil(multiply(a, b), c)
  assert_eq(9, result2)   // ceil(15*4/7) = ceil(60/7) = 9
  
  let result3 = multiply(add(a, b), divide_with_ceil(c, 3))
  assert_eq(57, result3)  // (15+4) * ceil(7/3) = 19 * 3 = 57
}

test "boundary_values_extreme_cases" {
  // 测试边界值和极值情况
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 单位值测试
  assert_eq(1, multiply(1, 1))
  assert_eq(5, multiply(5, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(5, divide_with_ceil(5, 1))
  
  // 相等值测试
  assert_eq(10, divide_with_ceil(10, 10))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "error_handling_division_by_zero" {
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  assert_eq(0, divide_with_ceil(-1000000, 0))
}

test "string_processing_comprehensive" {
  // 测试字符串处理的综合情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, special@#$%!", greet("special@#$%"))
  assert_eq_string("Hello, émoji 🚀!", greet("émoji 🚀"))
}

test "complex_business_scenario" {
  // 测试复杂业务场景：电商库存管理
  let total_orders = 157
  let items_per_box = 24
  let boxes_needed = divide_with_ceil(total_orders, items_per_box)
  
  assert_eq(7, boxes_needed)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_orders)
  assert_eq(11, remaining_space)
  
  // 计算空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_orders, 100), total_capacity)
  assert_eq(94, utilization_percent)  // ceil(15700/168) = 94%
}

test "mathematical_properties_verification" {
  // 测试数学性质的验证
  let x = 23
  let y = 17
  let z = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "performance_large_number_operations" {
  // 测试大数运算的性能
  let large_num1 = 100000
  let large_num2 = 99999
  
  // 大数加法
  assert_eq(199999, add(large_num1, large_num2))
  
  // 大数乘法（调整范围以避免溢出）
  assert_eq(999990000, multiply(large_num1, 9999))
  
  // 大数除法
  assert_eq(2, divide_with_ceil(large_num1, large_num2))
  assert_eq(100000, divide_with_ceil(large_num1, 1))
  assert_eq(1, divide_with_ceil(large_num1, large_num1))
  
  // 极大数测试（调整范围以避免溢出）
  let extreme_num = 1000000
  assert_eq(2000000, add(extreme_num, extreme_num))
  assert_eq(100000000000, multiply(extreme_num, 100000))
}

test "internationalization_special_characters" {
  // 测试国际化和特殊字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌟🌙!", greet("🌟🌙"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, mixed123中文!", greet("mixed123中文"))
}

test "sequential_operations_chain" {
  // 测试连续操作链
  let initial = 10
  
  // 操作链：((10 + 5) * 2) / 3 + 1
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 3)  // 10
  let step4 = add(step3, 1)          // 11
  let final_result = step4
  
  assert_eq(11, final_result)
  
  // 另一个复杂的操作链
  let chain_result = add(
    multiply(3, 4),
    divide_with_ceil(subtract(20, 5), 3)
  )
  assert_eq(17, chain_result)  // 3*4 + ceil(15/3) = 12 + 5 = 17
}

test "comprehensive_business_workflow" {
  // 测试综合业务工作流：项目管理场景
  let team_size = 8
  let sprint_weeks = 2
  let hours_per_week = 40
  let total_tasks = 320
  
  // 计算团队总工时
  let total_hours = multiply(team_size, multiply(sprint_weeks, hours_per_week))
  assert_eq(640, total_hours)  // 8 * 2 * 40 = 640
  
  // 计算可完成任务数（向下取整）
  let completable_tasks = multiply(total_hours, 2)  // 0.5 tasks/hour = 2 hours/task
  assert_eq(1280, completable_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, completable_tasks)
  assert_eq(1, sprints_needed)  // ceil(320/1280) = 1
  
  // 计算资源利用率
  let utilization = divide_with_ceil(multiply(total_tasks, 100), completable_tasks)
  assert_eq(25, utilization)  // ceil(32000/1280) = 25%
  
  // 验证项目可以在一个冲刺内完成
  assert_true(sprints_needed <= 1)
  
  // 测试团队扩展场景
  let expanded_team = add(team_size, 4)  // 增加4人
  let expanded_capacity = multiply(expanded_team, multiply(sprint_weeks, hours_per_week))
  assert_eq(960, expanded_capacity)  // 12 * 2 * 40 = 960
  
  let expanded_tasks = multiply(expanded_capacity, 2)
  let new_sprints_needed = divide_with_ceil(total_tasks, expanded_tasks)
  assert_eq(1, new_sprints_needed)  // 仍然只需要1个冲刺
  
  // 测试任务增加场景
  let increased_tasks = multiply(total_tasks, 3)  // 任务增加3倍
  let new_sprints_required = divide_with_ceil(increased_tasks, completable_tasks)
  assert_eq(3, new_sprints_required)  // 需要3个冲刺
}