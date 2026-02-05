// Azimuth 增强标准测试用例 - 使用标准 MoonBit 测试语法
// 包含 10 个高质量测试用例，覆盖核心功能和边界情况

test "enhanced_add_function_comprehensive" {
  // 增强加法函数综合测试
  // 基本加法
  assert_eq(15, add(7, 8))
  // 负数加法
  assert_eq(-3, add(-5, 2))
  // 零值加法
  assert_eq(7, add(0, 7))
  // 大数加法
  assert_eq(2000, add(1000, 1000))
  // 混合符号加法
  assert_eq(0, add(-10, 10))
}

test "enhanced_multiply_function_edge_cases" {
  // 增强乘法函数边界情况测试
  // 基本乘法
  assert_eq(24, multiply(6, 4))
  // 零乘法
  assert_eq(0, multiply(15, 0))
  // 负数乘法
  assert_eq(-18, multiply(-3, 6))
  // 双负数乘法
  assert_eq(20, multiply(-4, -5))
  // 大数乘法
  assert_eq(100000, multiply(100, 1000))
}

test "enhanced_divide_with_ceil_comprehensive" {
  // 增强向上取整除法综合测试
  // 基本除法
  assert_eq(4, divide_with_ceil(10, 3))
  // 整除情况
  assert_eq(5, divide_with_ceil(15, 3))
  // 零除数处理
  assert_eq(0, divide_with_ceil(10, 0))
  // 零被除数
  assert_eq(0, divide_with_ceil(0, 5))
  // 大数除法
  assert_eq(1001, divide_with_ceil(2000, 2))
}

test "enhanced_divide_with_ceil_negative_numbers" {
  // 增强向上取整除法负数测试
  // 负被除数
  assert_eq(-3, divide_with_ceil(-10, 3))
  // 负除数
  assert_eq(-4, divide_with_ceil(10, -3))
  // 双负数
  assert_eq(4, divide_with_ceil(-10, -3))
  // 边界负数情况
  assert_eq(-1, divide_with_ceil(-1, 2))
  // 负数整除
  assert_eq(-3, divide_with_ceil(-9, 3))
}

test "enhanced_greet_function_international" {
  // 增强问候函数国际化测试
  // 英文问候
  assert_eq_string("Hello, World!", greet("World"))
  // 中文问候
  assert_eq_string("Hello, 北京!", greet("北京"))
  // 空字符串
  assert_eq_string("Hello, !", greet(""))
  // 数字字符串
  assert_eq_string("Hello, 2024!", greet("2024"))
  // 特殊字符
  assert_eq_string("Hello, émoji 🚀!", greet("émoji 🚀"))
}

test "enhanced_subtract_function_comprehensive" {
  // 增强减法函数综合测试
  // 基本减法
  assert_eq(5, subtract(12, 7))
  // 负结果
  assert_eq(-4, subtract(3, 7))
  // 零结果
  assert_eq(0, subtract(10, 10))
  // 减去负数
  assert_eq(15, subtract(5, -10))
  // 大数减法
  assert_eq(1000, subtract(2000, 1000))
}

test "enhanced_mathematical_properties_validation" {
  // 增强数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律验证
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律验证
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法恒等式验证
  assert_eq(a, add(a, 0))
  
  // 乘法恒等式验证
  assert_eq(a, multiply(a, 1))
  
  // 减法恒等式验证
  assert_eq(0, subtract(a, a))
}

test "enhanced_business_inventory_management" {
  // 增强库存管理业务场景测试
  let total_products = 158
  let package_capacity = 25
  
  // 计算需要的包装数量
  let packages_needed = divide_with_ceil(total_products, package_capacity)
  assert_eq(7, packages_needed)
  
  // 计算总容量
  let total_capacity = multiply(packages_needed, package_capacity)
  assert_eq(175, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(17, remaining_space)
  
  // 验证空间利用率
  let utilization_rate = divide_with_ceil(
    multiply(total_products, 100), 
    total_capacity
  )
  assert_eq(91, utilization_rate)
}

test "enhanced_complex_workflow_calculation" {
  // 增强复杂工作流计算测试
  // 项目资源规划场景
  let team_members = 9
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  assert_eq(1080, total_hours)
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  assert_eq(2160, total_tasks)
  
  // 计算冲刺周期（每周期处理500个任务）
  let sprint_capacity = 500
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(5, sprints_needed)
  
  // 计算最后冲刺的任务数
  let full_sprints_tasks = multiply(subtract(sprints_needed, 1), sprint_capacity)
  let final_sprint_tasks = subtract(total_tasks, full_sprints_tasks)
  assert_eq(160, final_sprint_tasks)
}

test "enhanced_boundary_values_and_error_handling" {
  // 增强边界值和错误处理测试
  // 极值测试
  assert_eq(1, add(1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 大数测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000, multiply(10000, 100000))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(999, 1000))
  assert_eq(1000, divide_with_ceil(1000, 1))
  assert_eq(1, divide_with_ceil(1000, 1000))
  
  // 错误处理测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 字符串边界测试
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, singlechar!", greet("singlechar"))
}