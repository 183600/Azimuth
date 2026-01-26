// 标准 MoonBit 测试用例 - 覆盖 azimuth 库的核心功能
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-5, 9))
}

test "multiply_function_various_cases" {
  // 乘法函数的各种情况测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-20, multiply(-4, 5))
  assert_eq(16, multiply(-4, -4))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "subtract_function_basic" {
  // 减法函数基本测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律验证
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 3
  let divisor = 7
  let addition = 8
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(14, result)  // ceil(15*3/7) + 8 = 7 + 8 = 15
}

test "error_handling_and_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "real_world_project_management" {
  // 真实世界项目管理场景测试
  let team_members = 6
  let tasks_per_member = 8
  let working_days = 5
  let tasks_per_day = 3
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_member)
  assert_eq(48, total_tasks)
  
  // 计算团队每日任务处理能力
  let daily_capacity = multiply(team_members, tasks_per_day)
  assert_eq(18, daily_capacity)
  
  // 计算需要的完成天数
  let days_needed = divide_with_ceil(total_tasks, daily_capacity)
  assert_eq(3, days_needed)
  
  // 验证是否在规定工作日内完成
  assert_true(days_needed <= working_days)
}