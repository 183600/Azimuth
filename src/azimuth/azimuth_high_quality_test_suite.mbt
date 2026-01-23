// 高质量 MoonBit 测试用例
// 针对 azimuth 库核心功能的标准测试

test "add_function_core_functionality" {
  // 测试加法函数的核心功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_core_functionality" {
  // 测试乘法函数的核心功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_mathematical_properties" {
  // 测试向上取整除法的数学性质
  assert_eq(4, divide_with_ceil(10, 3))  // 10/3 = 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  assert_eq(0, divide_with_ceil(5, 0))   // 除数为0时返回0
}

test "greet_function_string_operations" {
  // 测试问候函数的字符串操作
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "mathematical_laws_verification" {
  // 验证基本数学定律
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let c = 5
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价计算
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "packaging_optimization_calculation" {
  // 包装优化计算：计算需要的包装箱数量
  let total_items = 47
  let items_per_box = 12
  
  // 计算需要的箱子数量（向上取整）
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, items_per_box)
  let unused_space = add(total_capacity, -total_items)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, unused_space)
}

test "negative_number_operations_comprehensive" {
  // 负数运算的综合测试
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试零值运算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 测试单位元运算
  assert_eq(7, multiply(7, 1))
  assert_eq(7, add(7, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 测试相等数运算
  assert_eq(25, multiply(5, 5))
  assert_eq(10, add(5, 5))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "complex_real_world_calculation" {
  // 复杂现实世界计算：项目管理场景
  let team_members = 8
  let tasks_per_member = 5
  let days_per_task = 3
  let buffer_percentage = 20
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_member)
  
  // 计算基础天数
  let base_days = multiply(total_tasks, days_per_task)
  
  // 计算缓冲时间（向上取整）
  let buffer_days = divide_with_ceil(multiply(base_days, buffer_percentage), 100)
  
  // 计算总项目时间
  let total_project_days = add(base_days, buffer_days)
  
  assert_eq(40, total_tasks)
  assert_eq(120, base_days)
  assert_eq(24, buffer_days)
  assert_eq(144, total_project_days)
}