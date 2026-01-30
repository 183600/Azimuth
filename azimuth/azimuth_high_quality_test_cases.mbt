// Azimuth 高质量测试用例
// 包含不超过10个标准 MoonBit 测试用例，覆盖核心功能

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(5, -6))
  assert_eq(0, add(0, 0))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-30, multiply(5, -6))
  assert_eq(0, multiply(0, 100))
  
  assert_eq(10, subtract(15, 5))
  assert_eq(-20, subtract(10, 30))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 交换律
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景测试
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprints = 3
  let tasks_per_sprint = divide_with_ceil(total_tasks, sprints)
  assert_eq(16, tasks_per_sprint)  // ceil(48/3) = 16
  
  // 验证任务分配
  let distributed_tasks = multiply(tasks_per_sprint, sprints)
  assert_eq(48, distributed_tasks)
  
  // 验证剩余任务
  let remaining_tasks = subtract(distributed_tasks, total_tasks)
  assert_eq(0, remaining_tasks)
}

test "boundary_conditions_and_edge_cases" {
  // 边界条件和边缘情况测试
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  
  // 零值测试
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数测试
  assert_eq(-10, add(-5, -5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, leading!", greet(" leading"))
  assert_eq_string("Hello, trailing !", greet("trailing "))
}

test "business_calculation_scenario" {
  // 业务计算场景测试
  // 场景：预算分配计算
  let total_budget = 25000
  let departments = 4
  let base_allocation = divide_with_ceil(total_budget, departments)
  assert_eq(6250, base_allocation)  // ceil(25000/4) = 6250
  
  // 计算实际分配总额
  let actual_allocation = multiply(base_allocation, departments)
  assert_eq(25000, actual_allocation)
  
  // 验证分配精度
  let allocation_accuracy = divide_with_ceil(multiply(total_budget, 100), actual_allocation)
  assert_eq(100, allocation_accuracy)  // 100% 精确分配
}

test "algorithm_efficiency_simulation" {
  // 算法效率模拟测试
  // 模拟不同算法的时间复杂度
  let input_size = 1000
  
  // O(n) 线性算法
  let linear_time = multiply(input_size, 2)
  
  // O(log n) 对数算法（简化模拟）
  let log_time = divide_with_ceil(input_size, 100)
  
  // O(n²) 平方算法（简化模拟）
  let quadratic_time = divide_with_ceil(multiply(input_size, input_size), 1000)
  
  // 验证时间复杂度关系
  assert_eq(2000, linear_time)
  assert_eq(10, log_time)
  assert_eq(1000, quadratic_time)
  
  // 验证效率差异
  assert_true(linear_time > log_time)
  assert_true(quadratic_time > linear_time)
}