// Azimuth 综合单元测试 - 8个高质量 MoonBit 测试用例
// 测试覆盖基本功能、边界情况、错误处理和实际应用场景

test "arithmetic_operation_chaining" {
  // 测试算术运算链式组合
  let base = 15
  let result = add(multiply(base, 2), divide_with_ceil(subtract(base, 5), 3))
  assert_eq(34, result)  // 15*2 + ceil(10/3) = 30 + 4 = 34
}

test "resource_planning_scenario" {
  // 测试资源规划场景
  let project_days = 22
  let team_members = 5
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  let tasks_per_day = divide_with_ceil(total_tasks, project_days)
  assert_eq(2, tasks_per_day)  // ceil(40/22) = 2
}

test "negative_number_division_edge_cases" {
  // 测试负数除法的边缘情况
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(2, divide_with_ceil(7, -4))
  assert_eq(1, divide_with_ceil(3, -4))
  assert_eq(2, divide_with_ceil(-7, -4))
}

test "string_input_validation" {
  // 测试字符串输入验证
  assert_eq_string("Hello, space separated!", greet("space separated"))
  assert_eq_string("Hello, special@chars#123!", greet("special@chars#123"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
  assert_eq_string("Hello, emoji 🎉🚀!", greet("emoji 🎉🚀"))
}

test "mathematical_identity_verification" {
  // 测试数学恒等式验证
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(0, subtract(x, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
  assert_eq(-x, multiply(x, -1))
}

test "capacity_planning_calculation" {
  // 测试容量规划计算
  let users = 157
  let server_capacity = 25
  let servers_needed = divide_with_ceil(users, server_capacity)
  assert_eq(7, servers_needed)  // ceil(157/25) = 7
  
  // 验证总容量
  let total_capacity = multiply(servers_needed, server_capacity)
  assert_eq(175, total_capacity)
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_capacity, users)
  assert_eq(18, remaining_capacity)
}

test "error_handling_comprehensive" {
  // 测试综合错误处理
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "complex_workflow_simulation" {
  // 测试复杂工作流模拟
  // 场景：订单处理流程
  let orders = 89
  let processing_time_per_order = 3
  let workers = 4
  let orders_per_worker = divide_with_ceil(orders, workers)
  assert_eq(23, orders_per_worker)  // ceil(89/4) = 23
  
  // 计算总处理时间
  let total_time = multiply(orders_per_worker, processing_time_per_order)
  assert_eq(69, total_time)
  
  // 计算实际处理的订单数
  let actual_processed = multiply(workers, orders_per_worker)
  assert_eq(92, actual_processed)
  
  // 验证处理能力足够
  assert_true(actual_processed >= orders)
}