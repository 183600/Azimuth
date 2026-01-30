// 新增的标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含8个高质量测试用例，覆盖各种场景和边界情况

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  subtract(10, 3) = 7
  subtract(0, 0) = 0
  subtract(-5, -3) = -2
  subtract(5, -3) = 8
  subtract(-5, 3) = -8
  subtract(100, 100) = 0
}

test "arithmetic_operation_chaining" {
  // 测试算术运算链式组合
  add(multiply(2, 3), subtract(10, 4)) = 12  // 2*3 + (10-4) = 6 + 6 = 12
  
  divide_with_ceil(
    subtract(multiply(5, 4), add(3, 2)),
    multiply(2, 1)
  ) = 8  // ceil((20-5)/2) = ceil(15/2) = 8
}

test "business_inventory_calculation" {
  // 业务场景：库存计算
  let initial_stock = 150
  let sold_items = 47
  let new_shipment = 80
  let boxes_per_shipment = 6
  
  subtract(initial_stock, sold_items) = 103  // 150 - 47
  add(subtract(initial_stock, sold_items), new_shipment) = 183  // 103 + 80
  divide_with_ceil(add(subtract(initial_stock, sold_items), new_shipment), boxes_per_shipment) = 31  // ceil(183/6) = 31
}

test "mathematical_identity_properties" {
  // 数学恒等式性质验证
  let x = 42
  
  // 加法恒等式
  add(x, 0) = x
  add(0, x) = x
  
  // 乘法恒等式
  multiply(x, 1) = x
  multiply(1, x) = x
  multiply(x, 0) = 0
  
  // 减法恒等式
  subtract(x, 0) = x
  subtract(x, x) = 0
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误的处理
  divide_with_ceil(10, 0) = 0
  divide_with_ceil(-10, 0) = 0
  divide_with_ceil(0, 0) = 0
  
  // 极值边界测试
  divide_with_ceil(1, 1) = 1
  divide_with_ceil(-1, -1) = 1
  divide_with_ceil(0, 1) = 0
  divide_with_ceil(0, -1) = 0
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况
  greet("") = "Hello, !"
  greet(" ") = "Hello,  !"
  greet("123") = "Hello, 123!"
  greet("!@#$%") = "Hello, !@#$%!"
  greet("mixedCASE123") = "Hello, mixedCASE123!"
  greet("   multiple   spaces   ") = "Hello,    multiple   spaces   !"
}

test "complex_project_planning" {
  // 复杂项目规划场景
  let team_size = 8
  let tasks_per_person = 5
  let weeks_per_sprint = 2
  let total_tasks = 150
  
  // 计算团队总任务能力
  multiply(team_size, tasks_per_person) = 40        // 8 * 5
  
  // 计算冲刺任务能力
  multiply(multiply(team_size, tasks_per_person), weeks_per_sprint) = 80      // 40 * 2
  
  // 计算需要的冲刺数
  divide_with_ceil(total_tasks, multiply(multiply(team_size, tasks_per_person), weeks_per_sprint)) = 2        // ceil(150/80) = 2
  
  // 计算剩余任务
  subtract(multiply(divide_with_ceil(total_tasks, multiply(multiply(team_size, tasks_per_person), weeks_per_sprint)), multiply(multiply(team_size, tasks_per_person), weeks_per_sprint)), total_tasks) = 10      // 2*80 - 150 = 10
}

test "mathematical_commutative_associative" {
  // 数学交换律和结合律验证
  let a = 7
  let b = 11
  let c = 5
  
  // 加法交换律
  add(a, b) = add(b, a)
  
  // 乘法交换律
  multiply(a, b) = multiply(b, a)
  
  // 加法结合律
  add(add(a, b), c) = add(a, add(b, c))
  
  // 乘法结合律
  multiply(multiply(a, b), c) = multiply(a, multiply(b, c))
  
  // 分配律
  multiply(a, add(b, c)) = add(multiply(a, b), multiply(a, c))
}