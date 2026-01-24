// 标准 MoonBit 测试用例 - 为 azimuth 库新增10个高质量测试
// 测试覆盖所有主要功能：add, multiply, greet, divide_with_ceil

test "add_function_comprehensive" {
  // 全面测试加法函数的正负数情况
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-7, -3))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1, add(2147483647, -2147483646))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(0, multiply(12345, 0))
  assert_eq(12345, multiply(12345, 1))
  assert_eq(-12345, multiply(12345, -1))
  assert_eq(12345, multiply(-12345, -1))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符和特殊符号支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀 MoonBit!", greet("🚀 MoonBit"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "divide_with_ceil_business_scenarios" {
  // 测试向上取整除法的实际业务场景
  // 场景1：计算运输车辆需求
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 场景2：计算存储空间需求
  let items = 156
  let box_capacity = 25
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(7, boxes_needed)
}

test "mathematical_commutative_laws" {
  // 验证数学交换律
  let a = 42
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 测试负数情况
  assert_eq(add(-5, 10), add(10, -5))
  assert_eq(multiply(-3, 7), multiply(7, -3))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：项目资源规划计算
  let team_members = 8
  let tasks_per_member = 6
  let sprint_capacity = 20
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_member)
  assert_eq(48, total_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(3, sprints_needed)
  
  // 计算总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(60, total_capacity)
  
  // 计算剩余能力
  let remaining_capacity = total_capacity - total_tasks
  assert_eq(12, remaining_capacity)
}

test "edge_cases_and_boundary_conditions" {
  // 测试边缘情况和边界条件
  // 除零的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 加法恒等式
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  
  // 乘法恒等式
  assert_eq(0, multiply(42, 0))
  assert_eq(42, multiply(42, 1))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  // 测试各种字符串输入
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !@#$%^&*()", greet("!@#$%^&*()"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：预算分配问题
  let total_budget = 10000
  let departments = 6
  
  // 计算每个部门的平均预算（向上取整）
  let budget_per_dept = divide_with_ceil(total_budget, departments)
  assert_eq(1667, budget_per_dept)
  
  // 计算实际分配的总预算
  let actual_total = multiply(budget_per_dept, departments)
  assert_eq(10002, actual_total)
  
  // 计算预算溢出
  let budget_overflow = actual_total - total_budget
  assert_eq(2, budget_overflow)
  
  // 场景2：优化分配策略
  let optimized_departments = departments + 1
  let optimized_budget = divide_with_ceil(total_budget, optimized_departments)
  assert_eq(1429, optimized_budget)
}

test "mathematical_distributive_law" {
  // 验证数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 8
  let c = 4
  
  // 计算左边：a * (b + c)
  let left_side = multiply(a, add(b, c))
  
  // 计算右边：a * b + a * c
  let right_side = add(multiply(a, b), multiply(a, c))
  
  // 验证分配律
  assert_eq(left_side, right_side)
  
  // 另一个测试案例
  let x = 12
  let y = 5
  let z = 3
  
  assert_eq(
    multiply(x, add(y, z)),
    add(multiply(x, y), multiply(x, z))
  )
  
  // 验证具体数值
  assert_eq(96, multiply(12, add(5, 3)))  // 12 * 8 = 96
  assert_eq(96, add(multiply(12, 5), multiply(12, 3)))  // 60 + 36 = 96
}