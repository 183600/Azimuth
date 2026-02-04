// Azimuth 项目新增标准测试用例
// 包含 9 个高质量测试用例，使用标准 MoonBit 测试语法

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "divide_with_ceil_comprehensive_edge_cases" {
  // 向上取整除法综合边界情况测试
  // 零值测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 相等数值测试
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(-1, divide_with_ceil(5, -5))
  assert_eq(-1, divide_with_ceil(-5, 5))
  
  // 大数测试
  assert_eq(1000, divide_with_ceil(1000000, 1000))
  assert_eq(1001, divide_with_ceil(1000001, 1000))
}

test "string_processing_unicode_support" {
  // 字符串处理 Unicode 支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
}

test "complex_sequential_calculations" {
  // 复杂连续计算测试
  let initial = 10
  
  // 计算链：((initial + 5) * 2) / 3 + 1
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 3)  // 10
  let result = add(step3, 1)         // 11
  
  assert_eq(11, result)
}

test "resource_allocation_business_scenario" {
  // 资源分配业务场景测试
  let employees = 23
  let projects = 4
  let min_employees_per_project = divide_with_ceil(employees, projects)
  
  assert_eq(6, min_employees_per_project)
  
  // 验证分配结果
  let total_allocated = multiply(min_employees_per_project, projects)
  assert_eq(24, total_allocated)
  
  // 验证剩余资源
  let remaining_employees = subtract(total_allocated, employees)
  assert_eq(1, remaining_employees)
}

test "mathematical_distributive_property" {
  // 数学分配律性质测试
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
  
  // 验证具体数值
  assert_eq(42, left_side)
  assert_eq(42, right_side)
}

test "error_handling_division_by_zero" {
  // 除零错误处理测试
  // 确保所有除零情况都返回 0
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  assert_eq(0, divide_with_ceil(-1000000, 0))
}

test "performance_large_number_operations" {
  // 大数运算性能测试
  let large_num1 = 100000
  let large_num2 = 10000
  
  // 大数加法
  assert_eq(110000, add(large_num1, large_num2))
  
  // 大数乘法
  assert_eq(1000000000, multiply(large_num1, large_num2))
  
  // 大数除法
  assert_eq(10, divide_with_ceil(large_num1, large_num2))
  
  // 大数减法
  assert_eq(90000, subtract(large_num1, large_num2))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let guests = 156
  let tables_available = 20
  let chairs_per_table = 8
  let budget_per_chair = 25
  let total_budget = 5000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(guests, chairs_per_table)
  assert_eq(20, tables_needed)
  
  // 验证桌子足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(160, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, guests)
  assert_eq(4, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, budget_per_chair)
  assert_eq(4000, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(1000, remaining_budget)
}