// Azimuth 项目标准测试用例
// 包含 10 个高质量的 MoonBit 测试用例

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列
  let initial = 20
  let step1 = subtract(initial, 5)        // 15
  let step2 = multiply(step1, 2)          // 30
  let step3 = divide_with_ceil(step2, 7)  // 5
  let step4 = add(step3, 10)              // 15
  assert_eq(15, step4)
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  let total_budget = 50000
  let team_members = 8
  let equipment_cost = 15000
  
  // 计算人均预算
  let remaining_budget = subtract(total_budget, equipment_cost)
  let budget_per_person = divide_with_ceil(remaining_budget, team_members)
  
  assert_eq(35000, remaining_budget)  // 50000 - 15000
  assert_eq(4375, budget_per_person)  // ceil(35000/8)
}

test "mathematical_inverse_operations" {
  // 数学逆运算测试
  let x = 42
  let y = 17
  
  // 加法和减法互逆
  assert_eq(x, subtract(add(x, y), y))
  assert_eq(y, subtract(add(x, y), x))
  
  // 乘法和除法的关系（注意向上取整）
  let product = multiply(x, 2)
  let quotient = divide_with_ceil(product, 2)
  assert_eq(x, quotient)
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 各种除零情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, spaces!", greet("  "))
  assert_eq_string("Hello, numbers123!", greet("numbers123"))
  assert_eq_string("Hello, special@#$!", greet("special@#$"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
}

test "business_calculation_pipeline" {
  // 业务计算流水线测试
  // 场景：计算产品定价
  let base_cost = 100
  let labor_cost = 50
  let overhead = 30
  let profit_margin = 20
  
  let total_cost = add(add(base_cost, labor_cost), overhead)
  let selling_price = add(total_cost, divide_with_ceil(multiply(total_cost, profit_margin), 100))
  
  assert_eq(180, total_cost)  // 100 + 50 + 30
  assert_eq(216, selling_price)  // 180 + ceil(180*20/100) = 180 + 36
}

test "performance_optimization_test" {
  // 性能优化测试
  let data_size = 1000
  let batch_size = 75
  
  // 计算需要的批次数
  let batches = divide_with_ceil(data_size, batch_size)
  assert_eq(14, batches)  // ceil(1000/75) = 14
  
  // 计算总处理能力
  let total_capacity = multiply(batches, batch_size)
  assert_eq(1050, total_capacity)
  
  // 计算剩余容量
  let unused_capacity = subtract(total_capacity, data_size)
  assert_eq(50, unused_capacity)
}

test "mathematical_boundary_conditions" {
  // 数学边界条件测试
  // 测试极值情况
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, subtract(-2147483647, 1))
  assert_eq(1, divide_with_ceil(2147483647, 2147483647))
  assert_eq(0, divide_with_ceil(-2147483648, 2147483647))
  
  // 测试零值运算
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(2147483647, add(0, 2147483647))
  assert_eq(-2147483647, subtract(0, 2147483647))
}

test "real_world_application_test" {
  // 真实世界应用测试
  // 场景：活动组织和管理
  let participants = 256
  let tables = 15
  let chairs_per_table = 8
  let staff_per_table = 1
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(32, tables_needed)  // ceil(256/8) = 32
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(256, total_seats)
  
  // 计算所需工作人员
  let total_staff = multiply(tables_needed, staff_per_table)
  assert_eq(32, total_staff)
  
  // 计算总人数（参与者+工作人员）
  let total_people = add(participants, total_staff)
  assert_eq(288, total_people)
}