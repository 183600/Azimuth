// 标准 MoonBit 测试用例
// 为 azimuth 库添加10个高质量测试用例

test "mathematical_commutative_law" {
  // 验证加法和乘法交换律
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000, divide_with_ceil(1999, 2))
  assert_eq(667, divide_with_ceil(2000, 3))
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "zero_division_comprehensive" {
  // 全面测试除零情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
}

test "mathematical_identity_properties" {
  // 验证数学恒等式性质
  let x = 123
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, subtract(x, x))
}

test "business_calculation_scenario" {
  // 业务计算场景：资源分配
  let total_items = 157
  let container_capacity = 25
  let containers_needed = divide_with_ceil(total_items, container_capacity)
  
  assert_eq(7, containers_needed)
  
  // 验证总容量
  let total_capacity = multiply(containers_needed, container_capacity)
  assert_eq(175, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(18, remaining_space)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-5, -5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial_value = 15
  let step1 = multiply(initial_value, 3)  // 45
  let step2 = add(step1, 10)             // 55
  let step3 = divide_with_ceil(step2, 8)  // 7
  let step4 = multiply(step3, 2)          // 14
  let final_result = subtract(step4, 5)   // 9
  
  assert_eq(9, final_result)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 最小值测试
  assert_eq(1, add(1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 大数测试
  assert_eq(20000, add(10000, 10000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(10001, divide_with_ceil(10000, 1))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(9999, 10000))
  assert_eq(10000, divide_with_ceil(10000, 1))
}

test "real_world_application_test" {
  // 真实世界应用测试：活动安排
  let participants = 250
  let tables_available = 30
  let chairs_per_table = 8
  let budget_per_chair = 25
  let total_budget = 6000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(32, tables_needed)
  
  // 检查桌子是否足够
  let chairs_available = multiply(tables_available, chairs_per_table)
  assert_eq(240, chairs_available)
  
  // 计算总成本
  let total_chairs_needed = multiply(tables_needed, chairs_per_table)
  let total_cost = multiply(total_chairs_needed, budget_per_chair)
  assert_eq(6400, total_cost)
  
  // 验证预算情况
  let budget_difference = subtract(total_cost, total_budget)
  assert_eq(400, budget_difference)
}