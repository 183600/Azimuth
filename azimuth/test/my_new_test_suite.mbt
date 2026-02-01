// 标准 Azimuth 测试套件 - 10个高质量 MoonBit 测试用例
// 覆盖核心功能和各种测试场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  // 测试加法、乘法、减法的基本功能
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(10, add(-3, 13))
  assert_eq(-15, multiply(-3, 5))
}

test "boundary_values_and_extremes" {
  // 边界值和极值测试
  // 测试零值、大数和边界情况
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "negative_number_handling" {
  // 负数处理测试
  // 验证函数对负数的正确处理
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(2, subtract(-3, -5))
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(3, divide_with_ceil(-8, -3))
}

test "string_processing_and_greeting" {
  // 字符串处理和问候功能测试
  // 测试问候函数的各种输入情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  // 验证基本的数学定律和性质
  let a = 12
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  let c = 5
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "business_logic_packaging_scenario" {
  // 业务逻辑：包装场景测试
  // 模拟实际业务中的物品包装计算
  let total_items = 47
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(6, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 测试多个函数组合使用的复杂计算场景
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let adjustment = 3
  
  // 计算流程：((base * multiplier) / divisor) + adjustment
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let final_result = add(step2, adjustment)
  
  // 验证：ceil(15*4/7) + 3 = ceil(60/7) + 3 = 9 + 3 = 12
  assert_eq(12, final_result)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边缘情况测试
  // 测试除零错误和各种异常情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值除法
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  
  // 测试负数除法边界
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 模拟资源分配和优化问题的计算
  let total_resources = 100
  let teams = 3
  let min_resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(34, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = multiply(min_resources_per_team, teams)
  assert_eq(102, allocated_total)
  
  // 计算资源利用率百分比
  let utilization_percentage = divide_with_ceil(multiply(total_resources, 100), allocated_total)
  assert_eq(98, utilization_percentage)
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  // 模拟活动座位安排和资源规划的复杂场景
  let participants = 87
  let tables_available = 10
  let chairs_per_table = 10
  let budget_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 验证桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, budget_per_chair)
  assert_eq(2250, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(750, remaining_budget)
}