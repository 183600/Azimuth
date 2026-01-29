// Azimuth 增强测试用例
// 包含10个高质量的标准 MoonBit 测试用例

test "basic_arithmetic_comprehensive" {
  // 综合基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(-10, 10))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-20, multiply(-5, 4))
  assert_eq(0, multiply(0, 100))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-9, subtract(5, 14))
  assert_eq(0, subtract(25, 25))
}

test "divide_with_ceil_comprehensive" {
  // 综合向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(15, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 结合律验证
  assert_eq(
    add(add(x, y), 5),
    add(x, add(y, 5))
  )
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：仓库货物装箱
  let total_items = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(11, remaining_space)
  
  // 验证空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_items, 100), total_capacity)
  assert_eq(94, utilization_percent)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源分配计算
  let project_budget = 50000
  let team_members = 8
  let base_salary = 4000
  let overhead_rate = 25  // 25%
  
  // 计算基本薪资总额
  let total_base_salary = multiply(team_members, base_salary)
  assert_eq(32000, total_base_salary)
  
  // 计算管理费用
  let overhead_cost = divide_with_ceil(multiply(total_base_salary, overhead_rate), 100)
  assert_eq(8000, overhead_cost)
  
  // 计算总成本
  let total_cost = add(total_base_salary, overhead_cost)
  assert_eq(40000, total_cost)
  
  // 验证预算充足
  let remaining_budget = subtract(project_budget, total_cost)
  assert_eq(10000, remaining_budget)
  
  assert_true(remaining_budget > 0)
}

test "error_handling_and_boundaries" {
  // 错误处理和边界测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let attendees = 250
  let tables_available = 25
  let chairs_per_table = 10
  let cost_per_chair = 25
  let total_budget = 7000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(25, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(250, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(0, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6250, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(750, remaining_budget)
}

test "algorithm_efficiency_comparison" {
  // 算法效率比较测试
  let data_size = 1000
  
  // 模拟线性算法 O(n)
  let linear_operations = multiply(data_size, 3)
  assert_eq(3000, linear_operations)
  
  // 模拟对数算法 O(log n) - 简化计算
  let log_operations = divide_with_ceil(data_size, 100)
  assert_eq(10, log_operations)
  
  // 模拟常数算法 O(1)
  let constant_operations = 5
  assert_eq(5, constant_operations)
  
  // 计算效率比率
  let linear_to_log_ratio = divide_with_ceil(linear_operations, log_operations)
  assert_eq(300, linear_to_log_ratio)
  
  let linear_to_constant_ratio = divide_with_ceil(linear_operations, constant_operations)
  assert_eq(600, linear_to_constant_ratio)
}

test "comprehensive_edge_cases" {
  // 综合边缘情况测试
  // 零值运算
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(0, -5))
  
  assert_eq(0, subtract(5, 5))
  assert_eq(5, subtract(5, 0))
  assert_eq(-5, subtract(0, 5))
  
  // 单位运算
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(-5, 1))
  assert_eq(5, multiply(5, 1))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000, divide_with_ceil(1999, 2))
  
  // 字符串边界情况
  assert_eq_string("Hello, x!", greet("x"))
  assert_eq_string("Hello, 1234567890!", greet("1234567890"))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}