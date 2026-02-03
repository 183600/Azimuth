// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含10个高质量测试用例，覆盖核心功能

test "azimuth_basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(2, divide_with_ceil(7, 4))
}

test "azimuth_mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法恒等式
  assert_eq(a, add(a, 0))
  assert_eq(a, add(0, a))
}

test "azimuth_divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数情况
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数情况
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-3, divide_with_ceil(7, -4))
  assert_eq(2, divide_with_ceil(-7, -4))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "azimuth_string_processing_functionality" {
  // 字符串处理功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "azimuth_business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "azimuth_complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 3
  let divisor = 7
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(11, result)  // ceil(15*3/7) + 5 = 7 + 5 = 12
}

test "azimuth_negative_numbers_comprehensive" {
  // 负数综合测试
  // 加法负数测试
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(-2, add(3, -5))
  
  // 乘法负数测试
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-15, multiply(3, -5))
  
  // 减法负数测试
  assert_eq(8, subtract(3, -5))
  assert_eq(-8, subtract(-3, 5))
}

test "azimuth_boundary_values_and_limits" {
  // 边界值和限制测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  
  // 大数测试（在安全范围内）
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "azimuth_resource_allocation_optimization" {
  // 资源分配优化测试
  let total_tasks = 95
  let team_members = 6
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  
  assert_eq(16, tasks_per_member)
  
  // 验证分配结果
  let allocated_tasks = multiply(tasks_per_member, team_members)
  assert_eq(96, allocated_tasks)
  
  // 验证任务覆盖率
  let uncovered_tasks = subtract(allocated_tasks, total_tasks)
  assert_eq(1, uncovered_tasks)
}

test "azimuth_real_world_event_planning" {
  // 真实世界事件规划测试
  let participants = 87
  let tables_available = 12
  let chairs_per_table = 8
  let budget_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(1, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, budget_per_chair)
  assert_eq(2200, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
}