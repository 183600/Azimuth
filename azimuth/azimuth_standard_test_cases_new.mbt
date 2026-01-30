// Azimuth 标准测试用例 - 新增10个高质量测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(3, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边缘情况
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-2, divide_with_ceil(-7, 4))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_calculation" {
  // 业务场景：包装计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流
  let base_value = 15
  let multiplier = 3
  let divisor = 7
  let addition = 10
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(16, result)  // ceil(15*3/7) + 10 = 7 + 10 = 17
}

test "resource_allocation_scenario" {
  // 资源分配场景
  let total_resources = 95
  let teams = 4
  let resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(24, resources_per_team)
  
  // 验证分配结果
  let total_allocated = multiply(resources_per_team, teams)
  assert_eq(96, total_allocated)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), total_allocated)
  assert_eq(99, utilization)  // floor(9500/96) = 98%
}

test "error_handling_validation" {
  // 错误处理和验证
  // 除零错误处理
  assert_eq(0, divide_with_ceil(15, 0))
  assert_eq(0, divide_with_ceil(-15, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值验证
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "real_world_event_planning" {
  // 真实世界活动规划
  let participants = 85
  let tables = 10
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)
  
  // 计算可用座位数
  let available_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, available_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(available_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 验证座位充足性
  assert_true(available_seats >= participants)
}