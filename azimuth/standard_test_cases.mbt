// 标准 MoonBit 测试用例 - Azimuth 库
// 包含10个高质量测试用例，覆盖核心功能和边界情况

test "add_function_basic" {
  // 测试加法函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-2, 3))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_international" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "mathematical_properties" {
  // 测试数学性质（交换律、结合律等）
  let x = 7
  let y = 13
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
}

test "business_logic_packaging" {
  // 测试业务逻辑：包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = total_capacity - total_items
  assert_eq(1, remaining_space)
}

test "error_handling_division" {
  // 测试错误处理：除零和边界情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(13, result)  // ceil(10*3/4) + 5 = 8 + 5 = 13
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 10000
  let team_members = 6
  
  // 使用向上取整确保覆盖所有情况
  let max_share = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, max_share)  // ceil(10000/6) = 1667
  
  // 验证总分配
  let total_allocated = multiply(max_share, team_members)
  assert_eq(10002, total_allocated)
}

test "real_world_event_planning" {
  // 测试真实世界场景：活动规划
  let participants = 87
  let chairs_per_table = 8
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 验证座位充足
  assert_true(total_seats >= participants)
  
  // 计算剩余座位
  let remaining_seats = total_seats - participants
  assert_eq(1, remaining_seats)
}