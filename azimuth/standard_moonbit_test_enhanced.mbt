// 标准 MoonBit 测试用例 - 为 azimuth 库新增的高质量测试
// 测试覆盖核心功能、边界情况和真实应用场景

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(8, 7))
  assert_eq(-3, add(5, -8))
  assert_eq(0, add(100, -100))
  assert_eq(2000000, add(1000000, 1000000))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  assert_eq(56, multiply(7, 8))
  assert_eq(-40, multiply(5, -8))
  assert_eq(0, multiply(12345, 0))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边缘情况测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 24
  let y = 18
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律验证
  assert_eq(
    multiply(x, add(y, 5)),
    add(multiply(x, y), multiply(x, 5))
  )
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：计算仓库存储需求
  let products = 158
  let boxes_per_pallet = 24
  let pallets_needed = divide_with_ceil(products, boxes_per_pallet)
  
  assert_eq(7, pallets_needed)
  
  // 验证总存储容量
  let total_capacity = multiply(pallets_needed, boxes_per_pallet)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, products)
  assert_eq(10, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_size = 12
  let work_days = 22
  let daily_hours = 8
  let task_complexity = 3
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(work_days, daily_hours))
  
  // 计算复杂度调整后的工时
  let adjusted_hours = multiply(total_hours, task_complexity)
  
  // 计算需要的冲刺次数（每冲刺200小时）
  let sprints_needed = divide_with_ceil(adjusted_hours, 200)
  
  assert_eq(2112, total_hours)  // 12 * 22 * 8
  assert_eq(6336, adjusted_hours)  // 2112 * 3
  assert_eq(32, sprints_needed)  // ceil(6336/200)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值边界测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}