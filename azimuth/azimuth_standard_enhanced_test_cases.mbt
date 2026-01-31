// Azimuth 标准增强测试用例
// 包含8个高质量的 MoonBit 测试用例，覆盖核心功能

test "arithmetic_commutative_laws" {
  // 测试算术交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(42, add(15, 27))
  assert_eq(405, multiply(15, 27))
}

test "divide_with_ceil_precision_scenarios" {
  // 测试向上取整除法的精度场景
  // 场景：计算资源分配
  let total_resources = 100
  let team_size = 3
  let resources_per_team = divide_with_ceil(total_resources, team_size)
  
  assert_eq(34, resources_per_team)
  
  // 验证总分配资源
  let total_allocated = multiply(resources_per_team, team_size)
  assert_eq(102, total_allocated)
  
  // 验证剩余资源
  let remaining = subtract(total_allocated, total_resources)
  assert_eq(2, remaining)
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Universe!", greet("Universe"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, école!", greet("école"))
}

test "complex_business_workflow" {
  // 测试复杂业务工作流
  // 场景：项目成本计算
  let team_members = 5
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 12
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  assert_eq(480, total_hours)
  
  // 计算总成本
  let total_cost = multiply(total_hours, hourly_rate)
  assert_eq(36000, total_cost)
  
  // 计算分期付款（每期不超过10000）
  let payment_limit = 10000
  let payments_needed = divide_with_ceil(total_cost, payment_limit)
  assert_eq(4, payments_needed)
}

test "mathematical_boundary_conditions" {
  // 测试数学边界条件
  // 零值测试
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, add(0, 12345))
  assert_eq(0, divide_with_ceil(0, 12345))
  
  // 单位值测试
  assert_eq(12345, multiply(1, 12345))
  assert_eq(12346, add(1, 12345))
  assert_eq(12345, divide_with_ceil(12345, 1))
  
  // 负数边界测试
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(-12344, add(-1, -12343))
}

test "inventory_management_optimization" {
  // 测试库存管理优化
  // 产品A：47个，每箱容量6
  let product_a_qty = 47
  let product_a_capacity = 6
  let boxes_a = divide_with_ceil(product_a_qty, product_a_capacity)
  
  // 产品B：83个，每箱容量12
  let product_b_qty = 83
  let product_b_capacity = 12
  let boxes_b = divide_with_ceil(product_b_qty, product_b_capacity)
  
  // 计算总箱子数
  let total_boxes = add(boxes_a, boxes_b)
  assert_eq(16, total_boxes)  // ceil(47/6) + ceil(83/12) = 8 + 7 = 15
  
  // 计算总容量
  let total_capacity = add(multiply(boxes_a, product_a_capacity), multiply(boxes_b, product_b_capacity))
  assert_eq(132, total_capacity)  // 8*6 + 7*12 = 48 + 84 = 132
  
  // 计算利用率
  let total_items = add(product_a_qty, product_b_qty)
  let utilization_rate = divide_with_ceil(multiply(total_items, 100), total_capacity)
  assert_eq(98, utilization_rate)  // ceil(13000/132) = 99%
}

test "error_handling_comprehensive" {
  // 测试综合错误处理
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  
  // 极值运算测试
  let large_num = 100000
  assert_eq(10000000000, multiply(large_num, large_num))
  assert_eq(200000, add(large_num, large_num))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 2147483647))
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))
}

test "sequential_calculation_pipeline" {
  // 测试顺序计算流水线
  // 场景：数据处理管道
  let input_data = 25
  
  // 阶段1：数据预处理
  let stage1 = add(input_data, 5)  // 30
  
  // 阶段2：数据转换
  let stage2 = multiply(stage1, 3)  // 90
  
  // 阶段3：数据分块
  let chunk_size = 20
  let stage3 = divide_with_ceil(stage2, chunk_size)  // ceil(90/20) = 5
  
  // 阶段4：最终处理
  let stage4 = add(stage3, 2)  // 7
  
  // 验证最终结果
  assert_eq(7, stage4)
  
  // 验证中间步骤
  assert_eq(30, stage1)
  assert_eq(90, stage2)
  assert_eq(5, stage3)
}