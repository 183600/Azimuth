// 标准 MoonBit 测试用例
// 为 azimuth 库添加10个高质量测试用例

test "arithmetics_add_comprehensive" {
  // 全面测试加法运算
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-15, add(-7, -8))
  assert_eq(1, add(-7, 8))
  assert_eq(2000000, add(1000000, 1000000))
}

test "arithmetics_multiply_properties" {
  // 测试乘法运算的数学性质
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(7, 0))
  assert_eq(-21, multiply(-7, 3))
  assert_eq(21, multiply(-7, -3))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_international_chars" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, empty!", greet(""))
}

test "mathematical_commutative_laws" {
  // 验证数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let total_items = 47
  let items_per_package = 6
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(8, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
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

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 95
  let team_count = 4
  let resources_per_team = divide_with_ceil(total_resources, team_count)
  
  assert_eq(24, resources_per_team)
  
  // 验证分配结果
  let total_allocated = multiply(resources_per_team, team_count)
  assert_eq(96, total_allocated)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), total_allocated)
  assert_eq(99, utilization)
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 87
  let tables_available = 8
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 计算可用座位数
  let available_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(available_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 验证座位充足性
  assert_true(available_seats >= participants)
}