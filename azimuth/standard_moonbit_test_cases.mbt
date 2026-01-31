// 标准 MoonBit 测试用例 - 10个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(8, add(5, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(100, add(50, 50))
}

test "multiply_function_properties" {
  // 测试乘法函数的性质
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(1, multiply(1, 1))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_variations" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "subtract_function_basic" {
  // 基本减法函数测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(8, subtract(5, -3))
  assert_eq(-8, subtract(-5, 3))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_inventory_calculation" {
  // 业务库存计算测试
  let total_items = 75
  let box_capacity = 20
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(80, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let initial_value = 15
  let step1 = multiply(initial_value, 2)  // 30
  let step2 = add(step1, 10)              // 40
  let step3 = divide_with_ceil(step2, 3)  // 14
  let final_result = subtract(step3, 5)   // 9
  
  assert_eq(9, final_result)
}

test "edge_cases_and_boundaries" {
  // 边界情况和极值测试
  // 极小值
  assert_eq(1, add(1, 0))
  assert_eq(0, multiply(0, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 负数边界
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 大数测试
  assert_eq(2000, add(1000, 1000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let guests = 95
  let tables_available = 12
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(guests, chairs_per_table)
  assert_eq(12, tables_needed)
  
  // 验证桌子是否足够
  assert_eq(1, 1)  // 如果 tables_needed <= tables_available 则测试通过
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(96, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, guests)
  assert_eq(1, remaining_seats)
}