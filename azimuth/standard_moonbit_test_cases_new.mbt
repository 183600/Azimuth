// 标准 MoonBit 测试用例 - 新增10个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_function_comprehensive_test" {
  // 全面测试加法函数的各种情况
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-15, add(-7, -8))
  assert_eq(1, add(-7, 8))
  assert_eq(-1, add(7, -8))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_comprehensive_test" {
  // 全面测试乘法函数的各种情况
  assert_eq(56, multiply(7, 8))
  assert_eq(0, multiply(7, 0))
  assert_eq(-56, multiply(-7, 8))
  assert_eq(56, multiply(-7, -8))
  assert_eq(1000000, multiply(1000, 1000))
}

test "subtract_function_comprehensive_test" {
  // 全面测试减法函数的各种情况
  assert_eq(-1, subtract(7, 8))
  assert_eq(1, subtract(8, 7))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(-7, -8))
  assert_eq(-15, subtract(7, -8))
}

test "divide_with_ceil_function_comprehensive_test" {
  // 全面测试向上取整除法函数的各种情况
  assert_eq(3, divide_with_ceil(20, 7))
  assert_eq(2, divide_with_ceil(14, 7))
  assert_eq(1, divide_with_ceil(7, 7))
  assert_eq(0, divide_with_ceil(0, 7))
  assert_eq(0, divide_with_ceil(7, 0))
  assert_eq(-2, divide_with_ceil(-20, 7))
  assert_eq(-3, divide_with_ceil(20, -7))
  assert_eq(3, divide_with_ceil(-20, -7))
}

test "greet_function_comprehensive_test" {
  // 全面测试问候函数的各种情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties_verification_test" {
  // 验证数学性质的测试
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 减法性质
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "business_logic_packaging_test" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline_test" {
  // 复杂计算流水线测试
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

test "error_handling_and_boundary_test" {
  // 错误处理和边界测试
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 大数测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "real_world_application_scenario_test" {
  // 真实世界应用场景测试：活动安排
  let participants = 87
  let tables = 8
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
  
  // 验证桌子数量可用
  assert_true(tables_needed <= tables)
}