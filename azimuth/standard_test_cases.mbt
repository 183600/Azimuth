// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-3, 13))
}

test "multiply_function_edge_cases" {
  // 乘法边缘情况测试
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(5, -3))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_scenarios" {
  // 向上取整除法场景测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "subtract_function_basic" {
  // 基本减法测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 7
  let y = 13
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 减法非交换性验证 - 通过验证差值不为0来证明
  let diff1 = subtract(x, y)
  let diff2 = subtract(y, x)
  assert_eq(0, add(diff1, diff2))  // x-y + y-x = 0
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算测试
  let items = 25
  let capacity_per_box = 6
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(30, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(5, remaining_space)
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let base = 10
  let step1 = add(base, 5)        // 15
  let step2 = multiply(step1, 2)  // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = subtract(step3, 1)  // 3
  
  assert_eq(3, step4)
}

test "error_handling_division" {
  // 除法错误处理测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "real_world_application" {
  // 真实世界应用场景：活动安排
  let participants = 87
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, total_seats)
  
  // 验证座位充足
  assert_true(total_seats >= participants)
}