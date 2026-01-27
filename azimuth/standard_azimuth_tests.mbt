// 标准 MoonBit 测试用例 - azimuth 库
// 包含10个高质量测试用例，覆盖主要功能

test "add_basic_functionality" {
  // 测试加法基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-3, 13))
}

test "multiply_comprehensive" {
  // 测试乘法综合功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(1000, multiply(10, 100))
}

test "greet_string_handling" {
  // 测试字符串问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "subtract_functionality" {
  // 测试减法功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 13
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 减法性质
  assert_eq(x, subtract(x, 0))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线
  let base = 10
  let result = divide_with_ceil(
    add(multiply(base, 2), multiply(base, 3)),
    subtract(base, 2)
  )
  
  // 计算：(10*2 + 10*3) / (10-2) = 50 / 8 = ceil(6.25) = 7
  assert_eq(7, result)
}

test "edge_cases_and_boundaries" {
  // 边界情况和极值测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "real_world_application" {
  // 真实世界应用场景：活动安排
  let participants = 87
  let tables = 8
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 计算可用座位数
  let available_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, available_seats)
  
  // 验证座位充足性
  let remaining_seats = subtract(available_seats, participants)
  assert_eq(3, remaining_seats)
}