// 标准 MoonBit 测试用例 - 为 azimuth 项目创建
// 包含 8 个高质量测试用例，覆盖主要功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(4, subtract(10, 6))
  assert_eq(-4, subtract(6, 10))
}

test "multiplication_properties" {
  // 乘法性质测试
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(100, multiply(10, 10))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-2, divide_with_ceil(-7, 4))
}

test "greet_function_variations" {
  // 问候函数变体测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 场景：计算装盒需求
  let items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 13
  let b = 27
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(40, add(13, 27))
  assert_eq(351, multiply(13, 27))
}

test "edge_case_handling" {
  // 边界情况处理测试
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(0, multiply(1000000, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  
  // 负数边界测试
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(1000000, multiply(-1000000, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "real_world_application_test" {
  // 真实世界应用测试
  // 场景：活动座位安排
  let participants = 87
  let tables = 10
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let empty_seats = subtract(total_seats, participants)
  assert_eq(1, empty_seats)
  
  // 验证座位充足
  assert_true(total_seats >= participants)
}