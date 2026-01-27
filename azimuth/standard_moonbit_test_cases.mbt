// 标准 MoonBit 测试用例 - 为 azimuth 项目添加的新测试
// 这些测试用例涵盖基本功能、边界情况和实际应用场景

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
}

test "multiplication_with_zero" {
  // 测试乘法与零的特殊情况
  assert_eq(0, multiply(0, 10))
  assert_eq(0, multiply(10, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(-5, 0))
}

test "greeting_function_variations" {
  // 测试问候函数的各种变化
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "ceil_division_basic_cases" {
  // 测试向上取整除法的基本情况
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-5, -3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-2, divide_with_ceil(-7, 4))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：包装问题
  let items = 23
  let box_capacity = 5
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(25, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(2, remaining_space)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
}

test "edge_case_boundary_values" {
  // 测试边界值情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(101, divide_with_ceil(100, 1))
}

test "real_world_application" {
  // 真实世界应用：活动座位安排
  let participants = 67
  let chairs_per_table = 8
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 计算总座位数
  let total_chairs = multiply(tables_needed, chairs_per_table)
  assert_eq(72, total_chairs)
  
  // 验证座位充足
  assert_true(total_chairs >= participants)
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, émoji 🚀!", greet("émoji 🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}