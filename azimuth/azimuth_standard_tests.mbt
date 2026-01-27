// Azimuth 标准测试用例
// 使用标准 MoonBit 测试语法编写的高质量测试用例

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(15, -5))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(1, multiply(1, 100))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base = 10
  let step1 = multiply(base, 3)
  let step2 = add(step1, 5)
  let step3 = divide_with_ceil(step2, 4)
  assert_eq(4, step3)  // ceil((10*3+5)/4) = ceil(35/4) = 9
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_resources = 100
  let teams = 3
  let resources_per_team = divide_with_ceil(total_resources, teams)
  assert_eq(34, resources_per_team)
  
  // 验证分配结果
  let total_allocated = multiply(resources_per_team, teams)
  assert_eq(102, total_allocated)
}

test "edge_cases_and_boundaries" {
  // 边界情况和极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(1000000, multiply(1000, 1000))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 87
  let chairs_per_table = 10
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, total_seats)
  
  // 验证座位充足
  let extra_seats = subtract(total_seats, participants)
  assert_eq(3, extra_seats)
}