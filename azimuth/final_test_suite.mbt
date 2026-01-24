// 最终的测试套件 - 10个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_function_comprehensive" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(2, -3))
  assert_eq(10, add(7, 3))
  assert_eq(-10, add(-5, -5))
}

test "multiply_function_comprehensive" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
}

test "greet_function_comprehensive" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "divide_with_ceil_comprehensive" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "mathematical_properties_addition" {
  // 加法交换律
  assert_eq(add(5, 7), add(7, 5))
  assert_eq(add(-3, 8), add(8, -3))
  assert_eq(add(0, 10), add(10, 0))
}

test "mathematical_properties_multiplication" {
  // 乘法交换律
  assert_eq(multiply(4, 6), multiply(6, 4))
  assert_eq(multiply(-2, 5), multiply(5, -2))
  assert_eq(multiply(0, 7), multiply(7, 0))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = divide_with_ceil(add(15, 10), 5)
  assert_eq(5, result2)  // ceil(25/5) = 5
  
  let result3 = multiply(divide_with_ceil(20, 3), 2)
  assert_eq(14, result3)  // ceil(20/3) * 2 = 7 * 2 = 14
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "real_world_application_scenarios" {
  // 真实世界应用场景
  // 场景：计算装盒问题
  let items = 25
  let box_capacity = 8
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(4, boxes_needed)  // ceil(25/8) = 4
  
  // 场景：计算运输车辆
  let packages = 47
  let truck_capacity = 10
  let trucks_needed = divide_with_ceil(packages, truck_capacity)
  assert_eq(5, trucks_needed)  // ceil(47/10) = 5
  
  // 场景：计算活动安排
  let participants = 87
  let chairs_per_table = 8
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
}