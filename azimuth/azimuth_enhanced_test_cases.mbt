// Azimuth 增强测试用例
// 覆盖核心功能的边界情况和实际应用场景

test "arithmetics_zero_handling" {
  // 测试算术运算的零值处理
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(5, subtract(5, 0))
}

test "arithmetics_negative_numbers" {
  // 测试算术运算的负数处理
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(-1, divide_with_ceil(-3, 4))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let base = 10
  let step1 = add(base, 5)  // 15
  let step2 = multiply(step1, 2)  // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = subtract(step3, 1)  // 3
  assert_eq(3, step4)
}

test "resource_allocation_scenario" {
  // 测试资源分配场景
  let total_items = 47
  let capacity_per_unit = 12
  let units_needed = divide_with_ceil(total_items, capacity_per_unit)
  assert_eq(4, units_needed)
  
  let total_capacity = multiply(units_needed, capacity_per_unit)
  assert_eq(48, total_capacity)
  
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "error_handling_boundary" {
  // 测试错误处理边界情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, -1))
}

test "performance_large_numbers" {
  // 测试大数运算性能
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  assert_eq(0, subtract(1000000, 1000000))
}

test "real_world_application" {
  // 测试真实世界应用场景
  // 场景：活动座位安排
  let participants = 87
  let chairs_per_table = 10
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  let total_chairs = multiply(tables_needed, chairs_per_table)
  assert_eq(90, total_chairs)
  
  let empty_chairs = subtract(total_chairs, participants)
  assert_eq(3, empty_chairs)
}