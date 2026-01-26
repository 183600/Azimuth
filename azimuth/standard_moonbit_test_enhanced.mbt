// 标准 MoonBit 增强测试用例
// 包含10个高质量测试用例，覆盖项目的主要功能

test "arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-2, add(-5, 3))
  assert_eq(0, add(5, -5))
  assert_eq(42, multiply(6, 7))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(0, multiply(0, 123))
}

test "divide_with_ceil_precision_scenarios" {
  // 向上取整除法的精度场景测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(0, divide_with_ceil(0, 7))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(
    multiply(a, add(b, 3)),
    add(multiply(a, b), multiply(a, 3))
  )
  
  // 减法验证
  assert_eq(4, subtract(12, 8))
  assert_eq(-4, subtract(8, 12))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let total_items = 95
  let box_capacity = 12
  
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(8, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(96, total_capacity)
  
  let unused_space = subtract(total_capacity, total_items)
  assert_eq(1, unused_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 20
  let multiplier = 3
  let divisor = 8
  let addition = 7
  
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(14, result)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 150
  let teams = 4
  
  let min_per_team = divide_with_ceil(total_resources, teams)
  assert_eq(38, min_per_team)
  
  let total_allocated = multiply(min_per_team, teams)
  assert_eq(152, total_allocated)
  
  let resource_efficiency = multiply(100, divide_with_ceil(total_resources, total_allocated))
  assert_eq(98, resource_efficiency)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, subtract(5, 5))
}

test "real_world_application_scenario" {
  // 真实世界应用场景：活动安排
  let participants = 156
  let tables = 12
  let chairs_per_table = 15
  
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)
  
  let available_chairs = multiply(tables_needed, chairs_per_table)
  assert_eq(165, available_chairs)
  
  let extra_chairs = subtract(available_chairs, participants)
  assert_eq(9, extra_chairs)
  
  assert_true(available_chairs >= participants)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的处理
  assert_eq(0, divide_with_ceil(50, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}