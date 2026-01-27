// Azimuth 库标准测试用例
// 包含10个高质量的 MoonBit 测试用例

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-3, 13))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(100, multiply(10, 10))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(-5, -20))
  assert_eq(-15, subtract(5, 20))
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
  assert_eq_string("Hello, 世界!", greet("世界"))
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

test "business_logic_packaging" {
  // 业务逻辑：包装计算测试
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(5, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base = 8
  let multiplier = 3
  let divisor = 5
  let addition = 7
  
  let step1 = multiply(base, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(12, result)  // ceil(8*3/5) + 7 = 5 + 7 = 12
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "real_world_resource_allocation" {
  // 真实世界资源分配测试
  let participants = 47
  let tables = 6
  let chairs_per_table = 8
  
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(6, tables_needed)
  
  let total_chairs = multiply(tables_needed, chairs_per_table)
  assert_eq(48, total_chairs)
  
  let empty_chairs = subtract(total_chairs, participants)
  assert_eq(1, empty_chairs)
}