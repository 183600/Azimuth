// 标准 MoonBit 测试用例
// 为 azimuth 库添加的核心功能测试

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_scenarios" {
  // 测试向上取整除法的各种场景
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
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
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  assert_eq(4, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base = 10
  let step1 = multiply(base, 3)
  let step2 = add(step1, 5)
  let step3 = divide_with_ceil(step2, 4)
  assert_eq(9, step3)  // ceil((10*3+5)/4) = ceil(35/4) = 9
}

test "edge_cases_and_boundaries" {
  // 边界情况和极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1, add(1, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let teams = 3
  let resources_per_team = divide_with_ceil(total_resources, teams)
  assert_eq(34, resources_per_team)
  
  let total_allocated = multiply(resources_per_team, teams)
  assert_eq(102, total_allocated)
}