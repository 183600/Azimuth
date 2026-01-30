// Azimuth 专注单元测试 - 标准 MoonBit 测试语法
// 覆盖核心功能的简洁测试用例

test "add_function_core_functionality" {
  // 测试加法核心功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 测试乘法综合功能
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(7, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(16, multiply(-4, -4))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法精度
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_variations" {
  // 测试问候函数变体
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let x = 15
  let y = 25
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算订单：(商品价格 × 数量) + 运费
  let price = 45
  let quantity = 6
  let shipping = 20
  let total = add(multiply(price, quantity), shipping)
  assert_eq(290, total)
}

test "negative_number_division" {
  // 测试负数除法
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-3, 5))
}

test "resource_allocation_calculation" {
  // 测试资源分配计算
  let total_resources = 47
  let team_size = 8
  let teams_needed = divide_with_ceil(total_resources, team_size)
  assert_eq(6, teams_needed)
}

test "string_processing_with_greet" {
  // 测试字符串处理与问候
  let first_name = "John"
  let last_name = "Doe"
  let full_name = first_name + " " + last_name
  let greeting = greet(full_name)
  assert_eq_string("Hello, John Doe!", greeting)
}

test "edge_case_boundary_values" {
  // 测试边界值情况
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(0, -1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(100, 100))
}