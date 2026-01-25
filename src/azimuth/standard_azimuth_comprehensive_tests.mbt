// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含实用的测试场景，覆盖主要函数功能

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
  assert_eq(0, add(5, -5))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(100, multiply(10, 10))
  assert_eq(6, multiply(-2, -3))
}

test "divide_with_ceil_function_comprehensive" {
  // 全面测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "practical_business_calculation" {
  // 实际业务计算场景
  let unit_price = 45
  let quantity = 6
  let shipping_cost = 18
  let discount = 25
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(288, total_before_discount)
  assert_eq(263, final_total)
}

test "packaging_optimization_calculation" {
  // 包装优化计算场景
  let total_products = 58
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(total_products, capacity_per_box)
  
  assert_eq(5, boxes_needed)
}

test "complex_expression_evaluation" {
  // 复杂表达式求值
  let x = 15
  let y = 8
  let z = 4
  
  // 测试 (x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(91, result)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极端值测试
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(0, -100))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(100, divide_with_ceil(100, 1))
}