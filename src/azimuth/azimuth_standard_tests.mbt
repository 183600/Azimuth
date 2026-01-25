// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的简洁测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000, add(999, 1))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "mathematical_laws" {
  // 测试数学定律
  let a = 6
  let b = 9
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(a, b)), add(multiply(a, a), multiply(a, b)))
}

test "business_calculation_scenario" {
  // 业务计算场景
  let unit_price = 45
  let quantity = 6
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let final_total = add(subtotal, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(250, final_total)
}

test "packaging_optimization" {
  // 包装优化计算
  let products_per_box = 12
  let total_products = 65
  
  let boxes_needed = divide_with_ceil(total_products, products_per_box)
  let remaining_space = multiply(boxes_needed, products_per_box) - total_products
  
  assert_eq(6, boxes_needed)
  assert_eq(7, remaining_space)
}

test "unicode_string_processing" {
  // Unicode 字符串处理
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
}

test "complex_arithmetic_expression" {
  // 复杂算术表达式
  let x = 8
  let y = 12
  let z = 5
  
  // 计算 (x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(99, result)  // (8 + 12) * 5 - ceil(8/12) = 20*5 - 1 = 100 - 1 = 99
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_budget = 5000
  let large_project_cost = 1200
  let medium_projects = 3
  let medium_project_cost = 800
  
  let large_projects = divide_with_ceil(total_budget / 2, large_project_cost)
  let medium_total_cost = multiply(medium_projects, medium_project_cost)
  let remaining_budget = add(total_budget, -add(multiply(large_projects, large_project_cost), medium_total_cost))
  
  assert_eq(2, large_projects)
  assert_eq(2400, medium_total_cost)
  assert_eq(1400, remaining_budget)
}