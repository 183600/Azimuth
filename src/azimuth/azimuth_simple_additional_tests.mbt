// 标准 MoonBit 测试用例 - 针对 azimuth 库的额外测试
// 包含核心功能的全面测试，使用标准 MoonBit 测试语法

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
}

test "multiply_function_associative_property" {
  // 测试乘法结合律
  let x = 4
  let y = 5
  let z = 6
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  assert_eq(120, multiply(4, multiply(5, 6)))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, divide_with_ceil(8, 3))  // 2.666... -> 3
  assert_eq(7, divide_with_ceil(13, 2)) // 6.5 -> 7
  assert_eq(0, divide_with_ceil(0, 5))  // 0/5 = 0 -> 0
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  let unit_price = 45
  let quantity = 7
  let tax_rate = 8
  let discount = 25
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(subtotal, tax_amount)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(315, subtotal)
  assert_eq(26, tax_amount)
  assert_eq(341, total_before_discount)
  assert_eq(316, final_total)
}

test "negative_number_division_ceil" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-4, 2))   // -4/2 = -2 -> -2
  assert_eq(-1, divide_with_ceil(-3, 2))   // -3/2 = -1.5 -> -1
  assert_eq(-3, divide_with_ceil(-7, 2))   // -7/2 = -3.5 -> -3
  assert_eq(0, divide_with_ceil(-5, 0))    // 除数为0返回0
}

test "mathematical_distributive_property" {
  // 测试分配律 a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 5 * (8 + 12) = 5 * 8 + 5 * 12 = 100
}

test "resource_allocation_calculation" {
  // 测试资源分配计算
  let total_budget = 5000
  let project_cost = 1200
  let project_count = 3
  
  let total_project_cost = multiply(project_cost, project_count)
  let remaining_budget = add(total_budget, -total_project_cost)
  let additional_projects = divide_with_ceil(remaining_budget, project_cost)
  
  assert_eq(3600, total_project_cost)
  assert_eq(1400, remaining_budget)
  assert_eq(2, additional_projects)
}

test "string_greeting_edge_cases" {
  // 测试字符串问候边界情况
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello, 12345!", greet("12345"))  // 数字字符串
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))  // 邮箱格式
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let base_value = 10
  let multiplier = 3
  let addition = 15
  let divisor = 4
  
  let step1 = multiply(base_value, multiplier)
  let step2 = add(step1, addition)
  let final_result = divide_with_ceil(step2, divisor)
  
  assert_eq(30, step1)
  assert_eq(45, step2)
  assert_eq(12, final_result)  // ceil(45/4) = 12
}