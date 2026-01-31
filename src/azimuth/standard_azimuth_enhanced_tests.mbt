// 标准 MoonBit 增强测试用例
// 针对 azimuth 库核心功能的高质量测试

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))  // 42 + 17 = 17 + 42
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 5 * (8 + 3) = 5 * 8 + 5 * 3 = 55
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  assert_eq(0, divide_with_ceil(0, 100)) // 0/100 = 0 -> 0
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
}

test "negative_number_arithmetic" {
  // 测试负数算术运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-4, add(-8, 4))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "business_order_calculation" {
  // 测试业务订单计算场景
  let unit_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(368, total_before_discount)
  assert_eq(348, final_total)
}

test "packaging_optimization_scenario" {
  // 测试包装优化场景
  let total_items = 87
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(8, boxes_needed)  // ceil(87/12) = 8
  
  // 测试剩余空间
  let total_capacity = multiply(boxes_needed, items_per_box)
  let empty_space = add(total_capacity, -total_items)
  assert_eq(9, empty_space)  // 8*12 - 87 = 96 - 87 = 9
}

test "mathematical_series_summation" {
  // 测试数学级数求和：计算等差数列的和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(120, sum_formula)  // 10/2 * (6 + 18) = 5 * 24 = 120
}

test "error_handling_edge_cases" {
  // 测试错误处理边界情况
  // 测试除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试零的运算性质
  assert_eq(0, multiply(0, 1000000))
  assert_eq(0, multiply(1000000, 0))
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算：复利计算
  let principal = 1000
  let rate_percent = 5
  
  // 简化的复利计算（每年计算一次）
  let amount_after_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let amount_after_year2 = add(amount_after_year1, divide_with_ceil(multiply(amount_after_year1, rate_percent), 100))
  let amount_after_year3 = add(amount_after_year2, divide_with_ceil(multiply(amount_after_year2, rate_percent), 100))
  
  // 验证结果（由于使用向上取整，结果会略高于标准复利计算）
  assert_eq(1050, amount_after_year1)  // 1000 + ceil(1000*5/100) = 1000 + 50 = 1050
  assert_eq(1103, amount_after_year2)  // 1050 + ceil(1050*5/100) = 1050 + 53 = 1103
  assert_eq(1158, amount_after_year3)  // 1103 + ceil(1103*5/100) = 1103 + 55 = 1158
}