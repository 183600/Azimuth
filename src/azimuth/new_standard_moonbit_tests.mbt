// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_identity_elements" {
  // 测试加法的单位元性质
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  assert_eq(0, add(0, 0))
  assert_eq(1000000, add(1000000, 0))
}

test "multiply_function_zero_property" {
  // 测试乘法的零元素性质
  assert_eq(0, multiply(7, 0))
  assert_eq(0, multiply(0, 15))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(-100, 0))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, divide_with_ceil(8, 3))  // 2.666... -> 3
  assert_eq(7, divide_with_ceil(13, 2)) // 6.5 -> 7
  assert_eq(100, divide_with_ceil(999, 10)) // 99.9 -> 100
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
}

test "mathematical_distributive_law" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 7
  let c = 9
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 5 * (7 + 9) = 5 * 7 + 5 * 9 = 80
}

test "negative_number_division_comprehensive" {
  // 全面测试负数除法
  assert_eq(-2, divide_with_ceil(-4, 2))   // -4/2 = -2 -> -2
  assert_eq(-1, divide_with_ceil(-3, 2))   // -3/2 = -1.5 -> -1
  assert_eq(-3, divide_with_ceil(-7, 2))   // -7/2 = -3.5 -> -3
  assert_eq(3, divide_with_ceil(-7, -2))   // -7/-2 = 3.5 -> 4
  assert_eq(0, divide_with_ceil(-5, 0))    // 除数为0的情况
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 简化的复利计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(10800, year1_amount)
  assert_eq(11664, year2_amount)
  assert_eq(12597, year3_amount)
}

test "inventory_optimization_problem" {
  // 库存优化问题
  let product_a_qty = 45
  let product_b_qty = 67
  let product_c_qty = 23
  let box_capacity = 20
  
  let total_items = add(add(product_a_qty, product_b_qty), product_c_qty)
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(135, total_items)
  assert_eq(7, boxes_needed)  // ceil(135/20) = 7
}

test "string_greeting_with_special_characters" {
  // 测试包含特殊字符的字符串问候
  assert_eq_string("Hello, C++!", greet("C++"))
  assert_eq_string("Hello, HTML5!", greet("HTML5"))
  assert_eq_string("Hello, API@v2.0!", greet("API@v2.0"))
  assert_eq_string("Hello, a+b=c!", greet("a+b=c"))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟计算
  let input_size = 1000
  let linear_factor = 3
  let logarithmic_factor = 10
  
  // 模拟 O(n + n log n) 算法的复杂度
  let linear_part = multiply(input_size, linear_factor)
  let log_part = multiply(divide_with_ceil(input_size, 100), logarithmic_factor)
  let total_complexity = add(linear_part, log_part)
  
  assert_eq(3000, linear_part)
  assert_eq(100, log_part)
  assert_eq(3100, total_complexity)
}