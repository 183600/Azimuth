// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_identity_elements" {
  // 测试加法的单位元性质
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1000000, @azimuth.add(1000000, 0))
}

test "multiply_function_zero_property" {
  // 测试乘法的零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 15))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-100, 0))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))  // 2.666... -> 3
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(13, 2)) // 6.5 -> 7
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(999, 10)) // 99.9 -> 100
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏!", @azimuth.greet("🌍🌎🌏"))
}

test "mathematical_distributive_law" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 7
  let c = 9
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)  // 5 * (7 + 9) = 5 * 7 + 5 * 9 = 80
}

test "negative_number_division_comprehensive" {
  // 全面测试负数除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-4, 2))   // -4/2 = -2 -> -2
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 2))   // -3/2 = -1.5 -> -1
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-7, 2))   // -7/2 = -3.5 -> -3
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-7, -2))   // -7/-2 = 3.5 -> 4
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 0))    // 除数为0的情况
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 简化的复利计算
  let year1_amount = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  let year2_amount = @azimuth.add(year1_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year1_amount, rate_percent), 100))
  let year3_amount = @azimuth.add(year2_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year2_amount, rate_percent), 100))
  
  @azimuth.assert_eq(10800, year1_amount)
  @azimuth.assert_eq(11664, year2_amount)
  @azimuth.assert_eq(12597, year3_amount)
}

test "inventory_optimization_problem" {
  // 库存优化问题
  let product_a_qty = 45
  let product_b_qty = 67
  let product_c_qty = 23
  let box_capacity = 20
  
  let total_items = @azimuth.add(@azimuth.add(product_a_qty, product_b_qty), product_c_qty)
  let boxes_needed = @azimuth.divide_with_ceil(total_items, box_capacity)
  
  @azimuth.assert_eq(135, total_items)
  @azimuth.assert_eq(7, boxes_needed)  // ceil(135/20) = 7
}

test "string_greeting_with_special_characters" {
  // 测试包含特殊字符的字符串问候
  @azimuth.assert_eq_string("Hello, C++!", @azimuth.greet("C++"))
  @azimuth.assert_eq_string("Hello, HTML5!", @azimuth.greet("HTML5"))
  @azimuth.assert_eq_string("Hello, API@v2.0!", @azimuth.greet("API@v2.0"))
  @azimuth.assert_eq_string("Hello, a+b=c!", @azimuth.greet("a+b=c"))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟计算
  let input_size = 1000
  let linear_factor = 3
  let logarithmic_factor = 10
  
  // 模拟 O(n + n log n) 算法的复杂度
  let linear_part = @azimuth.multiply(input_size, linear_factor)
  let log_part = @azimuth.multiply(@azimuth.divide_with_ceil(input_size, 100), logarithmic_factor)
  let total_complexity = @azimuth.add(linear_part, log_part)
  
  @azimuth.assert_eq(3000, linear_part)
  @azimuth.assert_eq(100, log_part)
  @azimuth.assert_eq(3100, total_complexity)
}