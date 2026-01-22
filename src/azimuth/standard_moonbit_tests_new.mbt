// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  assert_eq(100, left_side)  // 5 × (8 + 12) = 5 × 20 = 100
}

test "divide_with_ceil_fractional_precision" {
  // 测试向上取整除法的小数精度处理
  assert_eq(4, divide_with_ceil(11, 3))   // 11/3 = 3.666... -> 4
  assert_eq(7, divide_with_ceil(20, 3))   // 20/3 = 6.666... -> 7
  assert_eq(2, divide_with_ceil(4, 2))    // 4/2 = 2 -> 2
  assert_eq(1, divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  assert_eq(0, divide_with_ceil(0, 5))    // 0/5 = 0 -> 0
}

test "greet_function_unicode_comprehensive" {
  // 测试问候函数的全面Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🎉🎊!", greet("🎉🎊"))
}

test "complex_business_calculation_scenario" {
  // 测试复杂业务计算场景：电商订单计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping_cost = 15
  let discount = 20
  
  // 计算商品小计
  let subtotal = multiply(product_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价（含运费）
  let total_before_discount = add(add(subtotal, tax), shipping_cost)
  // 应用折扣
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax)
  assert_eq(400, total_before_discount)
  assert_eq(380, final_total)
}

test "negative_number_operations_comprehensive" {
  // 测试负数运算的全面情况
  // 负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(-2, add(-5, 3))
  assert_eq(0, add(-8, 8))
  
  // 负数乘法
  assert_eq(15, multiply(-3, -5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(-7, 0))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
  assert_eq(-2, divide_with_ceil(5, -3))
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 等差数列求和：1 + 3 + 5 + ... + 19
  let first_term = 1
  let common_diff = 2
  let num_terms = 10
  let last_term = add(first_term, multiply(common_diff, num_terms - 1))
  let arithmetic_sum = divide_with_ceil(multiply(num_terms, add(first_term, last_term)), 2)
  
  // 等比数列求和：2 + 4 + 8 + 16 + 32
  let geo_first = 2
  let geo_ratio = 2
  let geo_terms = 5
  let geometric_sum = multiply(geo_first, divide_with_ceil(multiply(geo_ratio, geo_ratio), geo_ratio - 1))
  
  assert_eq(19, last_term)
  assert_eq(100, arithmetic_sum)  // 10 × (1 + 19) / 2 = 100
  assert_eq(8, geometric_sum)     // 2 × (2^2) / (2-1) = 8
}

test "real_world_packaging_optimization" {
  // 测试现实世界包装优化问题
  let total_products = 157
  let box_capacity_a = 12
  let box_capacity_b = 18
  let box_capacity_c = 25
  
  // 计算使用不同包装箱需要的数量
  let boxes_a = divide_with_ceil(total_products, box_capacity_a)
  let boxes_b = divide_with_ceil(total_products, box_capacity_b)
  let boxes_c = divide_with_ceil(total_products, box_capacity_c)
  
  // 计算剩余空间
  let unused_space_a = multiply(boxes_a, box_capacity_a) - total_products
  let unused_space_b = multiply(boxes_b, box_capacity_b) - total_products
  let unused_space_c = multiply(boxes_c, box_capacity_c) - total_products
  
  assert_eq(14, boxes_a)    // ceil(157/12) = 14
  assert_eq(9, boxes_b)     // ceil(157/18) = 9
  assert_eq(7, boxes_c)     // ceil(157/25) = 7
  assert_eq(11, unused_space_a)  // 14×12 - 157 = 168 - 157 = 11
  assert_eq(5, unused_space_b)   // 9×18 - 157 = 162 - 157 = 5
  assert_eq(18, unused_space_c)  // 7×25 - 157 = 175 - 157 = 18
}

test "string_processing_with_special_characters" {
  // 测试特殊字符的字符串处理
  let test_cases = [
    ("123", "Hello, 123!"),
    ("abc123", "Hello, abc123!"),
    ("test@example.com", "Hello, test@example.com!"),
    ("A+B=C", "Hello, A+B=C!"),
    ("100%", "Hello, 100%!"),
    ("C++", "Hello, C++!")
  ]
  
  // 测试各种特殊字符组合
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, abc123!", greet("abc123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))
  assert_eq_string("Hello, 100%!", greet("100%"))
  assert_eq_string("Hello, C++!", greet("C++"))
}

test "extreme_boundary_value_operations" {
  // 测试极端边界值运算
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近最大值的运算
  assert_eq(2147483646, add(max_int, -1))
  assert_eq(2147483647, multiply(max_int, 1))
  assert_eq(0, multiply(max_int, 0))
  
  // 测试接近最小值的运算
  assert_eq(-2147483647, add(min_int, 1))
  assert_eq(-2147483648, multiply(min_int, 1))
  assert_eq(0, multiply(min_int, 0))
  
  // 测试极端值的除法安全性
  assert_eq(0, divide_with_ceil(max_int, 0))
  assert_eq(0, divide_with_ceil(min_int, 0))
  assert_eq(1, divide_with_ceil(max_int, max_int))
  assert_eq(1, divide_with_ceil(min_int, min_int))
}