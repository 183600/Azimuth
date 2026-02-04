// Azimuth 增强测试套件
// 针对核心功能的高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_comprehensive_test" {
  // 全面测试加法函数
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
  assert_eq(0, add(100, -100))
  assert_eq(2147483647, add(2147483646, 1))
}

test "multiply_function_comprehensive_test" {
  // 全面测试乘法函数
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(100, multiply(10, 10))
  assert_eq(-100, multiply(10, -10))
  assert_eq(1, multiply(1, 2147483647))
}

test "subtract_function_comprehensive_test" {
  // 全面测试减法函数
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
  assert_eq(20, subtract(-5, -25))
  assert_eq(2147483647, subtract(2147483647, 0))
}

test "divide_with_ceil_edge_cases_test" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(100, 100))
}

test "greet_function_unicode_test" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, áéíóú!", greet("áéíóú"))
  assert_eq_string("Hello, ñ!", greet("ñ"))
}

test "mathematical_properties_test" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
  
  // 分配律
  assert_eq(multiply(a, add(b, 2)), add(multiply(a, b), multiply(a, 2)))
}

test "business_calculation_scenario_test" {
  // 业务计算场景测试
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_calculation_test" {
  // 包装计算测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining_space = subtract(total_capacity, total_items)
  
  assert_eq(48, total_capacity)
  assert_eq(1, remaining_space)
}

test "financial_calculation_test" {
  // 金融计算测试
  let principal = 1000
  let rate_percent = 5
  
  // 简化的复利计算（每年计算一次）
  let amount_after_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let amount_after_year2 = add(amount_after_year1, divide_with_ceil(multiply(amount_after_year1, rate_percent), 100))
  
  assert_eq(1050, amount_after_year1)
  assert_eq(1103, amount_after_year2)
}

test "error_handling_robustness_test" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
  assert_eq(1000000000000, multiply(big_num, big_num))
}