// Azimuth 综合测试套件
// 包含针对核心功能的高质量测试用例

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
  assert_eq(-100, add(50, -150))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(-5, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_international" {
  // 测试国际化问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, José!", greet("José"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律 a * (b + c) = a * b + a * c
  let c = 5
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "real_world_packaging_scenario" {
  // 实际包装场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let box_cost = 5
  let total_box_cost = multiply(boxes_needed, box_cost)
  
  assert_eq(4, boxes_needed)
  assert_eq(20, total_box_cost)
}

test "string_processing_combined" {
  // 字符串处理组合测试
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
  
  // 测试特殊字符
  let email = "test@example.com"
  let email_greeting = greet(email)
  assert_eq_string("Hello, test@example.com!", email_greeting)
}

test "algorithm_complex_calculation" {
  // 算法复杂计算
  // 模拟计算数列前n项和：1 + 2 + 3 + ... + n
  let n = 20
  let series_sum = divide_with_ceil(multiply(n, add(n, 1)), 2)
  
  // 验证结果：1 + 2 + ... + 20 = 210
  assert_eq(210, series_sum)
  
  // 模拟计算阶乘的简化版本
  let factorial_5 = multiply(multiply(multiply(multiply(1, 2), 3), 4), 5)
  assert_eq(120, factorial_5)
}