// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法编写的10个测试用例

test "add_function_comprehensive_test" {
  // 全面测试加法功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1, add(2147483647, -2147483646))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(0, 100))
  assert_eq(-24, multiply(-8, 3))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, multiply(1, 1))
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "complex_business_calculation_test" {
  // 复杂业务计算测试
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 交换律验证
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律验证
  let z = 5
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "error_handling_robustness_test" {
  // 错误处理健壮性测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 边界值测试
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "inventory_management_scenario_test" {
  // 库存管理场景测试
  let items_per_box = 24
  let total_items = 100
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let remaining_space = multiply(boxes_needed, items_per_box) - total_items
  
  assert_eq(5, boxes_needed)
  assert_eq(20, remaining_space)
}

test "string_processing_with_greet_test" {
  // 字符串处理与问候函数结合测试
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + " " + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张 三!", greeting)
  
  // 特殊字符测试
  let special_chars = "Hello & Goodbye"
  assert_eq_string("Hello, Hello & Goodbye!", greet(special_chars))
}

test "financial_calculation_compound_interest_test" {
  // 金融计算复利测试
  let principal = 1000
  let rate_percent = 5
  
  // 计算三年复利（简化版）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}