// 标准 MoonBit 测试用例 - 针对 azimuth 库
// 使用标准的 MoonBit 测试语法编写

test "add_function_comprehensive" {
  // 全面的加法功能测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_properties" {
  // 乘法性质测试
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(10, 0))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
}

test "divide_with_ceil_scenarios" {
  // 向上取整除法场景测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_variations" {
  // 问候函数变体测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证
  let a = 8
  let b = 12
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax)
  assert_eq(307, total)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(0, 100))
  assert_eq(100, multiply(1, 100))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
  assert_eq_string("Hello, emoji🚀!", greet("emoji🚀"))
}

test "real_world_inventory_scenario" {
  // 真实世界库存场景
  let initial_stock = 120
  let daily_sales = 8
  let days = 7
  let restock = 50
  
  let sold = multiply(daily_sales, days)
  let before_restock = add(initial_stock, 0 - sold)
  let final_stock = add(before_restock, restock)
  
  assert_eq(56, sold)
  assert_eq(64, before_restock)
  assert_eq(114, final_stock)
}