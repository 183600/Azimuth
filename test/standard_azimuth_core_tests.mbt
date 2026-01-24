// 标准 Azimuth 核心功能测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(45, 55))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合运算
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(0, 10))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(5, 5))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(-2, divide_with_ceil(-8, 4))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 6
  let b = 9
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "real_world_business_calculation" {
  // 实际业务计算：订单总价计算
  let unit_price = 35
  let quantity = 6
  let tax_rate = 8
  let shipping = 12
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(210, subtotal)
  assert_eq(17, tax)
  assert_eq(239, total)
}

test "complex_expression_evaluation" {
  // 复杂表达式求值
  let x = 10
  let y = 15
  let z = 20
  
  // 计算 (x + y) * z - x
  let result = add(multiply(add(x, y), z), -x)
  assert_eq(490, result)  // (10 + 15) * 20 - 10 = 25 * 20 - 10 = 500 - 10 = 490
}

test "inventory_management_scenario" {
  // 库存管理场景
  let initial_stock = 120
  let daily_sales = 8
  let days = 7
  
  let total_sold = multiply(daily_sales, days)
  let remaining_stock = add(initial_stock, -total_sold)
  
  assert_eq(56, total_sold)
  assert_eq(64, remaining_stock)
}

test "packaging_optimization_test" {
  // 包装优化测试
  let total_items = 47
  let items_per_large_box = 12
  let items_per_small_box = 5
  
  let large_boxes = divide_with_ceil(total_items, items_per_large_box)
  let small_boxes = divide_with_ceil(total_items, items_per_small_box)
  
  assert_eq(4, large_boxes)  // ceil(47/12) = 4
  assert_eq(10, small_boxes) // ceil(47/5) = 10
}

test "unicode_and_special_characters_handling" {
  // Unicode 和特殊字符处理测试
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, Café!", greet("Café"))
}