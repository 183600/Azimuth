// 标准 MoonBit 测试套件
// 包含8个高质量测试用例，覆盖核心功能和边界情况

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(10, 8))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-8, subtract(5, 13))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "zero_boundary_conditions" {
  // 零值边界条件测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 7
  let y = 13
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 模拟购物车计算
  let item_price = 25
  let quantity = 4
  let shipping = 10
  let tax_rate = 10
  
  let subtotal = multiply(item_price, quantity)
  let total_before_tax = add(subtotal, shipping)
  let tax = divide_with_ceil(multiply(total_before_tax, tax_rate), 100)
  let final_total = add(total_before_tax, tax)
  
  assert_eq(100, subtotal)
  assert_eq(110, total_before_tax)
  assert_eq(11, tax)
  assert_eq(121, final_total)
}

test "edge_case_divisions" {
  // 除法边界情况测试
  // 测试向上取整除法的各种边界情况
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(1, divide_with_ceil(-1, 2))
  assert_eq(-1, divide_with_ceil(1, -2))
}

test "practical_business_calculations" {
  // 实际业务计算测试
  // 模拟库存管理计算
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  let new_shipment = 200
  
  let weekly_sales = multiply(daily_sales, days)
  let remaining_stock = subtract(initial_stock, weekly_sales)
  let final_stock = add(remaining_stock, new_shipment)
  let weeks_of_supply = divide_with_ceil(final_stock, weekly_sales)
  
  assert_eq(175, weekly_sales)
  assert_eq(325, remaining_stock)
  assert_eq(525, final_stock)
  assert_eq(3, weeks_of_supply)
}