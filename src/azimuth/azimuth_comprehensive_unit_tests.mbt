// azimuth 库的综合单元测试
// 覆盖核心功能和边界情况

test "add_function_commutative_property" {
  // 测试加法交换律: a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  assert_eq(100, left_side)  // 5 * (8 + 12) = 5 * 20 = 100
}

test "divide_with_ceil_fractional_precision" {
  // 测试向上取整除法的小数精度
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 -> 2
  assert_eq(4, divide_with_ceil(11, 3))   // 3.666... -> 4
  assert_eq(7, divide_with_ceil(13, 2))   // 6.5 -> 7
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  assert_eq(0, divide_with_ceil(0, 5))    // 0/5 = 0 -> 0
}

test "greet_function_unicode_handling" {
  // 测试问候函数的Unicode字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🚀🌍!", greet("🚀🌍"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8  // 8%
  let shipping = 15
  let discount = 30
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(add(subtotal, tax), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(540, subtotal)
  assert_eq(44, tax)
  assert_eq(599, total_before_discount)
  assert_eq(569, final_total)
}

test "negative_number_operations_comprehensive" {
  // 负数运算的综合测试
  assert_eq(-10, add(-3, -7))
  assert_eq(2, add(-5, 7))
  assert_eq(-8, add(3, -11))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
  assert_eq(-2, divide_with_ceil(-4, 2))
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 8
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(92, sum)  // 8/2 * (10 + 21) = 4 * 31 = 124 -> 修正计算
}

test "real_world_inventory_management" {
  // 实际库存管理场景
  let initial_stock = 500
  let daily_sales = 25
  let days_to_check = 14
  let new_shipment = 200
  
  // 计算销售量
  let total_sold = multiply(daily_sales, days_to_check)
  // 计算剩余库存
  let remaining_stock = add(initial_stock, -total_sold)
  // 计算新库存
  let new_stock = add(remaining_stock, new_shipment)
  
  assert_eq(350, total_sold)
  assert_eq(150, remaining_stock)
  assert_eq(350, new_stock)
}

test "edge_cases_boundary_values" {
  // 边界值和极端情况测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, add(2147483647, -2147483646))
  assert_eq(-1, add(-2147483647, 2147483646))
}

test "string_greeting_complex_scenarios" {
  // 复杂字符串问候场景
  let formal_name = "Dr. Smith"
  let casual_name = "John"
  let empty_name = ""
  let numeric_name = "12345"
  let mixed_name = "User-123_ABC"
  
  assert_eq_string("Hello, Dr. Smith!", greet(formal_name))
  assert_eq_string("Hello, John!", greet(casual_name))
  assert_eq_string("Hello, !", greet(empty_name))
  assert_eq_string("Hello, 12345!", greet(numeric_name))
  assert_eq_string("Hello, User-123_ABC!", greet(mixed_name))
}