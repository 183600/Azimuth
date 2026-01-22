// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的10个标准测试

test "add_function_standard" {
  // 标准加法功能测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-7, 5))
  assert_eq(1000, add(600, 400))
}

test "multiply_function_standard" {
  // 标准乘法功能测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-21, multiply(-7, 3))
  assert_eq(10000, multiply(100, 100))
}

test "greet_function_standard" {
  // 标准问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, User!", greet("User"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_standard" {
  // 标准向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(5, 8))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-7, 11))
  assert_eq(21, multiply(-7, -3))
  assert_eq(-21, multiply(7, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "mathematical_properties" {
  // 数学性质测试
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

test "combined_calculations" {
  // 组合计算测试
  let result1 = add(multiply(2, 4), multiply(3, 5))
  assert_eq(23, result1)  // 2*4 + 3*5 = 8 + 15 = 23
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
}

test "business_calculation_scenario" {
  // 业务计算场景测试
  let unit_price = 25
  let quantity = 4
  let tax_rate = 8
  let shipping = 10
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(100, subtotal)
  assert_eq(8, tax)
  assert_eq(118, total)
}

test "edge_case_handling" {
  // 边界情况处理测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(50, divide_with_ceil(100, 2))
  assert_eq(34, divide_with_ceil(100, 3))
}

test "unicode_and_special_characters" {
  // Unicode 和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}