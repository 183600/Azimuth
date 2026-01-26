// 新的标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的全面测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, add(8, 7))
  assert_eq(-2, add(5, -7))
  assert_eq(24, multiply(6, 4))
  assert_eq(-18, multiply(-3, 6))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(11, 3))   // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(2, 2))    // 2/2 = 1 -> 1
  assert_eq(0, divide_with_ceil(0, 5))    // 0/5 = 0 -> 0
  assert_eq(0, divide_with_ceil(10, 0))   // 除数为零返回0
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：订单总价计算
  let unit_price = 35
  let quantity = 6
  let tax_rate = 10
  let shipping = 20
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(210, subtotal)
  assert_eq(21, tax)
  assert_eq(251, total)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "large_number_operations" {
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(999999, add(500000, 499999))
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(100001, divide_with_ceil(200002, 2))
}

test "practical_packaging_calculation" {
  // 实际包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "combined_operations_test" {
  // 组合运算测试
  let result1 = add(multiply(3, 4), multiply(5, 6))
  assert_eq(42, result1)  // 3*4 + 5*6 = 12 + 30 = 42
  
  let result2 = multiply(add(7, 8), 2)
  assert_eq(30, result2)  // (7 + 8) * 2 = 15 * 2 = 30
  
  let result3 = divide_with_ceil(add(multiply(10, 3), 5), 7)
  assert_eq(5, result3)  // ceil((10*3 + 5)/7) = ceil(35/7) = 5
}

test "string_and_number_integration" {
  // 字符串和数字集成测试
  let age = 25
  let name = "Alice"
  let greeting = greet(name)
  
  assert_eq_string("Hello, Alice!", greeting)
  
  // 测试数字字符串
  assert_eq_string("Hello, 2024!", greet("2024"))
  
  // 测试包含数字的运算
  let birth_year = 1999
  let current_year = 2024
  let calculated_age = add(current_year, -birth_year)
  assert_eq(25, calculated_age)
}