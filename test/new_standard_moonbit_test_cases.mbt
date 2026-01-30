// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(7, 8))
  assert_eq(15, add(8, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_zero_property" {
  // 测试乘法零性质
  assert_eq(0, multiply(0, 10))
  assert_eq(0, multiply(10, 0))
  assert_eq(0, multiply(0, -5))
  assert_eq(0, multiply(-7, 0))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_with_special_characters" {
  // 测试问候函数特殊字符处理
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)
}

test "large_number_calculations" {
  // 测试大数计算
  assert_eq(1000000, add(500000, 500000))
  assert_eq(10000, multiply(100, 100))
  assert_eq(100001, add(100000, 1))
}

test "business_scenario_calculations" {
  // 测试业务场景计算
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(215, total)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 10
  let b = 15
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律 a * (b + c) = a * b + a * c
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和极端值
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
}