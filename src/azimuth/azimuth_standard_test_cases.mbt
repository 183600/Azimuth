// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含 10 个测试用例，覆盖所有主要功能

test "add_function_basic_operations" {
  // 测试基本加法运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_basic_operations" {
  // 测试基本乘法运算
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-4, multiply(-2, 2))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_standard_cases" {
  // 测试标准问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户!", greet("用户"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试基本向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))  // 加法交换律
  assert_eq(multiply(a, b), multiply(b, a))  // 乘法交换律
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "combined_operations_complex" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
}

test "greet_function_special_characters" {
  // 测试特殊字符的问候功能
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(5, 10))  // 被除数小于除数
  assert_eq(1, divide_with_ceil(10, 10))  // 被除数等于除数
  assert_eq(3, divide_with_ceil(15, 5))  // 被除数是除数的倍数
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
}

test "practical_business_calculation" {
  // 实际业务计算场景
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(200, subtotal)  // 25 * 8 = 200
  assert_eq(215, total)     // 200 + 15 = 215
}