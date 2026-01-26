// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能添加10个标准测试用例

test "addition_commutative_law" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiplication_commutative_law" {
  // 测试乘法交换律
  assert_eq(42, multiply(6, 7))
  assert_eq(42, multiply(7, 6))
  assert_eq(-24, multiply(-3, 8))
  assert_eq(-24, multiply(8, -3))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(3, divide_with_ceil(8, 3))  // 8/3 = 2.666... -> 3
  assert_eq(2, divide_with_ceil(6, 3))  // 6/3 = 2 -> 2
  assert_eq(1, divide_with_ceil(3, 4))  // 3/4 = 0.75 -> 1
  assert_eq(0, divide_with_ceil(5, 0))  // 除数为0返回0
}

test "greet_function_cases" {
  // 测试问候函数各种情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, User123!", greet("User123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "complex_combined_operations" {
  // 测试复杂组合运算
  let x = 12
  let y = 8
  let z = 5
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(100, result1)  // (12 + 8) * 5 = 20 * 5 = 100
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(101, result2)  // 12 * 8 + 5 = 96 + 5 = 101
}

test "negative_division_ceil" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-8, 4))  // -8/4 = -2 -> -2
  assert_eq(-1, divide_with_ceil(-3, 4))  // -3/4 = -0.75 -> -1
  assert_eq(2, divide_with_ceil(-8, -4))  // -8/-4 = 2 -> 2
  assert_eq(3, divide_with_ceil(-10, -3)) // -10/-3 = 3.333... -> 3
}

test "mathematical_identities" {
  // 测试数学单位元
  assert_eq(7, add(7, 0))    // 加法单位元
  assert_eq(7, multiply(7, 1))  // 乘法单位元
  assert_eq(0, multiply(7, 0))  // 乘法零元
  assert_eq(7, add(0, 7))    // 加法单位元（交换）
}

test "business_order_calculation" {
  // 业务订单计算
  let unit_price = 45
  let quantity = 6
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(285, total_before_discount)
  assert_eq(265, final_total)
}

test "large_number_operations" {
  // 大数运算测试
  let large_num1 = 100000
  let large_num2 = 200000
  
  assert_eq(300000, add(large_num1, large_num2))
  assert_eq(20000000000, multiply(large_num1, large_num2))
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
}

test "special_character_greetings" {
  // 特殊字符问候测试
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, C++!", greet("C++"))
  assert_eq_string("Hello, 100%!", greet("100%"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}