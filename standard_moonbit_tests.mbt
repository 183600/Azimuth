// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "standard_add_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "standard_multiply_zero_property" {
  // 测试乘法零元素性质
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(-10, 0))
}

test "standard_divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(4, divide_with_ceil(10, 3)) // 3.333... -> 4
  assert_eq(1, divide_with_ceil(1, 1))  // 1 -> 1
  assert_eq(0, divide_with_ceil(0, 5))  // 0 -> 0
}

test "standard_greet_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
}

test "standard_combined_arithmetic_operations" {
  // 测试组合算术运算
  let x = 8
  let y = 12
  let z = 5
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(100, result1)  // (8 + 12) * 5 = 20 * 5 = 100
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(101, result2)  // 8 * 12 + 5 = 96 + 5 = 101
}

test "standard_negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "standard_large_number_calculations" {
  // 测试大数计算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  assert_eq(33334, divide_with_ceil(1000000, 30))
}

test "standard_edge_case_division" {
  // 测试除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))     // 相等数
  assert_eq(1, divide_with_ceil(1, 100))   // 被除数小于除数
  assert_eq(100, divide_with_ceil(100, 1)) // 除数为1
  assert_eq(0, divide_with_ceil(10, 0))    // 除数为0
}

test "standard_string_greeting_edge_cases" {
  // 测试字符串问候边界情况
  assert_eq_string("Hello, !", greet(""))           // 空字符串
  assert_eq_string("Hello, A!", greet("A"))         // 单字符
  assert_eq_string("Hello, 123!", greet("123"))     // 数字字符串
  assert_eq_string("Hello, a b c!", greet("a b c")) // 包含空格
}

test "standard_real_world_calculation_scenario" {
  // 测试实际世界计算场景：订单总价计算
  let unit_price = 45
  let quantity = 8
  let shipping_cost = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(360, subtotal)           // 45 * 8 = 360
  assert_eq(375, total_before_discount) // 360 + 15 = 375
  assert_eq(355, final_total)        // 375 - 20 = 355
}