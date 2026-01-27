// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的简洁测试

test "add_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_by_zero" {
  // 测试乘以零的性质
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(-10, 0))
  assert_eq(0, multiply(0, -10))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, @user!", greet("@user"))
  assert_eq_string("Hello, test-123!", greet("test-123"))
  assert_eq_string("Hello, C++!", greet("C++"))
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(4, divide_with_ceil(7, 2))  // 3.5 -> 4
  assert_eq(3, divide_with_ceil(8, 3))  // 2.666... -> 3
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：购物车总价
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  
  let subtotal = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(105, subtotal)  // 25*3 + 15*2 = 75 + 30 = 105
}

test "negative_division_ceil" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-4, 2))   // -4/2 = -2 -> -2
  assert_eq(-1, divide_with_ceil(-3, 2))   // -3/2 = -1.5 -> -1
  assert_eq(3, divide_with_ceil(-9, -3))   // -9/-3 = 3 -> 3
}

test "large_number_calculations" {
  // 测试大数计算
  assert_eq(100000, add(50000, 50000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, divide_with_ceil(100000, 100000))
}

test "distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "packaging_efficiency" {
  // 测试包装效率计算
  let total_products = 47
  let products_per_box = 12
  let boxes_needed = divide_with_ceil(total_products, products_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, products_per_box)
  let unused_space = add(total_capacity, -total_products)
  assert_eq(1, unused_space)  // 4*12 - 47 = 48 - 47 = 1
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello,   !", greet("   "))  // 空格字符串
}