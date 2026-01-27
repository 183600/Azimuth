// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_operations" {
  // 测试加法的基本运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(7, 3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_basic_operations" {
  // 测试乘法的基本运算
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(5, divide_with_ceil(20, 4))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
  
  let result3 = divide_with_ceil(add(10, 5), 4)
  assert_eq(4, result3)  // ceil(15/4) = 4
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "practical_business_calculations" {
  // 测试实际业务计算
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_and_distribution_calculations" {
  // 测试包装和分销计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let total_weight = multiply(total_items, 2)  // 假设每个物品重2单位
  let weight_per_box = multiply(items_per_box, 2)
  let boxes_for_weight = divide_with_ceil(total_weight, weight_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  assert_eq(94, total_weight)  // 47 * 2 = 94
  assert_eq(4, boxes_for_weight)  // ceil(94/24) = 4
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, 123+456!", greet("123+456"))
}