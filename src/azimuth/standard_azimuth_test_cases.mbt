// 标准 MoonBit 测试用例 - azimuth 库
// 包含核心功能的全面测试，使用标准 MoonBit 测试语法

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(-2, 7))
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_standard_cases" {
  // 测试向上取整除法的标准情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(3, 4), multiply(5, 2))
  assert_eq(22, result1)  // 3*4 + 5*2 = 12 + 10 = 22
  
  let result2 = multiply(add(7, 3), 2)
  assert_eq(20, result2)  // (7 + 3) * 2 = 10 * 2 = 20
}

test "practical_business_scenario" {
  // 实际业务场景：计算订单总价
  let unit_price = 45
  let quantity = 6
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(270, subtotal)
  assert_eq(285, total)
}

test "packaging_calculation" {
  // 包装计算场景
  let total_items = 28
  let items_per_box = 7
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(28/7) = 4
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-8, 12))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}