// Azimuth 项目新的标准测试用例
// 使用标准 MoonBit 测试语法编写
// 包含 10 个测试用例，覆盖核心功能和边界情况

test "addition_boundary_values" {
  // 测试加法边界值
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(-1000000, add(-500000, -500000))
}

test "multiplication_identity_elements" {
  // 测试乘法单位元
  assert_eq(7, multiply(7, 1))
  assert_eq(-10, multiply(-10, 1))
  assert_eq(0, multiply(0, 1))
  assert_eq(0, multiply(0, -5))
  assert_eq(15, multiply(15, 1))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, divide_with_ceil(8, 3))  // 2.666... -> 3
  assert_eq(7, divide_with_ceil(13, 2)) // 6.5 -> 7
  assert_eq(1, divide_with_ceil(1, 1))  // 1 -> 1
  assert_eq(0, divide_with_ceil(0, 5))  // 0 -> 0
}

test "greet_function_special_cases" {
  // 测试问候函数的特殊情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "negative_number_division" {
  // 测试负数除法
  assert_eq(-2, divide_with_ceil(-4, 2))   // -4/2 = -2 -> -2
  assert_eq(-1, divide_with_ceil(-3, 2))   // -3/2 = -1.5 -> -1
  assert_eq(-3, divide_with_ceil(-7, 2))   // -7/2 = -3.5 -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // -10/-3 = 3.666... -> 4
  assert_eq(0, divide_with_ceil(-5, 0))    // 除数为零
}

test "combined_arithmetic_expressions" {
  // 测试组合算术表达式
  let x = 10
  let y = 15
  let z = 20
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(500, result1)  // (10 + 15) * 20 = 25 * 20 = 500
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(170, result2)  // 10 * 15 + 20 = 150 + 20 = 170
}

test "real_world_business_calculation" {
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

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a * (b + c) = a * b + a * c
  let c = 5
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "packaging_and_shipping_calculation" {
  // 测试包装和运输计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let shipping_cost_per_box = 5
  let total_shipping = multiply(boxes_needed, shipping_cost_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  assert_eq(20, total_shipping)  // 4 * 5 = 20
}

test "unicode_and_multilingual_support" {
  // 测试 Unicode 和多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}