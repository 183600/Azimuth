// 标准 MoonBit 测试用例
// 覆盖核心功能：add, multiply, greet 和断言函数

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 100))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_function_basic" {
  // 测试 greet 函数基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "greet_unicode_handling" {
  // 测试 greet 函数处理 Unicode 字符
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, user_123!", greet("user_123"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景
  let principal = 10000  // 本金
  let rate = 5          // 利率 5%
  let years = 3         // 年数
  
  // 简单利息计算：本金 + (本金 × 利率 × 年数)
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + (10000 × 5 × 3) = 160000
}

test "assertion_functions_test" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试边界条件
  assert_eq(2, add(1, 1))
  assert_eq(6, multiply(2, 3))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq_string("Hello, Test!", greet("Test"))
}

// 新增的高质量测试用例
test "mathematical_operations_combination" {
  // 数学运算组合测试
  let x = 5
  let y = 3
  let z = 2
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(16, result1)
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(17, result2)
}

test "negative_number_calculations" {
  // 负数计算测试
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算
  let apple_price = 5
  let apple_qty = 3
  let banana_price = 3
  let banana_qty = 4
  
  let apple_total = multiply(apple_price, apple_qty)
  let banana_total = multiply(banana_price, banana_qty)
  let cart_total = add(apple_total, banana_total)
  
  assert_eq(27, cart_total)
}

test "packaging_optimization" {
  // 包装优化计算
  let total_items = 23
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)
}

test "string_greeting_with_special_characters" {
  // 特殊字符问候测试
  assert_eq_string("Hello, @user!", greet("@user"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, Café!", greet("Café"))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  let base_price = 100
  let quantity = 5
  let discount = 20
  let tax = 15
  
  let subtotal = multiply(base_price, quantity)
  let after_discount = add(subtotal, -discount)
  let final_total = add(after_discount, tax)
  
  assert_eq(495, final_total)
}