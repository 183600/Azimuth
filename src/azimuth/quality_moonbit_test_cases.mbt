// 高质量 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含基本功能测试、边界条件测试和实际应用场景测试

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-3, 4))
  assert_eq(100, add(50, 50))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(20, multiply(-4, -5))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 用户!", greet("用户"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

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