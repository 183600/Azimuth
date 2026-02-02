// Azimuth 综合测试用例
// 包含 10 个高质量的 MoonBit 测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(42, multiply(6, 7))
  assert_eq(3, subtract(10, 7))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "boundary_conditions_test" {
  // 边界条件测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-5, subtract(0, 5))
  assert_eq(5, subtract(5, 0))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(2, add(-5, 7))
  assert_eq(-12, add(-5, -7))
  assert_eq(21, multiply(-3, -7))
  assert_eq(-21, multiply(3, -7))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-15, subtract(5, 20))
  assert_eq(25, subtract(-5, -30))
}

test "complex_combined_operations" {
  // 组合运算测试
  let result1 = add(multiply(3, 4), multiply(5, 6))
  assert_eq(42, result1)  // 3*4 + 5*6 = 12 + 30 = 42
  
  let result2 = multiply(add(7, 8), subtract(15, 5))
  assert_eq(150, result2)  // (7+8) * (15-5) = 15 * 10 = 150
  
  let result3 = divide_with_ceil(add(multiply(4, 7), multiply(3, 5)), 2)
  assert_eq(22, result3)  // ceil((4*7 + 3*5) / 2) = ceil((28 + 15) / 2) = ceil(43/2) = 22
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 计算购物车总价
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 18
  let item2_qty = 4
  let shipping = 12
  let discount = 15
  
  let subtotal = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  let total = add(subtotal, shipping)
  let final_total = add(total, -discount)
  
  assert_eq(147, subtotal)  // 25*3 + 18*4 = 75 + 72 = 147
  assert_eq(159, total)    // 147 + 12 = 159
  assert_eq(144, final_total)  // 159 - 15 = 144
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
}

test "string_greeting_comprehensive" {
  // 字符串问候综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "large_number_operations" {
  // 大数运算测试
  let big_num1 = 1000000
  let big_num2 = 500000
  
  assert_eq(1500000, add(big_num1, big_num2))
  assert_eq(500000, subtract(big_num1, big_num2))
  assert_eq(500000000000, multiply(big_num1, big_num2))
  assert_eq(2, divide_with_ceil(big_num1, big_num2))
  assert_eq(3, divide_with_ceil(big_num1, 400000))
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 12
  let b = 18
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
  
  // 分配律测试
  let c = 5
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
}

test "business_logic_complex" {
  // 复杂业务逻辑测试
  // 模拟订单处理流程
  let product_price = 89
  let quantity = 6
  let tax_rate = 8  // 8%
  let shipping_per_item = 3
  let discount_threshold = 500
  let discount_amount = 50
  
  // 计算商品总价
  let subtotal = multiply(product_price, quantity)
  
  // 计算运费
  let total_shipping = multiply(shipping_per_item, quantity)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, total_shipping), tax_amount)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(534, subtotal)
  assert_eq(18, total_shipping)
  assert_eq(43, tax_amount)  // ceil(534 * 8 / 100) = ceil(42.72) = 43
  assert_eq(595, pre_discount_total)
  assert_eq(545, final_total)
}