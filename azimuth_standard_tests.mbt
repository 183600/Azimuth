// Azimuth 项目标准 MoonBit 测试用例
// 这些测试用例补充了现有的测试覆盖，专注于边界情况和实际应用场景

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(5, divide_with_ceil(21, 5))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "divide_with_ceil_zero_dividend" {
  // 测试被除数为0的情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "complex_arithmetic_sequence" {
  // 测试复杂数学序列：斐波那契数列前几项的计算
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  let f0 = 0
  let f1 = 1
  let f2 = add(f0, f1)  // 1
  let f3 = add(f1, f2)  // 2
  let f4 = add(f2, f3)  // 3
  let f5 = add(f3, f4)  // 5
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
}

test "geometric_progression" {
  // 测试等比数列：a, ar, ar², ar³, ...
  let a = 3
  let r = 2
  
  let term1 = a
  let term2 = multiply(a, r)
  let term3 = multiply(term2, r)
  let term4 = multiply(term3, r)
  let term5 = multiply(term4, r)
  
  assert_eq(3, term1)
  assert_eq(6, term2)
  assert_eq(12, term3)
  assert_eq(24, term4)
  assert_eq(48, term5)
}

test "string_with_various_lengths" {
  // 测试不同长度字符串的处理
  let empty = ""
  let single_char = "A"
  let short = "Hi"
  let word = "MoonBit"
  let sentence = "Hello MoonBit World"
  let paragraph = "This is a longer paragraph that contains multiple sentences. It tests how the greet function handles longer text inputs."
  
  assert_eq_string("Hello, !", greet(empty))
  assert_eq_string("Hello, A!", greet(single_char))
  assert_eq_string("Hello, Hi!", greet(short))
  assert_eq_string("Hello, MoonBit!", greet(word))
  assert_eq_string("Hello, Hello MoonBit World!", greet(sentence))
  assert_eq_string("Hello, " + paragraph + "!", greet(paragraph))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 7
  let y = 13
  
  // 加法恒等式
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(x, 0), x)
  
  // 乘法恒等式
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(multiply(x, 1), x)
  assert_eq(multiply(x, 0), 0)
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "extreme_boundary_combinations" {
  // 测试极值边界组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值与普通值的组合运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 极值之间的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 极值与接近极值的运算
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
}

test "real_world_calculations" {
  // 测试实际应用场景的计算
  // 场景1：计算购物车总价（含折扣和税费）
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 3
  let discount = 50
  let tax_rate = 8  // 8%
  
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let cart_total = add(subtotal1, subtotal2)
  let discounted_total = add(cart_total, -discount)
  let tax_amount = multiply(discounted_total, tax_rate) / 100
  let final_total = add(discounted_total, tax_amount)
  
  assert_eq(1214, final_total)
}

test "assert_functions_comprehensive" {
  // 全面测试断言函数
  // 测试 assert_true 和 assert_false
  assert_true(5 > 3)
  assert_true(add(2, 3) == 5)
  assert_true(multiply(4, 5) == 20)
  
  assert_false(5 < 3)
  assert_false(add(2, 3) == 6)
  assert_false(multiply(4, 5) == 19)
  
  // 测试 assert_eq
  assert_eq(10, add(4, 6))
  assert_eq(20, multiply(4, 5))
  assert_eq(0, multiply(5, 0))
  
  // 测试 assert_eq_string
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}