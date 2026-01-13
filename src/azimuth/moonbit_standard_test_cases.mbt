// 标准 MoonBit 测试用例
// 测试基本数学运算和字符串处理功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
}

test "add_overflow_handling" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(0, 123))
}

test "multiply_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation" {
  // 测试复杂计算场景
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c
  let sum = add(a, b)
  let result = multiply(sum, c)
  
  assert_eq(900, result)
}

test "string_processing" {
  // 测试字符串处理
  let name1 = "Alice"
  let name2 = "Bob"
  
  assert_eq_string("Hello, Alice!", greet(name1))
  assert_eq_string("Hello, Bob!", greet(name2))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, test@123!", greet("test@123"))
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法交换律
  let x = 15
  let y = 25
  
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "edge_cases" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值和最小值的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 自反性
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "real_world_scenario" {
  // 测试真实世界场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let discount = 50
  
  // 计算商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let subtotal = add(total1, total2)
  
  // 应用折扣
  let final_total = add(subtotal, -discount)
  
  // 验证最终价格：(199 * 2) + (299 * 1) - 50 = 398 + 299 - 50 = 647
  assert_eq(647, final_total)
}