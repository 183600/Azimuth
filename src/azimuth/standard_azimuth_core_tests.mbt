// 标准 Azimuth 核心测试用例
// 8个高质量的 MoonBit 测试用例，覆盖核心功能

test "arithmetic_precision_test" {
  // 算术精度测试
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiplication_edge_cases" {
  // 乘法边界情况测试
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, multiply(-1, -1))
}

test "division_ceiling_behavior" {
  // 向上取整除法行为测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, User!", greet("User"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "subtraction_comprehensive" {
  // 减法综合测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "mathematical_commutativity" {
  // 数学交换律测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证具体值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}

test "real_world_calculation" {
  // 真实世界计算测试
  let price_per_item = 15
  let quantity = 7
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(price_per_item, quantity)
  
  // 应用折扣
  let discount_amount = divide_with_ceil(multiply(subtotal, discount), 100)
  let final_price = subtract(subtotal, discount_amount)
  
  assert_eq(105, subtotal)
  assert_eq(21, discount_amount)
  assert_eq(84, final_price)
}

test "negative_number_division" {
  // 负数除法测试
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-2, divide_with_ceil(10, -5))
  assert_eq(2, divide_with_ceil(-10, -5))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}