// Azimuth 简洁测试套件
// 包含8个标准 MoonBit 测试用例，使用标准测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "negative_number_calculations" {
  // 负数计算测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(8, subtract(3, -5))
  assert_eq(-1, divide_with_ceil(-5, 3))
}

test "zero_value_operations" {
  // 零值运算测试
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "mathematical_commutativity" {
  // 数学交换律测试
  let a = 12
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_calculation_chain" {
  // 复杂计算链测试
  let base = 10
  let result1 = multiply(base, 3)  // 30
  let result2 = add(result1, 5)    // 35
  let result3 = subtract(result2, 8)  // 27
  let result4 = divide_with_ceil(result3, 4)  // 7
  
  assert_eq(30, result1)
  assert_eq(35, result2)
  assert_eq(27, result3)
  assert_eq(7, result4)
}

test "boundary_value_testing" {
  // 边界值测试
  // 测试单位值
  assert_eq(2, add(1, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(0, subtract(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 测试负单位值
  assert_eq(-2, add(-1, -1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(0, subtract(-1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "real_world_scenario" {
  // 真实世界场景测试：购物车计算
  let item_price = 25
  let quantity = 4
  let tax_rate = 10
  
  // 计算商品总价
  let subtotal = multiply(item_price, quantity)  // 100
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)  // 10
  
  // 计算总价
  let total = add(subtotal, tax)  // 110
  
  assert_eq(100, subtotal)
  assert_eq(10, tax)
  assert_eq(110, total)
}