// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，包含多样化测试场景

test "string_concatenation" {
  // 测试字符串连接功能
  let str1 = "Hello"
  let str2 = " "
  let str3 = "MoonBit"
  let result = str1 + str2 + str3
  assert_eq_string("Hello MoonBit", result)
}

test "arithmetics_addition" {
  // 测试加法运算
  let result1 = add(10, 20)
  let result2 = add(-5, 15)
  let result3 = add(0, 42)
  assert_eq(30, result1)
  assert_eq(10, result2)
  assert_eq(42, result3)
}

test "arithmetics_multiplication" {
  // 测试乘法运算
  let result1 = multiply(6, 7)
  let result2 = multiply(-4, 5)
  let result3 = multiply(0, 100)
  assert_eq(42, result1)
  assert_eq(-20, result2)
  assert_eq(0, result3)
}

test "complex_expressions" {
  // 测试复杂表达式
  let a = 5
  let b = 3
  let c = 2
  
  // 测试 (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(16, result1)
  
  // 测试 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)
}

test "greet_function_variations" {
  // 测试问候函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_series" {
  // 测试数学级数计算
  // 计算 1 + 2 + 3 + 4 + 5
  let sum1 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum1)
  
  // 计算 2 * 3 * 4
  let product1 = multiply(multiply(2, 3), 4)
  assert_eq(24, product1)
}

test "boundary_conditions" {
  // 测试边界条件
  // 测试大数相加
  let large1 = 1000000
  let large2 = 2000000
  let result1 = add(large1, large2)
  assert_eq(3000000, result1)
  
  // 测试负数边界
  let negative1 = -1000
  let negative2 = -2000
  let result2 = add(negative1, negative2)
  assert_eq(-3000, result2)
}

test "real_world_scenarios" {
  // 测试真实世界场景
  // 计算购物总价：单价 * 数量 + 运费
  let price = 50
  let quantity = 3
  let shipping = 10
  let total = add(multiply(price, quantity), shipping)
  assert_eq(160, total)
  
  // 计算折扣价：原价 - 折扣金额
  let original_price = 100
  let discount_rate = 20  // 20% 折扣
  let discount_amount = multiply(original_price, discount_rate) / 100
  let discounted_price = add(original_price, -discount_amount)
  assert_eq(80, discounted_price)
}