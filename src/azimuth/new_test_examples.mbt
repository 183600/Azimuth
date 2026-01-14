// 新的 MoonBit 测试用例示例
// 测试 azimuth 包的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-6, multiply(2, -3))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "overflow_handling" {
  // 测试溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
  
  // 测试乘法溢出
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 与0和1的运算
  assert_eq(a, add(a, 0))
  assert_eq(0, multiply(a, 0))
  assert_eq(a, multiply(a, 1))
}

test "edge_case_numbers" {
  // 测试边界数字
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, multiply(-1, -1))
}

test "complex_calculation" {
  // 测试复杂计算
  // 计算 (a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = add(a, b)  // 8
  let product = multiply(sum_ab, c)  // 32
  let result = add(product, -d)  // 22
  
  assert_eq(22, result)
}

test "real_world_scenario" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 2
  let shipping = 20
  let discount = 50
  
  // 计算总价
  let subtotal = multiply(item_price, quantity)  // 398
  let total = add(add(subtotal, shipping), -discount)  // 398 + 20 - 50 = 368
  
  assert_eq(368, total)
}

test "boolean_assertions" {
  // 测试布尔断言
  assert_true(5 > 3)
  assert_true(add(2, 3) == 5)
  assert_false(5 < 3)
  assert_false(add(2, 2) == 5)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  let result1 = greet(" ")
  assert_eq_string("Hello,  !", result1)
  
  let result2 = greet("\t")
  assert_eq_string("Hello, \t!", result2)
  
  let result3 = greet("A")
  assert_eq_string("Hello, A!", result3)
}

test "calculation_sequence" {
  // 测试计算序列
  // 计算 1 + 2 + 3 + 4 + 5
  let sum = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum)
  
  // 计算 2 * 3 * 4
  let product = multiply(multiply(2, 3), 4)
  assert_eq(24, product)
}