// 额外的综合测试用例

///|
test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(add(1, 2), 3)
  assert_eq(add(-1, 1), 0)
  assert_eq(add(-5, -3), -8)
}

///|
test "add_zero_handling" {
  // 测试零的处理
  assert_eq(add(0, 5), 5)
  assert_eq(add(5, 0), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(0, -7), -7)
}

///|
test "add_overflow_protection" {
  // 测试正数溢出保护
  assert_eq(add(2147483647, 1), 2147483647)
  assert_eq(add(1000000000, 2000000000), 2147483647)
  
  // 测试负数溢出保护
  assert_eq(add(-2147483648, -1), -2147483648)
  assert_eq(add(-1000000000, -2000000000), -2147483648)
}

///|
test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(multiply(2, 3), 6)
  assert_eq(multiply(-2, 3), -6)
  assert_eq(multiply(2, -3), -6)
  assert_eq(multiply(-2, -3), 6)
}

///|
test "multiply_special_cases" {
  // 测试特殊情况
  assert_eq(multiply(0, 5), 0)
  assert_eq(multiply(5, 0), 0)
  assert_eq(multiply(1, 7), 7)
  assert_eq(multiply(7, 1), 7)
  assert_eq(multiply(-1, 7), -7)
  assert_eq(multiply(7, -1), -7)
}

///|
test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  assert_eq(multiply(2147483647, 2), 2147483647)
  assert_eq(multiply(-2147483648, 2), -2147483648)
  assert_eq(multiply(46341, 46341), 2147483647)
}

///|
test "greet_functionality" {
  // 测试问候函数
  assert_eq(greet("World"), "Hello, World!")
  assert_eq(greet(""), "Hello, !")
  assert_eq(greet("MoonBit"), "Hello, MoonBit!")
}

///|
test "mathematical_properties" {
  // 测试数学性质
  
  // 加法结合律
  let a = 5
  let b = 3
  let c = 2
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

///|
test "practical_scenarios" {
  // 测试实际应用场景
  
  // 计算购物车总价
  let item1_price = 99
  let item1_quantity = 2
  let item2_price = 149
  let item2_quantity = 1
  
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let grand_total = add(total1, total2)
  
  assert_eq(grand_total, 347)
  
  // 温度转换（摄氏度到华氏度：F = C * 9/5 + 32）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160)  // 乘以9/5再+32，使用整数运算
  assert_eq(fahrenheit / 5, 77)  // 25°C = 77°F
}