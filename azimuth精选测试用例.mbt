// Azimuth 项目精选测试用例
// 使用标准 MoonBit 测试语法

test "add_function_various_cases" {
  // 测试 add 函数的各种情况
  assert_eq(10, add(7, 3))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(100, -100))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "multiply_function_zero_cases" {
  // 测试 multiply 函数的零值情况
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, -100))
  assert_eq(0, multiply(-100, 0))
}

test "multiply_function_negative_cases" {
  // 测试 multiply 函数的负数情况
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(100, multiply(-10, -10))
  assert_eq(-50, multiply(10, -5))
}

test "greet_function_emoji_cases" {
  // 测试 greet 函数的 emoji 情况
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 💻!", greet("💻"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 🎉!", greet("🎉"))
}

test "complex_nested_operations" {
  // 测试复杂嵌套操作
  let a = 5
  let b = 3
  let c = 2
  
  // 计算 (a + b) * (c + 1)
  let sum1 = add(a, b)
  let sum2 = add(c, 1)
  let result = multiply(sum1, sum2)
  assert_eq(40, result)
  
  // 计算 a * b + c * (a + b)
  let product1 = multiply(a, b)
  let sum3 = add(a, b)
  let product2 = multiply(c, sum3)
  let result2 = add(product1, product2)
  assert_eq(31, result2)
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  
  // 测试加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 测试乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 测试乘法零元
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \", greet("\\"))
  assert_eq_string("Hello, \"!", greet("\""))
  assert_eq_string("Hello, \'!", greet("'"))
}

test "calculation_precision" {
  // 测试计算精度
  let large_num = 1000000
  let small_num = 1
  
  // 测试大数计算
  let sum = add(large_num, small_num)
  assert_eq(1000001, sum)
  
  // 测试大数乘法
  let product = multiply(large_num, small_num)
  assert_eq(1000000, product)
  
  // 测试连续计算
  let result = add(multiply(100, 100), multiply(200, 50))
  assert_eq(20000, result)
}

test "real_world_shopping_cart" {
  // 测试真实世界购物车场景
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount = 100
  
  // 计算每种商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let subtotal = add(total1, total2)
  let final_total = add(subtotal, -discount)
  
  // 验证最终价格：(299*2 + 199*3) - 100 = (598 + 597) - 100 = 1195 - 100 = 1095
  assert_eq(1095, final_total)
}

test "temperature_conversion" {
  // 测试温度转换场景
  let celsius = 25
  let fahrenheit = 77
  
  // 模拟摄氏度转华氏度：F = C * 9/5 + 32
  let c_to_f_result = add(multiply(celsius, 9), 160) / 5
  assert_eq(fahrenheit, c_to_f_result)
  
  // 模拟华氏度转摄氏度：C = (F - 32) * 5/9
  let f_to_c_result = multiply(add(fahrenheit, -32), 5) / 9
  assert_eq(celsius, f_to_c_result)
}