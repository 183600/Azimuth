// 新的实用 MoonBit 测试用例
// 包含基本功能、边界条件和实际应用场景的测试

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-3, add(-8, 5))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(12, multiply(3, 4))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(0, multiply(0, 100))
}

test "greet_functionality" {
  // 测试问候函数功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
}

test "add_overflow_handling" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(-1500000000, -1000000000))
}

test "multiply_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘溢出
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 测试最小值特殊处理
  assert_eq(min_val, multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算表达式：(a + b) * c - d
  
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum_ab = add(a, b)  // 30
  let product = multiply(sum_ab, c)  // 90
  let result = add(product, -d)  // 75
  
  assert_eq(75, result)
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  // 计算商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let subtotal = add(total1, total2)
  
  // 计算最终总价
  let final_total = add(subtotal, shipping)
  
  assert_eq(717, final_total)  // 199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
}

test "temperature_conversion" {
  // 测试温度转换（摄氏度转华氏度简化版）
  // 公式：F = C * 2 + 30
  
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  
  // 测试长字符串
  let long_name = "VeryLongNameWithManyCharacters"
  assert_eq_string("Hello, VeryLongNameWithManyCharacters!", greet(long_name))
  
  // 测试特殊字符
  let special_name = "test@example.com"
  assert_eq_string("Hello, test@example.com!", greet(special_name))
  
  // 测试包含空格的名字
  let spaced_name = "John Doe"
  assert_eq_string("Hello, John Doe!", greet(spaced_name))
}

test "mathematical_properties" {
  // 测试数学性质
  
  // 加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  let c = 8
  let d = 12
  assert_eq(multiply(c, d), multiply(d, c))
  
  // 分配律简化测试
  let e = 5
  let f = 3
  let g = 2
  assert_eq(multiply(e, add(f, g)), add(multiply(e, f), multiply(e, g)))
}