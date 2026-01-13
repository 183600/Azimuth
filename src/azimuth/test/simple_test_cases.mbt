// 简单的 MoonBit 测试用例
// 导入 azimuth 包以使用其中的函数
test "add_positive_numbers" {
  // 测试两个正数相加
  assert_eq(10, azimuth::add(3, 7))
  assert_eq(100, azimuth::add(50, 50))
  assert_eq(0, azimuth::add(0, 0))
}

test "add_negative_numbers" {
  // 测试两个负数相加
  assert_eq(-10, azimuth::add(-3, -7))
  assert_eq(-100, azimuth::add(-50, -50))
  assert_eq(0, azimuth::add(0, 0))
}

test "add_mixed_numbers" {
  // 测试正负数相加
  assert_eq(0, azimuth::add(5, -5))
  assert_eq(-2, azimuth::add(3, -5))
  assert_eq(2, azimuth::add(-3, 5))
}

test "multiply_basic" {
  // 测试基本乘法
  assert_eq(6, azimuth::multiply(2, 3))
  assert_eq(0, azimuth::multiply(5, 0))
  assert_eq(5, azimuth::multiply(5, 1))
  assert_eq(-5, azimuth::multiply(5, -1))
}

test "multiply_negative" {
  // 测试负数乘法
  assert_eq(15, azimuth::multiply(-3, -5))
  assert_eq(-15, azimuth::multiply(3, -5))
  assert_eq(-15, azimuth::multiply(-3, 5))
}

test "greet_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth::greet(""))
}

test "greet_special_chars" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  assert_eq_string("Hello, @#$!", azimuth::greet("@#$"))
}

test "complex_operations" {
  // 测试复合运算
  // (2 + 3) * 4 = 20
  let sum = azimuth::add(2, 3)
  let result = azimuth::multiply(sum, 4)
  assert_eq(20, result)
  
  // 5 + (6 * 7) = 47
  let product = azimuth::multiply(6, 7)
  let result2 = azimuth::add(5, product)
  assert_eq(47, result2)
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值溢出
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::multiply(max_val, 2))
  
  // 测试最小值溢出
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
}

test "sequential_operations" {
  // 测试连续运算
  // 1 + 2 + 3 + 4 + 5 = 15
  let result1 = azimuth::add(azimuth::add(1, 2), azimuth::add(3, 4))
  let final_sum = azimuth::add(result1, 5)
  assert_eq(15, final_sum)
  
  // 2 * 3 * 4 = 24
  let result2 = azimuth::multiply(azimuth::multiply(2, 3), 4)
  assert_eq(24, result2)
}

test "string_processing" {
  // 测试字符串处理
  assert_eq_string("Hello, John Doe!", azimuth::greet("John Doe"))
  assert_eq_string("Hello,   spaced   !", azimuth::greet("  spaced  "))
  assert_eq_string("Hello, emoji😊!", azimuth::greet("emoji😊"))
  assert_eq_string("Hello, newline\n!", azimuth::greet("newline\n"))
}

test "mathematical_sequences" {
  // 测试数学序列
  // 等差数列：1 + 3 + 5 + 7 + 9 = 25
  let sum1 = azimuth::add(azimuth::add(1, 3), azimuth::add(5, 7))
  let total1 = azimuth::add(sum1, 9)
  assert_eq(25, total1)
  
  // 等比数列：2 + 4 + 8 = 14
  let sum2 = azimuth::add(azimuth::multiply(1, 2), azimuth::multiply(2, 2))
  let total2 = azimuth::add(sum2, azimuth::multiply(3, 2))
  assert_eq(14, total2)
}

test "real_world_scenarios" {
  // 测试真实世界场景
  // 购物车计算：商品价格 * 数量 + 运费
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = azimuth::multiply(item_price, quantity)
  let total = azimuth::add(subtotal, shipping)
  assert_eq(612, total)  // 199 * 3 + 15 = 597 + 15 = 612
  
  // 温度转换：摄氏度转华氏度（简化版）
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
}