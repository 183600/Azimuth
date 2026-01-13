// 新的综合 MoonBit 测试用例
// 测试 azimuth 包的核心功能

// 基本运算测试
test "add_basic_functionality" {
  azimuth::assert_eq(10, azimuth::add(6, 4))
  azimuth::assert_eq(0, azimuth::add(5, -5))
  azimuth::assert_eq(-8, azimuth::add(-3, -5))
}

test "multiply_basic_functionality" {
  azimuth::assert_eq(12, azimuth::multiply(3, 4))
  azimuth::assert_eq(-15, azimuth::multiply(3, -5))
  azimuth::assert_eq(0, azimuth::multiply(7, 0))
}

test "greet_functionality" {
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, 用户123!", azimuth::greet("用户123"))
}

// 边界值测试
test "boundary_value_tests" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  azimuth::assert_eq(max_val, azimuth::add(max_val, 1))
  azimuth::assert_eq(max_val, azimuth::multiply(max_val, 2))
  
  // 测试最小值边界
  azimuth::assert_eq(min_val, azimuth::add(min_val, -1))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, -1))
}

// 复合运算测试
test "complex_calculations" {
  // 计算 (2 + 3) * 4 - 5
  let sum = azimuth::add(2, 3)
  let product = azimuth::multiply(sum, 4)
  let result = azimuth::add(product, -5)
  azimuth::assert_eq(15, result)
  
  // 计算购物车总价：商品1(100*2) + 商品2(50*3)
  let item1_total = azimuth::multiply(100, 2)
  let item2_total = azimuth::multiply(50, 3)
  let cart_total = azimuth::add(item1_total, item2_total)
  azimuth::assert_eq(350, cart_total)
}

// 字符串处理测试
test "string_processing_tests" {
  // 测试不同类型的字符串
  azimuth::assert_eq_string("Hello, John_Doe!", azimuth::greet("John_Doe"))
  azimuth::assert_eq_string("Hello, test@example.com!", azimuth::greet("test@example.com"))
  azimuth::assert_eq_string("Hello, 12345!", azimuth::greet("12345"))
  
  // 测试包含空格的字符串
  azimuth::assert_eq_string("Hello, First Last!", azimuth::greet("First Last"))
  azimuth::assert_eq_string("Hello,  spaced  !", azimuth::greet("  spaced  "))
}

// 数学运算特性测试
test "mathematical_properties" {
  // 测试加法交换律
  let a = 15
  let b = 25
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 测试乘法交换律
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 测试乘法分配律
  let c = 5
  let left = azimuth::multiply(a, azimuth::add(b, c))
  let right = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  azimuth::assert_eq(left, right)
}

// 实际应用场景测试
test "real_world_scenarios" {
  // 温度转换：摄氏度转华氏度（简化版 F = C * 2 + 30）
  let celsius = 20
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  azimuth::assert_eq(70, fahrenheit)
  
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  let area = azimuth::multiply(length, width)
  let perimeter = azimuth::add(azimuth::multiply(length, 2), azimuth::multiply(width, 2))
  azimuth::assert_eq(40, area)
  azimuth::assert_eq(26, perimeter)
}

// 特殊情况测试
test "special_cases" {
  // 测试连续运算
  let result1 = azimuth::add(azimuth::add(1, 2), azimuth::add(3, 4))
  azimuth::assert_eq(10, result1)
  
  let result2 = azimuth::multiply(azimuth::multiply(2, 3), azimuth::multiply(4, 5))
  azimuth::assert_eq(120, result2)
  
  // 测试混合符号运算
  let positive = 100
  let negative = -30
  azimuth::assert_eq(70, azimuth::add(positive, negative))
  azimuth::assert_eq(-3000, azimuth::multiply(positive, negative))
}

// 错误处理和边界情况测试
test "error_handling_and_edge_cases" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  azimuth::assert_eq(3000000, azimuth::add(large_num1, large_num2))
  azimuth::assert_eq(2000000000000, azimuth::multiply(large_num1, large_num2))
  
  // 测试极值运算
  let max_val = 2147483647
  let min_val = -2147483648
  azimuth::assert_eq(-1, azimuth::add(max_val, min_val))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, 1))
}