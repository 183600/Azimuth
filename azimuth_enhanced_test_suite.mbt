// Azimuth 项目增强测试套件
// 使用标准 MoonBit 测试语法

// 导入 azimuth 模块
use azimuth

test "enhanced_add_comprehensive" {
  // 全面测试加法函数
  // 基本加法
  assert_eq(10, azimuth.add(7, 3))
  assert_eq(0, azimuth.add(-5, 5))
  assert_eq(-8, azimuth.add(-3, -5))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(min_val, azimuth.add(min_val, -1))
  assert_eq(-1, azimuth.add(max_val, min_val))
  
  // 零值测试
  assert_eq(42, azimuth.add(0, 42))
  assert_eq(-42, azimuth.add(-42, 0))
  assert_eq(0, azimuth.add(0, 0))
}

test "enhanced_multiply_comprehensive" {
  // 全面测试乘法函数
  // 基本乘法
  assert_eq(15, azimuth.multiply(3, 5))
  assert_eq(-20, azimuth.multiply(-4, 5))
  assert_eq(12, azimuth.multiply(-3, -4))
  
  // 特殊值测试
  assert_eq(0, azimuth.multiply(7, 0))
  assert_eq(7, azimuth.multiply(7, 1))
  assert_eq(-7, azimuth.multiply(7, -1))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, azimuth.multiply(max_val, 1))
  assert_eq(min_val, azimuth.multiply(min_val, 1))
  assert_eq(min_val, azimuth.multiply(min_val, -1))
}

test "enhanced_greet_comprehensive" {
  // 全面测试问候函数
  // 基本问候
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, Alice!", azimuth.greet("Alice"))
  
  // 特殊字符测试
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
  assert_eq_string("Hello, @#$!", azimuth.greet("@#$"))
  
  // Unicode 测试
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
  assert_eq_string("Hello, 🌍!", azimuth.greet("🌍"))
}

test "enhanced_complex_calculations" {
  // 复杂计算测试
  // 计算矩形面积和周长
  let length = 8
  let width = 6
  let area = azimuth.multiply(length, width)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  
  assert_eq(48, area)
  assert_eq(28, perimeter)
  
  // 计算平均值
  let scores = azimuth.add(azimuth.add(85, 90), 95)
  let average = scores / 3
  assert_eq(90, average)
}

test "enhanced_financial_calculations" {
  // 金融计算测试
  // 简单利息计算
  let principal = 10000
  let rate = 5
  let time = 3
  let interest = azimuth.multiply(principal, azimuth.multiply(rate, time))
  let total = azimuth.add(principal, interest)
  
  assert_eq(160000, total)
  
  // 折扣计算
  let original_price = 200
  let discount_percent = 15
  let discount_amount = azimuth.multiply(original_price, discount_percent) / 100
  let final_price = azimuth.add(original_price, -discount_amount)
  
  assert_eq(170, final_price)
}

test "enhanced_temperature_conversions" {
  // 温度转换测试
  // 摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 20
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
  
  // 华氏度转摄氏度（简化版：C = (F - 30) ÷ 2）
  let fahrenheit_back = 70
  let celsius_back = azimuth.add(fahrenheit_back, -30) / 2
  assert_eq(20, celsius_back)
}

test "enhanced_statistical_operations" {
  // 统计运算测试
  // 计算总和
  let numbers = azimuth.add(azimuth.add(azimuth.add(15, 25), 35), 45)
  assert_eq(120, numbers)
  
  // 计算极差
  let max_val = 45
  let min_val = 15
  let range = azimuth.add(max_val, -min_val)
  assert_eq(30, range)
  
  // 计算中位数（奇数个元素）
  let sorted_list = azimuth.add(azimuth.add(10, 20), 30)
  let median = sorted_list / 3
  assert_eq(20, median)
}

test "enhanced_assertion_functions" {
  // 断言函数测试
  // 测试 assert_true 和 assert_false
  azimuth.assert_true(5 > 3)
  azimuth.assert_true(0 == 0)
  azimuth.assert_false(5 < 3)
  azimuth.assert_false(1 == 0)
  
  // 测试 assert_eq 和 assert_eq_string
  azimuth.assert_eq(42, azimuth.add(20, 22))
  azimuth.assert_eq(0, azimuth.multiply(5, 0))
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
}

test "enhanced_divide_with_ceil" {
  // 向上取整除法测试
  // 基本测试
  assert_eq(3, azimuth.divide_with_ceil(10, 3))
  assert_eq(4, azimuth.divide_with_ceil(11, 3))
  assert_eq(4, azimuth.divide_with_ceil(12, 3))
  
  // 负数测试
  assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  assert_eq(-3, azimuth.divide_with_ceil(-11, 3))
  assert_eq(-4, azimuth.divide_with_ceil(-12, 3))
  
  // 特殊情况
  assert_eq(0, azimuth.divide_with_ceil(0, 5))
  assert_eq(5, azimuth.divide_with_ceil(5, 1))
}

test "enhanced_error_boundary_handling" {
  // 错误边界处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护
  azimuth.assert_true(azimuth.add(max_val, 100) == max_val)
  azimuth.assert_true(azimuth.add(min_val, -100) == min_val)
  azimuth.assert_true(azimuth.multiply(max_val, 2) == max_val)
  azimuth.assert_true(azimuth.multiply(min_val, -2) == min_val)
  
  // 验证特殊情况处理
  azimuth.assert_true(azimuth.multiply(min_val, -1) == min_val)
  azimuth.assert_true(azimuth.add(max_val, min_val) == -1)
}