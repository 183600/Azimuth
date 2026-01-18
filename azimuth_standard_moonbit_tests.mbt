// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含核心功能测试

// 导入必要的模块
use azimuth

test "add_basic_functionality" {
  // 测试基本的加法功能
  let result1 = azimuth.add(2, 3)
  @assertion.assert_eq(5, result1)?
  
  let result2 = azimuth.add(-5, 10)
  @assertion.assert_eq(5, result2)?
  
  let result3 = azimuth.add(0, 0)
  @assertion.assert_eq(0, result3)?
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  let result1 = azimuth.multiply(4, 5)
  @assertion.assert_eq(20, result1)?
  
  let result2 = azimuth.multiply(-3, 7)
  @assertion.assert_eq(-21, result2)?
  
  let result3 = azimuth.multiply(0, 100)
  @assertion.assert_eq(0, result3)?
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  let result1 = azimuth.greet("Alice")
  @assertion.assert_eq_string("Hello, Alice!", result1)?
  
  let result2 = azimuth.greet("Bob")
  @assertion.assert_eq_string("Hello, Bob!", result2)?
}

test "add_negative_numbers" {
  // 测试负数加法
  let result1 = azimuth.add(-10, -20)
  @assertion.assert_eq(-30, result1)?
  
  let result2 = azimuth.add(-15, 25)
  @assertion.assert_eq(10, result2)?
  
  let result3 = azimuth.add(50, -75)
  @assertion.assert_eq(-25, result3)?
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  let result1 = azimuth.multiply(-4, -6)
  @assertion.assert_eq(24, result1)?
  
  let result2 = azimuth.multiply(-7, 3)
  @assertion.assert_eq(-21, result2)?
  
  let result3 = azimuth.multiply(8, -9)
  @assertion.assert_eq(-72, result3)?
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  let result1 = azimuth.greet("Alice@123")
  @assertion.assert_eq_string("Hello, Alice@123!", result1)?
  
  let result2 = azimuth.greet("Bob-Smith")
  @assertion.assert_eq_string("Hello, Bob-Smith!", result2)?
  
  let result3 = azimuth.greet("")
  @assertion.assert_eq_string("Hello, !", result3)?
}

test "mathematical_combinations" {
  // 测试数学运算组合
  let a = 10
  let b = 5
  let c = 3
  
  // 测试 (a + b) * c
  let result1 = azimuth.multiply(azimuth.add(a, b), c)
  @assertion.assert_eq(45, result1)?
  
  // 测试 a * b + c
  let result2 = azimuth.add(azimuth.multiply(a, b), c)
  @assertion.assert_eq(53, result2)?
}

test "edge_cases" {
  // 测试边界情况
  // 大数加法
  let result1 = azimuth.add(1000000, 2000000)
  @assertion.assert_eq(3000000, result1)?
  
  // 大数乘法
  let result2 = azimuth.multiply(1000, 1000)
  @assertion.assert_eq(1000000, result2)?
  
  // 单位数运算
  let result3 = azimuth.add(1, 1)
  @assertion.assert_eq(2, result3)?
  
  let result4 = azimuth.multiply(1, 1)
  @assertion.assert_eq(1, result4)?
}

test "real_world_scenarios" {
  // 测试真实世界场景
  // 购物车计算
  let item_price = 25
  let quantity = 4
  let tax_rate = 8
  let shipping = 10
  
  let subtotal = azimuth.multiply(item_price, quantity)
  let tax = azimuth.multiply(subtotal, tax_rate) / 100
  let total = azimuth.add(azimuth.add(subtotal, tax), shipping)
  
  @assertion.assert_eq(118, total)?
  
  // 时间计算
  let hours = 2
  let minutes = 30
  let total_minutes = azimuth.add(azimuth.multiply(hours, 60), minutes)
  
  @assertion.assert_eq(150, total_minutes)?
}

test "unicode_support" {
  // 测试Unicode支持
  let chinese_name = "张三"
  let japanese_name = "田中"
  let emoji_name = "🌟"
  
  let result1 = azimuth.greet(chinese_name)
  @assertion.assert_eq_string("Hello, 张三!", result1)?
  
  let result2 = azimuth.greet(japanese_name)
  @assertion.assert_eq_string("Hello, 田中!", result2)?
  
  let result3 = azimuth.greet(emoji_name)
  @assertion.assert_eq_string("Hello, 🌟!", result3)?
}