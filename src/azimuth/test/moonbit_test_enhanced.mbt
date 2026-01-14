// MoonBit 增强测试用例
// 测试 azimuth 包的核心功能
test "enhanced_add_functionality" {
  // 测试加法函数的各种情况
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 100))  // 测试溢出保护
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -100))  // 测试下溢保护
}

test "enhanced_multiply_functionality" {
  // 测试乘法函数的各种情况
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 2))  // 测试溢出保护
}

test "enhanced_string_greetings" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))  // 测试 emoji
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))  // 测试空格
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 15
  let b = 25
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 5)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 5)))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算 (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum = @azimuth.add(a, b)  // 30
  let product = @azimuth.multiply(sum, c)  // 90
  let result = @azimuth.add(product, -d)  // 75
  
  @azimuth.assert_eq(75, result)
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值附近的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(697, cart_total)  // 199*2 + 299*1 = 398 + 299 = 697
}

test "error_handling_validation" {
  // 测试错误处理和验证
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护是否正常工作
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "performance_related_operations" {
  // 测试性能相关的操作
  // 计算等差数列的和：1 + 2 + 3 + ... + 100
  let n = 100
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, sum)
  
  // 计算等比数列的和：1 + 2 + 4 + 8 + 16
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 4), 8), 16)
  @azimuth.assert_eq(31, geo_sum)
}

test "advanced_string_processing" {
  // 测试高级字符串处理
  // 测试各种特殊字符
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", @azimuth.greet(special_chars))
  
  // 测试 Unicode 字符
  let unicode = "你好世界🌍"
  @azimuth.assert_eq_string("Hello, 你好世界🌍!", @azimuth.greet(unicode))
  
  // 测试数字和字母混合
  let alphanumeric = "User2023@Example"
  @azimuth.assert_eq_string("Hello, User2023@Example!", @azimuth.greet(alphanumeric))
}