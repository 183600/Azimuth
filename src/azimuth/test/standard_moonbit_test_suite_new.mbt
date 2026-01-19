// 标准 MoonBit 测试套件
// 包含基本的算术运算、字符串处理和边界条件测试

// 基本加法测试
test "add_basic_functionality" {
  // 直接使用 assert_eq 函数测试 add 函数
  let result1 = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result1)
  
  let result2 = @azimuth.add(0, 0)
  @azimuth.assert_eq(0, result2)
  
  let result3 = @azimuth.add(1, -2)
  @azimuth.assert_eq(-1, result3)
}

// 基本乘法测试
test "multiply_basic_functionality" {
  let result1 = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result1)
  
  let result2 = @azimuth.multiply(5, 0)
  @azimuth.assert_eq(0, result2)
  
  let result3 = @azimuth.multiply(2, -3)
  @azimuth.assert_eq(-6, result3)
}

// 问候函数测试
test "greet_basic_functionality" {
  let result1 = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result1)
  
  let result2 = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result2)
}

// 边界值测试
test "boundary_value_tests" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试最小值加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试最大值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

// 数学性质测试
test "mathematical_properties" {
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
}

// 实际应用场景测试
test "real_world_scenarios" {
  // 购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(797, cart_total)  // 299*2 + 199*1 = 598 + 199 = 797
  
  // 简单温度转换（摄氏度转华氏度：F = C * 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
}

// 字符串处理测试
test "string_processing" {
  // 测试包含数字的字符串
  let numeric_name = "User123"
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(numeric_name))
  
  // 测试包含特殊字符的字符串
  let special_name = "test@example.com"
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet(special_name))
  
  // 测试包含Unicode字符的字符串
  let unicode_name = "张三"
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(unicode_name))
}

// 复合计算测试
test "complex_calculations" {
  // 计算 (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum_ab, c)  // 45
  let result = @azimuth.add(product, -d)  // 38
  
  @azimuth.assert_eq(38, result)
}

// 错误处理测试
test "error_handling" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}