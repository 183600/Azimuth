// Azimuth 项目新标准测试用例
// 使用标准 MoonBit 测试语法，提供简洁而全面的测试覆盖

test "basic_addition_functionality" {
  // 基础加法功能测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_multiplication_functionality" {
  // 基础乘法功能测试
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "greet_function_basic_cases" {
  // 问候函数基本测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_identity_properties" {
  // 数学恒等性质测试
  // 加法单位元
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  
  // 乘法单位元
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-17, 0))
}

test "greet_function_special_characters" {
  // 问候函数特殊字符测试
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

test "compound_calculations" {
  // 复合计算测试
  // (a + b) * c
  let a = 5
  let b = 3
  let c = 2
  let sum_ab = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum_ab, c)
  @azimuth.assert_eq(16, result)
  
  // a * b + c * d
  let d = 4
  let result2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(c, d))
  @azimuth.assert_eq(23, result2)
}

test "boundary_value_tests" {
  // 边界值测试
  // 大数测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000000000, @azimuth.multiply(1000000, 1000000))
  
  // 负数边界测试
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 购物车计算
  let price1 = 299
  let quantity1 = 2
  let price2 = 199
  let quantity2 = 3
  
  let total1 = @azimuth.multiply(price1, quantity1)
  let total2 = @azimuth.multiply(price2, quantity2)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(1195, cart_total)
  
  // 平均值计算 (整数除法)
  let sum = @azimuth.add(85, @azimuth.add(90, 95))
  let average = sum / 3
  @azimuth.assert_eq(90, average)
}

test "sequential_operations" {
  // 连续操作测试
  let counter = 0
  
  // 连续加法
  counter = @azimuth.add(counter, 10)
  counter = @azimuth.add(counter, 20)
  counter = @azimuth.add(counter, 30)
  @azimuth.assert_eq(60, counter)
  
  // 连续乘法
  let product = 1
  product = @azimuth.multiply(product, 2)
  product = @azimuth.multiply(product, 3)
  product = @azimuth.multiply(product, 4)
  @azimuth.assert_eq(24, product)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 溢出保护验证
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 验证溢出时的行为
  let overflow_result = @azimuth.add(max_int, 1)
  @azimuth.assert_true(overflow_result == max_int)
  
  let underflow_result = @azimuth.add(min_int, -1)
  @azimuth.assert_true(underflow_result == min_int)
  
  // 验证断言函数
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("hello", "hello")
}