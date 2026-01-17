// 用户请求的综合测试用例
// 测试 azimuth 模块的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(-7, @azimuth.add(-3, -4))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
}

test "greet_function_variations" {
  // 测试问候函数的不同变体
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "zero_value_operations" {
  // 测试零值运算
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "complex_calculations" {
  // 测试复杂计算
  let base = 10
  let height = 5
  let width = 3
  
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let area = @azimuth.multiply(@azimuth.add(base, width), height) / 2
  @azimuth.assert_eq(32, area)
}

test "string_special_characters" {
  // 测试特殊字符字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
}

test "assert_functions_verification" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "real_world_application" {
  // 测试实际应用场景：计算折扣价格
  let original_price = 200
  let discount_percent = 15
  let quantity = 3
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  // 计算折扣后单价
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  // 计算总价
  let total_price = @azimuth.multiply(discounted_price, quantity)
  
  @azimuth.assert_eq(510, total_price)
}