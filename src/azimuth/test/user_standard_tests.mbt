// 用户添加的标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试 azimuth 库的核心功能

test "addition_basic_functionality" {
  // 测试基本的加法功能
  let result = @azimuth.add(15, 25)
  @azimuth.assert_eq(40, result)
}

test "addition_with_negative_numbers" {
  // 测试负数加法
  let result1 = @azimuth.add(-10, 20)
  @azimuth.assert_eq(10, result1)
  
  let result2 = @azimuth.add(-15, -25)
  @azimuth.assert_eq(-40, result2)
}

test "multiplication_basic_functionality" {
  // 测试基本的乘法功能
  let result = @azimuth.multiply(7, 8)
  @azimuth.assert_eq(56, result)
}

test "multiplication_with_negative_numbers" {
  // 测试负数乘法
  let result1 = @azimuth.multiply(-6, 7)
  @azimuth.assert_eq(-42, result1)
  
  let result2 = @azimuth.multiply(-8, -9)
  @azimuth.assert_eq(72, result2)
}

test "greet_function_basic_test" {
  // 测试基本的问候功能
  let result = @azimuth.greet("用户")
  @azimuth.assert_eq_string("Hello, 用户!", result)
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串
  let result = @azimuth.greet("Test@123")
  @azimuth.assert_eq_string("Hello, Test@123!", result)
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let result = @azimuth.add(max_val, 0)
  @azimuth.assert_eq(max_val, result)
}

test "boundary_value_multiplication" {
  // 测试边界值乘法
  let result = @azimuth.multiply(1000000, 1000)
  @azimuth.assert_eq(1000000000, result)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算 (a + b) * c
  let a = 10
  let b = 20
  let c = 3
  
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(90, result)
}

test "real_world_shopping_cart" {
  // 测试实际应用场景：计算购物车总价
  let item_price = 99
  let quantity = 5
  let shipping = 10
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(505, total)  // 99*5 + 10 = 495 + 10 = 505
}