// Azimuth 项目的附加测试用例
// 使用标准的 MoonBit 测试语法

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
}

test "greet_function_test" {
  // 测试问候函数
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 10
  let b = 20
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 5)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 5)))
}

test "boundary_values_test" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算矩形的面积和周长
  let length = 10
  let width = 5
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(50, area)
  @azimuth.assert_eq(30, perimeter)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(617, total)
}

test "string_handling" {
  // 测试字符串处理
  let name_with_spaces = "John Doe"
  let name_with_numbers = "User123"
  let name_with_symbols = "test@example.com"
  
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet(name_with_spaces))
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(name_with_numbers))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet(name_with_symbols))
}

test "negative_operations" {
  // 测试负数运算
  let a = -10
  let b = -5
  
  @azimuth.assert_eq(-15, @azimuth.add(a, b))
  @azimuth.assert_eq(50, @azimuth.multiply(a, b))
  @azimuth.assert_eq(5, @azimuth.add(a, 15))
  @azimuth.assert_eq(20, @azimuth.multiply(a, -2))
}

test "overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  
  // 确保溢出被正确处理
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值的溢出保护
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}