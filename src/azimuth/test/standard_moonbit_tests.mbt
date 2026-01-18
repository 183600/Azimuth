// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试 azimuth 库的核心功能

test "basic_addition_functionality" {
  // 测试基本的加法功能
  let result = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, result)
  
  // 测试加法交换律
  let result1 = @azimuth.add(15, 25)
  let result2 = @azimuth.add(25, 15)
  @azimuth.assert_eq(result1, result2)
}

test "addition_with_zero_and_negative" {
  // 测试与0相加
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  
  // 测试负数加法
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(-5, @azimuth.add(10, -15))
}

test "basic_multiplication_functionality" {
  // 测试基本的乘法功能
  let result = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, result)
  
  // 测试乘法交换律
  let result1 = @azimuth.multiply(8, 9)
  let result2 = @azimuth.multiply(9, 8)
  @azimuth.assert_eq(result1, result2)
}

test "multiplication_with_special_values" {
  // 测试与0和1相乘
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(123, @azimuth.multiply(123, 1))
  @azimuth.assert_eq(123, @azimuth.multiply(1, 123))
  
  // 测试与-1相乘
  @azimuth.assert_eq(-123, @azimuth.multiply(123, -1))
  @azimuth.assert_eq(-123, @azimuth.multiply(-1, 123))
}

test "string_greeting_functionality" {
  // 测试基本的问候功能
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
  
  // 测试空字符串
  let empty_result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", empty_result)
}

test "string_with_special_characters" {
  // 测试包含特殊字符的字符串
  let special_result = @azimuth.greet("测试@123")
  @azimuth.assert_eq_string("Hello, 测试@123!", special_result)
  
  // 测试包含空格的字符串
  let spaced_result = @azimuth.greet("Hello World")
  @azimuth.assert_eq_string("Hello, Hello World!", spaced_result)
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  let max_add_result = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, max_add_result)
  
  // 测试最小值加法
  let min_add_result = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, min_add_result)
  
  // 测试最小值乘法
  let min_mul_result = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, min_mul_result)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 10
  let width = 5
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(30, perimeter)
}

test "mathematical_formula_evaluation" {
  // 测试数学公式评估：(a + b) × c
  let a = 8
  let b = 12
  let c = 3
  
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(60, result)
}

test "real_world_application_test" {
  // 测试实际应用场景：计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算最终总价
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(612, total)  // 199*3 + 15 = 597 + 15 = 612
}