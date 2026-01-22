// 标准 MoonBit 测试套件
// 测试 azimuth 包的核心功能

test "add_function_basic" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "greet_function_basic" {
  // 测试 greet 函数的基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_function_special_characters" {
  // 测试 greet 函数处理特殊字符
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, emoji😊!", @azimuth.greet("emoji😊"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：矩形面积和周长
  let length = 10
  let width = 5
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(50, area)
  @azimuth.assert_eq(30, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：折扣计算
  let original_price = 1000
  let discount_percentage = 20
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(800, final_price)
}

test "sequential_operations" {
  // 测试连续运算
  // 1 + 2 + 3 + 4 + 5 = 15
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  let final_sum = @azimuth.add(result1, 5)
  @azimuth.assert_eq(15, final_sum)
  
  // 2 * 3 * 4 = 24
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(24, result2)
}

test "assertion_functions" {
  // 测试断言函数
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}