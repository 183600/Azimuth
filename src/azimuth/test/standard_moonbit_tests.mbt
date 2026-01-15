// 标准 MoonBit 测试用例
// 覆盖 Azimuth 项目的核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(16, @azimuth.multiply(4, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法恒等式
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
  
  // 乘法恒等式
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(a, @azimuth.multiply(1, a))
}

test "boundary_values_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值边界测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算 (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 8
  
  let sum = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum, c)  // 45
  let result = @azimuth.add(product, -d)  // 37
  
  @azimuth.assert_eq(37, result)
  
  // 计算平方和：a² + b²
  let a_squared = @azimuth.multiply(a, a)  // 100
  let b_squared = @azimuth.multiply(b, b)  // 25
  let sum_of_squares = @azimuth.add(a_squared, b_squared)  // 125
  
  @azimuth.assert_eq(125, sum_of_squares)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 场景1：计算购物车总价
  let item1_price = 50
  let item1_quantity = 2
  let item2_price = 30
  let item2_quantity = 3
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 100
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 90
  let cart_total = @azimuth.add(total1, total2)  // 190
  
  @azimuth.assert_eq(190, cart_total)
  
  // 场景2：计算矩形面积和周长
  let length = 8
  let width = 5
  
  let area = @azimuth.multiply(length, width)  // 40
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))  // 26
  
  @azimuth.assert_eq(40, area)
  @azimuth.assert_eq(26, perimeter)
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  // 测试特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // 测试Unicode字符
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试长字符串
  let long_name = "VeryLongNameForTestingPurposes"
  @azimuth.assert_eq_string("Hello, VeryLongNameForTestingPurposes!", @azimuth.greet(long_name))
}

test "sequential_operations" {
  // 测试连续运算
  // 连续加法：1 + 2 + 3 + 4 + 5
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, sum1)
  
  // 连续乘法：2 * 3 * 4
  let product1 = @azimuth.multiply(@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(24, product1)
  
  // 混合运算：(a + b) * (c + d)
  let a = 3
  let b = 4
  let c = 5
  let d = 6
  
  let sum_ab = @azimuth.add(a, b)  // 7
  let sum_cd = @azimuth.add(c, d)  // 11
  let result = @azimuth.multiply(sum_ab, sum_cd)  // 77
  
  @azimuth.assert_eq(77, result)
}

test "assertion_functions_test" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试断言函数与项目函数的结合
  let add_result = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, add_result)
  
  let multiply_result = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, multiply_result)
  
  let greet_result = @azimuth.greet("Test")
  @azimuth.assert_eq_string("Hello, Test!", greet_result)
}