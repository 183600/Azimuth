// 新增的 MoonBit 测试用例
// 测试基本功能、边界条件和实际应用场景

test "add_function_basic_tests" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
}

test "multiply_function_basic_tests" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-5, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "greet_function_tests" {
  // 测试 greet 函数
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 测试交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试结合律
  let c = 5
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(a, b), c),
    @azimuth.add(a, @azimuth.add(b, c))
  )
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算 (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(38, result)  // (10 + 5) * 3 - 7 = 45 - 7 = 38
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  let shipping = 15
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  let final_total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(710, final_total)  // 199*2 + 99*3 + 15 = 398 + 297 + 15 = 710
}

test "assert_function_tests" {
  // 测试断言函数
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "string_processing_tests" {
  // 测试字符串处理
  let name_with_spaces = "John Doe"
  let name_with_numbers = "User123"
  let name_with_symbols = "test@example.com"
  
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet(name_with_spaces))
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(name_with_numbers))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet(name_with_symbols))
}

test "error_handling_tests" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_eq(@azimuth.add(max_val, 1, true) == max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1, true) == min_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2, true) == max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1, true) == min_val)
}