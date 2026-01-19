// 核心功能测试用例
// 测试 azimuth 库的主要功能：add, multiply, greet 函数

test "add_function_basic_tests" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-1, -1))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic_tests" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "greet_function_comprehensive_tests" {
  // 测试 greet 函数的各种情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "boundary_value_tests" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律：a * (b + c) = a * b + a * c
  let c = 5
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "real_world_calculation_scenarios" {
  // 测试实际计算场景
  // 场景1：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(697, cart_total)  // 199*2 + 299*1 = 398 + 299 = 697
  
  // 场景2：计算矩形面积和周长
  let length = 15
  let width = 8
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  @azimuth.assert_eq(120, area)        // 15 * 8 = 120
  @azimuth.assert_eq(46, perimeter)   // 2 * (15 + 8) = 46
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, mixed123!", @azimuth.greet("mixed123"))
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  // 计算：((a + b) * c) - (d * e)
  let a = 10
  let b = 15
  let c = 4
  let d = 8
  let e = 6
  
  let sum_ab = @azimuth.add(a, b)           // 10 + 15 = 25
  let product1 = @azimuth.multiply(sum_ab, c)  // 25 * 4 = 100
  let product2 = @azimuth.multiply(d, e)       // 8 * 6 = 48
  let result = @azimuth.add(product1, -product2)  // 100 - 48 = 52
  
  @azimuth.assert_eq(52, result)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 测试计算结果验证
  let calc_result = @azimuth.add(@azimuth.multiply(3, 4), 5)
  @azimuth.assert_true(calc_result == 17)
}

test "performance_related_operations" {
  // 测试性能相关操作
  // 测试连续运算
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(2000000000000, @azimuth.multiply(large_num1, large_num2))
}