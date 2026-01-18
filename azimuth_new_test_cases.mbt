// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，专门测试 azimuth 模块功能

test "add_comprehensive_functionality" {
  // 全面测试加法功能
  // 测试正常情况
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))  // 溢出保护
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1)) // 溢出保护
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val))
  
  // 测试与0相加
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-42, @azimuth.add(0, -42))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiply_comprehensive_functionality" {
  // 全面测试乘法功能
  // 测试正常情况
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(12, @azimuth.multiply(-4, -3))
  
  // 测试特殊情况
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(-8, @azimuth.multiply(8, -1))
  
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1)) // 特殊情况处理
}

test "greet_function_variations" {
  // 测试greet函数的各种输入
  // 基本测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  
  // 空字符串测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 特殊字符测试
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, user@123!", @azimuth.greet("user@123"))
  
  // 长字符串测试
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongNameForTestingPurposes!", @azimuth.greet(long_name))
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
  
  // 结合律
  let add_assoc1 = @azimuth.add(@azimuth.add(a, b), c)
  let add_assoc2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(add_assoc1, add_assoc2)
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(617, total) // 199*3 + 20 = 617
  
  // 温度转换（简化版）：F = C * 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit) // 25*2 + 30 = 80
  
  // 面积计算
  let length = 10
  let width = 6
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, area)
}

test "boundary_value_combinations" {
  // 边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 极值乘法组合
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 接近边界值的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "complex_calculations" {
  // 复杂计算测试
  // 阶乘计算：5! = 5 * 4 * 3 * 2 * 1
  let factorial = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial)
  
  // 平方和：1² + 2² + 3² + 4² + 5²
  let square_sum = @azimuth.add(@azimuth.multiply(1, 1), 
    @azimuth.add(@azimuth.multiply(2, 2), 
    @azimuth.add(@azimuth.multiply(3, 3), 
    @azimuth.add(@azimuth.multiply(4, 4), @azimuth.multiply(5, 5)))))
  @azimuth.assert_eq(55, square_sum)
  
  // 复合表达式：(a + b) * (c - d)
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let result = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(c, -d))
  @azimuth.assert_eq(180, result) // (10+5)*(20-8) = 15*12 = 180
}

test "assertion_functions_test" {
  // 测试断言函数
  // 测试正常情况
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试计算结果
  let add_result = @azimuth.add(15, 25)
  @azimuth.assert_eq(40, add_result)
  
  let mul_result = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, mul_result)
  
  let greet_result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", greet_result)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  
  // 测试条件表达式
  let x = 10
  let y = 20
  let max_result = if (x > y) { x } else { y }
  @azimuth.assert_eq(20, max_result)
}