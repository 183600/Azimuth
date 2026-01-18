// 用户指定的标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
  @azimuth.assert_eq(-50, @azimuth.add(0, -50))
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本情况
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet("user@example.com"))
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法边界值
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试乘法边界值
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 15
  let b = 25
  
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
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算：(a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum_ab = @azimuth.add(a, b)  // 30
  let product = @azimuth.multiply(sum_ab, c)  // 90
  let result = @azimuth.add(product, -d)  // 75
  
  @azimuth.assert_eq(75, result)
  
  // 计算：a * b + c * d
  let e = 4
  let f = 6
  let g = 7
  let h = 8
  
  let product1 = @azimuth.multiply(e, f)  // 24
  let product2 = @azimuth.multiply(g, h)  // 56
  let result2 = @azimuth.add(product1, product2)  // 80
  
  @azimuth.assert_eq(80, result2)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 场景1：计算购物总价
  let item_price = 199
  let quantity = 2
  let shipping = 20
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(418, total)  // 199*2 + 20 = 398 + 20 = 418
  
  // 场景2：计算矩形周长
  let length = 8
  let width = 5
  
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(26, perimeter)  // 2*8 + 2*5 = 16 + 10 = 26
}

test "sequential_operations" {
  // 测试连续操作
  // 连续加法：1 + 2 + 3 + 4
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4)
  @azimuth.assert_eq(10, sum1)
  
  // 连续乘法：2 * 3 * 4
  let product1 = @azimuth.multiply(@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(24, product1)
  
  // 混合连续操作：(a + b) * (c + d)
  let i = 5
  let j = 10
  let k = 3
  let l = 7
  
  let sum1 = @azimuth.add(i, j)  // 15
  let sum2 = @azimuth.add(k, l)  // 10
  let result = @azimuth.multiply(sum1, sum2)  // 150
  
  @azimuth.assert_eq(150, result)
}

test "string_special_cases" {
  // 测试字符串特殊情况
  // 包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet(special_chars))
  
  // 包含空格的字符串
  let spaced_name = "John Doe"
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet(spaced_name))
  
  // 包含数字和字母混合的字符串
  let mixed_name = "User123"
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(mixed_name))
  
  // 包含emoji的字符串
  let emoji_name = "🌍"
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet(emoji_name))
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊值的乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}