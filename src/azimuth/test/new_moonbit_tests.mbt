// 新的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试 Azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 测试基本的算术运算
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 3)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3))
  )
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形的周长：2 × (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(30, perimeter)
  
  // 计算圆的面积（简化）：π × r² ≈ 3 × r²
  let radius = 4
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(48, area)
}

test "real_world_applications" {
  // 测试实际应用场景
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
  
  // 购物车计算
  let price = 199
  let quantity = 3
  let shipping = 15
  let total = @azimuth.add(@azimuth.multiply(price, quantity), shipping)
  @azimuth.assert_eq(612, total)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "sequential_operations" {
  // 测试连续操作
  // 连续相加：(a + b) + (c + d)
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  let result = @azimuth.add(@azimuth.add(a, b), @azimuth.add(c, d))
  @azimuth.assert_eq(100, result)
  
  // 连续相乘：(a × b) × (c × d)
  let product = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, product)
}

test "conditional_calculations" {
  // 测试条件计算
  let x = 15
  let y = 20
  
  // 如果 x < y，则计算 x + y，否则计算 x × y
  let condition = x < y
  let result = if condition { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  @azimuth.assert_eq(35, result)
  
  // 反向测试
  let condition2 = x > y
  let result2 = if condition2 { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  @azimuth.assert_eq(300, result2)
}

test "performance_and_large_numbers" {
  // 测试性能和大数处理
  // 大数运算
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 复杂计算：平方和
  let n = 10
  // 计算 1² + 2² + ... + 10² = 385
  let sum_of_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4)), @azimuth.multiply(5, 5)), @azimuth.multiply(6, 6)), @azimuth.multiply(7, 7)), @azimuth.multiply(8, 8)), @azimuth.multiply(9, 9)), @azimuth.multiply(10, 10))
  @azimuth.assert_eq(385, sum_of_squares)
}