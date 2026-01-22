// 简单的用户 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "boundary_value_tests" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
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

test "real_world_calculations" {
  // 测试实际应用计算
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(612, total)
  
  // 温度转换（简化版）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 简单利息计算
  let principal = 1000
  let rate = 5
  let years = 2
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let amount = @azimuth.add(principal, interest)
  @azimuth.assert_eq(11000, amount)
}

test "complex_number_operations" {
  // 测试复杂数字运算
  // 嵌套计算：(a + b) × (c + d)
  let a = 5
  let b = 3
  let c = 7
  let d = 2
  
  let sum1 = @azimuth.add(a, b)  // 8
  let sum2 = @azimuth.add(c, d)  // 9
  let result = @azimuth.multiply(sum1, sum2)  // 72
  @azimuth.assert_eq(72, result)
  
  // 连续运算：a × b + c × d
  let product1 = @azimuth.multiply(a, b)  // 15
  let product2 = @azimuth.multiply(c, d)  // 14
  let combined = @azimuth.add(product1, product2)  // 29
  @azimuth.assert_eq(29, combined)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(@azimuth.add(max_val, 1000, true) == max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1000, true) == min_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 10, true) == max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 10, true) == min_val)
  
  // 测试特殊值
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

test "assertion_functions" {
  // 测试断言函数
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-100, -100)
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
}

test "performance_and_large_numbers" {
  // 测试性能和大数处理
  // 大数运算
  let large1 = 1000000
  let large2 = 2000000
  @azimuth.assert_eq(3000000, @azimuth.add(large1, large2))
  @azimuth.assert_eq(2000000000000, @azimuth.multiply(large1, large2))
  
  // 连续运算性能测试
  let result = @azimuth.add(@azimuth.add(@azimuth.add(1000, 2000), 3000), 4000)
  @azimuth.assert_eq(10000, result)
  
  // 复杂数学序列：等差数列求和 1+2+...+10
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(1, n)) / 2
  @azimuth.assert_eq(55, sum)
}

test "geometry_and_spatial_calculations" {
  // 测试几何和空间计算
  // 矩形计算
  let length = 15
  let width = 8
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(120, area)
  @azimuth.assert_eq(46, perimeter)
  
  // 三角形面积（简化）：底 × 高 ÷ 2
  let base = 12
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(36, triangle_area)
  
  // 立方体体积：长 × 宽 × 高
  let depth = 5
  let volume = @azimuth.multiply(@azimuth.multiply(length, width), depth)
  @azimuth.assert_eq(600, volume)
}