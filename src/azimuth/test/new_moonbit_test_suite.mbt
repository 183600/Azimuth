// 新的 MoonBit 测试套件
// 包含10个测试用例，使用标准 MoonBit 测试语法

test "add_function_comprehensive" {
  // 测试加法函数的各种情况
  // 基本加法
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  
  // 零值加法
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 大数加法
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的各种情况
  // 基本乘法
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  
  // 零值和单位值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(100, @azimuth.multiply(100, 1))
  @azimuth.assert_eq(100, @azimuth.multiply(1, 100))
  
  // 大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串
  let long_name = "VeryLongNameForTestingPurposes"
  @azimuth.assert_eq_string("Hello, VeryLongNameForTestingPurposes!", @azimuth.greet(long_name))
  
  // 特殊字符
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "overflow_handling" {
  // 测试溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 13
  let b = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  let c = 5
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 矩形周长计算：2 × (长 + 宽)
  let length = 15
  let width = 8
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(46, perimeter)
  
  // 三角形面积计算：底 × 高 ÷ 2
  let base = 12
  let height = 9
  let area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(54, area)
  
  // 简单利息计算：本金 × 利率 × 时间
  let principal = 5000
  let rate = 8  // 8%
  let time = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  @azimuth.assert_eq(120000, interest)
}

test "string_processing_comprehensive" {
  // 测试字符串处理的综合情况
  // 数字字符串
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  
  // 混合字符
  @azimuth.assert_eq_string("Hello, User2023!", @azimuth.greet("User2023"))
  
  // Unicode字符
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  
  // 空格处理
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与零的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值与零的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "sequential_operations" {
  // 测试连续运算
  // 连续加法：(a + b) + (c + d)
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  let result1 = @azimuth.add(@azimuth.add(a, b), @azimuth.add(c, d))
  @azimuth.assert_eq(100, result1)
  
  // 连续乘法：(a × b) × (c × d)
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  // 混合运算：a × b + c × d
  let result3 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(c, d))
  @azimuth.assert_eq(1400, result3)
}

test "real_world_applications" {
  // 测试实际应用场景
  // 购物车总价计算
  let item1_price = 99
  let item1_qty = 2
  let item2_price = 149
  let item2_qty = 1
  let shipping = 15
  
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  let total = @azimuth.add(@azimuth.add(subtotal1, subtotal2), shipping)
  @azimuth.assert_eq(362, total)
  
  // 温度转换（简化版）：华氏度 = 摄氏度 × 2 + 30
  let celsius = 18
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(66, fahrenheit)
  
  // 平均分计算
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let average = @azimuth.add(@azimuth.add(score1, score2), score3) / 3
  @azimuth.assert_eq(85, average)
}