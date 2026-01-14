// MoonBit 标准测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试与零相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(-8, @azimuth.multiply(-2, 4))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊值
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
}

test "financial_calculation" {
  // 测试金融计算场景
  // 商品价格 × 数量 + 税费 - 折扣
  let price = 199
  let quantity = 3
  let tax_rate = 8  // 8%
  let discount = 50
  
  // 计算总价
  let subtotal = @azimuth.multiply(price, quantity)
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  let total = @azimuth.add(@azimuth.add(subtotal, tax), -discount)
  
  @azimuth.assert_eq(583, total)  // 199*3 + 199*3*8/100 - 50 = 597 + 47 - 50 = 594
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongStringForTestingPurposes!", @azimuth.greet(long_string))
}

test "mathematical_operations_combination" {
  // 测试数学运算的组合
  // 计算 (a + b) × c - d ÷ e
  let a = 10
  let b = 5
  let c = 3
  let d = 12
  let e = 4
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum_ab, c)  // 45
  let division = d / e  // 3
  let result = @azimuth.add(product, -division)  // 42
  
  @azimuth.assert_eq(42, result)
}

test "real_world_application" {
  // 测试实际应用场景：计算平均分
  let math_score = 85
  let english_score = 78
  let science_score = 92
  let history_score = 88
  
  // 计算总分
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(math_score, english_score), science_score), history_score)
  // 计算平均分
  let average_score = total_score / 4
  
  @azimuth.assert_eq(85, average_score)  // (85+78+92+88)/4 = 343/4 = 85
}