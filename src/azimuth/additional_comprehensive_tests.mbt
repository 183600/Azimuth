// 额外的综合测试用例 - MoonBit 标准测试语法
// 测试 azimuth 包的核心功能

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 负数溢出测试
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(min_val, add(-1500000000, -1000000000))
}

test "multiply_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最小值与-1相乘的特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  
  // 大数相乘溢出测试
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 零值测试
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  // 正数测试
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(4, divide_with_ceil(15, 4))
  
  // 负数测试
  assert_eq(-2, divide_with_ceil(-10, 4))
  assert_eq(-3, divide_with_ceil(-11, 4))
  
  // 整除测试
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(-2, divide_with_ceil(-8, 4))
}

test "greet_unicode_handling" {
  // 测试 greet 函数的 Unicode 处理
  // 中文字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 张三!", greet("张三"))
  
  // Emoji 测试
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, 👋!", greet("👋"))
  
  // 混合字符测试
  assert_eq_string("Hello, Test测试123!", greet("Test测试123"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂算术序列
  let a = 5
  let b = 10
  let c = 15
  
  // 计算 (a + b) * (b + c) - a * c
  let sum1 = add(a, b)
  let sum2 = add(b, c)
  let product1 = multiply(sum1, sum2)
  let product2 = multiply(a, c)
  let result = add(product1, -product2)
  
  assert_eq(275, result)
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let x = 12
  let y = 18
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 包含负数的交换律测试
  let neg_x = -12
  let neg_y = 18
  assert_eq(add(neg_x, neg_y), add(neg_y, neg_x))
  assert_eq(multiply(neg_x, neg_y), multiply(neg_y, neg_x))
}

test "assert_functions_comprehensive" {
  // 综合测试断言函数
  // 基本断言测试
  assert_true(true)
  assert_false(false)
  
  // 数值断言测试
  assert_eq(0, 0)
  assert_eq(100, 100)
  assert_eq(-50, -50)
  
  // 字符串断言测试
  assert_eq_string("", "")
  assert_eq_string("test", "test")
  assert_eq_string("Hello, World!", "Hello, World!")
  
  // 布尔表达式断言
  assert_true(10 > 5)
  assert_true(3 >= 3)
  assert_false(10 < 5)
  assert_false(3 <= 2)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let principal = 10000
  let rate = 5
  let years = 3
  let fee = 50
  
  // 计算简单利息：本金 × 利率 × 年数
  let interest = multiply(principal, multiply(rate, years))
  
  // 计算总额：本金 + 利息 - 手续费
  let total = add(add(principal, interest), -fee)
  
  assert_eq(160450, total)
}

test "temperature_conversion_formula" {
  // 温度转换公式测试（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  
  // 测试常见温度点
  let freezing_c = 0
  let freezing_f = add(multiply(freezing_c, 2), 30)
  assert_eq(30, freezing_f)
  
  let room_c = 20
  let room_f = add(multiply(room_c, 2), 30)
  assert_eq(70, room_f)
  
  let boiling_c = 100
  let boiling_f = add(multiply(boiling_c, 2), 30)
  assert_eq(230, boiling_f)
}

test "geometry_area_calculations" {
  // 几何面积计算测试
  // 矩形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = multiply(length, width)
  assert_eq(96, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = multiply(base, height) / 2
  assert_eq(30, triangle_area)
  
  // 梯形面积：（上底 + 下底）× 高 ÷ 2
  let top_base = 5
  let bottom_base = 15
  let trapezoid_height = 8
  let trapezoid_area = multiply(add(top_base, bottom_base), trapezoid_height) / 2
  assert_eq(80, trapezoid_area)
}