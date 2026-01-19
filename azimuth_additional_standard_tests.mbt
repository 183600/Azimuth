// Azimuth 项目附加标准测试用例
// 使用标准 MoonBit 测试语法，包含断言验证

test "add_function_various_cases" {
  // 测试加法函数的各种情况
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(10, (-10)))
  @azimuth.assert_eq(-3, @azimuth.add((-8), 5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "multiply_function_various_cases" {
  // 测试乘法函数的各种情况
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, (-5)))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(1000, @azimuth.multiply(100, 10))
}

test "divide_with_ceil_function_cases" {
  // 测试向上取整除法函数的各种情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(7, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(9, (-3)))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(100, 10))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
}

test "greet_function_extended" {
  // 测试问候函数的扩展用法
  @azimuth.assert_eq_string("Hello, Azimuth!", @azimuth.greet("Azimuth"))
  @azimuth.assert_eq_string("Hello, Open Source!", @azimuth.greet("Open Source"))
  @azimuth.assert_eq_string("Hello, Testing!", @azimuth.greet("Testing"))
  @azimuth.assert_eq_string("Hello, Developer!", @azimuth.greet("Developer"))
}

test "assert_functions_extended" {
  // 测试断言函数的扩展用法
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(-1, -1)
}

test "mathematical_formulas" {
  // 测试数学公式
  // 圆面积: π * r^2
  let radius = 5
  let area = @azimuth.multiply(@azimuth.multiply(3, radius), radius)  // 简化π为3
  @azimuth.assert_eq(75, area)
  
  // 正方形面积: side^2
  let side = 6
  let square_area = @azimuth.multiply(side, side)
  @azimuth.assert_eq(36, square_area)
}

test "business_calculations" {
  // 测试商业计算
  let price = 100
  let quantity = 5
  let tax_amount = 10
  
  let subtotal = @azimuth.multiply(price, quantity)
  let final_total = @azimuth.add(subtotal, tax_amount)
  
  // 计算: 100*5 + 10 = 500 + 10 = 510
  @azimuth.assert_eq(510, final_total)
}

test "complex_calculations" {
  // 测试复杂计算
  let base = 10
  let height = 5
  let width = 3
  
  // 计算梯形面积: (base + height) * width / 2 (使用向上取整除法)
  let trapezoid_area = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(base, height), width), 2)
  @azimuth.assert_eq(23, trapezoid_area)  // (10+5)*3/2 = 15*3/2 = 45/2 = 22.5 -> 23
}

test "edge_case_calculations" {
  // 测试边界情况计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, (-1)))
  
  // 测试零值相乘
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

test "string_processing" {
  // 测试字符串处理
  let name1 = "MoonBit"
  let name2 = "Azimuth"
  let name3 = "Developer"
  
  let greeting1 = @azimuth.greet(name1)
  let greeting2 = @azimuth.greet(name2)
  let greeting3 = @azimuth.greet(name3)
  
  @azimuth.assert_eq_string("Hello, MoonBit!", greeting1)
  @azimuth.assert_eq_string("Hello, Azimuth!", greeting2)
  @azimuth.assert_eq_string("Hello, Developer!", greeting3)
}