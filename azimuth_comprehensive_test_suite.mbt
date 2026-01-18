// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法

// 导入 azimuth 模块
@lib.import("azimuth")

test "add_function_comprehensive" {
  // 测试加法函数的综合情况
  assert_eq(0, add(0, 0))
  assert_eq(10, add(7, 3))
  assert_eq(-10, add(-7, -3))
  assert_eq(0, add(5, -5))
  assert_eq(-2, add(-5, 3))
  
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 2))
}

test "greet_function_comprehensive" {
  // 测试 greet 函数的综合情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
}

test "mathematical_properties" {
  // 测试数学基本性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
  
  // 结合律
  assert_eq(add(add(a, b), 5), add(a, add(b, 5)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  let shipping = 15
  
  let item1_total = multiply(item1_price, item1_quantity)
  let item2_total = multiply(item2_price, item2_quantity)
  let subtotal = add(item1_total, item2_total)
  let total = add(subtotal, shipping)
  
  assert_eq(694, total)  // 199*2 + 99*3 + 15 = 398 + 297 + 15 = 710
  
  // 温度转换（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 零值特性
  assert_eq(5, add(0, 5))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  
  // 单位元特性
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
  assert_eq(-7, multiply(-7, 1))
  assert_eq(-7, multiply(1, -7))
}

test "complex_calculations" {
  // 测试复杂计算
  // 阶乘计算（简化版）
  let factorial_5 = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial_5)
  
  // 平方和计算
  let square_sum = add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3))
  assert_eq(14, square_sum)  // 1² + 2² + 3² = 1 + 4 + 9 = 14
  
  // 复合表达式
  let result = add(multiply(3, add(4, 5)), multiply(2, 6))
  assert_eq(39, result)  // 3 * (4 + 5) + 2 * 6 = 3 * 9 + 12 = 27 + 12 = 39
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 单字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
  
  // 特殊字符组合
  assert_eq_string("Hello, \"John\"!", greet("\"John\""))
  assert_eq_string("Hello, line1\nline2!", greet("line1\nline2"))
}

test "assertion_functions_verification" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 复合条件测试
  let x = 15
  let y = 25
  assert_true(x < y)
  assert_false(x > y)
  assert_eq(add(x, y), 40)
  assert_eq(multiply(x, 2), 30)
}