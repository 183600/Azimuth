// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(10, add(-5, 15))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(5, 3))
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_functionality" {
  // 测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "complex_calculations" {
  // 测试复合计算
  let a = 10
  let b = 20
  let c = 5
  
  // (a + b) * c
  let result = multiply(add(a, b), c)
  assert_eq(150, result)
  
  // a * c + b * c
  let result2 = add(multiply(a, c), multiply(b, c))
  assert_eq(result, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 11
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算矩形周长：2 * (length + width)
  let length = 8
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
  
  // 计算简单利息：principal * rate * time
  let principal = 1000
  let rate = 5
  let time = 3
  let interest = multiply(principal, multiply(rate, time))
  assert_eq(15000, interest)
}

test "error_handling" {
  // 测试错误处理和边界情况
  assert_true(true)
  assert_false(false)
  
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(add(min_val, -100) == min_val)
}

// 新增的高质量测试用例
test "add_function_overflow_comprehensive" {
  // 测试加法函数的全面溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试零值加法
  assert_eq(5, add(0, 5))
  assert_eq(-5, add(0, -5))
  assert_eq(0, add(0, 0))
}

test "multiply_function_overflow_comprehensive" {
  // 测试乘法函数的全面溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, multiply(46341, 46341))
  
  // 测试负数溢出
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 测试最小值的特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
  
  // 测试零值乘法
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, line1\nline2!", greet("line1\nline2"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, emoji🌍!", greet("emoji🌍"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_mathematical_operations" {
  // 测试复杂数学运算组合
  let a = 10
  let b = 20
  let c = 5
  
  // 测试 (a + b) * c
  let sum_ab = add(a, b)
  let result = multiply(sum_ab, c)
  assert_eq(150, result)
  
  // 测试 a * b + c
  let product_ab = multiply(a, b)
  let result2 = add(product_ab, c)
  assert_eq(205, result2)
}

test "real_world_shopping_cart" {
  // 测试实际应用场景：购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  // 计算商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let subtotal = add(total1, total2)
  let final_total = add(subtotal, shipping)
  
  // 验证总价：199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
  assert_eq(717, final_total)
}

test "mathematical_properties_distributive" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 11
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 测试平方差公式：(a + b) * (a - b) = a² - b²
  let sum = add(a, b)
  let diff = add(a, -b)
  let product = multiply(sum, diff)
  let square_diff = add(multiply(a, a), multiply(-b, b))
  assert_eq(product, square_diff)
}

test "edge_cases_and_boundary_conditions" {
  // 测试边界条件和特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试单位值运算
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(-5, multiply(5, -1))
  
  // 测试连续运算
  let chain_result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, chain_result)
  
  // 测试嵌套运算
  let nested_result = multiply(add(10, 20), add(5, 15))
  assert_eq(600, nested_result)
}

test "temperature_conversion" {
  // 测试实际应用场景：温度转换（简化版）
  // 摄氏度转华氏度：F = C * 2 + 30（简化公式）
  let celsius_temp = 25
  let fahrenheit_temp = add(multiply(celsius_temp, 2), 30)
  assert_eq(80, fahrenheit_temp)
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  assert_eq(230, boiling_fahrenheit)
}