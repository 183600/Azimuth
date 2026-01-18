// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(5, -5))
}

test "add_function_overflow_protection" {
  // 测试加法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_function_overflow_protection" {
  // 测试乘法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(max_val, multiply(46341, 46341))
  
  // 测试负溢出
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(max_val, -2))
  
  // 测试特殊边界情况
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_standard_cases" {
  // 测试 greet 函数的标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "greet_function_special_characters" {
  // 测试 greet 函数处理特殊字符
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_mathematical_operations" {
  // 测试复杂的数学运算组合
  let a = 10
  let b = 20
  let c = 30
  
  // 测试分配律: a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 测试结合律: (a + b) + c = a + (b + c)
  let left_add = add(add(a, b), c)
  let right_add = add(a, add(b, c))
  assert_eq(left_add, right_add)
}

test "real_world_calculation_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 100
  let item1_quantity = 2
  let item2_price = 50
  let item2_quantity = 3
  let shipping_fee = 15
  
  // 计算商品总价
  let item1_total = multiply(item1_price, item1_quantity)
  let item2_total = multiply(item2_price, item2_quantity)
  let subtotal = add(item1_total, item2_total)
  
  // 计算最终总价
  let total = add(subtotal, shipping_fee)
  
  // 验证结果: 100*2 + 50*3 + 15 = 200 + 150 + 15 = 365
  assert_eq(365, total)
}

test "edge_case_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试连续运算
  let result = add(multiply(2, add(3, 4)), 5)
  assert_eq(19, result)  // 2 * (3 + 4) + 5 = 2 * 7 + 5 = 14 + 5 = 19
}

test "assertion_functions_verification" {
  // 测试断言函数的功能
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("MoonBit", "MoonBit")
  
  // 测试复合条件
  let x = 10
  let y = 20
  assert_true(x < y)
  assert_false(x > y)
  assert_eq(add(x, y), 30)
}