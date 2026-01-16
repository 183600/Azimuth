// 用户添加的 MoonBit 测试用例
// 覆盖核心功能和边界情况

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
}

test "add_function_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(0, 100))
}

test "multiply_function_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值处理
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_calculation_scenario" {
  // 测试复合计算场景
  // 计算表达式：(a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let result = add(product, -d)
  
  assert_eq(22, result)
}

test "financial_calculation" {
  // 测试金融计算场景
  // 简单利息计算：本金 + 本金 × 利率 × 时间
  let principal = 1000
  let rate = 5
  let time = 2
  
  let interest = multiply(principal, multiply(rate, time))
  let total = add(principal, interest)
  
  assert_eq(11000, total)
}

test "geometry_calculation" {
  // 测试几何计算
  // 计算矩形周长：2 × (长 + 宽)
  let length = 8
  let width = 5
  
  let sum_sides = add(length, width)
  let perimeter = multiply(2, sum_sides)
  
  assert_eq(26, perimeter)
}

test "assert_functions_comprehensive" {
  // 测试所有断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试复合条件
  assert_true(add(2, 3) == 5)
  assert_false(multiply(2, 3) == 7)
}

test "real_world_shopping_cart" {
  // 测试实际购物车计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let shipping = 20
  
  // 计算总价
  let total1 = multiply(item1_price, item1_qty)
  let total2 = multiply(item2_price, item2_qty)
  let subtotal = add(total1, total2)
  let final_total = add(subtotal, shipping)
  
  assert_eq(717, final_total)
}