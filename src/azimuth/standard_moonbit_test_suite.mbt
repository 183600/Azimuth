// 标准 MoonBit 测试套件
// 包含对 azimuth 库核心功能的测试

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, add(1000000000, 1500000000))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 123))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_functionality" {
  // 测试 greet 函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let length = 8
  let width = 6
  
  // 计算矩形周长：2 * (length + width)
  let perimeter = multiply(2, add(length, width))
  assert_eq(28, perimeter)
  
  // 计算矩形面积：length * width
  let area = multiply(length, width)
  assert_eq(48, area)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "edge_cases_and_special_values" {
  // 测试边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 零值测试
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(max_val, add(0, max_val))
  assert_eq(min_val, add(0, min_val))
  
  // 单位元测试
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
}

test "real_world_application" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let discount = 50
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = multiply(item_price, quantity)
  
  // 应用折扣
  let discounted_total = add(subtotal, -discount)
  
  // 计算税费
  let tax = multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价
  let final_total = add(discounted_total, tax)
  
  assert_eq(608, final_total)  // (199*3 - 50) + (199*3 - 50)*8/100 = 547 + 43 = 590
}