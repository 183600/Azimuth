// Azimuth 项目增强核心测试用例
// 覆盖核心数学函数、字符串处理和边界情况

test "basic_add_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_with_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值加法
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(0, add(0, 0))
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "basic_multiply_functionality" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(100, 0))
}

test "multiply_with_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试单位元
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  assert_eq(-42, multiply(42, -1))
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_variations" {
  // 测试greet函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算 (a + b) * (c - d)
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  
  let sum_ab = add(a, b)  // 15
  let diff_cd = add(c, -d)  // 12
  let result = multiply(sum_ab, diff_cd)  // 180
  
  assert_eq(180, result)
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
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "real_world_application" {
  // 测试实际应用场景：计算订单总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let discount = 50
  
  // 计算商品总价
  let subtotal = multiply(item_price, quantity)  // 597
  // 计算最终价格
  let final_total = add(add(subtotal, -discount), shipping)  // 562
  
  assert_eq(562, final_total)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与各种值的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(0, multiply(max_val, 0))
  
  // 最小值与各种值的运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(0, multiply(min_val, 0))
}