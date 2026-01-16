// 核心功能综合测试
// 测试 Azimuth 项目的主要功能模块

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 10
  let b = 15
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 5)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 5))
  )
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 12
  let width = 8
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(40, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：简单利息计算
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 计算利息：本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  @azimuth.assert_eq(150000, interest)
  
  // 计算总额：本金 + 利息
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 测试断言函数
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "real_world_application_test" {
  // 测试实际应用场景：购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let shipping = 20
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let subtotal = @azimuth.add(total1, total2)
  let final_total = @azimuth.add(subtotal, shipping)
  
  // 验证结果：299*2 + 199*3 + 20 = 598 + 597 + 20 = 1215
  @azimuth.assert_eq(1215, final_total)
}