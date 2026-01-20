// 专注的综合测试用例
// 覆盖 azimuth 包的核心功能和边界情况

test "add_overflow_protection" {
  // 测试加法溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 负数溢出测试
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(min_val, add(-1500000000, -1000000000))
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 零值测试
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(0, multiply(0, 0))
  
  // 单位值测试
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(1, min_val))
  
  // 负一测试（包括最小值特殊情况）
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(-max_val, multiply(max_val, -1))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  let y = 17
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(y, add(0, y))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(y, multiply(1, y))
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 单字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 长字符串
  let long_name = "VeryLongNameThatTestsStringHandling"
  assert_eq_string("Hello, VeryLongNameThatTestsStringHandling!", greet(long_name))
  
  // 特殊字符
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  
  // Unicode字符
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算矩形周长 (2 * (length + width))
  let length = 15
  let width = 8
  let perimeter = multiply(2, add(length, width))
  assert_eq(46, perimeter)
  
  // 场景2：计算复合利息 principal * (1 + rate * years)
  let principal = 1000
  let rate = 5
  let years = 3
  let total = multiply(principal, add(1, multiply(rate, years)))
  assert_eq(16000, total)
  
  // 场景3：温度转换 Celsius * 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "negative_number_operations" {
  // 测试负数运算
  let a = -10
  let b = -20
  let c = 15
  
  // 负数加法
  assert_eq(-30, add(a, b))
  assert_eq(5, add(a, c))
  
  // 负数乘法
  assert_eq(200, multiply(a, b))
  assert_eq(-150, multiply(a, c))
  assert_eq(-300, multiply(b, c))
  
  // 混合符号运算
  assert_eq(-5, add(-10, 5))
  assert_eq(0, add(-20, 20))
}

test "sequential_operations" {
  // 测试连续运算
  // 连续加法：1 + 2 + 3 + 4 + 5
  let sum_result = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum_result)
  
  // 连续乘法：2 * 3 * 4
  let product_result = multiply(multiply(2, 3), 4)
  assert_eq(24, product_result)
  
  // 混合连续运算：(1 + 2) * (3 + 4)
  let mixed_result = multiply(add(1, 2), add(3, 4))
  assert_eq(21, mixed_result)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(max_val, add(min_val, 1)))
  
  // 边界值与普通值的运算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
  
  // 边界值乘法测试
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(0, multiply(max_val, 0))
}

test "assert_function_comprehensive" {
  // 综合测试断言函数
  // 基本断言
  assert_true(true)
  assert_false(false)
  
  // 数值比较断言
  assert_eq(0, 0)
  assert_eq(42, 42)
  assert_eq(-100, -100)
  
  // 字符串比较断言
  assert_eq_string("", "")
  assert_eq_string("test", "test")
  assert_eq_string("Hello, World!", greet("World"))
  
  // 布尔表达式断言
  assert_true(5 > 3)
  assert_true(10 >= 10)
  assert_false(3 > 5)
  assert_false(10 <= 9)
}

test "real_world_business_calculations" {
  // 实际业务计算测试
  // 订单计算：商品价格 × 数量 + 运费
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(617, total)
  
  // 折扣计算：总价 × (100 - 折扣率) / 100
  let original_total = 1000
  let discount_rate = 15
  let discounted_total = multiply(original_total, add(100, -discount_rate)) / 100
  assert_eq(850, discounted_total)
  
  // 税费计算：净价 × 税率 / 100
  let net_price = 850
  let tax_rate = 8
  let tax_amount = multiply(net_price, tax_rate) / 100
  let final_price = add(net_price, tax_amount)
  assert_eq(918, final_price)
}