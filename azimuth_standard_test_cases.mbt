// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能测试

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(8, -3))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 空格测试 !", greet(" 空格测试 "))
  assert_eq_string("Hello, 特殊符号@#$!", greet("特殊符号@#$"))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算订单总价（商品价格 × 数量 + 运费 - 折扣）
  let product_price = 158
  let quantity = 3
  let shipping = 25
  let discount = 50
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(499, final_total)  // 158*3 + 25 - 50 = 474 + 25 - 50 = 449
}

test "mathematical_identity_properties" {
  // 数学恒等性质测试
  // 加法恒等元
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  
  // 乘法恒等元
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  
  // 乘法零元
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(0, 42))
}

test "packaging_and_shipping_calculation" {
  // 包装和运输计算测试
  // 场景：计算需要的包装箱数量和总运输成本
  let total_items = 47
  let items_per_box = 8
  let cost_per_box = 15
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let total_shipping_cost = multiply(boxes_needed, cost_per_box)
  
  assert_eq(6, boxes_needed)  // ceil(47/8) = 6
  assert_eq(90, total_shipping_cost)  // 6 * 15 = 90
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：计算投资回报（本金 × 利率 + 本金）
  let principal = 10000
  let interest_rate = 8  // 8%
  let years = 3
  
  let annual_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let total_interest = multiply(annual_interest, years)
  let final_amount = add(principal, total_interest)
  
  assert_eq(12400, final_amount)  // 10000 + (ceil(10000*8/100) * 3) = 10000 + 2400 = 12400
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除零处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(2000000000, add(1000000000, 1000000000))
  assert_eq(0, add(2147483647, -2147483647))
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数测试
  let names = ["", "A", "Alice", "Alice Smith", "张三", "José María", "🌟Star"]
  let expected = [
    "Hello, !",
    "Hello, A!",
    "Hello, Alice!",
    "Hello, Alice Smith!",
    "Hello, 张三!",
    "Hello, José María!",
    "Hello, 🌟Star!"
  ]
  
  // 测试各种名字输入
  assert_eq_string(expected[0], greet(names[0]))
  assert_eq_string(expected[1], greet(names[1]))
  assert_eq_string(expected[2], greet(names[2]))
  assert_eq_string(expected[3], greet(names[3]))
  assert_eq_string(expected[4], greet(names[4]))
}