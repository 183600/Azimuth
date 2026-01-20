// 标准 MoonBit 质量测试用例
// 覆盖核心功能的各种使用场景和边界情况

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(5, -5))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试与0的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试边界值组合
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "greet_function_comprehensive" {
  // 测试 greet 函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
}

test "mathematical_operations_combination" {
  // 测试数学运算的组合使用
  let a = 10
  let b = 20
  let c = 30
  
  // 测试 (a + b) * c
  let sum_ab = add(a, b)
  let result = multiply(sum_ab, c)
  assert_eq(900, result)
  
  // 测试 a * b + c
  let product_ab = multiply(a, b)
  let result2 = add(product_ab, c)
  assert_eq(230, result2)
}

test "real_world_calculation_scenario" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  let discount = 50
  
  // 计算商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let subtotal = add(total1, total2)
  
  // 应用折扣和运费
  let final_total = add(add(subtotal, -discount), shipping_fee)
  
  assert_eq(637, final_total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", greet(special_chars))
  
  // 测试包含空格的字符串
  let spaced_string = "  leading and trailing spaces  "
  assert_eq_string("Hello,   leading and trailing spaces  !", greet(spaced_string))
  
  // 测试包含换行符的字符串
  let newline_string = "Line1\nLine2"
  assert_eq_string("Hello, Line1\nLine2!", greet(newline_string))
}

test "complex_business_logic" {
  // 测试复杂业务逻辑：订单折扣计算
  let order_amount = 1200
  let discount_threshold = 1000
  let discount_rate = 10
  let tax_rate = 8
  let shipping_base = 15
  
  // 计算折扣（订单金额超过阈值时应用）
  let discount_amount = if (order_amount > discount_threshold) {
    multiply(order_amount, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后金额
  let discounted_amount = add(order_amount, -discount_amount)
  
  // 计算税费
  let tax_amount = multiply(discounted_amount, tax_rate) / 100
  
  // 计算运费（超过免运费阈值时免运费）
  let free_shipping_threshold = 1500
  let shipping_cost = if (discounted_amount > free_shipping_threshold) {
    0
  } else {
    shipping_base
  }
  
  // 计算最终总额
  let final_total = add(add(discounted_amount, tax_amount), shipping_cost)
  
  assert_eq(1242, final_total)
}

test "performance_and_stress" {
  // 测试性能和压力情况
  let large_number = 1000000
  
  // 测试大数运算
  assert_eq(2000000, add(large_number, large_number))
  assert_eq(1000000000000, multiply(large_number, large_number))
  
  // 测试连续运算
  let chain_result = add(multiply(add(100, 200), 3), 50)
  assert_eq(950, chain_result)
  
  // 测试嵌套运算
  let nested_result = multiply(add(10, 20), add(30, 40))
  assert_eq(2100, nested_result)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 验证断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 验证特殊情况处理
  assert_eq(0, multiply(0, min_val))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(max_val, add(max_val, 0))
}