// Azimuth 库的质量测试套件 - 10个标准 MoonBit 测试用例
// 覆盖基本功能、边界情况、数学性质和实际应用场景

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合功能
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(0, 100))
  assert_eq(-24, multiply(-8, 3))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, multiply(1, 1))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))   // 10/3 = 3.33... -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.33... -> -3
  assert_eq(4, divide_with_ceil(-10, -3)) // -10/-3 = 3.33... -> 4
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为0的安全处理
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a * (b + c) = a * b + a * c
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 加法单位元
  assert_eq(a, add(a, 0))
  
  // 乘法单位元
  assert_eq(a, multiply(a, 1))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "packaging_optimization_scenario" {
  // 包装优化场景：计算最优包装方案
  let small_items = 15
  let medium_items = 23
  let large_items = 8
  
  let small_box_capacity = 6
  let medium_box_capacity = 4
  let large_box_capacity = 2
  
  // 计算每种尺寸需要的包装箱数量
  let small_boxes = divide_with_ceil(small_items, small_box_capacity)
  let medium_boxes = divide_with_ceil(medium_items, medium_box_capacity)
  let large_boxes = divide_with_ceil(large_items, large_box_capacity)
  
  // 计算总包装箱数量
  let total_boxes = add(add(small_boxes, medium_boxes), large_boxes)
  
  assert_eq(3, small_boxes)   // ceil(15/6) = 3
  assert_eq(6, medium_boxes)  // ceil(23/4) = 6
  assert_eq(4, large_boxes)   // ceil(8/2) = 4
  assert_eq(13, total_boxes)  // 3 + 6 + 4 = 13
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(0, -1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq_string("Hello, !", greet(""))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
}

test "real_world_financial_calculation" {
  // 实际世界金融计算：贷款分期计算
  let loan_amount = 10000
  let interest_rate = 5
  let loan_term_months = 24
  
  // 计算月利率（简化计算）
  let monthly_interest_rate = divide_with_ceil(interest_rate, 12)
  
  // 计算总利息（简化计算）
  let total_interest = divide_with_ceil(multiply(loan_amount, multiply(monthly_interest_rate, loan_term_months)), 100)
  
  // 计算总还款额
  let total_payment = add(loan_amount, total_interest)
  
  // 计算月还款额
  let monthly_payment = divide_with_ceil(total_payment, loan_term_months)
  
  assert_eq(1, monthly_interest_rate)  // ceil(5/12) = 1
  assert_eq(240, total_interest)      // ceil(10000 * 1 * 24 / 100) = 240
  assert_eq(10240, total_payment)     // 10000 + 240 = 10240
  assert_eq(427, monthly_payment)     // ceil(10240/24) = 427
}