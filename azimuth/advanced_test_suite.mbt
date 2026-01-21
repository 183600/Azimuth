// 新的高级 MoonBit 测试用例
// 专注于性能、边界条件和实际应用场景

test "performance_stress_test" {
  // 性能压力测试：大量连续操作
  let base = 1000
  
  // 连续加法操作
  let mut sum = 0
  for i = 1; i <= 100; i = i + 1 {
    sum = add(sum, i)
  }
  assert_eq(5050, sum)
  
  // 连续乘法操作
  let mut product = 1
  for i = 1; i <= 5; i = i + 1 {
    product = multiply(product, i)
  }
  assert_eq(120, product)
  
  // 混合连续操作
  let mut result = base
  for i = 1; i <= 10; i = i + 1 {
    result = add(multiply(result, 2), divide_with_ceil(i, 3))
  }
  assert_eq(2047020, result)
}

test "extreme_boundary_combinations" {
  // 极端边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值系列运算
  let result1 = add(max_val, min_val)
  let result2 = multiply(max_val, 0)
  let result3 = multiply(max_val, 1)
  let result4 = multiply(max_val, -1)
  
  assert_eq(-1, result1)
  assert_eq(0, result2)
  assert_eq(max_val, result3)
  assert_eq(-2147483647, result4)
  
  // 最小值系列运算
  let result5 = add(min_val, max_val)
  let result6 = multiply(min_val, 0)
  let result7 = multiply(min_val, 1)
  let result8 = multiply(min_val, -1)
  
  assert_eq(-1, result5)
  assert_eq(0, result6)
  assert_eq(min_val, result7)
  assert_eq(min_val, result8) // 特殊情况：最小值乘以-1仍为最小值
}

test "real_world_financial_calculations" {
  // 实际金融计算场景测试
  // 场景：计算复利
  let principal = 10000  // 本金
  let rate = 5           // 利率 20% (1/5)
  let years = 3          // 年数
  
  // 计算复利：principal * (1 + rate)^years
  let year1 = add(principal, divide_with_ceil(multiply(principal, 1), rate))
  let year2 = add(year1, divide_with_ceil(multiply(year1, 1), rate))
  let year3 = add(year2, divide_with_ceil(multiply(year2, 1), rate))
  
  assert_eq(12000, year1)  // 10000 + 2000
  assert_eq(14400, year2)  // 12000 + 2400
  assert_eq(17280, year3)  // 14400 + 2880
  
  // 场景：计算贷款分期
  let loan_amount = 50000
  let months = 12
  let monthly_payment = divide_with_ceil(loan_amount, months)
  let total_payment = multiply(monthly_payment, months)
  
  assert_eq(4167, monthly_payment)
  assert_eq(50004, total_payment)
}

test "advanced_string_processing" {
  // 高级字符串处理测试
  // 测试各种特殊字符和Unicode
  assert_eq_string("Hello, \n\t\r\0!", greet("\n\t\r\0"))
  assert_eq_string("Hello, 🚀🌟⭐!", greet("🚀🌟⭐"))
  assert_eq_string("Hello, 数学公式∑∏∫!", greet("数学公式∑∏∫"))
  assert_eq_string("Hello, Mixed123ABC!", greet("Mixed123ABC"))
  assert_eq_string("Hello, Special@#$%!", greet("Special@#$%"))
  
  // 测试空格和制表符组合
  assert_eq_string("Hello, \t   \t!", greet("\t   \t"))
  assert_eq_string("Hello, \n\n\n!", greet("\n\n\n"))
  
  // 测试引号和转义字符
  assert_eq_string("Hello, \"\'!", greet("\"\'"))
}

test "mathematical_sequence_validation" {
  // 数学序列验证测试
  // 斐波那契数列计算（简化版）
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  
  // 阶乘计算
  let fact1 = 1
  let fact2 = multiply(fact1, 2)
  let fact3 = multiply(fact2, 3)
  let fact4 = multiply(fact3, 4)
  let fact5 = multiply(fact4, 5)
  
  assert_eq(2, fact2)
  assert_eq(6, fact3)
  assert_eq(24, fact4)
  assert_eq(120, fact5)
  
  // 幂运算（通过重复乘法）
  let power2_3 = multiply(multiply(2, 2), 2)
  let power3_4 = multiply(multiply(multiply(3, 3), 3), 3)
  
  assert_eq(8, power2_3)
  assert_eq(81, power3_4)
}

test "error_boundary_and_recovery" {
  // 错误边界和恢复测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 触发各种溢出情况
  let overflow1 = add(max_val, 1000)
  let overflow2 = add(min_val, -1000)
  let overflow3 = multiply(max_val, 100)
  let overflow4 = multiply(min_val, 100)
  let overflow5 = multiply(-1, min_val)
  
  // 验证溢出后的正常操作
  let normal1 = add(100, 200)
  let normal2 = multiply(10, 20)
  let normal3 = divide_with_ceil(25, 4)
  let normal4 = greet("Recovery")
  
  assert_eq(max_val, overflow1)
  assert_eq(min_val, overflow2)
  assert_eq(max_val, overflow3)
  assert_eq(min_val, overflow4)
  assert_eq(min_val, overflow5)
  
  assert_eq(300, normal1)
  assert_eq(200, normal2)
  assert_eq(7, normal3)
  assert_eq_string("Hello, Recovery!", normal4)
}

test "precision_and_accuracy_validation" {
  // 精度和准确性验证测试
  // 除法精度测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(1, divide_with_ceil(3, 3))
  assert_eq(1, divide_with_ceil(100, 100))
  
  // 小数精度测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(1, divide_with_ceil(4, 5))
  
  // 大数精度测试
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(500000, divide_with_ceil(1000000, 2))
  assert_eq(333334, divide_with_ceil(1000000, 3))
  assert_eq(250000, divide_with_ceil(1000000, 4))
  
  // 负数精度测试
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "complex_business_logic" {
  // 复杂业务逻辑测试
  // 场景：电商订单计算
  let item_price = 199
  let quantity = 3
  let shipping_fee = 50
  let tax_rate = 10  // 10% 税率 (1/10)
  
  // 计算商品总价
  let subtotal = multiply(item_price, quantity)
  assert_eq(597, subtotal)
  
  // 计算税费
  let tax_amount = divide_with_ceil(subtotal, tax_rate)
  assert_eq(60, tax_amount)
  
  // 计算最终总价
  let total_before_shipping = add(subtotal, tax_amount)
  let final_total = add(total_before_shipping, shipping_fee)
  
  assert_eq(657, total_before_shipping)
  assert_eq(707, final_total)
  
  // 场景：库存管理
  let initial_stock = 1000
  let sales = multiply(50, 3)  // 50个单位，卖3次
  let returns = 15
  let new_stock = 200
  
  let current_stock = add(subtract(initial_stock, sales), returns)
  let final_stock = add(current_stock, new_stock)
  
  assert_eq(150, sales)
  assert_eq(865, current_stock)
  assert_eq(1065, final_stock)
}

// 辅助函数：减法（通过加负数实现）
fn subtract(a : Int, b : Int) -> Int {
  add(a, multiply(b, -1))
}