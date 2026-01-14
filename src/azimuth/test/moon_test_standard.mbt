// 标准 MoonBit 测试用例集
// 包含10个精心设计的测试用例，覆盖各种场景

test "boundary_value_arithmetic" {
  // 测试边界值算术运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的加法
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试最大值与自身相加（应溢出）
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 测试最小值与自身相加（应溢出）
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_processing_comprehensive" {
  // 测试全面的字符串处理
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 15
  let b = 7
  let c = 3
  
  // 验证加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 验证乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
  
  // 验证结合律
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(a, b), c),
    @azimuth.add(a, @azimuth.add(b, c))
  )
}

test "real_world_currency_conversion" {
  // 测试实际世界的货币转换场景
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY (简化汇率)
  let transaction_fee = 5  // 5元手续费
  
  // 转换为人民币
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)
  
  // 加上手续费
  let total_cost = @azimuth.add(cny_amount, transaction_fee)
  
  @azimuth.assert_eq(705, total_cost)
  
  // 测试反向转换（包含整数除法）
  let received_cny = 1400
  let fee_deducted = @azimuth.add(received_cny, -transaction_fee)
  let usd_received = fee_deducted / exchange_rate
  
  @azimuth.assert_eq(199, usd_received)
}

test "error_handling_and_overflow" {
  // 测试错误处理和溢出情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -100) == min_val)
  
  // 测试乘法溢出保护
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
  
  // 测试特殊值处理
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 验证正常情况仍然工作
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "performance_large_data_calculations" {
  // 测试大数据量的性能计算
  let base_value = 1000
  
  // 计算前100个自然数的和：1 + 2 + ... + 100 = 5050
  // 使用公式：n × (n + 1) / 2
  let n = 100
  let sum_formula = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, sum_formula)
  
  // 计算等比数列：2^0 + 2^1 + 2^2 + 2^3 + 2^4 = 31
  let power_0 = 1
  let power_1 = @azimuth.multiply(2, power_0)
  let power_2 = @azimuth.multiply(2, power_1)
  let power_3 = @azimuth.multiply(2, power_2)
  let power_4 = @azimuth.multiply(2, power_3)
  
  let geometric_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(power_0, power_1), power_2), power_3), power_4)
  @azimuth.assert_eq(31, geometric_sum)
}

test "compound_function_operations" {
  // 测试复合函数运算
  // 计算：(a + b) × (c - d) + e × f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  // 步骤计算
  let sum_ab = @azimuth.add(a, b)  // 15
  let diff_cd = @azimuth.add(c, -d)  // 12
  let product_ab_cd = @azimuth.multiply(sum_ab, diff_cd)  // 180
  let product_ef = @azimuth.multiply(e, f)  // 21
  let final_result = @azimuth.add(product_ab_cd, product_ef)  // 201
  
  @azimuth.assert_eq(201, final_result)
  
  // 测试嵌套函数调用
  let nested_result = @azimuth.add(
    @azimuth.multiply(@azimuth.add(2, 3), @azimuth.add(4, 5)),
    @azimuth.multiply(6, 7)
  )
  @azimuth.assert_eq(77, nested_result)  // (2+3)*(4+5) + 6*7 = 5*9 + 42 = 45 + 42 = 87
}

test "conditional_logic_calculations" {
  // 测试条件逻辑计算
  let x = 15
  let y = 20
  let z = 25
  
  // 条件1：如果 x < y，计算 x + y，否则计算 x * y
  let condition1 = x < y
  let result1 = if condition1 { 
    @azimuth.add(x, y) 
  } else { 
    @azimuth.multiply(x, y) 
  }
  @azimuth.assert_eq(35, result1)
  
  // 条件2：如果 y > z，计算 y - z，否则计算 z - y
  let condition2 = y > z
  let result2 = if condition2 { 
    @azimuth.add(y, -z) 
  } else { 
    @azimuth.add(z, -y) 
  }
  @azimuth.assert_eq(5, result2)
  
  // 条件3：复合条件
  let condition3 = (x < y) && (y < z)
  let result3 = if condition3 { 
    @azimuth.add(@azimuth.add(x, y), z) 
  } else { 
    @azimuth.multiply(@azimuth.multiply(x, y), z) 
  }
  @azimuth.assert_eq(60, result3)
}

test "data_type_processing" {
  // 测试数据类型处理
  // 模拟整数到字符串的转换场景
  let year = 2023
  let month = 12
  let day = 25
  
  // 构建日期字符串
  let year_str = "2023"
  let month_str = "12"
  let day_str = "25"
  
  let date_string = year_str + "-" + month_str + "-" + day_str
  let greeting_with_date = @azimuth.greet(date_string)
  
  @azimuth.assert_eq_string("Hello, 2023-12-25!", greeting_with_date)
  
  // 模拟布尔值处理
  let is_active = true
  let status_message = if is_active { 
    @azimuth.greet("Active User") 
  } else { 
    @azimuth.greet("Inactive User") 
  }
  @azimuth.assert_eq_string("Hello, Active User!", status_message)
  
  // 测试数值验证场景
  let age = 25
  let age_category = if age < 18 { 
    "Minor" 
  } else if age < 65 { 
    "Adult" 
  } else { 
    "Senior" 
  }
  let category_greeting = @azimuth.greet(age_category)
  @azimuth.assert_eq_string("Hello, Adult!", category_greeting)
}

test "comprehensive_application_scenario" {
  // 测试综合应用场景：简化订单处理系统
  let unit_price = 199
  let quantity = 3
  let discount_threshold = 500
  let discount_rate = 10
  let tax_rate = 8
  let shipping_fee = 20
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 判断是否符合折扣条件
  let qualifies_for_discount = subtotal > discount_threshold
  let discount_amount = if qualifies_for_discount {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折扣后价格
  let discounted_total = @azimuth.add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax_amount), shipping_fee)
  
  @azimuth.assert_eq(674, final_total)
  
  // 验证折扣应用
  @azimuth.assert_true(qualifies_for_discount)
  @azimuth.assert_true(discount_amount > 0)
  
  // 测试小额订单（无折扣）
  let small_unit_price = 50
  let small_quantity = 2
  let small_subtotal = @azimuth.multiply(small_unit_price, small_quantity)
  let small_qualifies_for_discount = small_subtotal > discount_threshold
  let small_discount_amount = if small_qualifies_for_discount {
    @azimuth.multiply(small_subtotal, discount_rate) / 100
  } else {
    0
  }
  let small_final_total = @azimuth.add(@azimuth.add(small_subtotal, small_discount_amount), @azimuth.multiply(small_subtotal, tax_rate) / 100)
  
  @azimuth.assert_eq(127, small_final_total)
  @azimuth.assert_false(small_qualifies_for_discount)
  @azimuth.assert_eq(0, small_discount_amount)
}