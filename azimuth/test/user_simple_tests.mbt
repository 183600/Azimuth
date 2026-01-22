// 简化的测试用例
test "simple_add_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "simple_multiply_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "simple_greet_test" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "simple_divide_test" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
}

test "performance_stress_test" {
  // 性能压力测试 - 大量连续运算
  let mut result = 1
  result = @azimuth.add(result, 1)
  result = @azimuth.multiply(result, 2)
  result = @azimuth.add(result, 3)
  result = @azimuth.multiply(result, 4)
  result = @azimuth.add(result, 5)
  result = @azimuth.multiply(result, 6)
  result = @azimuth.add(result, 7)
  result = @azimuth.multiply(result, 8)
  result = @azimuth.add(result, 9)
  result = @azimuth.multiply(result, 10)
  @azimuth.assert_eq(3628800, result)
}

test "function_composition_complex" {
  // 复杂函数组合测试
  let base_value = 100
  
  // 链式运算：(base + 50) * 2 - 30
  let step1 = @azimuth.add(base_value, 50)
  let step2 = @azimuth.multiply(step1, 2)
  let step3 = @azimuth.add(step2, -30)
  @azimuth.assert_eq(220, step3)
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法的边界测试
  // 除零保护
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 特殊余数情况
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(5, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-3, 2))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  // 长字符串处理
  let long_string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  let long_result = @azimuth.greet(long_string)
  @azimuth.assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", long_result)
  
  // 特殊空白字符组合
  @azimuth.assert_eq_string("Hello, \t\n\r !", @azimuth.greet("\t\n\r "))
  
  // Unicode表情符号
  @azimuth.assert_eq_string("Hello, 🚀🌙⭐!", @azimuth.greet("🚀🌙⭐"))
  
  // 混合内容字符串
  @azimuth.assert_eq_string("Hello, Test123中文!@#!", @azimuth.greet("Test123中文!@#"))
}

test "real_world_business_scenarios" {
  // 真实业务场景测试
  // 场景1：计算订单总价（商品价格 × 数量 + 运费 - 折扣）
  let item_price = 199
  let quantity = 3
  let shipping = 25
  let discount = -50
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total_with_shipping = @azimuth.add(subtotal, shipping)
  let final_total = @azimuth.add(total_with_shipping, discount)
  @azimuth.assert_eq(622, final_total)
  
  // 场景2：计算员工月薪（基本工资 + 加班费 - 扣款）
  let base_salary = 5000
  let overtime_pay = 800
  let deductions = -300
  
  let gross_pay = @azimuth.add(base_salary, overtime_pay)
  let net_pay = @azimuth.add(gross_pay, deductions)
  @azimuth.assert_eq(5500, net_pay)
  
  // 场景3：个性化用户问候
  let user_role = "管理员"
  let user_name = "张三"
  let full_title = user_role + " " + user_name
  let greeting = @azimuth.greet(full_title)
  @azimuth.assert_eq_string("Hello, 管理员 张三!", greeting)
}