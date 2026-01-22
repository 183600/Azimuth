// 新增的标准 MoonBit 测试用例
// 这些测试用例补充现有测试，提供更全面的覆盖

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
  
  // 嵌套运算：((10 + 20) * (5 + 3)) + 15
  let inner_sum1 = @azimuth.add(10, 20)
  let inner_sum2 = @azimuth.add(5, 3)
  let product = @azimuth.multiply(inner_sum1, inner_sum2)
  let final_result = @azimuth.add(product, 15)
  @azimuth.assert_eq(175, final_result)
}

test "divide_with_ceil_edge_cases_comprehensive" {
  // 向上取整除法的综合边界测试
  // 除零保护
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(max_val, 1))
  @azimuth.assert_eq(1073741824, @azimuth.divide_with_ceil(max_val, 2))
  @azimuth.assert_eq(-2147483648, @azimuth.divide_with_ceil(min_val, 1))
  @azimuth.assert_eq(-1073741824, @azimuth.divide_with_ceil(min_val, 2))
  
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
  
  // 数字和符号组合
  @azimuth.assert_eq_string("Hello, 123-456-7890!", @azimuth.greet("123-456-7890"))
}

test "arithmetic_properties_verification" {
  // 算术性质验证测试
  // 分配律测试：(a + b) * c = a * c + b * c
  let a = 12
  let b = 8
  let c = 5
  
  let left_side = @azimuth.multiply(@azimuth.add(a, b), c)
  let right_side = @azimuth.add(@azimuth.multiply(a, c), @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 结合律测试：(a + b) + c = a + (b + c)
  let x = 15
  let y = 25
  let z = 35
  
  let add_left = @azimuth.add(@azimuth.add(x, y), z)
  let add_right = @azimuth.add(x, @azimuth.add(y, z))
  @azimuth.assert_eq(add_left, add_right)
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  let mult_left = @azimuth.multiply(@azimuth.multiply(x, y), z)
  let mult_right = @azimuth.multiply(x, @azimuth.multiply(y, z))
  @azimuth.assert_eq(mult_left, mult_right)
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
  
  // 场景3：计算项目工期（工作日数 + 假期天数）
  let work_days = 20
  let holidays = 3
  let buffer_days = 2
  
  let base_duration = @azimuth.add(work_days, holidays)
  let total_duration = @azimuth.add(base_duration, buffer_days)
  @azimuth.assert_eq(25, total_duration)
  
  // 场景4：个性化用户问候
  let user_role = "管理员"
  let user_name = "张三"
  let full_title = user_role + " " + user_name
  let greeting = @azimuth.greet(full_title)
  @azimuth.assert_eq_string("Hello, 管理员 张三!", greeting)
}

test "boundary_combination_stress" {
  // 边界组合压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 组合1：最大值与最小值的运算
  let sum1 = @azimuth.add(max_val, min_val)
  let product1 = @azimuth.multiply(max_val, 1)
  let product2 = @azimuth.multiply(min_val, 1)
  @azimuth.assert_eq(-1, sum1)
  @azimuth.assert_eq(max_val, product1)
  @azimuth.assert_eq(min_val, product2)
  
  // 组合2：连续边界运算
  let step1 = @azimuth.add(max_val - 1000, 500)
  let step2 = @azimuth.add(step1, 500)
  let step3 = @azimuth.multiply(step2, 1)
  @azimuth.assert_eq(max_val, step3)
  
  // 组合3：边界值与中间值的混合运算
  let mid_val = 1000000
  let mixed_sum = @azimuth.add(@azimuth.add(max_val - mid_val, mid_val), 0)
  let mixed_product = @azimuth.multiply(@azimuth.divide_with_ceil(mid_val, 1000), 1000)
  @azimuth.assert_eq(max_val, mixed_sum)
  @azimuth.assert_eq(1000000, mixed_product)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(min_val, 0))
  
  // 极端值组合的稳定性
  let extreme_sum1 = @azimuth.add(max_val, max_val)
  let extreme_sum2 = @azimuth.add(min_val, min_val)
  let extreme_product1 = @azimuth.multiply(max_val, max_val)
  let extreme_product2 = @azimuth.multiply(min_val, min_val)
  
  // 验证系统不会崩溃，而是返回合理的边界值
  @azimuth.assert_true(extreme_sum1 <= max_val)
  @azimuth.assert_true(extreme_sum2 >= min_val)
  @azimuth.assert_true(extreme_product1 <= max_val)
  @azimuth.assert_true(extreme_product2 >= min_val)
}