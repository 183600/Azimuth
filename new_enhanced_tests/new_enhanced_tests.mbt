// 新增的 MoonBit 测试用例 - 针对 azimuth 库的增强测试
// 包含复杂业务逻辑和边界条件测试

// 测试用例 1: 布尔断言功能测试（使用 assert_eq 代替）
test "boolean_assertions_functionality" {
  // 测试布尔值比较（使用 assert_eq 代替 assert_true/assert_false）
  @azimuth.assert_eq(1, if 5 > 3 { 1 } else { 0 })
  @azimuth.assert_eq(1, if 0 == 0 { 1 } else { 0 })
  @azimuth.assert_eq(1, if -10 < 0 { 1 } else { 0 })
  @azimuth.assert_eq(5, @azimuth.add(2, 3))  // 验证 add 函数返回期望值
  
  // 测试假值条件
  @azimuth.assert_eq(0, if 5 < 3 { 1 } else { 0 })
  @azimuth.assert_eq(0, if 1 == 0 { 1 } else { 0 })
  @azimuth.assert_eq(1, if 5 >= 3 { 1 } else { 0 })  // 使用 >= 操作符
}

// 测试用例 2: 复杂数学恒等式验证
test "advanced_mathematical_identities" {
  // 测试平方差公式: a² - b² = (a+b)(a-b)
  let a = 7
  let b = 3
  let left_side = @azimuth.add(@azimuth.multiply(a, a), -@azimuth.multiply(b, b))
  let right_side = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(a, -b))
  @azimuth.assert_eq(left_side, right_side)  // 49 - 9 = 40 = 10 * 4
  
  // 测试三项式平方: (a+b+c)² = a² + b² + c² + 2ab + 2ac + 2bc
  let x = 2
  let y = 3
  let z = 4
  let trinomial_square = @azimuth.multiply(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(@azimuth.add(x, y), z))
  let expanded = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(x, x), @azimuth.multiply(y, y)), @azimuth.multiply(z, z)), 
                   @azimuth.add(@azimuth.add(@azimuth.multiply(2, @azimuth.multiply(x, y)), @azimuth.multiply(2, @azimuth.multiply(x, z))), 
                        @azimuth.multiply(2, @azimuth.multiply(y, z))))
  @azimuth.assert_eq(trinomial_square, expanded)  // (2+3+4)² = 81
}

// 测试用例 3: 质数相关计算
test "prime_number_operations" {
  // 测试质数判断相关的计算
  let prime1 = 7
  let prime2 = 13
  let composite = 15
  
  // 验证质数的特性（简化测试）
  @azimuth.assert_eq(1, if prime1 > 1 { 1 } else { 0 })
  @azimuth.assert_eq(1, if prime2 > 1 { 1 } else { 0 })
  @azimuth.assert_eq(1, if composite > 1 { 1 } else { 0 })
  
  // 测试质数乘积
  let prime_product = @azimuth.multiply(prime1, prime2)
  @azimuth.assert_eq(91, prime_product)  // 7 * 13 = 91
  
  // 测试因数分解相关
  @azimuth.assert_eq(0, if prime_product % prime1 == 0 { 0 } else { 1 })  // 0 表示能整除
  @azimuth.assert_eq(0, if prime_product % prime2 == 0 { 0 } else { 1 })
}

// 测试用例 4: 字符串处理高级测试
test "advanced_string_processing" {
  // 测试包含特殊字符的字符串处理
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let greeting_special = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", greeting_special)
  
  // 测试包含数字和字母混合的字符串
  let mixed_string = "User123_Admin456"
  let greeting_mixed = @azimuth.greet(mixed_string)
  @azimuth.assert_eq_string("Hello, User123_Admin456!", greeting_mixed)
  
  // 测试包含空格的复杂字符串
  let complex_string = "John Doe Jr."
  let greeting_complex = @azimuth.greet(complex_string)
  @azimuth.assert_eq_string("Hello, John Doe Jr.!", greeting_complex)
}

// 测试用例 5: 极端边界条件测试
test "extreme_boundary_conditions" {
  // 测试接近整数边界的运算
  let near_max_int = 2147483640
  let near_min_int = -2147483640
  
  // 安全的边界运算
  @azimuth.assert_eq(2147483641, @azimuth.add(near_max_int, 1))
  @azimuth.assert_eq(-2147483641, @azimuth.add(near_min_int, -1))
  
  // 测试大数除法的精度
  let large_dividend = 1000000
  let large_divisor = 999
  let division_result = @azimuth.divide_with_ceil(large_dividend, large_divisor)
  @azimuth.assert_eq(1002, division_result)  // ceil(1000000/999) = 1002
  
  // 测试负大数运算
  let large_negative = -1000000
  let negative_result = @azimuth.divide_with_ceil(large_negative, large_divisor)
  @azimuth.assert_eq(-1001, negative_result)  // ceil(-1000000/999) = -1001
}

// 测试用例 6: 复杂业务逻辑模拟
test "complex_business_logic_simulation" {
  // 模拟电商平台的订单处理逻辑
  let product_price = 299
  let quantity = 3
  let tax_rate = 8  // 8%
  let shipping_threshold = 500  // 免运费门槛
  let shipping_cost = 25
  let member_discount = 20
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(product_price, quantity)
  
  // 计算税费
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 判断是否免运费
  let has_free_shipping = if subtotal >= shipping_threshold { 1 } else { 0 }
  let actual_shipping = if has_free_shipping == 1 { 0 } else { shipping_cost }
  
  // 计算会员折扣后的总价
  let pre_discount_total = @azimuth.add(@azimuth.add(subtotal, tax_amount), actual_shipping)
  let final_total = @azimuth.add(pre_discount_total, -member_discount)
  
  // 验证计算结果
  @azimuth.assert_eq(897, subtotal)  // 299 * 3
  @azimuth.assert_eq(72, tax_amount)  // ceil(897 * 8 / 100)
  @azimuth.assert_eq(1, has_free_shipping)  // 897 >= 500
  @azimuth.assert_eq(0, actual_shipping)  // 免运费
  @azimuth.assert_eq(949, final_total)  // 897 + 72 - 20
}

// 测试用例 7: 数据结构和算法模拟
test "data_structure_algorithm_simulation" {
  // 模拟栈操作的压栈和弹栈
  let stack_initial_size = 0
  let push_operations = 5
  let pop_operations = 3
  
  // 压栈操作
  let stack_after_push = @azimuth.add(stack_initial_size, push_operations)
  
  // 弹栈操作
  let stack_final_size = @azimuth.add(stack_after_push, -pop_operations)
  
  // 验证栈操作结果
  @azimuth.assert_eq(5, stack_after_push)
  @azimuth.assert_eq(2, stack_final_size)
  @azimuth.assert_eq(1, if stack_final_size >= 0 { 1 } else { 0 })
  
  // 模拟队列的入队和出队
  let queue_capacity = 10
  let current_size = 7
  let new_items = 4
  let processed_items = 3
  
  // 计算队列状态
  let size_after_enqueue = @azimuth.add(current_size, new_items)
  let overflow = if size_after_enqueue > queue_capacity { 1 } else { 0 }
  let final_queue_size = if overflow == 1 {
    queue_capacity  // 队列已满
  } else {
    @azimuth.add(size_after_enqueue, -processed_items)
  }
  
  @azimuth.assert_eq(11, size_after_enqueue)  // 7 + 4 = 11
  @azimuth.assert_eq(1, overflow)  // 11 > 10
  @azimuth.assert_eq(10, final_queue_size)  // 队列满容量
}

// 测试用例 8: 时间和日期计算模拟
test "time_date_calculation_simulation" {
  // 模拟工作时间计算
  let work_days_per_week = 5
  let weeks_per_month = 4
  let hours_per_day = 8
  let lunch_break_hours = 1
  
  // 计算月度有效工作时间
  let total_work_days = @azimuth.multiply(work_days_per_week, weeks_per_month)
  let daily_effective_hours = @azimuth.add(hours_per_day, -lunch_break_hours)
  let monthly_effective_hours = @azimuth.multiply(total_work_days, daily_effective_hours)
  
  // 计算项目完成时间
  let project_hours = 160
  let days_needed = @azimuth.divide_with_ceil(project_hours, daily_effective_hours)
  let weeks_needed = @azimuth.divide_with_ceil(days_needed, work_days_per_week)
  
  // 验证计算结果
  @azimuth.assert_eq(20, total_work_days)  // 5 * 4
  @azimuth.assert_eq(7, daily_effective_hours)  // 8 - 1
  @azimuth.assert_eq(140, monthly_effective_hours)  // 20 * 7
  @azimuth.assert_eq(23, days_needed)  // ceil(160/7)
  @azimuth.assert_eq(5, weeks_needed)  // ceil(23/5)
}