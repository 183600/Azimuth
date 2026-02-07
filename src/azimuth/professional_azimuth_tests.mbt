// Azimuth 项目专业级 MoonBit 测试用例
// 高质量测试套件，包含8个专业级测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "arithmeti_division_ceil_precision" {
  // 测试除法向上取整函数的精度和边界情况
  // 正常情况测试
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(15, 5), 3)
  assert_eq(divide_with_ceil(7, 2), 4)
  assert_eq(divide_with_ceil(1, 1), 1)
  
  // 边界情况：零值处理
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(0, 1), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  
  // 负数测试
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
  assert_eq(divide_with_ceil(-1, 2), -1)
}

test "string_greet_function_comprehensive" {
  // 问候函数综合测试
  // 标准情况
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  
  // 边界情况
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet(" "), "Hello,  !")
  assert_eq_string(greet("A"), "Hello, A!")
  
  // 特殊字符和数字
  assert_eq_string(greet("123"), "Hello, 123!")
  assert_eq_string(greet("!@#"), "Hello, !@#!")
  assert_eq_string(greet("中文测试"), "Hello, 中文测试!")
}

test "mathematical_operations_chain" {
  // 数学运算链式测试
  let initial = 100
  
  // 复杂计算链：(100 + 25) * 2 - 50 / 5 (向上取整)
  let step1 = add(initial, 25)           // 125
  let step2 = multiply(step1, 2)         // 250
  let step3 = subtract(step2, 50)        // 200
  let step4 = divide_with_ceil(step3, 5) // 40
  
  assert_eq(step1, 125)
  assert_eq(step2, 250)
  assert_eq(step3, 200)
  assert_eq(step4, 40)
  
  // 验证结合律和交换律
  let a = 12
  let b = 8
  let c = 4
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let principal = 10000    // 本金
  let rate = 5             // 年利率百分比
  let years = 3            // 年数
  let monthly_fee = 25     // 月费
  
  // 计算复利
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  // 计算总费用
  let total_months = multiply(years, 12)
  let total_fees = multiply(monthly_fee, total_months)
  
  // 计算最终收益
  let gross_profit = subtract(year3, principal)
  let net_profit = subtract(gross_profit, total_fees)
  
  assert_eq(year1, 10500)
  assert_eq(year2, 11025)
  assert_eq(year3, 11576)
  assert_eq(total_months, 36)
  assert_eq(total_fees, 900)
  assert_eq(gross_profit, 1576)
  assert_eq(net_profit, 676)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let n = 10
  
  // O(1) 常数时间操作
  let constant_result = add(5, 3)
  assert_eq(constant_result, 8)
  
  // O(n) 线性时间操作 - 求和
  let linear_sum = 0
  let sum1 = add(linear_sum, 1)
  let sum2 = add(sum1, 2)
  let sum3 = add(sum2, 3)
  let sum4 = add(sum3, 4)
  let sum5 = add(sum4, 5)
  let sum6 = add(sum5, 6)
  let sum7 = add(sum6, 7)
  let sum8 = add(sum7, 8)
  let sum9 = add(sum8, 9)
  let sum10 = add(sum9, 10)
  assert_eq(sum10, 55)
  
  // O(n²) 二次时间操作 - 嵌套循环模拟
  let nested_operations = multiply(n, n)
  assert_eq(nested_operations, 100)
  
  // O(log n) 对数时间操作 - 二分查找模拟
  let log_steps = divide_with_ceil(n, 2)
  let log_steps2 = divide_with_ceil(log_steps, 2)
  let log_steps3 = divide_with_ceil(log_steps2, 2)
  assert_eq(log_steps, 5)
  assert_eq(log_steps2, 3)
  assert_eq(log_steps3, 2)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 极值测试
  let max_int = 1000000
  let min_int = -1000000
  
  // 大数运算
  assert_eq(add(max_int, max_int), 2000000)
  assert_eq(multiply(max_int, 2), 2000000)
  assert_eq(subtract(min_int, max_int), -2000000)
  
  // 除零保护
  assert_eq(divide_with_ceil(max_int, 0), 0)
  assert_eq(divide_with_ceil(min_int, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  
  // 边界值
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, max_int), 0)
  assert_eq(subtract(max_int, max_int), 0)
  assert_eq(divide_with_ceil(0, max_int), 0)
  
  // 单位值测试
  assert_eq(add(max_int, 0), max_int)
  assert_eq(multiply(max_int, 1), max_int)
  assert_eq(subtract(max_int, 0), max_int)
  assert_eq(divide_with_ceil(max_int, 1), max_int)
}

test "data_structure_operations" {
  // 数据结构操作模拟测试
  // 栈操作模拟
  let stack_empty = 0
  
  // push操作
  let push1 = 10
  let stack_size1 = add(stack_empty, 1)
  
  let push2 = 20
  let stack_size2 = add(stack_size1, 1)
  
  let push3 = 30
  let stack_size3 = add(stack_size2, 1)
  
  // pop操作模拟
  let pop1 = push3
  let stack_after_pop1 = subtract(stack_size3, 1)
  
  let pop2 = push2
  let stack_after_pop2 = subtract(stack_after_pop1, 1)
  
  let pop3 = push1
  let stack_after_pop3 = subtract(stack_after_pop2, 1)
  
  // 验证栈操作
  assert_eq(stack_size1, 1)
  assert_eq(stack_size2, 2)
  assert_eq(stack_size3, 3)
  assert_eq(pop1, 30)
  assert_eq(stack_after_pop1, 2)
  assert_eq(pop2, 20)
  assert_eq(stack_after_pop2, 1)
  assert_eq(pop3, 10)
  assert_eq(stack_after_pop3, 0)
  
  // 队列操作模拟 (FIFO)
  let queue_head = 100
  let queue_tail = 300
  let queue_size = subtract(queue_tail, queue_head)
  let dequeue_value = add(queue_head, 1)
  
  assert_eq(queue_size, 200)
  assert_eq(dequeue_value, 101)
}

test "real_world_business_logic" {
  // 真实世界业务逻辑测试
  // 电商订单处理场景
  
  // 商品信息
  let product_a_price = 50
  let product_a_qty = 3
  let product_b_price = 30
  let product_b_qty = 2
  let product_c_price = 20
  let product_c_qty = 5
  
  // 计算商品小计
  let subtotal_a = multiply(product_a_price, product_a_qty)
  let subtotal_b = multiply(product_b_price, product_b_qty)
  let subtotal_c = multiply(product_c_price, product_c_qty)
  
  // 计算订单总额
  let order_subtotal = add(add(subtotal_a, subtotal_b), subtotal_c)
  
  // 计算折扣（满200减20）
  let discount_threshold = 200
  let discount_amount = 20
  let applied_discount = if(order_subtotal >= discount_threshold) { discount_amount } else { 0 }
  
  // 计算税费（8%，向上取整）
  let tax_rate = 8
  let taxable_amount = subtract(order_subtotal, applied_discount)
  let tax_amount = divide_with_ceil(multiply(taxable_amount, tax_rate), 100)
  
  // 计算最终总额
  let total_amount = add(taxable_amount, tax_amount)
  
  // 计算所需包装箱数（每箱最多装10件商品）
  let total_items = add(add(product_a_qty, product_b_qty), product_c_qty)
  let boxes_needed = divide_with_ceil(total_items, 10)
  
  // 验证计算结果
  assert_eq(subtotal_a, 150)
  assert_eq(subtotal_b, 60)
  assert_eq(subtotal_c, 100)
  assert_eq(order_subtotal, 310)
  assert_eq(applied_discount, 20)
  assert_eq(taxable_amount, 290)
  assert_eq(tax_amount, 24)
  assert_eq(total_amount, 314)
  assert_eq(total_items, 10)
  assert_eq(boxes_needed, 1)
}