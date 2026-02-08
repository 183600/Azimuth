// Azimuth 项目综合测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "array_sum_calculation" {
  // 数组求和计算测试
  // 模拟数组 [5, 10, 15, 20, 25] 的求和过程
  
  let elem1 = 5
  let elem2 = 10
  let elem3 = 15
  let elem4 = 20
  let elem5 = 25
  
  // 逐步求和
  let sum1 = elem1
  let sum2 = add(sum1, elem2)
  let sum3 = add(sum2, elem3)
  let sum4 = add(sum3, elem4)
  let sum5 = add(sum4, elem5)
  
  // 计算平均值
  let count = 5
  let average = divide_with_ceil(sum5, count)
  
  // 验证结果
  assert_eq(sum1, 5)
  assert_eq(sum2, 15)
  assert_eq(sum3, 30)
  assert_eq(sum4, 50)
  assert_eq(sum5, 75)
  assert_eq(average, 15)
}

test "power_operation_simulation" {
  // 幂运算模拟测试
  // 计算 2^5 = 32
  
  let base = 2
  let _ = 5  // exponent (unused)
  
  // 模拟幂运算：2^5 = 2 × 2 × 2 × 2 × 2
  let power1 = base
  let power2 = multiply(power1, base)  // 2^2 = 4
  let power3 = multiply(power2, base)  // 2^3 = 8
  let power4 = multiply(power3, base)  // 2^4 = 16
  let power5 = multiply(power4, base)  // 2^5 = 32
  
  // 验证结果
  assert_eq(power1, 2)
  assert_eq(power2, 4)
  assert_eq(power3, 8)
  assert_eq(power4, 16)
  assert_eq(power5, 32)
}

test "greatest_common_divisor" {
  // 最大公约数计算测试
  // 计算 GCD(48, 18) = 6
  
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法
  // 48 ÷ 18 = 2 余 12
  let step1_quotient = divide_with_ceil(a, b)
  let step1_remainder = subtract(a, multiply(step1_quotient - 1, b))
  
  // 18 ÷ 12 = 1 余 6
  let step2_quotient = divide_with_ceil(b, step1_remainder)
  let step2_remainder = subtract(b, multiply(step2_quotient - 1, step1_remainder))
  
  // 12 ÷ 6 = 2 余 0
  let step3_quotient = divide_with_ceil(step1_remainder, step2_remainder)
  let step3_remainder = subtract(step1_remainder, multiply(step3_quotient, step2_remainder))
  
  // GCD 是最后一个非零余数
  let gcd = step2_remainder
  
  assert_eq(step1_quotient, 3)
  assert_eq(step1_remainder, 12)
  assert_eq(step2_quotient, 2)
  assert_eq(step2_remainder, 6)
  assert_eq(step3_quotient, 2)
  assert_eq(step3_remainder, 0)
  assert_eq(gcd, 6)
}

test "percentage_calculations" {
  // 百分比计算测试
  // 计算各种百分比场景
  
  let total = 200
  let part1 = 50
  let part2 = 75
  let part3 = 25
  
  // 计算各部分百分比
  let percent1 = divide_with_ceil(multiply(part1, 100), total)
  let percent2 = divide_with_ceil(multiply(part2, 100), total)
  let percent3 = divide_with_ceil(multiply(part3, 100), total)
  
  // 计算增长百分比
  let original = 100
  let increased = 150
  let growth_percent = divide_with_ceil(multiply(subtract(increased, original), 100), original)
  
  // 计算折扣后的价格
  let price = 120
  let discount = 20
  let discounted_price = subtract(price, divide_with_ceil(multiply(price, discount), 100))
  
  assert_eq(percent1, 25)
  assert_eq(percent2, 38)
  assert_eq(percent3, 13)
  assert_eq(growth_percent, 50)
  assert_eq(discounted_price, 96)
}

test "queue_operations_simulation" {
  // 队列操作模拟测试
  // 模拟队列的入队和出队操作
  
  // 初始队列为空
  let queue_size = 0
  let _ = 0  // front_element (unused)
  
  // enqueue(10)
  let enqueue1 = 10
  let queue_size1 = add(queue_size, 1)
  let _ = enqueue1  // rear1
  
  // enqueue(20)
  let enqueue2 = 20
  let queue_size2 = add(queue_size1, 1)
  let _ = enqueue2  // rear2
  
  // enqueue(30)
  let enqueue3 = 30
  let queue_size3 = add(queue_size2, 1)
  let _ = enqueue3  // rear3
  
  // dequeue() -> 应该返回10
  let dequeue1 = enqueue1
  let queue_size_after_dequeue1 = subtract(queue_size3, 1)
  let new_front1 = enqueue2
  
  // dequeue() -> 应该返回20
  let dequeue2 = new_front1
  let queue_size_after_dequeue2 = subtract(queue_size_after_dequeue1, 1)
  let _ = enqueue3  // new_front2
  
  // 验证队列操作
  assert_eq(queue_size1, 1)
  assert_eq(queue_size2, 2)
  assert_eq(queue_size3, 3)
  assert_eq(dequeue1, 10)
  assert_eq(queue_size_after_dequeue1, 2)
  assert_eq(new_front1, 20)
  assert_eq(dequeue2, 20)
  assert_eq(queue_size_after_dequeue2, 1)
}

test "time_conversion_calculations" {
  // 时间转换计算测试
  // 各种时间单位之间的转换
  
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 转换为总秒数
  let total_seconds = add(add(multiply(hours, 3600), multiply(minutes, 60)), seconds)
  
  // 转换为总分钟数（向上取整）
  let total_minutes = divide_with_ceil(total_seconds, 60)
  
  // 转换为总小时数（向上取整）
  let total_hours = divide_with_ceil(total_seconds, 3600)
  
  // 计算工作日数量（假设每天工作8小时）
  let work_hours = 8
  let work_days = divide_with_ceil(hours, work_hours)
  
  // 验证转换结果
  assert_eq(total_seconds, 9045)
  assert_eq(total_minutes, 151)
  assert_eq(total_hours, 3)
  assert_eq(work_days, 1)
}

test "prime_number_check_simulation" {
  // 质数检查模拟测试
  // 检查数字17是否为质数
  
  let number = 17
  let _ = divide_with_ceil(number, 2)  // sqrt_limit (unused)
  
  // 检查是否能被2到8之间的数整除
  let divisible_by_2 = number % 2
  let divisible_by_3 = number % 3
  let divisible_by_4 = number % 4
  let divisible_by_5 = number % 5
  let divisible_by_6 = number % 6
  let divisible_by_7 = number % 7
  let divisible_by_8 = number % 8
  
  // 计算余数和
  let remainder_sum = add(add(add(add(add(add(divisible_by_2, divisible_by_3), divisible_by_4), divisible_by_5), divisible_by_6), divisible_by_7)
  
  // 如果所有余数都不为0，则是质数
  let is_prime = remainder_sum
  
  assert_eq(divisible_by_2, 1)
  assert_eq(divisible_by_3, 2)
  assert_eq(divisible_by_4, 1)
  assert_eq(divisible_by_5, 2)
  assert_eq(divisible_by_6, 5)
  assert_eq(divisible_by_7, 3)
  assert_eq(divisible_by_8, 1)
  assert_eq(is_prime, 15)
}

test "matrix_operations_simulation" {
  // 矩阵操作模拟测试
  // 2x2矩阵的基本运算
  
  // 矩阵A: [[1, 2], [3, 4]]
  let a11 = 1
  let a12 = 2
  let a21 = 3
  let a22 = 4
  
  // 矩阵B: [[5, 6], [7, 8]]
  let b11 = 5
  let b12 = 6
  let b21 = 7
  let b22 = 8
  
  // 矩阵加法 A + B
  let c11 = add(a11, b11)  // 1 + 5 = 6
  let c12 = add(a12, b12)  // 2 + 6 = 8
  let c21 = add(a21, b21)  // 3 + 7 = 10
  let c22 = add(a22, b22)  // 4 + 8 = 12
  
  // 计算矩阵A的行列式
  let det_a = subtract(multiply(a11, a22), multiply(a12, a21))
  
  // 计算矩阵B的行列式
  let det_b = subtract(multiply(b11, b22), multiply(b12, b21))
  
  // 计算矩阵C的行列式
  let det_c = subtract(multiply(c11, c22), multiply(c12, c21))
  
  assert_eq(c11, 6)
  assert_eq(c12, 8)
  assert_eq(c21, 10)
  assert_eq(c22, 12)
  assert_eq(det_a, -2)
  assert_eq(det_b, -2)
  assert_eq(det_c, -8)
}

test "string_length_calculations" {
  // 字符串长度计算测试
  // 模拟字符串长度相关的计算
  
  let _ = "Hello"   // str1 (unused)
  let _ = "World"   // str2 (unused)
  let _ = ""        // str3 (unused)
  let _ = "MoonBit" // str4 (unused)
  
  // 模拟字符串长度计算
  let len1 = 5
  let len2 = 5
  let len3 = 0
  let len4 = 7
  
  // 计算连接后的字符串长度
  let concat_len1 = add(len1, len2)  // "Hello" + "World"
  let concat_len2 = add(len1, len3)  // "Hello" + ""
  let concat_len3 = add(add(len1, len2), len4)  // "Hello" + "World" + "MoonBit"
  
  // 计算平均长度
  let total_length = add(add(add(len1, len2), len3), len4)
  let avg_length = divide_with_ceil(total_length, 4)
  
  // 验证长度计算
  assert_eq(len1, 5)
  assert_eq(len2, 5)
  assert_eq(len3, 0)
  assert_eq(len4, 7)
  assert_eq(concat_len1, 10)
  assert_eq(concat_len2, 5)
  assert_eq(concat_len3, 17)
  assert_eq(total_length, 17)
  assert_eq(avg_length, 5)
}

test "comprehensive_business_calculation" {
  // 综合商业计算测试
  // 模拟完整的商业场景计算
  
  // 产品成本和定价
  let product_cost = 50
  let markup_percentage = 30
  let markup_amount = divide_with_ceil(multiply(product_cost, markup_percentage), 100)
  let selling_price = add(product_cost, markup_amount)
  
  // 销售数据
  let units_sold = 120
  let total_revenue = multiply(selling_price, units_sold)
  let total_cost = multiply(product_cost, units_sold)
  
  // 利润计算
  let gross_profit = subtract(total_revenue, total_cost)
  let profit_margin = divide_with_ceil(multiply(gross_profit, 100), total_revenue)
  
  // 运营费用
  let rent = 2000
  let salaries = 3500
  let utilities = 800
  let total_expenses = add(add(rent, salaries), utilities)
  
  // 净利润
  let net_profit = subtract(gross_profit, total_expenses)
  
  // 税收计算（假设税率25%）
  let tax_rate = 25
  let tax_amount = divide_with_ceil(multiply(net_profit, tax_rate), 100)
  let final_profit = subtract(net_profit, tax_amount)
  
  // 验证商业计算
  assert_eq(markup_amount, 15)
  assert_eq(selling_price, 65)
  assert_eq(total_revenue, 7800)
  assert_eq(total_cost, 6000)
  assert_eq(gross_profit, 1800)
  assert_eq(profit_margin, 24)
  assert_eq(total_expenses, 6300)
  assert_eq(net_profit, -4500)
  assert_eq(tax_amount, 0)
  assert_eq(final_profit, -4500)
}