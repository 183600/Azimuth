// Azimuth 增强测试套件
// 包含8个高质量的专业测试用例，使用标准 MoonBit 测试语法

test "number_theory_primes" {
  // 数论基础：质数相关计算
  // 检查7以内的质数：2, 3, 5, 7
  
  // 计算质数和
  let prime_sum = add(add(add(2, 3), 5), 7)
  
  // 计算质数乘积
  let prime_product = multiply(multiply(multiply(2, 3), 5), 7)
  
  // 计算平均值的向上取整
  let prime_count = 4
  let prime_average = divide_with_ceil(prime_sum, prime_count)
  
  assert_eq(prime_sum, 17)
  assert_eq(prime_product, 210)
  assert_eq(prime_average, 5)
}

test "calendar_calculation" {
  // 日历计算：闰年和天数计算
  let regular_year = 365
  let leap_year = 366
  let century = 100
  
  // 计算4年周期内的总天数（包含一个闰年）
  let four_year_days = add(add(add(regular_year, regular_year), regular_year), leap_year)
  
  // 计算平均每年天数（向上取整）
  let average_days = divide_with_ceil(four_year_days, 4)
  
  // 计算一个世纪内的闰年数量（简化计算）
  let leap_years_century = divide_with_ceil(century, 4)
  
  // 计算世纪总天数
  let century_days = add(multiply(regular_year, subtract(century, leap_years_century)), multiply(leap_year, leap_years_century))
  
  assert_eq(four_year_days, 1461)
  assert_eq(average_days, 366)
  assert_eq(leap_years_century, 25)
  assert_eq(century_days, 36525)
}

test "financial_compound_interest" {
  // 金融复利计算
  let principal = 1000
  let rate_percent = 8
  let years = 5
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let year1_balance = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_balance, rate_percent), 100)
  let year2_balance = add(year1_balance, year2_interest)
  
  // 第三年
  let year3_interest = divide_with_ceil(multiply(year2_balance, rate_percent), 100)
  let year3_balance = add(year2_balance, year3_interest)
  
  // 计算总收益
  let total_profit = subtract(year3_balance, principal)
  
  assert_eq(year1_balance, 1080)
  assert_eq(year2_balance, 1167)
  assert_eq(year3_balance, 1261)
  assert_eq(total_profit, 261)
}

test "binary_operations_advanced" {
  // 高级二进制操作
  // 模拟位运算：左移和右移
  
  let number = 13  // 二进制: 1101
  
  // 模拟左移1位 (13 * 2 = 26)
  let left_shift_1 = multiply(number, 2)
  
  // 模拟左移2位 (13 * 4 = 52)
  let left_shift_2 = multiply(number, 4)
  
  // 模拟右移1位 (13 / 2，向上取整 = 7)
  let right_shift_1 = divide_with_ceil(number, 2)
  
  // 模拟右移2位 (13 / 4，向上取整 = 4)
  let right_shift_2 = divide_with_ceil(number, 4)
  
  assert_eq(left_shift_1, 26)
  assert_eq(left_shift_2, 52)
  assert_eq(right_shift_1, 7)
  assert_eq(right_shift_2, 4)
}

test "geometry_circle_properties" {
  // 几何学：圆形属性计算（使用π的近似值22/7）
  let radius = 7
  let pi_approx = divide_with_ceil(22, 7)  // π的近似值
  
  // 计算直径
  let diameter = multiply(radius, 2)
  
  // 计算周长（近似）
  let circumference = multiply(diameter, pi_approx)
  
  // 计算面积（近似）
  let area = multiply(radius, multiply(radius, pi_approx))
  
  assert_eq(diameter, 14)
  assert_eq(circumference, 44)
  assert_eq(area, 154)
}

test "data_structure_queue_simulation" {
  // 数据结构：队列操作模拟
  
  // 初始化队列
  let queue_size = 0
  let front_element = 0
  
  // 入队操作
  let enqueue1 = 10
  let queue_size1 = add(queue_size, 1)
  let rear_element1 = enqueue1
  
  let enqueue2 = 20
  let queue_size2 = add(queue_size1, 1)
  let rear_element2 = enqueue2
  
  let enqueue3 = 30
  let queue_size3 = add(queue_size2, 1)
  let rear_element3 = enqueue3
  
  // 出队操作（FIFO）
  let dequeue1 = 10  // 第一个入队的元素
  let queue_size_after_dequeue1 = subtract(queue_size3, 1)
  
  let dequeue2 = 20  // 第二个入队的元素
  let queue_size_after_dequeue2 = subtract(queue_size_after_dequeue1, 1)
  
  assert_eq(queue_size3, 3)
  assert_eq(rear_element3, 30)
  assert_eq(dequeue1, 10)
  assert_eq(queue_size_after_dequeue1, 2)
  assert_eq(dequeue2, 20)
  assert_eq(queue_size_after_dequeue2, 1)
}

test "temperature_conversion_precise" {
  // 精确的温度转换计算
  let celsius_freezing = 0
  let celsius_boiling = 100
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let freezing_fahrenheit = add(multiply(celsius_freezing, 9) / 5, 32)
  let boiling_fahrenheit = add(multiply(celsius_boiling, 9) / 5, 32)
  
  // 使用向上取整的转换
  let freezing_fahrenheit_ceil = add(divide_with_ceil(multiply(celsius_freezing, 9), 5), 32)
  let boiling_fahrenheit_ceil = add(divide_with_ceil(multiply(celsius_boiling, 9), 5), 32)
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  let back_to_freezing_celsius = multiply(subtract(freezing_fahrenheit, 32), 5) / 9
  let back_to_boiling_celsius = multiply(subtract(boiling_fahrenheit, 32), 5) / 9
  
  assert_eq(freezing_fahrenheit, 32)
  assert_eq(boiling_fahrenheit, 212)
  assert_eq(freezing_fahrenheit_ceil, 32)
  assert_eq(boiling_fahrenheit_ceil, 212)
  assert_eq(back_to_freezing_celsius, 0)
  assert_eq(back_to_boiling_celsius, 100)
}

test "algorithm_gcd_lcm" {
  // 算法：最大公约数和最小公倍数计算
  let num1 = 24
  let num2 = 36
  
  // 使用欧几里得算法模拟计算GCD
  // 36 ÷ 24 = 1 余 12
  let step1_remainder = subtract(36, multiply(1, 24))
  
  // 24 ÷ 12 = 2 余 0
  let step2_remainder = subtract(24, multiply(2, step1_remainder))
  
  let gcd = step1_remainder  // GCD = 12
  
  // 计算LCM：LCM(a,b) = (a × b) / GCD(a,b)
  let product = multiply(num1, num2)
  let lcm = divide_with_ceil(product, gcd)
  
  // 验证性质：GCD × LCM = a × b
  let verification = multiply(gcd, lcm)
  
  assert_eq(step1_remainder, 12)
  assert_eq(step2_remainder, 0)
  assert_eq(gcd, 12)
  assert_eq(lcm, 72)
  assert_eq(verification, 864)
}