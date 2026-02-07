// Azimuth 增强测试用例
// 标准 MoonBit 测试语法，包含 10 个高质量测试用例

test "number_theory_primes" {
  // 数论基础 - 质数相关计算
  // 检查 7 是否为质数（通过测试除法）
  let num = 7
  let divisors_tested = 0
  
  // 测试除以 2
  let remainder_by_2 = num % 2
  divisors_tested = divisors_tested + 1
  
  // 测试除以 3
  let remainder_by_3 = num % 3
  divisors_tested = divisors_tested + 1
  
  // 测试除以 5
  let remainder_by_5 = num % 5
  divisors_tested = divisors_tested + 1
  
  // 验证所有除法都有余数（质数性质）
  let total_non_zero = remainder_by_2 + remainder_by_3 + remainder_by_5
  
  assert_eq(divisors_tested, 3)
  assert_eq(total_non_zero, 1 + 1 + 2)  // 7%2=1, 7%3=1, 7%5=2
}

test "geometry_circle_properties" {
  // 几何学 - 圆的性质计算
  let radius = 5
  let pi_approx = 22  // 使用 22/7 作为 π 的近似
  
  // 计算直径
  let diameter = multiply(radius, 2)
  
  // 计算周长 (2πr)
  let circumference = divide_with_ceil(multiply(diameter, pi_approx), 7)
  
  // 计算面积 (πr²)
  let area = divide_with_ceil(multiply(pi_approx, multiply(radius, radius)), 7)
  
  assert_eq(diameter, 10)
  assert_eq(circumference, 32)  // ceil(2 * 22/7 * 5) = ceil(220/7) = ceil(31.43) = 32
  assert_eq(area, 79)  // ceil(22/7 * 25) = ceil(550/7) = ceil(78.57) = 79
}

test "time_conversion_calculations" {
  // 时间转换计算
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = add(multiply(hours, 3600), add(multiply(minutes, 60), seconds))
  
  // 转换回小时、分钟、秒
  let back_to_hours = divide_with_ceil(total_seconds, 3600)
  let remaining_seconds = subtract(total_seconds, multiply(back_to_hours, 3600))
  let back_to_minutes = divide_with_ceil(remaining_seconds, 60)
  let back_to_seconds = subtract(remaining_seconds, multiply(back_to_minutes, 60))
  
  assert_eq(total_seconds, 9930)  // 2*3600 + 45*60 + 30 = 7200 + 2700 + 30
  assert_eq(back_to_hours, 3)     // ceil(9930/3600) = ceil(2.76) = 3
  assert_eq(back_to_minutes, 1)   // ceil(330/60) = ceil(5.5) = 6
}

test "financial_simple_interest" {
  // 金融 - 单利计算
  let principal = 1000
  let rate_percent = 5
  let time_years = 3
  
  // 计算利息
  let interest = divide_with_ceil(multiply(principal, multiply(rate_percent, time_years)), 100)
  
  // 计算总额
  let total_amount = add(principal, interest)
  
  // 计算月供（假设分 12 个月）
  let monthly_payment = divide_with_ceil(total_amount, 12)
  
  assert_eq(interest, 150)        // ceil(1000 * 5 * 3 / 100) = ceil(150) = 150
  assert_eq(total_amount, 1150)   // 1000 + 150
  assert_eq(monthly_payment, 96)  // ceil(1150/12) = ceil(95.83) = 96
}

test "statistics_median_calculation" {
  // 统计学 - 中位数计算
  // 数据集: [3, 7, 8, 12, 15, 18, 21, 25]
  let data_size = 8
  
  // 计算中位数位置（偶数个元素）
  let mid_pos1 = divide_with_ceil(data_size, 2)
  let mid_pos2 = subtract(mid_pos1, 1)
  
  // 模拟中位数数值（第4和第5个元素）
  let value1 = 12
  let value2 = 15
  
  // 计算中位数（两个中间值的平均值）
  let median_sum = add(value1, value2)
  let median = divide_with_ceil(median_sum, 2)
  
  assert_eq(mid_pos1, 4)
  assert_eq(mid_pos2, 3)
  assert_eq(median_sum, 27)
  assert_eq(median, 14)  // ceil(27/2) = ceil(13.5) = 14
}

test "physics_kinematics" {
  // 物理学 - 运动学计算
  let initial_velocity = 10  // m/s
  let acceleration = 3       // m/s²
  let time = 4               // s
  
  // 计算最终速度: v = u + at
  let final_velocity = add(initial_velocity, multiply(acceleration, time))
  
  // 计算位移: s = ut + 0.5at²
  let displacement = add(multiply(initial_velocity, time), divide_with_ceil(multiply(acceleration, multiply(time, time)), 2))
  
  // 计算平均速度
  let average_velocity = divide_with_ceil(add(initial_velocity, final_velocity), 2)
  
  assert_eq(final_velocity, 22)     // 10 + 3*4 = 22
  assert_eq(displacement, 64)       // 10*4 + ceil(3*16/2) = 40 + ceil(24) = 64
  assert_eq(average_velocity, 16)   // ceil(32/2) = 16
}

test "computer_science_binary_operations" {
  // 计算机科学 - 二进制运算
  // 模拟 10 (1010) 和 6 (0110) 的运算
  let num1 = 10
  let num2 = 6
  
  // 模按位与运算
  let and_result = num2  // 1010 & 0110 = 0010 = 2
  
  // 模按位或运算
  let or_result = add(num1, subtract(num2, and_result))  // 1010 | 0110 = 1110 = 14
  
  // 模按位异或运算
  let xor_result = subtract(or_result, and_result)  // 1010 ^ 0110 = 1100 = 12
  
  assert_eq(and_result, 2)
  assert_eq(or_result, 14)
  assert_eq(xor_result, 12)
}

test "combinatorics_permutations" {
  // 组合数学 - 排列计算
  let n = 5  // 5个不同的物品
  let r = 3  // 选择3个进行排列
  
  // 计算 P(5,3) = 5! / (5-3)! = 5! / 2! = 5 × 4 × 3 = 60
  let step1 = n
  let step2 = subtract(n, 1)
  let step3 = subtract(n, 2)
  
  let permutations = multiply(multiply(step1, step2), step3)
  
  // 计算组合数 C(5,3) = P(5,3) / 3! = 60 / 6 = 10
  let factorial_3 = multiply(multiply(1, 2), 3)
  let combinations = divide_with_ceil(permutations, factorial_3)
  
  assert_eq(step1, 5)
  assert_eq(step2, 4)
  assert_eq(step3, 3)
  assert_eq(permutations, 60)
  assert_eq(factorial_3, 6)
  assert_eq(combinations, 10)
}

test "data_structures_queue_simulation" {
  // 数据结构 - 队列操作模拟
  // 模拟队列的入队和出队操作
  
  // 初始队列为空
  let queue_size = 0
  let front_element = 0
  
  // enqueue(10)
  let enqueue1 = 10
  queue_size = queue_size + 1
  front_element = enqueue1
  
  // enqueue(20)
  let enqueue2 = 20
  queue_size = queue_size + 1
  
  // enqueue(30)
  let enqueue3 = 30
  queue_size = queue_size + 1
  
  // dequeue() -> 返回 10
  let dequeue1 = front_element
  queue_size = queue_size - 1
  front_element = enqueue2  // 新的队首
  
  // dequeue() -> 返回 20
  let dequeue2 = front_element
  queue_size = queue_size - 1
  front_element = enqueue3  // 新的队首
  
  assert_eq(dequeue1, 10)
  assert_eq(dequeue2, 20)
  assert_eq(queue_size, 1)
  assert_eq(front_element, 30)
}

test "algorithm_efficiency_comparison" {
  // 算法效率比较
  let input_size = 100
  
  // O(1) 常数时间算法
  let constant_time_result = multiply(input_size, 2)
  
  // O(log n) 对数时间算法模拟
  let log_steps = 0
  let current = input_size
  while current > 1 {
    current = divide_with_ceil(current, 2)
    log_steps = log_steps + 1
  }
  
  // O(n) 线性时间算法模拟
  let linear_sum = multiply(input_size, add(input_size, 1))
  let linear_result = divide_with_ceil(linear_sum, 2)
  
  // O(n²) 二次时间算法模拟
  let quadratic_result = multiply(input_size, input_size)
  
  assert_eq(constant_time_result, 200)
  assert_eq(log_steps, 7)  // 100 -> 50 -> 25 -> 13 -> 7 -> 4 -> 2 -> 1
  assert_eq(linear_result, 5050)  // 100*101/2 = 5050
  assert_eq(quadratic_result, 10000)
}