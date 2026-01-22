// 独特的科学计算和算法测试用例
// 包含数学、物理和计算机科学领域的实际应用场景

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // 手动计算前几项
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  
  @azimuth.assert_eq(0, f0)
  @azimuth.assert_eq(1, f1)
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  @azimuth.assert_eq(21, f8)
  
  // 验证黄金比例近似值
  let ratio = f7 / f6  // 13 / 8 = 1 (整数除法)
  @azimuth.assert_eq(1, ratio)
}

test "physics_kinetic_energy" {
  // 物理计算：动能 E = 1/2 * m * v²
  // 简化为整数计算：E = m * v² / 2
  
  let mass = 10      // 质量 (kg)
  let velocity = 5   // 速度 (m/s)
  
  // 计算速度的平方
  let velocity_squared = @azimuth.multiply(velocity, velocity)  // 25
  
  // 计算动能
  let kinetic_energy = @azimuth.multiply(mass, velocity_squared) / 2  // 10 * 25 / 2 = 125
  
  @azimuth.assert_eq(125, kinetic_energy)
  
  // 测试不同质量和速度的组合
  let mass2 = 20
  let velocity2 = 3
  let velocity2_squared = @azimuth.multiply(velocity2, velocity2)  // 9
  let kinetic_energy2 = @azimuth.multiply(mass2, velocity2_squared) / 2  // 20 * 9 / 2 = 90
  
  @azimuth.assert_eq(90, kinetic_energy2)
}

test "binary_number_simulation" {
  // 模拟二进制数运算
  // 将二进制数 1010 (10) 和 1100 (12) 进行加法
  
  let binary1 = 1010  // 表示十进制 10
  let binary2 = 1100  // 表示十进制 12
  
  // 模拟二进制加法（直接使用十进制计算）
  let decimal1 = 10
  let decimal2 = 12
  let sum_decimal = @azimuth.add(decimal1, decimal2)  // 22
  
  // 将结果转换回二进制表示（模拟）
  // 22 的二进制是 10110
  let binary_result = 10110
  
  // 验证计算过程
  @azimuth.assert_eq(22, sum_decimal)
  
  // 模拟二进制左移运算（相当于乘以2）
  let left_shift_result = @azimuth.multiply(decimal1, 2)  // 10 * 2 = 20
  @azimuth.assert_eq(20, left_shift_result)
  
  // 模拟二进制右移运算（相当于除以2）
  let right_shift_result = decimal1 / 2  // 10 / 2 = 5
  @azimuth.assert_eq(5, right_shift_result)
}

test "greatest_common_divisor" {
  // 计算最大公约数（GCD）- 欧几里得算法模拟
  // GCD(48, 18) = 6
  
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法步骤
  // 48 ÷ 18 = 2 余 12
  let remainder1 = a % b  // 48 % 18 = 12
  
  // 18 ÷ 12 = 1 余 6
  let remainder2 = b % remainder1  // 18 % 12 = 6
  
  // 12 ÷ 6 = 2 余 0
  let remainder3 = remainder1 % remainder2  // 12 % 6 = 0
  
  // 当余数为0时，除数就是GCD
  let gcd = remainder2  // 6
  
  @azimuth.assert_eq(6, gcd)
  
  // 验证另一对数：GCD(56, 98) = 14
  let c = 56
  let d = 98
  
  let rem1 = d % c  // 98 % 56 = 42
  let rem2 = c % rem1  // 56 % 42 = 14
  let rem3 = rem1 % rem2  // 42 % 14 = 0
  
  let gcd2 = rem2  // 14
  @azimuth.assert_eq(14, gcd2)
}

test "prime_number_check" {
  // 质数检查模拟
  // 检查数字是否为质数（通过检查是否能被小于它的数整除）
  
  let num1 = 17  // 质数
  let num2 = 15  // 非质数
  let num3 = 23  // 质数
  let num4 = 25  // 非质数
  
  // 检查 17 是否为质数
  // 17 不能被 2, 3, 4, 5, 6, 7, 8 整除
  let is_17_prime = (num1 % 2 != 0) && (num1 % 3 != 0) && (num1 % 4 != 0) && (num1 % 5 != 0) && (num1 % 6 != 0) && (num1 % 7 != 0) && (num1 % 8 != 0)
  @azimuth.assert_eq(is_17_prime, true)
  
  // 检查 15 是否为质数
  // 15 能被 3 整除
  let is_15_prime = (num2 % 2 != 0) && (num2 % 3 != 0) && (num2 % 4 != 0) && (num2 % 5 != 0) && (num2 % 6 != 0) && (num2 % 7 != 0)
  @azimuth.assert_eq(0, 1)(is_15_prime)
  
  // 检查 23 是否为质数
  // 23 不能被 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 整除
  let is_23_prime = (num3 % 2 != 0) && (num3 % 3 != 0) && (num3 % 4 != 0) && (num3 % 5 != 0) && (num3 % 6 != 0) && (num3 % 7 != 0) && (num3 % 8 != 0) && (num3 % 9 != 0) && (num3 % 10 != 0) && (num3 % 11 != 0)
  @azimuth.assert_eq(is_23_prime, true)
  
  // 检查 25 是否为质数
  // 25 能被 5 整除
  let is_25_prime = (num4 % 2 != 0) && (num4 % 3 != 0) && (num4 % 4 != 0) && (num4 % 5 != 0) && (num4 % 6 != 0)
  @azimuth.assert_eq(0, 1)(is_25_prime)
}

test "data_structure_queue_simulation" {
  // 模拟队列数据结构（先进先出）
  // 使用数组索引模拟队列操作
  
  // 初始化队列
  let queue_size = 5
  let front_index = 0
  let rear_index = 0
  
  // 入队操作：添加元素
  // 元素1: 10
  let element1 = 10
  rear_index = @azimuth.add(rear_index, 1)  // rear_index = 1
  
  // 元素2: 20
  let element2 = 20
  rear_index = @azimuth.add(rear_index, 1)  // rear_index = 2
  
  // 元素3: 30
  let element3 = 30
  rear_index = @azimuth.add(rear_index, 1)  // rear_index = 3
  
  // 队列中的元素数量
  let queue_count = @azimuth.add(rear_index, -front_index)  // 3 - 0 = 3
  @azimuth.assert_eq(3, queue_count)
  
  // 出队操作：移除元素
  // 移除第一个元素 (10)
  front_index = @azimuth.add(front_index, 1)  // front_index = 1
  queue_count = @azimuth.add(rear_index, -front_index)  // 3 - 1 = 2
  @azimuth.assert_eq(2, queue_count)
  
  // 移除第二个元素 (20)
  front_index = @azimuth.add(front_index, 1)  // front_index = 2
  queue_count = @azimuth.add(rear_index, -front_index)  // 3 - 2 = 1
  @azimuth.assert_eq(1, queue_count)
  
  // 继续入队
  let element4 = 40
  rear_index = @azimuth.add(rear_index, 1)  // rear_index = 4
  queue_count = @azimuth.add(rear_index, -front_index)  // 4 - 2 = 2
  @azimuth.assert_eq(2, queue_count)
}

test "sorting_algorithm_simulation" {
  // 模拟冒泡排序算法
  // 对数组 [5, 2, 8, 1, 9] 进行排序
  
  // 初始数组
  let a1 = 5
  let a2 = 2
  let a3 = 8
  let a4 = 1
  let a5 = 9
  
  // 第一轮冒泡
  // 比较 a1 和 a2: 5 > 2，交换
  let temp_a1 = a2
  let temp_a2 = a1
  a1 = temp_a1  // 2
  a2 = temp_a2  // 5
  
  // 比较 a2 和 a3: 5 < 8，不交换
  // 比较 a3 和 a4: 8 > 1，交换
  let temp_a3 = a4
  let temp_a4 = a3
  a3 = temp_a3  // 1
  a4 = temp_a4  // 8
  
  // 比较 a4 和 a5: 8 < 9，不交换
  // 第一轮结果: [2, 5, 1, 8, 9]
  
  // 第二轮冒泡
  // 比较 a1 和 a2: 2 < 5，不交换
  // 比较 a2 和 a3: 5 > 1，交换
  let temp_a2_2 = a3
  let temp_a3_2 = a2
  a2 = temp_a2_2  // 1
  a3 = temp_a3_2  // 5
  
  // 比较 a3 和 a4: 5 < 8，不交换
  // 第二轮结果: [2, 1, 5, 8, 9]
  
  // 第三轮冒泡
  // 比较 a1 和 a2: 2 > 1，交换
  let temp_a1_3 = a2
  let temp_a2_3 = a1
  a1 = temp_a1_3  // 1
  a2 = temp_a2_3  // 2
  
  // 最终排序结果: [1, 2, 5, 8, 9]
  @azimuth.assert_eq(1, a1)
  @azimuth.assert_eq(2, a2)
  @azimuth.assert_eq(5, a3)
  @azimuth.assert_eq(8, a4)
  @azimuth.assert_eq(9, a5)
}

test "chemistry_molar_mass" {
  // 化学计算：摩尔质量
  // 计算水分子 H₂O 的摩尔质量
  // H: 1 g/mol, O: 16 g/mol
  
  let hydrogen_mass = 1  // 氢的原子质量
  let oxygen_mass = 16   // 氧的原子质量
  
  // H₂O = 2 × H + 1 × O
  let water_molar_mass = @azimuth.add(@azimuth.multiply(2, hydrogen_mass), oxygen_mass)
  @azimuth.assert_eq(18, water_molar_mass)
  
  // 计算二氧化碳 CO₂ 的摩尔质量
  // C: 12 g/mol, O: 16 g/mol
  let carbon_mass = 12
  
  // CO₂ = 1 × C + 2 × O
  let co2_molar_mass = @azimuth.add(carbon_mass, @azimuth.multiply(2, oxygen_mass))
  @azimuth.assert_eq(44, co2_molar_mass)
  
  // 计算甲烷 CH₄ 的摩尔质量
  // CH₄ = 1 × C + 4 × H
  let ch4_molar_mass = @azimuth.add(carbon_mass, @azimuth.multiply(4, hydrogen_mass))
  @azimuth.assert_eq(16, ch4_molar_mass)
}

test "computer_science_hash_function" {
  // 模拟简单的哈希函数
  // 使用多项式滚动哈希：hash = (c1 × p^0 + c2 × p^1 + c3 × p^2) mod m
  // 简化为整数计算
  
  let base = 31  // 基数
  let mod = 1000000007  // 大质数（简化处理）
  
  // 对于字符串 "abc"
  // a = 1, b = 2, c = 3 (简化映射)
  let char_a = 1
  let char_b = 2
  let char_c = 3
  
  // 计算 hash = 1 × 31^0 + 2 × 31^1 + 3 × 31^2
  let power_0 = 1  // 31^0
  let power_1 = base  // 31^1
  let power_2 = @azimuth.multiply(base, base)  // 31^2 = 961
  
  let hash_abc = @azimuth.add(@azimuth.add(@azimuth.multiply(char_a, power_0), @azimuth.multiply(char_b, power_1)), @azimuth.multiply(char_c, power_2))
  @azimuth.assert_eq(2928, hash_abc)  // 1 + 62 + 2883 = 2946
  
  // 对于字符串 "bca"
  // b = 2, c = 3, a = 1
  let hash_bca = @azimuth.add(@azimuth.add(@azimuth.multiply(char_b, power_0), @azimuth.multiply(char_c, power_1)), @azimuth.multiply(char_a, power_2))
  @azimuth.assert_eq(997, hash_bca)  // 2 + 93 + 961 = 1056
  
  // 验证不同的字符串产生不同的哈希值
  @azimuth.assert_eq(hash_abc != hash_bca, true)
}

test "astronomical_distance_calculation" {
  // 天文计算：距离和时间
  // 光速约为 300,000 km/s
  // 计算光从太阳到地球的时间
  
  let light_speed = 300000  // km/s
  let sun_earth_distance = 150000000  // km (平均距离)
  
  // 计算时间 = 距离 / 速度
  let travel_time_seconds = sun_earth_distance / light_speed
  @azimuth.assert_eq(500, travel_time_seconds)  // 150,000,000 / 300,000 = 500 秒
  
  // 转换为分钟
  let travel_time_minutes = travel_time_seconds / 60
  @azimuth.assert_eq(8, travel_time_minutes)  // 500 / 60 = 8 分钟
  
  // 计算光年到天文单位的转换（简化）
  // 1 光年 ≈ 63,241 天文单位
  let light_year_au = 63241
  
  // 计算最近的恒星（比邻星）距离：4.24 光年
  let proxima_centauri_ly = 424  // 简化为整数（4.24 × 100）
  let proxima_centauri_au = @azimuth.multiply(proxima_centauri_ly, light_year_au) / 100
  @azimuth.assert_eq(268150, proxima_centauri_au)  // 424 × 63241 / 100 = 268150
  
  // 计算旅行者1号探测器的距离（简化）
  let voyager_distance_au = 159  // 天文单位
  let voyager_travel_time_days = @azimuth.multiply(voyager_distance_au, 500) / 60 / 24  // 简化计算
  @azimuth.assert_eq(552, voyager_travel_time_days)  // 159 × 500 / 60 / 24 ≈ 552 天
}