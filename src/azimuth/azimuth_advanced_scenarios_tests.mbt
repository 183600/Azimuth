// 高级场景测试用例 - 针对 azimuth 库的实用场景测试
// 包含科学计算、工程应用和实际业务场景的综合测试

test "scientific_notation_simulation" {
  // 模拟科学计数法计算：计算 3.2 × 10^6 + 1.8 × 10^6
  // 使用整数运算模拟：3.2×10^6 = 3200000, 1.8×10^6 = 1800000
  let mantissa1 = 32
  let mantissa2 = 18
  let exponent = 5  // 10^5，因为我们将系数缩小了100倍
  
  let scaled_result = add(multiply(mantissa1, multiply(10, exponent)), multiply(mantissa2, multiply(10, exponent)))
  let actual_result = divide_with_ceil(scaled_result, 100)
  
  assert_eq(5000000, actual_result)  // 3.2M + 1.8M = 5.0M
}

test "time_zone_conversion" {
  // 时区转换计算：北京时间下午3点转换为纽约时间
  let beijing_hour = 15  // 下午3点
  let time_difference = 13  // 北京比纽约快13小时
  
  let new_york_hour_24 = add(beijing_hour, -time_difference)
  let new_york_hour = if new_york_hour_24 < 0 {
    add(new_york_hour_24, 24)
  } else {
    new_york_hour_24
  }
  
  assert_eq(2, new_york_hour)  // 北京下午3点 = 纽约凌晨2点
}

test "digital_storage_calculation" {
  // 数字存储计算：计算文件所需的存储块数量
  let file_size_mb = 45
  let block_size_mb = 8
  let metadata_overhead_mb = 2
  
  let total_size_needed = add(file_size_mb, metadata_overhead_mb)
  let blocks_needed = divide_with_ceil(total_size_needed, block_size_mb)
  
  assert_eq(6, blocks_needed)  // ceil(47/8) = 6
}

test "chemical_mixture_calculation" {
  // 化学混合物计算：计算两种溶液混合后的浓度
  let solution1_volume = 150  // ml
  let solution1_concentration = 20  // 20%
  let solution2_volume = 100  // ml
  let solution2_concentration = 30  // 30%
  
  let solute1_amount = divide_with_ceil(multiply(solution1_volume, solution1_concentration), 100)
  let solute2_amount = divide_with_ceil(multiply(solution2_volume, solution2_concentration), 100)
  let total_solute = add(solute1_amount, solute2_amount)
  let total_volume = add(solution1_volume, solution2_volume)
  let final_concentration = divide_with_ceil(multiply(total_solute, 100), total_volume)
  
  assert_eq(24, final_concentration)  // 约24%浓度
}

test "financial_compound_interest" {
  // 金融复利计算：计算3年期投资回报
  let principal = 10000
  let annual_rate = 8  // 8%
  let years = 3
  
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, annual_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, annual_rate), 100))
  
  assert_eq(12597, year3_amount)  // 使用向上取整的复利计算
}

test "statistical_variance_simulation" {
  // 统计方差模拟：计算数据集的方差
  let data_points = [12, 15, 18, 21, 24]  // 平均值为18
  let mean = 18
  let n = 5
  
  // 计算偏差平方和：(12-18)² + (15-18)² + (18-18)² + (21-18)² + (24-18)²
  let deviation1 = multiply(mean - 12, mean - 12)
  let deviation2 = multiply(mean - 15, mean - 15)
  let deviation3 = multiply(mean - 18, mean - 18)
  let deviation4 = multiply(21 - mean, 21 - mean)
  let deviation5 = multiply(24 - mean, 24 - mean)
  
  let sum_of_squares = add(add(add(add(deviation1, deviation2), deviation3), add(deviation4, deviation5)), 0)
  let variance = divide_with_ceil(sum_of_squares, n)
  
  assert_eq(18, variance)  // 方差为18
}

test "geometric_progression" {
  // 几何级数计算：计算前5项的和
  let first_term = 3
  let ratio = 2
  let terms = 5
  
  // 手动计算前5项：3, 6, 12, 24, 48
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  let sum = add(add(add(add(term1, term2), add(term3, term4)), term5), 0)
  
  assert_eq(93, sum)  // 3 + 6 + 12 + 24 + 48 = 93
}

test "temperature_conversion" {
  // 温度转换计算：摄氏度转华氏度
  let celsius = 25
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  assert_eq(77, fahrenheit)  // 25°C = 77°F
  
  // 华氏度转摄氏度
  let fahrenheit_back = 77
  let celsius_back = divide_with_ceil(multiply(fahrenheit_back - 32, 5), 9)
  
  assert_eq(25, celsius_back)  // 77°F = 25°C
}

test "binary_number_simulation" {
  // 二进制数模拟：计算二进制1101的十进制值
  // 1×2³ + 1×2² + 0×2¹ + 1×2⁰ = 8 + 4 + 0 + 1 = 13
  let bit3 = 1  // 2³位
  let bit2 = 1  // 2²位
  let bit1 = 0  // 2¹位
  let bit0 = 1  // 2⁰位
  
  let decimal_value = add(add(multiply(bit3, 8), multiply(bit2, 4)), add(multiply(bit1, 2), multiply(bit0, 1)))
  
  assert_eq(13, decimal_value)
  
  // 测试另一个二进制数：1010 = 10
  let binary_1010 = add(add(multiply(1, 8), multiply(0, 4)), add(multiply(1, 2), multiply(0, 1)))
  assert_eq(10, binary_1010)
}

test "cryptography_simple_hash" {
  // 简单密码学哈希模拟：使用多项式滚动哈希
  let input = "hello"
  let base = 31
  let mod = 1000000007  // 大质数，简化计算
  
  // 模拟字符串的哈希计算：h = (a×31⁴ + b×31³ + c×31² + d×31¹ + e×31⁰) mod m
  // 简化为整数计算
  let char_code_sum = add(add(add(add(104, 101), 108), 108), 111)  // "hello"的ASCII码和
  let hash_value = multiply(char_code_sum, base) % mod
  
  assert_eq(6856, hash_value)  // (104+101+108+108+111) × 31 = 532 × 31 = 16492
  
  // 测试不同输入的哈希值
  let input2 = "world"
  let char_code_sum2 = add(add(add(add(119, 111), 114), 108), 100)  // "world"的ASCII码和
  let hash_value2 = multiply(char_code_sum2, base) % mod
  
  assert_eq(8438, hash_value2)  // (119+111+114+108+100) × 31 = 552 × 31 = 17112
}