// Azimuth 高级增强测试用例
// 包含10个新的高质量 MoonBit 测试用例，覆盖复杂的实际应用场景

test "recursive_calculation_scenario" {
  // 递归计算场景：计算阶乘的简化版本
  // 计算 5! = 5 * 4 * 3 * 2 * 1 = 120
  let n1 = 5
  let result1 = multiply(n1, multiply(n1 - 1, multiply(n1 - 2, multiply(n1 - 3, n1 - 4))))
  assert_eq(120, result1)
  
  // 计算 4! = 4 * 3 * 2 * 1 = 24
  let n2 = 4
  let result2 = multiply(n2, multiply(n2 - 1, multiply(n2 - 2, n2 - 3)))
  assert_eq(24, result2)
}

test "time_calculation_scenario" {
  // 时间计算场景：将秒转换为时分秒格式
  let total_seconds = 3665  // 1小时1分5秒
  
  // 计算小时数
  let hours = divide_with_ceil(total_seconds, 3600)
  assert_eq(2, hours)  // ceil(3665/3600) = 2
  
  // 计算剩余秒数
  let remaining_after_hours = subtract(total_seconds, multiply(hours - 1, 3600))
  
  // 计算分钟数
  let minutes = divide_with_ceil(remaining_after_hours, 60)
  assert_eq(2, minutes)  // ceil(65/60) = 2
  
  // 计算剩余秒数
  let seconds = subtract(remaining_after_hours, multiply(minutes - 1, 60))
  assert_eq(5, seconds)
}

test "geometric_calculation_scenario" {
  // 几何计算场景：计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 计算面积
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 计算周长
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
  
  // 计算对角线长度的平方（简化计算）
  let diagonal_squared = add(multiply(length, length), multiply(width, width))
  assert_eq(89, diagonal_squared)  // 8² + 5² = 64 + 25 = 89
}

test "statistical_calculation_scenario" {
  // 统计计算场景：计算平均值和总和
  // 使用单独的变量而不是数组来避免导入问题
  let value1 = 85
  let value2 = 92
  let value3 = 78
  let value4 = 96
  let value5 = 89
  
  // 计算总和
  let sum = add(add(add(add(value1, value2), value3), value4), value5)
  assert_eq(440, sum)
  
  // 计算平均值（使用整数除法）
  let count = 5
  let average = divide_with_ceil(sum, count)
  assert_eq(88, average)  // ceil(440/5) = 88
}

test "bitwise_related_calculation" {
  // 位运算相关计算：模拟2的幂次计算
  // 计算 2^0, 2^1, 2^2, 2^3, 2^4
  let power_of_2_0 = 1
  let power_of_2_1 = multiply(2, power_of_2_0)
  let power_of_2_2 = multiply(2, power_of_2_1)
  let power_of_2_3 = multiply(2, power_of_2_2)
  let power_of_2_4 = multiply(2, power_of_2_3)
  
  assert_eq(1, power_of_2_0)
  assert_eq(2, power_of_2_1)
  assert_eq(4, power_of_2_2)
  assert_eq(8, power_of_2_3)
  assert_eq(16, power_of_2_4)
  
  // 计算这些幂次的和
  let sum_of_powers = add(add(add(add(power_of_2_0, power_of_2_1), power_of_2_2), power_of_2_3), power_of_2_4)
  assert_eq(31, sum_of_powers)
}

test "sequence_calculation_scenario" {
  // 数列计算场景：计算等差数列的和
  // 首项 a1 = 3，公差 d = 2，项数 n = 6
  // 数列：3, 5, 7, 9, 11, 13
  let a1 = 3
  let d = 2
  let n = 6
  
  // 计算末项 an = a1 + (n-1)*d
  let an = add(a1, multiply(n - 1, d))
  assert_eq(13, an)
  
  // 计算和 S = n * (a1 + an) / 2
  let sum_before_division = multiply(n, add(a1, an))
  let sum = divide_with_ceil(sum_before_division, 2)
  assert_eq(48, sum)  // ceil(6 * (3+13) / 2) = 48
}

test "percentage_calculation_scenario" {
  // 百分比计算场景：计算折扣和税费
  let original_price = 1200
  let discount_percent = 15  // 15% 折扣
  let tax_percent = 8        // 8% 税费
  
  // 计算折扣金额
  let discount_amount = divide_with_ceil(multiply(original_price, discount_percent), 100)
  assert_eq(180, discount_amount)  // ceil(1200 * 15 / 100) = 180
  
  // 计算折后价格
  let discounted_price = subtract(original_price, discount_amount)
  assert_eq(1020, discounted_price)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(discounted_price, tax_percent), 100)
  assert_eq(82, tax_amount)  // ceil(1020 * 8 / 100) = 82
  
  // 计算最终价格
  let final_price = add(discounted_price, tax_amount)
  assert_eq(1102, final_price)
}

test "data_validation_scenario" {
  // 数据验证场景：验证输入数据的有效性
  // 模拟验证规则：数值必须在指定范围内
  let min_value = 10
  let max_value = 100
  
  // 测试有效值
  let valid_value1 = 25
  let valid_value2 = 50
  let valid_value3 = 95
  
  // 验证值是否在范围内
  let is_valid1 = (valid_value1 >= min_value) && (valid_value1 <= max_value)
  let is_valid2 = (valid_value2 >= min_value) && (valid_value2 <= max_value)
  let is_valid3 = (valid_value3 >= min_value) && (valid_value3 <= max_value)
  
  assert_true(is_valid1)
  assert_true(is_valid2)
  assert_true(is_valid3)
  
  // 测试无效值
  let invalid_value1 = 5
  let invalid_value2 = 105
  
  let is_invalid1 = (invalid_value1 >= min_value) && (invalid_value1 <= max_value)
  let is_invalid2 = (invalid_value2 >= min_value) && (invalid_value2 <= max_value)
  
  assert_false(is_invalid1)
  assert_false(is_invalid2)
}

test "combinatorial_mathematics_scenario" {
  // 组合数学场景：计算排列数 P(n,k) = n!/(n-k)!
  // 计算 P(5,2) = 5! / 3! = 120 / 6 = 20
  let n = 5
  let k = 2
  
  // 计算 n!
  let n_factorial = multiply(n, multiply(n - 1, multiply(n - 2, multiply(n - 3, n - 4))))
  
  // 计算 (n-k)!
  let n_minus_k = n - k
  let n_minus_k_factorial = multiply(n_minus_k, multiply(n_minus_k - 1, n_minus_k - 2))
  
  // 计算排列数
  let permutation = divide_with_ceil(n_factorial, n_minus_k_factorial)
  assert_eq(20, permutation)
}

test "physics_calculation_scenario" {
  // 物理计算场景：计算自由落体运动
  // 公式：h = v0*t + 0.5*g*t^2（简化为整数计算）
  let initial_velocity = 10  // m/s
  let gravity = 10           // m/s^2 (简化为10)
  let time = 3               // seconds
  
  // 计算位移：h = v0*t + 0.5*g*t^2
  let velocity_component = multiply(initial_velocity, time)
  let gravity_component = divide_with_ceil(multiply(gravity, multiply(time, time)), 2)
  let total_displacement = add(velocity_component, gravity_component)
  
  assert_eq(30, velocity_component)    // 10 * 3 = 30
  assert_eq(45, gravity_component)     // ceil(10 * 9 / 2) = 45
  assert_eq(75, total_displacement)    // 30 + 45 = 75
  
  // 计算最终速度：v = v0 + g*t
  let final_velocity = add(initial_velocity, multiply(gravity, time))
  assert_eq(40, final_velocity)  // 10 + 10*3 = 40
}