// 额外的 MoonBit 测试用例
// 这些测试用例补充现有的测试，提供更全面的覆盖

test "division_simulation" {
  // 模拟除法运算：使用减法实现整数除法
  let dividend = 27
  let divisor = 4
  
  // 手动计算：27 ÷ 4 = 6 余 3
  let quotient = 6
  let remainder = 3
  
  // 验证：divisor × quotient + remainder = dividend
  let verification = add(multiply(divisor, quotient), remainder)
  assert_eq(dividend, verification)
}

test "modulus_operation_simulation" {
  // 模拟取模运算：使用减法实现取模
  let number = 23
  let modulus = 5
  
  // 手动计算：23 % 5 = 3
  let expected_remainder = 3
  
  // 验证：number - (number ÷ modulus) × modulus = remainder
  let quotient = 4  // 23 ÷ 5 = 4
  let calculated_remainder = add(number, multiply(-quotient, modulus))
  assert_eq(expected_remainder, calculated_remainder)
}

test "absolute_value_calculation" {
  // 绝对值计算测试
  let positive = 42
  let negative = -42
  let zero = 0
  
  // 正数的绝对值是本身
  assert_eq(positive, positive)
  
  // 负数的绝对值是其相反数
  assert_eq(42, multiply(negative, -1))
  
  // 零的绝对值是零
  assert_eq(zero, zero)
}

test "sign_function_simulation" {
  // 符号函数模拟：返回数的符号（-1, 0, 1）
  let positive_num = 10
  let negative_num = -10
  let zero_num = 0
  
  // 正数返回1
  assert_true(positive_num > 0)
  
  // 负数返回-1
  assert_true(negative_num < 0)
  
  // 零返回0
  assert_eq(0, zero_num)
}

test "min_max_functions" {
  // 最小值和最大值函数测试
  let a = 15
  let b = 25
  let c = 20
  
  // 找出最大值
  let max_ab = if a > b { a } else { b }
  let max_abc = if max_ab > c { max_ab } else { c }
  assert_eq(25, max_abc)
  
  // 找出最小值
  let min_ab = if a < b { a } else { b }
  let min_abc = if min_ab < c { min_ab } else { c }
  assert_eq(15, min_abc)
}

test "range_checking" {
  // 范围检查测试
  let value = 50
  let min_range = 10
  let max_range = 100
  
  // 检查值是否在范围内
  let in_range = value >= min_range && value <= max_range
  assert_true(in_range)
  
  // 测试边界值
  assert_true(min_range >= min_range && min_range <= max_range)
  assert_true(max_range >= min_range && max_range <= max_range)
  
  // 测试超出范围的值
  let out_of_range_low = 5
  let out_of_range_high = 150
  assert_false(out_of_range_low >= min_range && out_of_range_low <= max_range)
  assert_false(out_of_range_high >= min_range && out_of_range_high <= max_range)
}

test "clamping_function" {
  // 限制函数：将值限制在指定范围内
  let value = 150
  let min_val = 0
  let max_val = 100
  
  // 如果值超过最大值，则返回最大值
  let clamped_value = if value > max_val { max_val } else { if value < min_val { min_val } else { value } }
  assert_eq(max_val, clamped_value)
  
  // 测试低于最小值的情况
  let low_value = -10
  let clamped_low = if low_value > max_val { max_val } else { if low_value < min_val { min_val } else { low_value } }
  assert_eq(min_val, clamped_low)
  
  // 测试在范围内的情况
  let normal_value = 50
  let clamped_normal = if normal_value > max_val { max_val } else { if normal_value < min_val { min_val } else { normal_value } }
  assert_eq(50, clamped_normal)
}

test "linear_interpolation" {
  // 线性插值测试：在两个值之间进行插值
  let start = 0
  let end = 100
  let t = 25  // 插值参数（0-100）
  
  // 线性插值公式：start + (end - start) × t / 100
  let diff = add(end, -start)
  let interpolated = add(start, multiply(diff, t) / 100)
  assert_eq(25, interpolated)
  
  // 测试中间点
  let mid_point = 50
  let mid_interpolated = add(start, multiply(diff, mid_point) / 100)
  assert_eq(50, mid_interpolated)
}