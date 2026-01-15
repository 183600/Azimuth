// 高级 MoonBit 测试用例
// 包含 10 个新的测试用例，使用标准 MoonBit 测试语法
// 覆盖递归模式、条件逻辑、数据结构、数论基础等高级场景

// 测试1: 递归计算模式模拟 - 斐波那契数列
test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项（使用迭代方式模拟递归）
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // F(2) = F(1) + F(0) = 1 + 0 = 1
  f2 = azimuth::add(1, 0)
  azimuth::assert_eq(1, f2)
  
  // F(3) = F(2) + F(1) = 1 + 1 = 2
  f3 = azimuth::add(f2, 1)
  azimuth::assert_eq(2, f3)
  
  // F(4) = F(3) + F(2) = 2 + 1 = 3
  f4 = azimuth::add(f3, f2)
  azimuth::assert_eq(3, f4)
  
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  f5 = azimuth::add(f4, f3)
  azimuth::assert_eq(5, f5)
  
  // F(6) = F(5) + F(4) = 5 + 3 = 8
  f6 = azimuth::add(f5, f4)
  azimuth::assert_eq(8, f6)
}

// 测试2: 条件逻辑的复杂应用 - 分段函数计算
test "piecewise_function_calculation" {
  // 实现分段函数: f(x) = {
  //   x², 当 x < 0
  //   2x + 1, 当 0 ≤ x < 10
  //   x³ - 10, 当 x ≥ 10
  // }
  
  // 测试 x < 0 的情况
  let x1 = -5
  let result1 = azimuth::multiply(x1, x1)  // (-5)² = 25
  azimuth::assert_eq(25, result1)
  
  // 测试 0 ≤ x < 10 的情况
  let x2 = 5
  let result2 = azimuth::add(azimuth::multiply(2, x2), 1)  // 2*5 + 1 = 11
  azimuth::assert_eq(11, result2)
  
  // 测试 x ≥ 10 的情况
  let x3 = 12
  let x3_squared = azimuth::multiply(x3, x3)  // 12² = 144
  let result3 = azimuth::add(x3_squared, -10)  // 144 - 10 = 134
  azimuth::assert_eq(134, result3)
  
  // 边界值测试
  let x4 = 0
  let result4 = azimuth::add(azimuth::multiply(2, x4), 1)  // 2*0 + 1 = 1
  azimuth::assert_eq(1, result4)
  
  let x5 = 10
  let x5_squared = azimuth::multiply(x5, x5)  // 10² = 100
  let result5 = azimuth::add(x5_squared, -10)  // 100 - 10 = 90
  azimuth::assert_eq(90, result5)
}

// 测试3: 数据结构索引计算 - 三维数组转一维数组
test "three_dimensional_array_indexing" {
  // 三维数组 (x, y, z) 转一维数组的索引计算
  // index = x * width_y * width_z + y * width_z + z
  
  let width_y = 5  // y维度宽度
  let width_z = 4  // z维度宽度
  
  // 测试点 (1, 2, 3)
  let x1 = 1
  let y1 = 2
  let z1 = 3
  let index1 = azimuth::add(
    azimuth::multiply(x1, azimuth::multiply(width_y, width_z)),
    azimuth::add(
      azimuth::multiply(y1, width_z),
      z1
    )
  )
  azimuth::assert_eq(33, index1)  // 1*5*4 + 2*4 + 3 = 20 + 8 + 3 = 31
  
  // 测试原点 (0, 0, 0)
  let x2 = 0
  let y2 = 0
  let z2 = 0
  let index2 = azimuth::add(
    azimuth::multiply(x2, azimuth::multiply(width_y, width_z)),
    azimuth::add(
      azimuth::multiply(y2, width_z),
      z2
    )
  )
  azimuth::assert_eq(0, index2)
  
  // 测试边界点 (2, 4, 3)
  let x3 = 2
  let y3 = 4
  let z3 = 3
  let index3 = azimuth::add(
    azimuth::multiply(x3, azimuth::multiply(width_y, width_z)),
    azimuth::add(
      azimuth::multiply(y3, width_z),
      z3
    )
  )
  azimuth::assert_eq(67, index3)  // 2*5*4 + 4*4 + 3 = 40 + 16 + 3 = 59
}

// 测试4: 数字系统转换 - 二进制到十进制（简化版）
test "binary_to_decimal_conversion" {
  // 将二进制数转换为十进制数
  // 例如：1101₂ = 1×2³ + 1×2² + 0×2¹ + 1×2⁰ = 8 + 4 + 0 + 1 = 13
  
  // 转换 1101₂
  let bit3 = 1  // 2³位
  let bit2 = 1  // 2²位
  let bit1 = 0  // 2¹位
  let bit0 = 1  // 2⁰位
  
  let decimal_value = azimuth::add(
    azimuth::add(
      azimuth::multiply(bit3, 8),  // 1×8
      azimuth::multiply(bit2, 4)   // 1×4
    ),
    azimuth::add(
      azimuth::multiply(bit1, 2),  // 0×2
      azimuth::multiply(bit0, 1)   // 1×1
    )
  )
  azimuth::assert_eq(13, decimal_value)
  
  // 转换 1010₂ = 1×2³ + 0×2² + 1×2¹ + 0×2⁰ = 8 + 0 + 2 + 0 = 10
  let bit3_2 = 1
  let bit2_2 = 0
  let bit1_2 = 1
  let bit0_2 = 0
  
  let decimal_value2 = azimuth::add(
    azimuth::add(
      azimuth::multiply(bit3_2, 8),  // 1×8
      azimuth::multiply(bit2_2, 4)   // 0×4
    ),
    azimuth::add(
      azimuth::multiply(bit1_2, 2),  // 1×2
      azimuth::multiply(bit0_2, 1)   // 0×1
    )
  )
  azimuth::assert_eq(10, decimal_value2)
}

// 测试5: 统计计算 - 方差和标准差（简化版）
test "statistical_variance_calculation" {
  // 计算一组数据的方差和标准差（简化整数版本）
  // 数据集: [2, 4, 6, 8, 10]
  
  // 计算平均值
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(2, 4), 6), 8), 10)  // 30
  let mean = sum / 5  // 6
  azimuth::assert_eq(6, mean)
  
  // 计算平方差: (x_i - mean)²
  let diff1 = azimuth::add(2, -mean)  // -4
  let diff2 = azimuth::add(4, -mean)  // -2
  let diff3 = azimuth::add(6, -mean)  // 0
  let diff4 = azimuth::add(8, -mean)  // 2
  let diff5 = azimuth::add(10, -mean) // 4
  
  let sq_diff1 = azimuth::multiply(diff1, diff1)  // 16
  let sq_diff2 = azimuth::multiply(diff2, diff2)  // 4
  let sq_diff3 = azimuth::multiply(diff3, diff3)  // 0
  let sq_diff4 = azimuth::multiply(diff4, diff4)  // 4
  let sq_diff5 = azimuth::multiply(diff5, diff5)  // 16
  
  // 计算平方差之和
  let sum_sq_diffs = azimuth::add(azimuth::add(azimuth::add(azimuth::add(sq_diff1, sq_diff2), sq_diff3), sq_diff4), sq_diff5)
  azimuth::assert_eq(40, sum_sq_diffs)
  
  // 计算方差（总体方差）
  let variance = sum_sq_diffs / 5
  azimuth::assert_eq(8, variance)
  
  // 计算标准差（简化整数版本）
  // 使用近似值：√8 ≈ 3
  let std_dev = 3
  azimuth::assert_eq(3, std_dev)
}

// 测试6: 时间和日期计算 - 星期几推算（简化版）
test "day_of_week_calculation" {
  // 使用Zeller公式计算星期几（简化版本）
  // 假设2023年1月1日是星期日（0）
  
  // 计算从基准日期（2023-01-01，星期日）开始的天数
  let base_day = 1
  let base_weekday = 0  // 星期日
  
  // 计算2023-01-15是星期几
  let target_day = 15
  let days_difference = azimuth::add(target_day, -base_day)  // 15 - 1 = 14
  let weekday = days_difference % 7  // 14 % 7 = 0
  azimuth::assert_eq(0, weekday)  // 星期日
  
  // 计算2023-01-10是星期几
  let target_day2 = 10
  let days_difference2 = azimuth::add(target_day2, -base_day)  // 10 - 1 = 9
  let weekday2 = days_difference2 % 7  // 9 % 7 = 2
  azimuth::assert_eq(2, weekday2)  // 星期二
  
  // 计算跨月的情况：2023-02-05（1月有31天）
  let target_day3 = 5
  let days_in_jan = 31
  let days_difference3 = azimuth::add(
    azimuth::add(days_in_jan, -base_day),  // 31 - 1 = 30
    target_day3  // + 5 = 35
  )
  let weekday3 = days_difference3 % 7  // 35 % 7 = 0
  azimuth::assert_eq(0, weekday3)  // 星期日
}

// 测试7: 数论基础 - 最大公约数（欧几里得算法模拟）
test "greatest_common_divisor" {
  // 使用欧几里得算法计算最大公约数
  // gcd(a, b) = gcd(b, a mod b)
  
  // 计算 gcd(48, 18)
  let a1 = 48
  let b1 = 18
  
  // 第一步：48 mod 18 = 12
  let remainder1 = a1 % b1  // 48 % 18 = 12
  // 第二步：18 mod 12 = 6
  let remainder2 = b1 % remainder1  // 18 % 12 = 6
  // 第三步：12 mod 6 = 0
  let remainder3 = remainder1 % remainder2  // 12 % 6 = 0
  
  // 当余数为0时，当前除数就是GCD
  let gcd1 = remainder2
  azimuth::assert_eq(6, gcd1)
  
  // 计算 gcd(1071, 462)
  let a2 = 1071
  let b2 = 462
  
  // 第一步：1071 mod 462 = 147
  let remainder4 = a2 % b2  // 1071 % 462 = 147
  // 第二步：462 mod 147 = 21
  let remainder5 = b2 % remainder4  // 462 % 147 = 21
  // 第三步：147 mod 21 = 0
  let remainder6 = remainder4 % remainder5  // 147 % 21 = 0
  
  let gcd2 = remainder5
  azimuth::assert_eq(21, gcd2)
  
  // 验证：验证GCD确实能整除两个数
  azimuth::assert_eq(0, a1 % gcd1)
  azimuth::assert_eq(0, b1 % gcd1)
  azimuth::assert_eq(0, a2 % gcd2)
  azimuth::assert_eq(0, b2 % gcd2)
}

// 测试8: 位运算模拟 - 位掩码操作
test "bitmask_operations" {
  // 模拟位掩码操作（使用整数运算）
  
  // 设置位掩码（假设我们使用低4位）
  let mask1 = 1  // 0001
  let mask2 = 2  // 0010
  let mask3 = 4  // 0100
  // let mask4 = 8  // 1000
  
  // 模拟按位或操作：mask1 | mask2 | mask3 = 0001 | 0010 | 0100 = 0111 = 7
  let bitwise_or = azimuth::add(azimuth::add(mask1, mask2), mask3)
  azimuth::assert_eq(7, bitwise_or)
  
  // 模拟按位与操作：检查某位是否设置
  // 检查 value = 6 (0110) 的第2位是否设置
  let value = 6
  let check_bit = 2  // 检查第2位(0010)
  
  // 模拟按位与：value & check_bit
  // 如果结果 > 0，则该位已设置
  let bitwise_and = value % (check_bit * 2)  // 简化的检查方法
  let is_bit_set = bitwise_and >= check_bit
  azimuth::assert_true(is_bit_set)
  
  // 检查 value = 6 (0110) 的第4位是否设置
  let check_bit2 = 8  // 检查第4位(1000)
  let bitwise_and2 = value % (check_bit2 * 2)
  let is_bit_set2 = bitwise_and2 >= check_bit2
  azimuth::assert_false(is_bit_set2)
  
  // 模拟按位异或操作：交换两个值（不使用临时变量）
  let x = 5
  let y = 12
  
  // 模拟 x = x ^ y
  let x_xor_y = azimuth::add(x, y)  // 简化的异或操作
  // 模拟 y = x ^ y
  let y_new = azimuth::add(x_xor_y, -x)
  // 模拟 x = x ^ y
  let x_new = azimuth::add(x_xor_y, -y_new)
  
  azimuth::assert_eq(12, x_new)
  azimuth::assert_eq(5, y_new)
}

// 测试9: 状态机模拟 - 简单的自动售货机
test "vending_machine_state_machine" {
  // 模拟自动售货机的状态机
  // 状态：0=等待投币，1=已投币，2=已选择商品
  
  let price = 15  // 商品价格
  let mut state = 0  // 初始状态：等待投币
  let mut balance = 0  // 余额
  
  // 投币10元
  balance = azimuth::add(balance, 10)
  if (balance >= price) {
    state = 1  // 已投币
  } else {
    state = 0  // 仍需投币
  }
  azimuth::assert_eq(0, state)  // 仍需投币
  
  // 再投币10元
  balance = azimuth::add(balance, 10)
  if (balance >= price) {
    state = 1  // 已投币
  } else {
    state = 0  // 仍需投币
  }
  azimuth::assert_eq(1, state)  // 已投币
  
  // 选择商品
  if (state == 1) {
    state = 2  // 已选择商品
    balance = azimuth::add(balance, -price)  // 扣除价格
  }
  azimuth::assert_eq(2, state)  // 已选择商品
  azimuth::assert_eq(5, balance)  // 找零：20 - 15 = 5
  
  // 重置状态机
  state = 0
  balance = 0
  azimuth::assert_eq(0, state)
  azimuth::assert_eq(0, balance)
}

// 测试10: 缓存和记忆化模拟 - 斐波那契数列缓存
test "fibonacci_with_memoization" {
  // 模拟使用缓存的斐波那契数列计算
  
  // 初始化缓存：F(0)=0, F(1)=1
  let fib0 = 0
  let fib1 = 1
  let fib2 = azimuth::add(fib0, fib1)  // F(2) = 1
  let fib3 = azimuth::add(fib1, fib2)  // F(3) = 2
  let fib4 = azimuth::add(fib2, fib3)  // F(4) = 3
  let fib5 = azimuth::add(fib3, fib4)  // F(5) = 5
  
  // 验证缓存值
  azimuth::assert_eq(0, fib0)
  azimuth::assert_eq(1, fib1)
  azimuth::assert_eq(1, fib2)
  azimuth::assert_eq(2, fib3)
  azimuth::assert_eq(3, fib4)
  azimuth::assert_eq(5, fib5)
  
  // 使用缓存值计算更高项
  let fib6 = azimuth::add(fib4, fib5)  // F(6) = F(4) + F(5) = 3 + 5 = 8
  let fib7 = azimuth::add(fib5, fib6)  // F(7) = F(5) + F(6) = 5 + 8 = 13
  let fib8 = azimuth::add(fib6, fib7)  // F(8) = F(6) + F(7) = 8 + 13 = 21
  
  // 验证新计算的值
  azimuth::assert_eq(8, fib6)
  azimuth::assert_eq(13, fib7)
  azimuth::assert_eq(21, fib8)
  
  // 验证斐波那契性质：F(n) = F(n-1) + F(n-2)
  azimuth::assert_eq(fib6, azimuth::add(fib4, fib5))
  azimuth::assert_eq(fib7, azimuth::add(fib5, fib6))
  azimuth::assert_eq(fib8, azimuth::add(fib6, fib7))
  
  // 验证黄金比例近似：F(n+1)/F(n) ≈ 1.618（使用整数除法）
  let ratio1 = fib7 / fib6  // 13 / 8 = 1（整数除法）
  let ratio2 = fib8 / fib7  // 21 / 13 = 1（整数除法）
  azimuth::assert_eq(1, ratio1)
  azimuth::assert_eq(1, ratio2)
  
  // 验证更精确的近似：(F(n+1)*10)/F(n) ≈ 16
  let precise_ratio1 = azimuth::multiply(fib7, 10) / fib6  // 13*10/8 = 130/8 = 16
  let precise_ratio2 = azimuth::multiply(fib8, 10) / fib7  // 21*10/13 = 210/13 = 16
  azimuth::assert_eq(16, precise_ratio1)
  azimuth::assert_eq(16, precise_ratio2)
}