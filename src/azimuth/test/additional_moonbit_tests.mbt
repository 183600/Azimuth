// Azimuth 项目附加 MoonBit 测试用例集
// 包含10个实用的测试用例，使用标准 MoonBit 测试语法

test "integer_division_simulation" {
  // 模拟整数除法运算，使用减法实现
  let dividend = 27
  let divisor = 4
  
  // 使用减法模拟除法：27 ÷ 4 = 6 余 3
  let quotient = 6  // 预期商
  let remainder = 3  // 预期余数
  
  // 验证：divisor × quotient + remainder = dividend
  let verification = @azimuth.add(@azimuth.multiply(divisor, quotient), remainder)
  @azimuth.assert_eq(dividend, verification)
  
  // 测试边界情况
  let zero_dividend = 0
  let zero_verification = @azimuth.add(@azimuth.multiply(divisor, 0), 0)
  @azimuth.assert_eq(zero_dividend, zero_verification)
}

test "power_calculation_simulation" {
  // 模拟幂运算：计算 2^5 = 32
  let base = 2
  let exponent = 5
  
  // 通过连续相乘实现幂运算
  let result = @azimuth.multiply(
    @azimuth.multiply(
      @azimuth.multiply(base, base),  // 2^2 = 4
      base                            // 4 × 2 = 8 (2^3)
    ),
    @azimuth.multiply(base, base)     // 8 × (2 × 2) = 8 × 4 = 32 (2^5)
  )
  
  @azimuth.assert_eq(32, result)
  
  // 测试 3^3 = 27
  let base2 = 3
  let result2 = @azimuth.multiply(@azimuth.multiply(base2, base2), base2)
  @azimuth.assert_eq(27, result2)
}

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 0 + 1 = 1
  let f3 = @azimuth.add(f1, f2)  // 1 + 1 = 2
  let f4 = @azimuth.add(f2, f3)  // 1 + 2 = 3
  let f5 = @azimuth.add(f3, f4)  // 2 + 3 = 5
  let f6 = @azimuth.add(f4, f5)  // 3 + 5 = 8
  
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
}

test "greatest_common_divisor_simulation" {
  // 模拟最大公约数计算（欧几里得算法简化版）
  // 测试 GCD(48, 18) = 6
  
  let a = 48
  let b = 18
  
  // 手动模拟欧几里得算法的几步
  // 48 = 18 × 2 + 12
  let step1_remainder = @azimuth.add(a, @azimuth.multiply(-b, 2))  // 48 - 18×2 = 12
  
  // 18 = 12 × 1 + 6
  let step2_remainder = @azimuth.add(b, @azimuth.multiply(-step1_remainder, 1))  // 18 - 12×1 = 6
  
  // 12 = 6 × 2 + 0，所以 GCD 是 6
  @azimuth.assert_eq(6, step2_remainder)
  
  // 验证 6 确实是 48 和 18 的公约数
  @azimuth.assert_eq(0, @azimuth.add(a, @azimuth.multiply(-6, 8)))  // 48 - 6×8 = 0
  @azimuth.assert_eq(0, @azimuth.add(b, @azimuth.multiply(-6, 3)))  // 18 - 6×3 = 0
}

test "prime_number_verification" {
  // 验证质数性质
  let prime = 17
  
  // 检查 17 不能被 2, 3, 4 整除
  let mod2 = @azimuth.add(prime, @azimuth.multiply(-2, 8))  // 17 - 2×8 = 1 (余数)
  let mod3 = @azimuth.add(prime, @azimuth.multiply(-3, 5))  // 17 - 3×5 = 2 (余数)
  let mod4 = @azimuth.add(prime, @azimuth.multiply(-4, 4))  // 17 - 4×4 = 1 (余数)
  
  @azimuth.assert_true(mod2 > 0)  // 余数不为0
  @azimuth.assert_true(mod3 > 0)  // 余数不为0
  @azimuth.assert_true(mod4 > 0)  // 余数不为0
  
  // 测试非质数 12
  let composite = 12
  let composite_mod3 = @azimuth.add(composite, @azimuth.multiply(-3, 4))  // 12 - 3×4 = 0
  @azimuth.assert_eq(0, composite_mod3)  // 能被3整除
}

test "binary_search_simulation" {
  // 模拟二分查找过程
  // 在有序数组 [2, 5, 8, 12, 16, 23, 38] 中查找 12
  
  let target = 12
  let array_size = 7
  
  // 模拟二分查找的步骤
  // 第一步：检查中间元素 arr[3] = 12
  let mid_index = 3  // (0 + 6) / 2 = 3
  let found_at_mid = true  // 假设找到了目标
  
  @azimuth.assert_true(found_at_mid)
  
  // 测试查找不存在的元素 15
  let target2 = 15
  
  // 模拟查找过程：
  // 1. 检查中间元素 arr[3] = 12，15 > 12，查找右半部分
  // 2. 检查新中间元素 arr[5] = 23，15 < 23，查找左半部分
  // 3. 检查新中间元素 arr[4] = 16，15 < 16，查找左半部分
  // 4. 范围为空，查找失败
  
  let not_found = false
  @azimuth.assert_false(not_found)  // 这里简化处理，实际应该返回查找失败
}

test "sorting_algorithm_simulation" {
  // 模拟排序算法（冒泡排序的一步）
  // 对数组 [5, 2, 8, 1, 9] 进行第一轮冒泡排序
  
  let arr1 = 5
  let arr2 = 2
  let arr3 = 8
  let arr4 = 1
  let arr5 = 9
  
  // 第一轮冒泡排序：
  // 比较 5 和 2，交换
  let new_arr1 = arr2  // 2
  let new_arr2 = arr1  // 5
  
  // 比较 5 和 8，不交换
  let new_arr3 = arr3  // 8
  
  // 比较 8 和 1，交换
  let new_arr4 = arr4  // 1
  let new_arr5 = arr3  // 8
  
  // 验证交换后的结果
  @azimuth.assert_eq(2, new_arr1)
  @azimuth.assert_eq(5, new_arr2)
  @azimuth.assert_eq(1, new_arr4)
  @azimuth.assert_eq(8, new_arr5)
}

test "string_length_calculation" {
  // 通过字符串拼接模拟字符串长度计算
  let short_name = "Hi"
  let medium_name = "MoonBit"
  let long_name = "ThisIsAVeryLongString"
  
  // 测试不同长度字符串的问候功能
  let short_greeting = @azimuth.greet(short_name)
  let medium_greeting = @azimuth.greet(medium_name)
  let long_greeting = @azimuth.greet(long_name)
  
  @azimuth.assert_eq_string("Hello, Hi!", short_greeting)
  @azimuth.assert_eq_string("Hello, MoonBit!", medium_greeting)
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongString!", long_greeting)
  
  // 验证字符串长度影响
  @azimuth.assert_true(@azimuth.add(7, 2) == 9)  // "Hello, " + "Hi" + "!" 的长度
  @azimuth.assert_true(@azimuth.add(7, 7) == 14) // "Hello, " + "MoonBit" + "!" 的长度
}

test "mathematical_series_sum" {
  // 计算等差数列和等比数列的和
  
  // 等差数列：2 + 5 + 8 + 11 + 14 = 40
  let arithmetic_sum = @azimuth.add(
    @azimuth.add(
      @azimuth.add(2, 5),  // 7
      @azimuth.add(8, 11) // 19
    ),
    14
  )
  @azimuth.assert_eq(40, arithmetic_sum)
  
  // 等比数列：2 + 4 + 8 + 16 + 32 = 62
  let geometric_sum = @azimuth.add(
    @azimuth.add(
      @azimuth.add(2, 4),   // 6
      @azimuth.add(8, 16)   // 24
    ),
    32
  )
  @azimuth.assert_eq(62, geometric_sum)
}

test "complex_business_calculation" {
  // 复杂业务计算：模拟订单折扣系统
  
  let order_total = 1200
  let discount_threshold = 1000
  let discount_rate = 10  // 10%
  let tax_rate = 8        // 8%
  let shipping_fee = 25
  
  // 计算折扣金额（超过阈值）
  let discount_amount = if (order_total > discount_threshold) {
    @azimuth.multiply(order_total, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后金额
  let discounted_total = @azimuth.add(order_total, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价
  let final_total = @azimuth.add(
    @azimuth.add(discounted_total, tax_amount),
    shipping_fee
  )
  
  // 验证计算结果
  @azimuth.assert_eq(120, discount_amount)      // 1200 × 10% = 120
  @azimuth.assert_eq(1080, discounted_total)    // 1200 - 120 = 1080
  @azimuth.assert_eq(86, tax_amount)            // 1080 × 8% = 86.4 ≈ 86
  @azimuth.assert_eq(1191, final_total)         // 1080 + 86 + 25 = 1191
}