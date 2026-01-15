// Azimuth 项目额外的 MoonBit 测试用例
// 补充测试覆盖，确保代码质量和功能完整性

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  // 正常情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(20, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(100, 10))
  
  // 负数情况
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 4))
}

test "boundary_condition_combinations" {
  // 测试边界条件的复杂组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算组合
  let sum_max_min = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, sum_max_min)
  
  // 最大值与自身的运算
  let sum_max_max = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, sum_max_max)
  
  let mul_max_max = @azimuth.multiply(max_val, max_val)
  @azimuth.assert_eq(max_val, mul_max_max)
  
  // 最小值与自身的运算
  let sum_min_min = @azimuth.add(min_val, min_val)
  @azimuth.assert_eq(min_val, sum_min_min)
  
  let mul_min_min = @azimuth.multiply(min_val, min_val)
  @azimuth.assert_eq(min_val, mul_min_min)
  
  // 链式运算
  let chain_result = @azimuth.add(@azimuth.multiply(max_val, 0), min_val)
  @azimuth.assert_eq(min_val, chain_result)
}

test "discount_calculation_business_logic" {
  // 测试商业折扣计算逻辑
  let original_price = 1000
  let discount_rate = 20  // 20% 折扣
  let quantity = 5
  let shipping_fee = 50
  
  // 计算折扣后的单价
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  // 计算总价
  let subtotal = @azimuth.multiply(discounted_price, quantity)
  let total_cost = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(800, discounted_price)  // 1000 - 200
  @azimuth.assert_eq(4000, subtotal)         // 800 * 5
  @azimuth.assert_eq(4050, total_cost)       // 4000 + 50
  
  // 测试批量折扣（购买超过一定数量有额外折扣）
  let bulk_quantity = 10
  let bulk_discount_rate = 5  // 额外5%折扣
  let bulk_discount_amount = @azimuth.multiply(discounted_price, bulk_discount_rate) / 100
  let bulk_price = @azimuth.add(discounted_price, -bulk_discount_amount)
  let bulk_total = @azimuth.add(@azimuth.multiply(bulk_price, bulk_quantity), shipping_fee)
  
  @azimuth.assert_eq(760, bulk_price)  // 800 - 40
  @azimuth.assert_eq(7650, bulk_total) // 760 * 10 + 50
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 等差数列：1, 4, 7, 10, 13, ... (公差为3)
  let arithmetic_first = 1
  let arithmetic_diff = 3
  let arithmetic_n = 5
  
  // 第n项 = 首项 + (n-1) × 公差
  let arithmetic_nth = @azimuth.add(arithmetic_first, @azimuth.multiply(arithmetic_n - 1, arithmetic_diff))
  @azimuth.assert_eq(13, arithmetic_nth)
  
  // 等比数列：2, 6, 18, 54, ... (公比为3)
  let geometric_first = 2
  let geometric_ratio = 3
  let geometric_n = 4
  
  // 第n项 = 首项 × 公比^(n-1)
  let geometric_nth = @azimuth.multiply(geometric_first, @azimuth.multiply(@azimuth.multiply(3, 3), 3))
  @azimuth.assert_eq(54, geometric_nth)
  
  // 平方数列：1, 4, 9, 16, 25, ...
  let square_n = 5
  let square_nth = @azimuth.multiply(square_n, square_n)
  @azimuth.assert_eq(25, square_nth)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试各种特殊字符组合
  let test_cases = [
    "",                    // 空字符串
    "a",                   // 单个字符
    " ",                   // 单个空格
    "  ",                  // 多个空格
    "\t",                  // 制表符
    "\n",                  // 换行符
    "name\tmiddle\tlast",  // 包含制表符
    "line1\nline2",        // 包含换行符
    "   leading",          // 前导空格
    "trailing   ",         // 尾随空格
    "  both  ",            // 前后都有空格
  ]
  
  // 测试每种情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \t!", @azimuth.greet("\t"))
  @azimuth.assert_eq_string("Hello, \n!", @azimuth.greet("\n"))
  @azimuth.assert_eq_string("Hello, name\tmiddle\tlast!", @azimuth.greet("name\tmiddle\tlast"))
  @azimuth.assert_eq_string("Hello, line1\nline2!", @azimuth.greet("line1\nline2"))
  @azimuth.assert_eq_string("Hello,    leading!", @azimuth.greet("   leading"))
  @azimuth.assert_eq_string("Hello, trailing   !", @azimuth.greet("trailing   "))
  @azimuth.assert_eq_string("Hello,   both  !", @azimuth.greet("  both  "))
}

test "numerical_precision_handling" {
  // 测试数值精度处理
  // 大数运算
  let large_num1 = 100000000
  let large_num2 = 200000000
  
  let large_sum = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(300000000, large_sum)
  
  let large_product = @azimuth.multiply(10000, 20000)
  @azimuth.assert_eq(200000000, large_product)
  
  // 小数模拟（使用整数表示）
  let price_in_cents = 9999  // $99.99
  let quantity = 3
  let total_cents = @azimuth.multiply(price_in_cents, quantity)
  @azimuth.assert_eq(29997, total_cents)
  
  // 转换回元（整数除法）
  let total_dollars = total_cents / 100
  let remaining_cents = total_cents % 100
  @azimuth.assert_eq(299, total_dollars)
  @azimuth.assert_eq(97, remaining_cents)
  
  // 百分比计算
  let total = 1000
  let percentage = 23  // 23%
  let percentage_value = @azimuth.multiply(total, percentage) / 100
  @azimuth.assert_eq(230, percentage_value)
}

test "conditional_logic_combinations" {
  // 测试条件逻辑组合
  // 模拟复杂的业务规则
  let age = 25
  let income = 50000
  let credit_score = 750
  let is_student = false
  
  // 贷款资格评估（简化版）
  let age_eligible = age >= 18 && age <= 65
  let income_eligible = income >= 30000
  let credit_eligible = credit_score >= 700
  let student_bonus = is_student && credit_score >= 650
  
  // 综合评估
  let base_eligible = if age_eligible && income_eligible && credit_eligible {
    1
  } else {
    0
  }
  
  let final_eligible = @azimuth.add(base_eligible, if student_bonus { 1 } else { 0 })
  
  @azimuth.assert_eq(1, final_eligible)
  
  // 测试不同情况
  let age2 = 17
  let income2 = 60000
  let credit_score2 = 800
  let is_student2 = true
  
  let age_eligible2 = age2 >= 18 && age2 <= 65
  let income_eligible2 = income2 >= 30000
  let credit_eligible2 = credit_score2 >= 700
  let student_bonus2 = is_student2 && credit_score2 >= 650
  
  let base_eligible2 = if age_eligible2 && income_eligible2 && credit_eligible2 {
    1
  } else {
    0
  }
  
  let final_eligible2 = @azimuth.add(base_eligible2, if student_bonus2 { 1 } else { 0 })
  
  @azimuth.assert_eq(1, final_eligible2)  // 学生特殊资格
}

test "algorithm_gcd_calculation" {
  // 测试算法实现：最大公约数（GCD）
  // 使用欧几里得算法的模拟实现
  
  // GCD(48, 18) = 6
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法
  let step1_a = b        // 18
  let step1_b = a % b    // 48 % 18 = 12
  let step2_a = step1_b  // 12
  let step2_b = step1_a % step1_b  // 18 % 12 = 6
  let step3_a = step2_b  // 6
  let step3_b = step2_a % step2_b  // 12 % 6 = 0
  
  // 当余数为0时，当前a就是GCD
  let gcd_result = step3_a
  @azimuth.assert_eq(6, gcd_result)
  
  // GCD(100, 25) = 25
  let c = 100
  let d = 25
  
  let gcd_step1_a = d      // 25
  let gcd_step1_b = c % d  // 100 % 25 = 0
  
  // 余数为0，GCD就是d
  let gcd_result2 = gcd_step1_a
  @azimuth.assert_eq(25, gcd_result2)
  
  // GCD(17, 13) = 17 (质数)
  let e = 17
  let f = 13
  
  let prime_step1_a = f     // 13
  let prime_step1_b = e % f // 17 % 13 = 4
  let prime_step2_a = prime_step1_b  // 4
  let prime_step2_b = prime_step1_a % prime_step1_b  // 13 % 4 = 1
  let prime_step3_a = prime_step2_b  // 1
  let prime_step3_b = prime_step2_a % prime_step2_b  // 4 % 1 = 0
  
  let gcd_result3 = prime_step3_a
  @azimuth.assert_eq(1, gcd_result3)
}

test "algorithm_fibonacci_sequence" {
  // 测试算法实现：斐波那契数列
  // F(0)=0, F(1)=1, F(n)=F(n-1)+F(n-2)
  
  // 计算前几项
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f1, f0)  // 1
  let f3 = @azimuth.add(f2, f1)  // 2
  let f4 = @azimuth.add(f3, f2)  // 3
  let f5 = @azimuth.add(f4, f3)  // 5
  let f6 = @azimuth.add(f5, f4)  // 8
  let f7 = @azimuth.add(f6, f5)  // 13
  let f8 = @azimuth.add(f7, f6)  // 21
  
  @azimuth.assert_eq(0, f0)
  @azimuth.assert_eq(1, f1)
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  @azimuth.assert_eq(21, f8)
  
  // 计算斐波那契数列的和
  let sum_first_8 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(f0, f1), f2), f3), f4), f5), f6), f7)
  @azimuth.assert_eq(33, sum_first_8)
  
  // 计算特定项的倍数
  let f5_times_3 = @azimuth.multiply(f5, 3)
  @azimuth.assert_eq(15, f5_times_3)
}

test "comprehensive_application_scenario" {
  // 测试综合应用场景：电商订单处理系统
  // 产品信息
  let product_a_price = 299
  let product_a_qty = 2
  let product_b_price = 199
  let product_b_qty = 3
  let product_c_price = 99
  let product_c_qty = 1
  
  // 优惠规则
  let discount_threshold = 800  // 满800减100
  let discount_amount = 100
  let vip_discount_rate = 5     // VIP会员额外5%折扣
  let shipping_fee = 20
  let free_shipping_threshold = 1000  // 满1000免运费
  
  // 计算商品总价
  let total_a = @azimuth.multiply(product_a_price, product_a_qty)
  let total_b = @azimuth.multiply(product_b_price, product_b_qty)
  let total_c = @azimuth.multiply(product_c_price, product_c_qty)
  let subtotal = @azimuth.add(@azimuth.add(total_a, total_b), total_c)
  
  @azimuth.assert_eq(598, total_a)   // 299 * 2
  @azimuth.assert_eq(597, total_b)   // 199 * 3
  @azimuth.assert_eq(99, total_c)    // 99 * 1
  @azimuth.assert_eq(1294, subtotal) // 598 + 597 + 99
  
  // 应用满减优惠
  let discount_applied = if subtotal >= discount_threshold {
    discount_amount
  } else {
    0
  }
  
  let after_discount = @azimuth.add(subtotal, -discount_applied)
  @azimuth.assert_eq(1194, after_discount)  // 1294 - 100
  
  // 应用VIP折扣
  let is_vip = true
  let vip_discount = if is_vip {
    @azimuth.multiply(after_discount, vip_discount_rate) / 100
  } else {
    0
  }
  
  let after_vip = @azimuth.add(after_discount, -vip_discount)
  @azimuth.assert_eq(1134, after_vip)  // 1194 - 60 (5% of 1194 ≈ 60)
  
  // 计算运费
  let final_shipping = if after_vip >= free_shipping_threshold {
    0
  } else {
    shipping_fee
  }
  
  let final_total = @azimuth.add(after_vip, final_shipping)
  @azimuth.assert_eq(1134, final_total)  // 免运费
  
  // 计算订单积分（1元1积分）
  let points_earned = final_total
  @azimuth.assert_eq(1134, points_earned)
  
  // 计算会员等级提升（每1000积分升一级）
  let level_increase = points_earned / 1000
  @azimuth.assert_eq(1, level_increase)
}