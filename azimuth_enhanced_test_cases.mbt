// Azimuth 项目增强测试用例
// 包含 8 个高质量的标准 MoonBit 测试用例

test "power_calculation_simulation" {
  // 幂运算模拟测试
  // 计算 2^4 = 16
  
  let base = 2
  let exponent = 4
  
  // 模拟幂运算：2^4 = 2 × 2 × 2 × 2
  let power1 = base
  let power2 = multiply(power1, base)  // 2^2 = 4
  let power3 = multiply(power2, base)  // 2^3 = 8
  let power4 = multiply(power3, base)  // 2^4 = 16
  
  assert_eq(power2, 4)
  assert_eq(power3, 8)
  assert_eq(power4, 16)
}

test "percentage_calculations" {
  // 百分比计算测试
  let original_value = 200
  let percentage = 15
  
  // 计算百分比值
  let percentage_value = divide_with_ceil(multiply(original_value, percentage), 100)
  
  // 计算增加百分比后的值
  let increased_value = add(original_value, percentage_value)
  
  // 计算减少百分比后的值
  let decreased_value = subtract(original_value, percentage_value)
  
  assert_eq(percentage_value, 30)
  assert_eq(increased_value, 230)
  assert_eq(decreased_value, 170)
}

test "average_calculation" {
  // 平均值计算测试
  let numbers_sum = add(add(add(add(45, 67), 23), 89), 56)
  let count = 5
  let average = divide_with_ceil(numbers_sum, count)
  
  // 验证总和和平均值
  assert_eq(numbers_sum, 280)
  assert_eq(average, 56)
}

test "modulo_operations_simulation" {
  // 模运算模拟测试
  let dividend = 27
  let divisor = 5
  
  // 计算商和余数
  let quotient = divide_with_ceil(dividend, divisor)
  let product = multiply(quotient, divisor)
  let remainder = subtract(dividend, product)
  
  // 修正向上取整的影响
  let corrected_quotient = subtract(quotient, 1)
  let corrected_product = multiply(corrected_quotient, divisor)
  let corrected_remainder = subtract(dividend, corrected_product)
  
  assert_eq(quotient, 6)
  assert_eq(corrected_quotient, 5)
  assert_eq(corrected_remainder, 2)
}

test "factorial_simulation_iterative" {
  // 阶乘迭代计算模拟
  // 计算 4! = 4 × 3 × 2 × 1 = 24
  
  let n = 4
  let result1 = multiply(4, 3)  // 4 × 3 = 12
  let result2 = multiply(result1, 2)  // 12 × 2 = 24
  let result3 = multiply(result2, 1)  // 24 × 1 = 24
  
  assert_eq(result1, 12)
  assert_eq(result2, 24)
  assert_eq(result3, 24)
}

test "greatest_common_divisor_simulation" {
  // 最大公约数模拟测试（欧几里得算法简化版）
  // 求 GCD(48, 18)
  
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法步骤
  let step1 = divide_with_ceil(a, b)  // 48 / 18 = 3
  let remainder1 = subtract(a, multiply(step1 - 1, b))  // 48 - 2×18 = 12
  let step2 = divide_with_ceil(b, remainder1)  // 18 / 12 = 2
  let remainder2 = subtract(b, multiply(step2 - 1, remainder1))  // 18 - 1×12 = 6
  let step3 = divide_with_ceil(remainder1, remainder2)  // 12 / 6 = 2
  let remainder3 = subtract(remainder1, multiply(step3 - 1, remainder2))  // 12 - 1×6 = 6
  
  // 当余数为0时，上一个余数就是GCD
  let gcd = remainder2
  
  assert_eq(step1, 3)
  assert_eq(remainder1, 12)
  assert_eq(step2, 2)
  assert_eq(remainder2, 6)
  assert_eq(step3, 2)
  assert_eq(remainder3, 6)
  assert_eq(gcd, 6)
}

test "prime_check_simulation" {
  // 质数检查模拟测试
  // 检查数字 17 是否为质数
  
  let number = 17
  let sqrt_approx = divide_with_ceil(number, 2)  // 简化的平方根近似
  
  // 检查是否能被 2 整除
  let divisible_by_2 = subtract(number, multiply(divide_with_ceil(number, 2), 2))
  
  // 检查是否能被 3 整除
  let divisible_by_3 = subtract(number, multiply(divide_with_ceil(number, 3), 3))
  
  // 检查是否能被 5 整除
  let divisible_by_5 = subtract(number, multiply(divide_with_ceil(number, 5), 5))
  
  // 检查是否能被 7 整除
  let divisible_by_7 = subtract(number, multiply(divide_with_ceil(number, 7), 7))
  
  // 如果所有余数都不为0，则可能是质数
  let is_prime_candidate = multiply(multiply(multiply(divisible_by_2, divisible_by_3), divisible_by_5), divisible_by_7)
  
  assert_eq(sqrt_approx, 9)
  assert_eq(divisible_by_2, 1)
  assert_eq(divisible_by_3, 2)
  assert_eq(divisible_by_5, 2)
  assert_eq(divisible_by_7, 3)
  assert_eq(is_prime_candidate, 12)
}

test "shopping_cart_total" {
  // 购物车总价计算测试
  let item1_price = 29
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  let item3_price = 8
  let item3_quantity = 4
  
  // 计算各商品小计
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let subtotal3 = multiply(item3_price, item3_quantity)
  
  // 计算商品总计
  let items_total = add(add(subtotal1, subtotal2), subtotal3)
  
  // 计算折扣（满100减10）
  let discount = divide_with_ceil(items_total, 100) * 10
  let subtotal_after_discount = subtract(items_total, discount)
  
  // 计算税费（8%）
  let tax = divide_with_ceil(multiply(subtotal_after_discount, 8), 100)
  
  // 计算最终总价
  let final_total = add(subtotal_after_discount, tax)
  
  assert_eq(subtotal1, 58)
  assert_eq(subtotal2, 45)
  assert_eq(subtotal3, 32)
  assert_eq(items_total, 135)
  assert_eq(discount, 10)
  assert_eq(subtotal_after_discount, 125)
  assert_eq(tax, 10)
  assert_eq(final_total, 135)
}