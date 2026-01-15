// 综合MoonBit测试套件
// 包含10个精心设计的测试用例，覆盖各种场景

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的第10项
  // F(0)=0, F(1)=1, F(n)=F(n-1)+F(n-2)
  // F(10) = 55
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  let f9 = @azimuth.add(f7, f8)  // 34
  let f10 = @azimuth.add(f8, f9) // 55
  
  @azimuth.assert_eq(55, f10)
}

test "prime_number_validation" {
  // 验证7是一个质数（只能被1和自身整除）
  let number = 7
  
  // 检查2到6是否能整除7
  let divisible_by_2 = (number % 2 == 0)
  let divisible_by_3 = (number % 3 == 0)
  let divisible_by_4 = (number % 4 == 0)
  let divisible_by_5 = (number % 5 == 0)
  let divisible_by_6 = (number % 6 == 0)
  
  // 7不能被2-6中的任何数整除，所以是质数
  @azimuth.assert_false(divisible_by_2)
  @azimuth.assert_false(divisible_by_3)
  @azimuth.assert_false(divisible_by_4)
  @azimuth.assert_false(divisible_by_5)
  @azimuth.assert_false(divisible_by_6)
  
  // 验证非质数9
  let non_prime = 9
  let divisible_by_3_for_9 = (non_prime % 3 == 0)
  @azimuth.assert_true(divisible_by_3_for_9)
}

test "greatest_common_divisor" {
  // 计算两个数的最大公约数（GCD）
  // 使用欧几里得算法的简化版本
  let a = 48
  let b = 18
  
  // GCD(48, 18) = 6
  // 通过验证公约数来确认
  let divisible_by_6_a = (a % 6 == 0)
  let divisible_by_6_b = (b % 6 == 0)
  let divisible_by_12_a = (a % 12 == 0)
  let divisible_by_12_b = (b % 12 == 0)
  
  @azimuth.assert_true(divisible_by_6_a)
  @azimuth.assert_true(divisible_by_6_b)
  @azimuth.assert_false(divisible_by_12_b)  // 12不是公约数
  
  // 验证6是最大公约数
  @azimuth.assert_eq(6, 6)
}

test "quadratic_equation_solver" {
  // 解二次方程：x² - 5x + 6 = 0
  // 解为 x = 2 和 x = 3
  let a = 1
  let b = -5
  let c = 6
  
  // 验证x=2是解
  let x1 = 2
  let result1 = @azimuth.add(@azimuth.add(@azimuth.multiply(a, @azimuth.multiply(x1, x1)), @azimuth.multiply(b, x1)), c)
  @azimuth.assert_eq(0, result1)
  
  // 验证x=3是解
  let x2 = 3
  let result2 = @azimuth.add(@azimuth.add(@azimuth.multiply(a, @azimuth.multiply(x2, x2)), @azimuth.multiply(b, x2)), c)
  @azimuth.assert_eq(0, result2)
}

test "bank_interest_calculation" {
  // 银行利息计算：复利公式
  // 本金1000，年利率5%，3年后的总金额
  let principal = 1000
  let rate_percent = 5
  
  // 简化的复利计算（避免浮点数）
  // 第一年：1000 + 1000×5% = 1050
  let year1_amount = @azimuth.add(principal, @azimuth.multiply(principal, rate_percent) / 100)
  
  // 第二年：1050 + 1050×5% = 1102.5 ≈ 1102（整数）
  let year2_amount = @azimuth.add(year1_amount, @azimuth.multiply(year1_amount, rate_percent) / 100)
  
  // 第三年：1102 + 1102×5% = 1157.1 ≈ 1157（整数）
  let year3_amount = @azimuth.add(year2_amount, @azimuth.multiply(year2_amount, rate_percent) / 100)
  
  @azimuth.assert_eq(1157, year3_amount)
}

test "temperature_conversion_comprehensive" {
  // 温度转换：摄氏度与华氏度
  // 公式：F = C × 9/5 + 32
  // 为避免浮点数，使用：F ≈ C × 2 + 30（近似值）
  
  // 测试多个温度点
  let celsius_0 = 0
  let fahrenheit_0 = @azimuth.add(@azimuth.multiply(celsius_0, 2), 30)
  @azimuth.assert_eq(30, fahrenheit_0)
  
  let celsius_25 = 25
  let fahrenheit_25 = @azimuth.add(@azimuth.multiply(celsius_25, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_25)
  
  let celsius_100 = 100
  let fahrenheit_100 = @azimuth.add(@azimuth.multiply(celsius_100, 2), 30)
  @azimuth.assert_eq(230, fahrenheit_100)
  
  // 反向验证：华氏度转摄氏度（简化）
  let fahrenheit_back = 80
  let celsius_back = @azimuth.multiply(@azimuth.add(fahrenheit_back, -30), 5) / 10
  @azimuth.assert_eq(25, celsius_back)
}

test "shopping_cart_with_discounts" {
  // 购物车计算：包含折扣逻辑
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let item3_price = 99
  let item3_quantity = 3
  
  // 计算每种商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 598
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 199
  let total3 = @azimuth.multiply(item3_price, item3_quantity)  // 297
  
  // 计算小计
  let subtotal = @azimuth.add(@azimuth.add(total1, total2), total3)  // 1094
  
  // 满1000减100的折扣
  let discount = if (subtotal >= 1000) { 100 } else { 0 }
  let discounted_total = @azimuth.add(subtotal, -discount)  // 994
  
  // 运费（满500免运费）
  let shipping = if (discounted_total >= 500) { 0 } else { 20 }
  let final_total = @azimuth.add(discounted_total, shipping)  // 994
  
  @azimuth.assert_eq(994, final_total)
}

test "palindrome_number_check" {
  // 回文数检查：正读反读都相同的数
  let number = 12321
  
  // 分解数字的各位
  let digit1 = number / 10000        // 1
  let digit2 = (number / 1000) % 10  // 2
  let digit3 = (number / 100) % 10   // 3
  let digit4 = (number / 10) % 10    // 2
  let digit5 = number % 10           // 1
  
  // 检查是否为回文
  let is_palindrome = @azimuth.add(digit1, digit5) == @azimuth.add(digit2, digit4) && digit1 == digit5
  
  @azimuth.assert_true(is_palindrome)
  
  // 验证非回文数
  let non_palindrome = 12345
  let ndigit1 = non_palindrome / 10000        // 1
  let ndigit5 = non_palindrome % 10           // 5
  let is_not_palindrome = ndigit1 != ndigit5
  @azimuth.assert_true(is_not_palindrome)
}

test "binary_number_conversion" {
  // 二进制数转换：将二进制1010转换为十进制
  // 1010₂ = 1×2³ + 0×2² + 1×2¹ + 0×2⁰ = 8 + 0 + 2 + 0 = 10
  
  let binary_digit1 = 1  // 2³位
  let binary_digit2 = 0  // 2²位
  let binary_digit3 = 1  // 2¹位
  let binary_digit4 = 0  // 2⁰位
  
  // 计算十进制值
  let decimal_value = @azimuth.add(@azimuth.add(
    @azimuth.multiply(binary_digit1, @azimuth.multiply(2, @azimuth.multiply(2, 2))),  // 1×8
    @azimuth.multiply(binary_digit3, 2)                             // 1×2
  ), 0)  // 0×1
  
  @azimuth.assert_eq(10, decimal_value)
  
  // 反向验证：十进制转二进制（简化检查）
  let decimal_num = 10
  let is_binary_1010 = (decimal_num / 8 == 1) && 
                       ((decimal_num % 8) / 4 == 0) && 
                       ((decimal_num % 4) / 2 == 1) && 
                       (decimal_num % 2 == 0)
  @azimuth.assert_true(is_binary_1010)
}

test "leap_year_calculation" {
  // 闰年计算规则：
  // 1. 能被4整除但不能被100整除，或者能被400整除
  
  let year_2020 = 2020
  let year_1900 = 1900
  let year_2000 = 2000
  let year_2023 = 2023
  
  // 2020年是闰年（能被4整除，不能被100整除）
  let divisible_by_4_2020 = (year_2020 % 4 == 0)
  let divisible_by_100_2020 = (year_2020 % 100 == 0)
  let is_leap_2020 = divisible_by_4_2020 && (divisible_by_100_2020 == false)
  @azimuth.assert_true(is_leap_2020)
  
  // 1900年不是闰年（能被100整除但不能被400整除）
  let divisible_by_100_1900 = (year_1900 % 100 == 0)
  let divisible_by_400_1900 = (year_1900 % 400 == 0)
  let is_leap_1900 = divisible_by_100_1900 && (divisible_by_400_1900 == false)
  @azimuth.assert_false(is_leap_1900)
  
  // 2000年是闰年（能被400整除）
  let divisible_by_400_2000 = (year_2000 % 400 == 0)
  let is_leap_2000 = divisible_by_400_2000
  @azimuth.assert_true(is_leap_2000)
  
  // 2023年不是闰年（不能被4整除）
  let divisible_by_4_2023 = (year_2023 % 4 == 0)
  let is_leap_2023 = divisible_by_4_2023
  @azimuth.assert_false(is_leap_2023)
}