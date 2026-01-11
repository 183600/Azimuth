// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

// 测试乘法幂等性：0和1的幂等性质
///|
test "multiplication_idempotent_elements" {
  // 0是乘法的零元：a * 0 = 0
  assert_eq(0, multiply(123, 0))
  assert_eq(0, multiply(-456, 0))
  assert_eq(0, multiply(0, 789))
  assert_eq(0, multiply(0, -321))
  
  // 1是乘法的单位元：a * 1 = a
  assert_eq(123, multiply(123, 1))
  assert_eq(-456, multiply(-456, 1))
  assert_eq(789, multiply(1, 789))
  assert_eq(-321, multiply(1, -321))
  
  // 自乘幂等性：0 * 0 = 0，1 * 1 = 1
  assert_eq(0, multiply(0, 0))
  assert_eq(1, multiply(1, 1))
}

// 测试数学恒等式：平方差公式
///|
test "mathematical_identity_difference_of_squares" {
  // 验证平方差公式：a² - b² = (a + b)(a - b)
  // 由于我们没有减法，使用 a² + (-b²) = (a + b)(a + (-b))
  
  let a = 15
  let b = 7
  
  // 计算 a² - b²
  let a_squared = multiply(a, a)
  let b_squared = multiply(b, b)
  let difference = add(a_squared, multiply(-1, b_squared))
  
  // 计算 (a + b)(a - b)
  let sum = add(a, b)
  let diff = add(a, multiply(-1, b))
  let product = multiply(sum, diff)
  
  // 验证两者相等
  assert_eq(difference, product)
  
  // 测试另一组数值
  let x = 20
  let y = 12
  
  let x_squared = multiply(x, x)
  let y_squared = multiply(y, y)
  let diff2 = add(x_squared, multiply(-1, y_squared))
  
  let sum2 = add(x, y)
  let diff_y = add(x, multiply(-1, y))
  let product2 = multiply(sum2, diff_y)
  
  assert_eq(diff2, product2)
}

// 测试实际应用场景：计算折扣价格
///|
test "practical_discount_calculation" {
  // 原价和折扣率
  let original_price = 1000  // 1000元
  let discount_percentage = 20  // 20%折扣
  
  // 折扣后价格 = 原价 * (1 - 折扣率/100)
  // 我们通过整数运算避免小数
  let discount_factor = add(100, multiply(-1, discount_percentage))  // 100 - 20 = 80
  let discounted_price_times_100 = multiply(original_price, discount_factor)  // 1000 * 80 = 80000
  let discounted_price = multiply(8, 100)  // 800元，验证计算
  
  // 验证折扣金额
  let discount_amount_times_100 = multiply(original_price, discount_percentage)  // 1000 * 20 = 20000
  assert_eq(20000, discount_amount_times_100)
  
  // 验证折扣后价格 + 折扣金额 = 原价
  let total_times_100 = add(discounted_price_times_100, discount_amount_times_100)
  assert_eq(100000, total_times_100)  // 1000 * 100
}

// 测试实际应用场景：BMI指数计算
///|
test "practical_bmi_calculation" {
  // BMI = 体重(kg) / 身高(m)²
  // 我们通过整数运算来验证BMI计算的基本原理
  
  let weight = 70  // 70kg
  let height_cm = 175  // 175cm
  
  // 验证基本数学运算
  assert_eq(70, weight)
  assert_eq(175, height_cm)
  
  // 验证身高平方计算
  let height_squared = multiply(height_cm, height_cm)  // 175² = 30625
  assert_eq(30625, height_squared)
  
  // 验证体重乘以1000
  let weight_times_1000 = multiply(weight, 1000)  // 70 * 1000 = 70000
  assert_eq(70000, weight_times_1000)
  
  // 验证BMI计算的基本原理：BMI在正常范围内(18.5-24.9)
  // 我们不进行精确计算，只验证数学运算
  let normal_bmi_low = 185  // 18.5 * 10
  let normal_bmi_high = 249  // 24.9 * 10
  
  assert_eq(true, normal_bmi_low < normal_bmi_high)
}

// 测试斐波那契数列计算
///|
test "fibonacci_sequence_calculation" {
  // 使用加法计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  let f0 = 0
  let f1 = 1
  
  // F(2) = F(1) + F(0) = 1 + 0 = 1
  let f2 = add(f1, f0)
  assert_eq(1, f2)
  
  // F(3) = F(2) + F(1) = 1 + 1 = 2
  let f3 = add(f2, f1)
  assert_eq(2, f3)
  
  // F(4) = F(3) + F(2) = 2 + 1 = 3
  let f4 = add(f3, f2)
  assert_eq(3, f4)
  
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  let f5 = add(f4, f3)
  assert_eq(5, f5)
  
  // F(6) = F(5) + F(4) = 5 + 3 = 8
  let f6 = add(f5, f4)
  assert_eq(8, f6)
  
  // F(7) = F(6) + F(5) = 8 + 5 = 13
  let f7 = add(f6, f5)
  assert_eq(13, f7)
  
  // F(8) = F(7) + F(6) = 13 + 8 = 21
  let f8 = add(f7, f6)
  assert_eq(21, f8)
}

// 测试阶乘计算
///|
test "factorial_calculation" {
  // n! = n * (n-1) * (n-2) * ... * 1
  // 0! = 1, 1! = 1
  
  // 1! = 1
  let factorial_1 = 1
  assert_eq(1, factorial_1)
  
  // 2! = 2 * 1 = 2
  let factorial_2 = multiply(2, 1)
  assert_eq(2, factorial_2)
  
  // 3! = 3 * 2 * 1 = 6
  let factorial_3 = multiply(3, multiply(2, 1))
  assert_eq(6, factorial_3)
  
  // 4! = 4 * 3 * 2 * 1 = 24
  let factorial_4 = multiply(4, multiply(3, multiply(2, 1)))
  assert_eq(24, factorial_4)
  
  // 5! = 5 * 4 * 3 * 2 * 1 = 120
  let factorial_5 = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial_5)
  
  // 6! = 6 * 5! = 6 * 120 = 720
  let factorial_6 = multiply(6, factorial_5)
  assert_eq(720, factorial_6)
}

// 测试最大公约数（欧几里得算法模拟）
///|
test "gcd_calculation_simulation" {
  // 模拟欧几里得算法计算最大公约数
  // gcd(a, b) = gcd(b, a mod b)
  
  // gcd(48, 18) = 6
  let a = 48
  let b = 18
  
  // 48 mod 18 = 48 - 2 * 18 = 12
  let remainder1 = add(a, multiply(-2, b))
  assert_eq(12, remainder1)
  
  // 18 mod 12 = 18 - 1 * 12 = 6
  let remainder2 = add(b, multiply(-1, remainder1))
  assert_eq(6, remainder2)
  
  // 12 mod 6 = 12 - 2 * 6 = 0
  let remainder3 = add(remainder1, multiply(-2, remainder2))
  assert_eq(0, remainder3)
  
  // gcd是最后一个非零余数：6
  let gcd = remainder2
  assert_eq(6, gcd)
  
  // 验证：6能整除48和18
  assert_eq(0, add(48, multiply(-8, 6)))  // 48 - 8 * 6 = 0
  assert_eq(0, add(18, multiply(-3, 6)))  // 18 - 3 * 6 = 0
}

// 测试二进制位运算相关的数学运算
///|
test "binary_operations_simulation" {
  // 模拟二进制左移（乘以2的幂）
  // x << n = x * 2^n
  
  let x = 13  // 二进制 1101
  
  // 13 << 1 = 13 * 2 = 26
  let left_shift_1 = multiply(x, 2)
  assert_eq(26, left_shift_1)
  
  // 13 << 2 = 13 * 4 = 52
  let left_shift_2 = multiply(x, 4)
  assert_eq(52, left_shift_2)
  
  // 13 << 3 = 13 * 8 = 104
  let left_shift_3 = multiply(x, 8)
  assert_eq(104, left_shift_3)
  
  // 模拟二进制右移（除以2的幂，使用减法模拟）
  // x >> n = x / 2^n
  
  // 52 >> 1 = 52 / 2 = 26
  // 验证：26 * 2 = 52
  assert_eq(52, multiply(26, 2))
  
  // 52 >> 2 = 52 / 4 = 13
  // 验证：13 * 4 = 52
  assert_eq(52, multiply(13, 4))
}

// 测试字符串拼接的边界情况
///|
test "string_concatenation_edge_cases" {
  // 测试包含数字和特殊字符的字符串
  assert_eq("Hello, 12345!", greet("12345"))
  assert_eq("Hello, test@example.com!", greet("test@example.com"))
  assert_eq("Hello, https://moonbit.lang!", greet("https://moonbit.lang"))
  
  // 测试包含空格的字符串
  assert_eq("Hello, John Doe!", greet("John Doe"))
  assert_eq("Hello,   !", greet("  "))  // 两个空格
  assert_eq("Hello,  leading space!", greet(" leading space"))  // 前导空格
  assert_eq("Hello, trailing space !", greet("trailing space "))  // 尾随空格
  
  // 测试字符串长度
  let short_name = "A"
  let long_name = "ThisIsAVeryLongNameToTestStringHandling"
  
  let short_greeting = greet(short_name)
  let long_greeting = greet(long_name)
  
  // 验证长度差异
  assert_eq(true, long_greeting.length() > short_greeting.length())
  assert_eq(9, short_greeting.length())  // "Hello, A!" = 9个字符
}

// 测试数学恒等式：立方和公式
///|
test "mathematical_identity_sum_of_cubes" {
  // 验证立方和公式：a³ + b³ = (a + b)(a² - ab + b²)
  
  let a = 4
  let b = 3
  
  // 计算 a³ + b³
  let a_cubed = multiply(a, multiply(a, a))  // 4³ = 64
  let b_cubed = multiply(b, multiply(b, b))  // 3³ = 27
  let sum_of_cubes = add(a_cubed, b_cubed)  // 64 + 27 = 91
  
  // 计算 (a + b)(a² - ab + b²)
  let sum = add(a, b)  // 4 + 3 = 7
  
  let a_squared = multiply(a, a)  // 4² = 16
  let b_squared = multiply(b, b)  // 3² = 9
  let ab = multiply(a, b)  // 4 * 3 = 12
  
  let second_factor = add(add(a_squared, multiply(-1, ab)), b_squared)  // 16 - 12 + 9 = 13
  let product = multiply(sum, second_factor)  // 7 * 13 = 91
  
  // 验证两者相等
  assert_eq(sum_of_cubes, product)
  assert_eq(91, sum_of_cubes)
  assert_eq(91, product)
}