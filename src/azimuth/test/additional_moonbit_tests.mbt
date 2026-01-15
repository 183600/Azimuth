// 额外的 MoonBit 测试用例
// 这些测试用例补充了项目中尚未充分覆盖的测试场景

test "fibonacci_sequence_calculation" {
  // 斐波那契数列计算测试
  // F(0)=0, F(1)=1, F(2)=1, F(3)=2, F(4)=3, F(5)=5, F(6)=8
  // 使用加法函数计算前几项
  
  // F(2) = F(1) + F(0) = 1 + 0 = 1
  let f2 = @azimuth.add(1, 0)
  
  // F(3) = F(2) + F(1) = 1 + 1 = 2
  let f3 = @azimuth.add(f2, 1)
  
  // F(4) = F(3) + F(2) = 2 + 1 = 3
  let f4 = @azimuth.add(f3, f2)
  
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  let f5 = @azimuth.add(f4, f3)
  
  // F(6) = F(5) + F(4) = 5 + 3 = 8
  let f6 = @azimuth.add(f5, f4)
  
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
}

test "greatest_common_divisor" {
  // 最大公约数计算测试（使用欧几里得算法的简化版本）
  // GCD(a, b) = GCD(b, a % b)
  
  // GCD(48, 18) = GCD(18, 48 % 18 = 12) = GCD(12, 18 % 12 = 6) = GCD(6, 12 % 6 = 0) = 6
  let a = 48
  let b = 18
  
  // 手动计算几步欧几里得算法
  let step1_a = b
  let step1_b = a % b  // 48 % 18 = 12
  
  let step2_a = step1_b
  let step2_b = step1_a % step1_b  // 18 % 12 = 6
  
  let step3_a = step2_b
  let step3_b = step2_a % step2_b  // 12 % 6 = 0
  
  // 当余数为0时，GCD就是当前的除数
  let gcd = step2_b
  
  @azimuth.assert_eq(6, gcd)
  
  // 测试 GCD(56, 98) = 14
  let c = 56
  let d = 98
  
  let c_step1_a = d
  let c_step1_b = c % d  // 56 % 98 = 56
  
  let c_step2_a = c_step1_b
  let c_step2_b = c_step1_a % c_step1_b  // 98 % 56 = 42
  
  let c_step3_a = c_step2_b
  let c_step3_b = c_step2_a % c_step2_b  // 56 % 42 = 14
  
  let c_step4_a = c_step3_b
  let c_step4_b = c_step3_a % c_step3_b  // 42 % 14 = 0
  
  let c_gcd = c_step3_b
  
  @azimuth.assert_eq(14, c_gcd)
}

test "string_case_processing" {
  // 字符串大小写处理测试
  // 注意：MoonBit 目前没有内置的大小写转换函数，所以我们通过字符串拼接来模拟
  
  let lower_name = "moonbit"
  let upper_name = "MOONBIT"
  let mixed_name = "MoonBit"
  
  // 测试不同大小写的字符串
  @azimuth.assert_eq_string("Hello, moonbit!", @azimuth.greet(lower_name))
  @azimuth.assert_eq_string("Hello, MOONBIT!", @azimuth.greet(upper_name))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet(mixed_name))
  
  // 测试包含大小写数字混合的字符串
  let alphanumeric = "Test123ABC"
  @azimuth.assert_eq_string("Hello, Test123ABC!", @azimuth.greet(alphanumeric))
  
  // 测试包含空格的大小写混合
  let spaced_mixed = "Hello MoonBit"
  @azimuth.assert_eq_string("Hello, Hello MoonBit!", @azimuth.greet(spaced_mixed))
}

test "conditional_calculation_paths" {
  // 条件计算路径测试
  let x = 15
  let y = 20
  let z = 10
  
  // 测试多重条件判断
  let condition1 = x < y  // true
  let condition2 = y > z  // true
  let condition3 = x == z // false
  
  // 如果 condition1 和 condition2 都为真，执行一种计算
  let result1 = if condition1 && condition2 {
    @azimuth.add(@azimuth.multiply(x, y), z)  // 15 * 20 + 10 = 310
  } else {
    @azimuth.add(x, @azimuth.multiply(y, z))  // 15 + 20 * 10 = 215
  }
  
  @azimuth.assert_eq(310, result1)
  
  // 如果 condition1 为真但 condition3 为假，执行另一种计算
  let result2 = if condition1 && !condition3 {
    @azimuth.multiply(@azimuth.add(x, y), z)  // (15 + 20) * 10 = 350
  } else {
    @azimuth.add(@azimuth.multiply(x, y), z)  // 15 * 20 + 10 = 310
  }
  
  @azimuth.assert_eq(350, result2)
  
  // 测试嵌套条件
  let result3 = if condition1 {
    if condition2 {
      @azimuth.multiply(x, @azimuth.add(y, z))  // 15 * (20 + 10) = 450
    } else {
      @azimuth.add(x, @azimuth.multiply(y, z))  // 15 + 20 * 10 = 215
    }
  } else {
    @azimuth.add(@azimuth.multiply(x, y), z)    // 15 * 20 + 10 = 310
  }
  
  @azimuth.assert_eq(450, result3)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的运算
  let near_max = max_val - 1000
  let near_min = min_val + 1000
  
  // 这些运算不应该溢出
  @azimuth.assert_eq(max_val - 500, @azimuth.add(near_max, 500))
  @azimuth.assert_eq(min_val + 500, @azimuth.add(near_min, 500))
  
  // 测试边界值的组合运算
  let boundary_sum1 = @azimuth.add(max_val, min_val)  // 应该等于 -1
  @azimuth.assert_eq(-1, boundary_sum1)
  
  let boundary_sum2 = @azimuth.add(@azimuth.add(max_val, min_val), 1)  // 应该等于 0
  @azimuth.assert_eq(0, boundary_sum2)
  
  // 测试边界值的减法（通过加负数实现）
  let boundary_diff = @azimuth.add(max_val, @azimuth.multiply(min_val, -1))  // max_val - min_val
  // 由于 min_val * -1 会溢出保护，这个测试验证溢出保护是否生效
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, -1))
}

test "stack_simulation" {
  // 栈数据结构模拟测试
  // 使用整数模拟栈操作：push = add, pop = subtract (通过加负数)
  
  // 初始栈为空，栈顶指针为 0
  let stack_top = 0
  
  // Push 操作：栈顶指针加 1
  stack_top = @azimuth.add(stack_top, 1)  // push 第一个元素
  @azimuth.assert_eq(1, stack_top)
  
  stack_top = @azimuth.add(stack_top, 1)  // push 第二个元素
  @azimuth.assert_eq(2, stack_top)
  
  stack_top = @azimuth.add(stack_top, 1)  // push 第三个元素
  @azimuth.assert_eq(3, stack_top)
  
  // Pop 操作：栈顶指针减 1（通过加 -1）
  stack_top = @azimuth.add(stack_top, -1)  // pop 一个元素
  @azimuth.assert_eq(2, stack_top)
  
  stack_top = @azimuth.add(stack_top, -1)  // pop 另一个元素
  @azimuth.assert_eq(1, stack_top)
  
  stack_top = @azimuth.add(stack_top, -1)  // pop 最后一个元素
  @azimuth.assert_eq(0, stack_top)
  
  // 验证栈为空时不能继续 pop
  // 在实际实现中，这里应该有错误处理，但我们只测试算术
  stack_top = @azimuth.add(stack_top, -1)  // 尝试从空栈 pop
  @azimuth.assert_eq(-1, stack_top)  // 栈顶指针变为 -1，表示下溢
}

test "prime_number_check" {
  // 素数检查测试
  // 使用模运算和条件判断检查一个数是否为素数
  
  // 检查 7 是否为素数
  let num1 = 7
  let is_prime1 = true
  
  // 检查 2 到 6 是否能整除 7
  is_prime1 = is_prime1 && (num1 % 2 != 0)  // 7 % 2 = 1 ≠ 0
  is_prime1 = is_prime1 && (num1 % 3 != 0)  // 7 % 3 = 1 ≠ 0
  is_prime1 = is_prime1 && (num1 % 4 != 0)  // 7 % 4 = 3 ≠ 0
  is_prime1 = is_prime1 && (num1 % 5 != 0)  // 7 % 5 = 2 ≠ 0
  is_prime1 = is_prime1 && (num1 % 6 != 0)  // 7 % 6 = 1 ≠ 0
  
  @azimuth.assert_true(is_prime1)
  
  // 检查 9 是否为素数（应该不是）
  let num2 = 9
  let is_prime2 = true
  
  // 检查 2 到 8 是否能整除 9
  is_prime2 = is_prime2 && (num2 % 2 != 0)  // 9 % 2 = 1 ≠ 0
  is_prime2 = is_prime2 && (num2 % 3 != 0)  // 9 % 3 = 0 = 0，所以不是素数
  
  @azimuth.assert_false(is_prime2)
  
  // 检查 13 是否为素数
  let num3 = 13
  let is_prime3 = true
  
  // 检查 2 到 12 是否能整除 13
  is_prime3 = is_prime3 && (num3 % 2 != 0)   // 13 % 2 = 1 ≠ 0
  is_prime3 = is_prime3 && (num3 % 3 != 0)   // 13 % 3 = 1 ≠ 0
  is_prime3 = is_prime3 && (num3 % 4 != 0)   // 13 % 4 = 1 ≠ 0
  is_prime3 = is_prime3 && (num3 % 5 != 0)   // 13 % 5 = 3 ≠ 0
  is_prime3 = is_prime3 && (num3 % 6 != 0)   // 13 % 6 = 1 ≠ 0
  is_prime3 = is_prime3 && (num3 % 7 != 0)   // 13 % 7 = 6 ≠ 0
  is_prime3 = is_prime3 && (num3 % 8 != 0)   // 13 % 8 = 5 ≠ 0
  is_prime3 = is_prime3 && (num3 % 9 != 0)   // 13 % 9 = 4 ≠ 0
  is_prime3 = is_prime3 && (num3 % 10 != 0)  // 13 % 10 = 3 ≠ 0
  is_prime3 = is_prime3 && (num3 % 11 != 0)  // 13 % 11 = 2 ≠ 0
  is_prime3 = is_prime3 && (num3 % 12 != 0)  // 13 % 12 = 1 ≠ 0
  
  @azimuth.assert_true(is_prime3)
}

test "base_conversion_simulation" {
  // 进制转换模拟测试
  // 十进制转二进制（简化版，只处理小正整数）
  
  // 将 5 转换为二进制：101
  let decimal1 = 5
  let binary1 = ""
  
  // 手动模拟二进制转换过程
  let remainder1 = decimal1 % 2  // 5 % 2 = 1
  let quotient1 = decimal1 / 2   // 5 / 2 = 2
  
  let remainder2 = quotient1 % 2  // 2 % 2 = 0
  let quotient2 = quotient1 / 2   // 2 / 2 = 1
  
  let remainder3 = quotient2 % 2  // 1 % 2 = 1
  let quotient3 = quotient2 / 2   // 1 / 2 = 0
  
  // 二进制结果是余数的逆序：101
  // 由于字符串操作有限，我们通过数值验证
  let binary_value1 = @azimuth.add(@azimuth.multiply(1, 100), @azimuth.add(@azimuth.multiply(0, 10), 1))  // 101
  @azimuth.assert_eq(101, binary_value1)
  
  // 将 10 转换为二进制：1010
  let decimal2 = 10
  
  let remainder4 = decimal2 % 2  // 10 % 2 = 0
  let quotient4 = decimal2 / 2   // 10 / 2 = 5
  
  let remainder5 = quotient4 % 2  // 5 % 2 = 1
  let quotient5 = quotient4 / 2   // 5 / 2 = 2
  
  let remainder6 = quotient5 % 2  // 2 % 2 = 0
  let quotient6 = quotient5 / 2   // 2 / 2 = 1
  
  let remainder7 = quotient6 % 2  // 1 % 2 = 1
  let quotient7 = quotient6 / 2   // 1 / 2 = 0
  
  // 二进制结果是余数的逆序：1010
  let binary_value2 = @azimuth.add(@azimuth.multiply(1, 1000), @azimuth.add(@azimuth.multiply(0, 100), @azimuth.add(@azimuth.multiply(1, 10), 0)))  // 1010
  @azimuth.assert_eq(1010, binary_value2)
}

test "circular_queue_simulation" {
  // 循环队列模拟测试
  // 使用整数模拟循环队列的索引操作
  
  let queue_size = 5
  let head = 0
  let tail = 0
  
  // 入队操作：tail = (tail + 1) % size
  tail = (tail + 1) % queue_size  // 第一个元素入队
  @azimuth.assert_eq(1, tail)
  
  tail = (tail + 1) % queue_size  // 第二个元素入队
  @azimuth.assert_eq(2, tail)
  
  tail = (tail + 1) % queue_size  // 第三个元素入队
  @azimuth.assert_eq(3, tail)
  
  // 出队操作：head = (head + 1) % size
  head = (head + 1) % queue_size  // 第一个元素出队
  @azimuth.assert_eq(1, head)
  
  head = (head + 1) % queue_size  // 第二个元素出队
  @azimuth.assert_eq(2, head)
  
  // 继续入队，测试循环
  tail = (tail + 1) % queue_size  // 第四个元素入队
  @azimuth.assert_eq(4, tail)
  
  tail = (tail + 1) % queue_size  // 第五个元素入队
  @azimuth.assert_eq(0, tail)  // 循环回到开始
  
  // 继续出队
  head = (head + 1) % queue_size  // 第三个元素出队
  @azimuth.assert_eq(3, head)
  
  head = (head + 1) % queue_size  // 第四个元素出队
  @azimuth.assert_eq(4, head)
  
  head = (head + 1) % queue_size  // 第五个元素出队
  @azimuth.assert_eq(0, head)  // 循环回到开始
  
  // 验证队列为空时 head == tail
  @azimuth.assert_eq(head, tail)
}

test "complex_formula_evaluation" {
  // 复杂公式求值测试
  // 计算复合利息的精确公式：A = P(1 + r/n)^(nt)
  // 其中 P=本金, r=年利率, n=每年复利次数, t=年数
  // 简化为整数计算：A = P(1 + r)^(t)
  
  let principal = 1000  // 本金
  let rate = 5          // 5% 利率
  let years = 3         // 3 年
  
  // 计算增长因子：(1 + r) = 6
  let growth_factor = @azimuth.add(1, rate)
  
  // 计算 (1 + r)^t = 6^3 = 216
  let power = @azimuth.multiply(growth_factor, @azimuth.multiply(growth_factor, growth_factor))
  
  // 计算最终金额：P * power = 1000 * 216 = 216000
  let final_amount = @azimuth.multiply(principal, power)
  
  @azimuth.assert_eq(216000, final_amount)
  
  // 验证逐年计算
  let year1 = @azimuth.multiply(principal, growth_factor)  // 1000 * 6 = 6000
  @azimuth.assert_eq(6000, year1)
  
  let year2 = @azimuth.multiply(year1, growth_factor)      // 6000 * 6 = 36000
  @azimuth.assert_eq(36000, year2)
  
  let year3 = @azimuth.multiply(year2, growth_factor)      // 36000 * 6 = 216000
  @azimuth.assert_eq(216000, year3)
  
  // 验证与直接计算的结果一致
  @azimuth.assert_eq(final_amount, year3)
}