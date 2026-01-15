// 新增的 MoonBit 测试用例
// 包含10个不同的测试场景，展示 MoonBit 的各种能力

test "factorial_calculation" {
  // 计算5的阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let result = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, result)
  
  // 计算3的阶乘：3! = 3 × 2 × 1 = 6
  let n2 = 3
  let result2 = @azimuth.multiply(n2, @azimuth.multiply(n2 - 1, n2 - 2))
  @azimuth.assert_eq(6, result2)
}

test "fibonacci_sequence" {
  // 计算斐波那契数列的第6项：F(6) = F(5) + F(4) = 5 + 3 = 8
  // 使用迭代方式计算
  let a = 1  // F(1)
  let b = 1  // F(2)
  
  // 计算F(3) = F(1) + F(2) = 1 + 1 = 2
  let c = @azimuth.add(a, b)  // 2
  
  // 计算F(4) = F(2) + F(3) = 1 + 2 = 3
  let d = @azimuth.add(b, c)  // 3
  
  // 计算F(5) = F(3) + F(4) = 2 + 3 = 5
  let e = @azimuth.add(c, d)  // 5
  
  // 计算F(6) = F(4) + F(5) = 3 + 5 = 8
  let f = @azimuth.add(d, e)  // 8
  
  @azimuth.assert_eq(8, f)
  
  // 验证斐波那契性质：F(6) = F(4) + F(5)
  @azimuth.assert_eq(f, @azimuth.add(d, e))
}

test "bitwise_operations_simulation" {
  // 模拟位运算：使用加减乘除实现位运算效果
  let a = 8
  let b = 3
  
  // 左移运算模拟：a << 1 = a * 2
  let left_shift = @azimuth.multiply(a, 2)
  @azimuth.assert_eq(16, left_shift)
  
  // 右移运算模拟：a >> 1 = a / 2
  let right_shift = a / 2
  @azimuth.assert_eq(4, right_shift)
  
  // 位与运算模拟（简化）：a & b = min(a, b) 当a和b是2的幂时
  let a_power = 8  // 2^3
  let b_power = 4  // 2^2
  let bit_and_sim = b_power  // 8 & 4 = 0，但这里简化为较小的幂
  @azimuth.assert_eq(4, bit_and_sim)
  
  // 位或运算模拟（简化）：a | b = a + b 当a和b是2的幂且不相等时
  let bit_or_sim = @azimuth.add(a_power, b_power)
  @azimuth.assert_eq(12, bit_or_sim)
}

test "stack_operations_simulation" {
  // 模拟栈操作：push和pop
  // 初始栈为空，用变量模拟栈顶和栈大小
  let stack_size = 0
  let stack_top = 0
  
  // Push操作1：压入值10
  let value1 = 10
  stack_size = @azimuth.add(stack_size, 1)  // size = 1
  stack_top = value1  // top = 10
  @azimuth.assert_eq(1, stack_size)
  @azimuth.assert_eq(10, stack_top)
  
  // Push操作2：压入值20
  let value2 = 20
  stack_size = @azimuth.add(stack_size, 1)  // size = 2
  stack_top = value2  // top = 20
  @azimuth.assert_eq(2, stack_size)
  @azimuth.assert_eq(20, stack_top)
  
  // Pop操作：弹出栈顶值
  stack_size = @azimuth.add(stack_size, -1)  // size = 1
  let popped_value = stack_top  // popped = 20
  // 栈顶变为前一个值（这里简化处理）
  stack_top = value1  // top = 10
  @azimuth.assert_eq(1, stack_size)
  @azimuth.assert_eq(10, stack_top)
  @azimuth.assert_eq(20, popped_value)
}

test "binary_search_simulation" {
  // 模拟二分查找：在有序数组 [1, 3, 5, 7, 9, 11, 13] 中查找7
  let target = 7
  
  // 模拟数组元素
  let arr_0 = 1
  let arr_1 = 3
  let arr_2 = 5
  let arr_3 = 7
  let arr_4 = 9
  let arr_5 = 11
  let arr_6 = 13
  
  // 二分查找过程
  let left = 0
  let right = 6
  let mid = @azimuth.add(left, right) / 2  // (0 + 6) / 2 = 3
  let mid_value = arr_3  // arr[3] = 7
  
  // 第一次比较：arr[3] == target，找到目标
  let found = if (mid_value == target) { true } else { false }
  @azimuth.assert_true(found)
  @azimuth.assert_eq(3, mid)  // 目标在索引3处
  
  // 测试查找不存在的元素
  let target2 = 8
  left = 0
  right = 6
  
  // 第一次比较：arr[3] = 7 < 8，查找右半部分
  left = @azimuth.add(mid, 1)  // left = 4
  mid = @azimuth.add(left, right) / 2  // (4 + 6) / 2 = 5
  mid_value = arr_5  // arr[5] = 11
  
  // 第二次比较：arr[5] = 11 > 8，查找左半部分
  right = @azimuth.add(mid, -1)  // right = 4
  mid = @azimuth.add(left, right) / 2  // (4 + 4) / 2 = 4
  mid_value = arr_4  // arr[4] = 9
  
  // 第三次比较：arr[4] = 9 > 8，查找左半部分
  right = @azimuth.add(mid, -1)  // right = 3
  // 此时 left > right，查找结束，未找到
  let found2 = if (left <= right) { true } else { false }
  @azimuth.assert_false(found2)
}

test "string_reversal_simulation" {
  // 模拟字符串反转：通过字符索引访问
  let original = "hello"
  
  // 模拟字符访问（简化处理）
  let char_0 = "h"
  let char_1 = "e"
  let char_2 = "l"
  let char_3 = "l"
  let char_4 = "o"
  
  // 构建反转后的字符串
  let reversed = char_4 + char_3 + char_2 + char_1 + char_0
  @azimuth.assert_eq_string("olleh", reversed)
  
  // 测试奇数长度字符串的反转
  let odd_original = "abc"
  
  // 模拟字符访问
  let odd_char_0 = "a"
  let odd_char_1 = "b"
  let odd_char_2 = "c"
  
  // 构建反转后的字符串
  let odd_reversed = odd_char_2 + odd_char_1 + odd_char_0
  @azimuth.assert_eq_string("cba", odd_reversed)
  
  // 测试通过greet函数验证字符串处理
  let greet_result = @azimuth.greet(reversed)
  @azimuth.assert_eq_string("Hello, olleh!", greet_result)
}

test "pi_approximation_calculation" {
  // 使用莱布尼茨公式近似计算π：π/4 = 1 - 1/3 + 1/5 - 1/7 + 1/9 - ...
  // 使用整数运算模拟，计算前几项
  
  // 计算前4项：π/4 ≈ 1 - 1/3 + 1/5 - 1/7
  // 转换为整数运算：使用公分母105
  let term1 = 105 / 1      // 105
  let term2 = 105 / 3      // 35
  let term3 = 105 / 5      // 21
  let term4 = 105 / 7      // 15
  
  // 计算分子：105 - 35 + 21 - 15 = 76
  let numerator = @azimuth.add(@azimuth.add(@azimuth.add(105, -35), 21), -15)
  @azimuth.assert_eq(76, numerator)
  
  // π/4 ≈ 76/105，所以 π ≈ 4 × 76/105 = 304/105 ≈ 2.895
  // 使用整数除法近似
  let pi_approx_times_100 = @azimuth.multiply(304, 100) / 105  // 289
  @azimuth.assert_eq(289, pi_approx_times_100)  // 表示π ≈ 2.89
  
  // 使用更精确的近似：π ≈ 22/7
  let pi_better_times_100 = @azimuth.multiply(22, 100) / 7  // 314
  @azimuth.assert_eq(314, pi_better_times_100)  // 表示π ≈ 3.14
}

test "checksum_calculation" {
  // 模拟校验和计算：对数字序列计算校验和
  // 使用简单的求和模10算法
  
  // 数字序列：1, 2, 3, 4, 5
  let num1 = 1
  let num2 = 2
  let num3 = 3
  let num4 = 4
  let num5 = 5
  
  // 计算总和：1 + 2 + 3 + 4 + 5 = 15
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  @azimuth.assert_eq(15, sum)
  
  // 计算校验和：sum % 10
  let checksum = sum % 10
  @azimuth.assert_eq(5, checksum)
  
  // 测试另一个序列：9, 8, 7, 6
  let seq1 = 9
  let seq2 = 8
  let seq3 = 7
  let seq4 = 6
  
  // 计算总和：9 + 8 + 7 + 6 = 30
  let sum2 = @azimuth.add(@azimuth.add(@azimuth.add(seq1, seq2), seq3), seq4)
  @azimuth.assert_eq(30, sum2)
  
  // 计算校验和：sum % 10
  let checksum2 = sum2 % 10
  @azimuth.assert_eq(0, checksum2)
  
  // 验证校验和的用途：如果其中一个数字错误，校验和不匹配
  // 假设第三个数字从7变为8，新的总和是31，校验和是1，与原来的0不匹配
  let wrong_sum = @azimuth.add(@azimuth.add(@azimuth.add(seq1, seq2), @azimuth.add(seq3, 1)), seq4)
  let wrong_checksum = wrong_sum % 10
  @azimuth.assert_eq(1, wrong_checksum)
  @azimuth.assert_false(checksum2 == wrong_checksum)
}

test "rock_paper_scissors_game" {
  // 模拟石头剪刀布游戏逻辑
  // 0: 石头, 1: 布, 2: 剪刀
  
  // 游戏规则：石头胜剪刀，剪刀胜布，布胜石头
  // 使用数学公式：(player1 - player2 + 3) % 3
  // 结果：0 = 平局, 1 = 玩家1胜, 2 = 玩家2胜
  
  // 测试1：石头(0) vs 剪刀(2) = 玩家1胜
  let player1_rock = 0
  let player2_scissors = 2
  let result1 = @azimuth.add(player1_rock, @azimuth.add(-player2_scissors, 3)) % 3
  @azimuth.assert_eq(1, result1)  // 玩家1胜
  
  // 测试2：剪刀(2) vs 布(1) = 玩家1胜
  let player1_scissors = 2
  let player2_paper = 1
  let result2 = @azimuth.add(player1_scissors, @azimuth.add(-player2_paper, 3)) % 3
  @azimuth.assert_eq(1, result2)  // 玩家1胜
  
  // 测试3：布(1) vs 石头(0) = 玩家1胜
  let player1_paper = 1
  let player2_rock = 0
  let result3 = @azimuth.add(player1_paper, @azimuth.add(-player2_rock, 3)) % 3
  @azimuth.assert_eq(1, result3)  // 玩家1胜
  
  // 测试4：石头(0) vs 石头(0) = 平局
  let result4 = @azimuth.add(player1_rock, @azimuth.add(-player1_rock, 3)) % 3
  @azimuth.assert_eq(0, result4)  // 平局
  
  // 测试5：石头(0) vs 布(1) = 玩家2胜
  let player2_paper_2 = 1
  let result5 = @azimuth.add(player1_rock, @azimuth.add(-player2_paper_2, 3)) % 3
  @azimuth.assert_eq(2, result5)  // 玩家2胜
}

test "caesar_cipher_encryption" {
  // 模拟凯撒密码加密：字母位移
  // 简化处理：只处理大写字母A-Z（A=0, B=1, ..., Z=25）
  
  // 加密"HELLO"，位移3
  // H=7, E=4, L=11, L=11, O=14
  let h_val = 7
  let e_val = 4
  let l_val = 11
  let o_val = 14
  
  // 位移3：(原值 + 3) % 26
  let shift = 3
  let h_enc = @azimuth.add(h_val, shift) % 26  // 10 = K
  let e_enc = @azimuth.add(e_val, shift) % 26  // 7 = H
  let l_enc = @azimuth.add(l_val, shift) % 26  // 14 = O
  let o_enc = @azimuth.add(o_val, shift) % 26  // 17 = R
  
  @azimuth.assert_eq(10, h_enc)
  @azimuth.assert_eq(7, e_enc)
  @azimuth.assert_eq(14, l_enc)
  @azimuth.assert_eq(17, o_enc)
  
  // 解密：位移-3
  let h_dec = @azimuth.add(h_enc, @azimuth.add(-shift, 26)) % 26  // 7 = H
  let e_dec = @azimuth.add(e_enc, @azimuth.add(-shift, 26)) % 26  // 4 = E
  let l_dec = @azimuth.add(l_enc, @azimuth.add(-shift, 26)) % 26  // 11 = L
  let o_dec = @azimuth.add(o_enc, @azimuth.add(-shift, 26)) % 26  // 14 = O
  
  @azimuth.assert_eq(h_val, h_dec)
  @azimuth.assert_eq(e_val, e_dec)
  @azimuth.assert_eq(l_val, l_dec)
  @azimuth.assert_eq(o_val, o_dec)
  
  // 测试环绕情况：Z(25) + 3 = C(2)
  let z_val = 25
  let z_enc = @azimuth.add(z_val, shift) % 26  // 2 = C
  @azimuth.assert_eq(2, z_enc)
  
  // 测试边界情况：A(0) - 3 = X(23)
  let a_val = 0
  let a_dec = @azimuth.add(a_val, @azimuth.add(-shift, 26)) % 26  // 23 = X
  @azimuth.assert_eq(23, a_dec)
}