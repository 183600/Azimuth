// 增强的 MoonBit 测试用例
// 针对 azimuth 包的高级功能和边界情况测试

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(5, divide_with_ceil(15, 3))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款分期付款计算
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 30          // 30年
  let months_per_year = 12
  
  // 简化的月供计算：本金 ÷ (年数 × 12) + 利息
  let total_months = multiply(years, months_per_year)
  let monthly_principal = principal / total_months
  let monthly_interest = multiply(principal, annual_rate) / multiply(100, months_per_year)
  let monthly_payment = add(monthly_principal, monthly_interest)
  
  assert_eq(555, monthly_payment)  // 简化计算结果
}

test "data_structure_simulation" {
  // 模拟数据结构操作：栈的push和pop操作
  let initial_stack = 0
  let push1 = 10
  let push2 = 20
  let push3 = 30
  
  // 模拟栈操作（用加法模拟push，用减法模拟pop）
  let stack_after_pushes = add(add(add(initial_stack, push1), push2), push3)
  assert_eq(60, stack_after_pushes)
  
  // 模拟pop操作
  let stack_after_pop1 = add(stack_after_pushes, -push3)
  let stack_after_pop2 = add(stack_after_pop1, -push2)
  
  assert_eq(10, stack_after_pop2)
}

test "algorithm_complexity_test" {
  // 算法复杂度测试：计算斐波那契数列的第n项（简化版）
  let n = 8
  
  // 手动计算斐波那契数列：F(8) = 21
  let f1 = 1
  let f2 = 1
  let f3 = add(f1, f2)      // 2
  let f4 = add(f2, f3)      // 3
  let f5 = add(f3, f4)      // 5
  let f6 = add(f4, f5)      // 8
  let f7 = add(f5, f6)      // 13
  let f8 = add(f6, f7)      // 21
  
  assert_eq(21, f8)
}

test "string_processing_advanced" {
  // 高级字符串处理：模拟字符串格式化
  let prefix = "User: "
  let username = "Alice"
  let suffix = " has logged in at "
  let timestamp = "14:30"
  
  // 模拟复杂的字符串格式化
  let greeting = greet(username)
  let base_message = add(prefix, username)
  let full_message = add(add(base_message, suffix), timestamp)
  
  assert_eq_string("Hello, Alice!", greeting)
  assert_eq_string("User: Alice has logged in at 14:30", full_message)
}

test "mathematical_optimization" {
  // 数学优化问题：寻找最大乘积的数字分割
  let number = 10
  
  // 将10分割为3+3+4，得到最大乘积 3×3×4 = 36
  let part1 = 3
  let part2 = 3
  let part3 = 4
  
  let max_product = multiply(multiply(part1, part2), part3)
  
  // 验证分割的正确性
  let sum_of_parts = add(add(part1, part2), part3)
  assert_eq(number, sum_of_parts)
  assert_eq(36, max_product)
}

test "cryptography_simulation" {
  // 密码学模拟：简单的凯撒密码
  let original = 5  // 原始数字
  let shift = 3     // 偏移量
  let modulus = 10  // 模数
  
  // 加密：(original + shift) % modulus
  let encrypted = add(original, shift)
  if encrypted >= modulus {
    encrypted = encrypted - modulus
  }
  
  // 解密：(encrypted - shift + modulus) % modulus
  let decrypted = add(encrypted, -shift)
  if decrypted < 0 {
    decrypted = decrypted + modulus
  }
  
  assert_eq(8, encrypted)
  assert_eq(original, decrypted)
}

test "physics_calculation" {
  // 物理计算：自由落体运动（简化版）
  let initial_height = 100  // 初始高度（米）
  let gravity = 10          // 重力加速度（简化为10）
  let time = 3              // 时间（秒）
  
  // 计算下落距离：s = 0.5 × g × t²（简化为 s = g × t²）
  let distance = multiply(gravity, multiply(time, time))
  
  // 计算剩余高度
  let remaining_height = add(initial_height, -distance)
  
  assert_eq(90, distance)     // 10 × 3² = 90
  assert_eq(10, remaining_height)  // 100 - 90 = 10
}

test "statistical_analysis" {
  // 统计分析：计算方差和标准差（简化版）
  let data1 = 10
  let data2 = 15
  let data3 = 20
  let data4 = 25
  let data5 = 30
  
  // 计算平均值
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  let mean = sum / 5
  
  // 计算方差（简化版）：平方差的平均值
  let diff1 = add(data1, -mean)
  let diff2 = add(data2, -mean)
  let diff3 = add(data3, -mean)
  let diff4 = add(data4, -mean)
  let diff5 = add(data5, -mean)
  
  let variance = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), 
                            multiply(diff3, diff3)), multiply(diff4, diff4)), 
                            multiply(diff5, diff5)) / 5
  
  assert_eq(20, mean)
  assert_eq(40, variance)
}

test "game_mechanics_simulation" {
  // 游戏机制模拟：RPG角色属性计算
  let base_strength = 10
  let equipment_bonus = 5
  let level_multiplier = 2
  let buff_percentage = 20  // 20%增益
  
  // 计算基础力量
  let total_strength = add(base_strength, equipment_bonus)
  
  // 应用等级倍数
  let leveled_strength = multiply(total_strength, level_multiplier)
  
  // 应用增益百分比
  let buff_amount = multiply(leveled_strength, buff_percentage) / 100
  let final_strength = add(leveled_strength, buff_amount)
  
  assert_eq(30, total_strength)      // 10 + 5 = 15, 15 × 2 = 30
  assert_eq(36, final_strength)      // 30 + (30 × 20/100) = 30 + 6 = 36
}