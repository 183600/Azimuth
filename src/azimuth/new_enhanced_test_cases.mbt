// 新增的 MoonBit 测试用例
// 测试 azimuth 库的各种功能组合

test "advanced_arithmetic_combinations" {
  // 测试复杂的算术组合：(a + b) * (c - d) + e * f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = add(a, b)  // 15
  let diff_cd = add(c, -d)  // 12
  let product_ef = multiply(e, f)  // 21
  let product_ab_cd = multiply(sum_ab, diff_cd)  // 180
  let result = add(product_ab_cd, product_ef)  // 201
  
  assert_eq(201, result)
}

test "string_processing_with_numbers" {
  // 测试字符串与数字的组合处理
  let year = 2024
  let month = "January"
  let day = 15
  
  // 创建包含数字的字符串
  let date_string = "Date: " + month + " " + day.to_string() + ", " + year.to_string()
  
  // 测试 greet 函数处理包含数字的字符串
  let numeric_name = "User2024"
  let greeting = greet(numeric_name)
  assert_eq_string("Hello, User2024!", greeting)
  
  // 测试包含特殊字符和数字的字符串
  let complex_name = "Test@123"
  let complex_greeting = greet(complex_name)
  assert_eq_string("Hello, Test@123!", complex_greeting)
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算斐波那契数列前10项的和
  // 简化版：1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  let fib6 = add(fib4, fib5)  // 8
  let fib7 = add(fib5, fib6)  // 13
  let fib8 = add(fib6, fib7)  // 21
  let fib9 = add(fib7, fib8)  // 34
  let fib10 = add(fib8, fib9) // 55
  
  let sum_fib = add(add(add(add(add(add(add(add(add(fib1, fib2), fib3), fib4), fib5), fib6), fib7), fib8), fib9), fib10)
  assert_eq(143, sum_fib)
}

test "geometry_practical_calculations" {
  // 测试实际几何计算
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 8
  let bottom_base = 12
  let height = 5
  
  let sum_bases = add(top_base, bottom_base)  // 20
  let area = multiply(sum_bases, height) / 2  // 50
  assert_eq(50, area)
  
  // 计算圆柱体表面积（简化）：2 × π × r × (r + h)
  let radius = 3
  let cylinder_height = 8
  let surface_area = multiply(multiply(2, 3), multiply(radius, add(radius, cylinder_height)))
  assert_eq(198, surface_area)
}

test "financial_compound_interest" {
  // 测试复利计算
  let principal = 10000  // 本金
  let rate = 5  // 年利率5%
  let years = 3  // 3年
  
  // 简化复利计算：每年利息加入本金
  let year1_amount = add(principal, multiply(principal, rate) / 100)  // 10500
  let year2_amount = add(year1_amount, multiply(year1_amount, rate) / 100)  // 11025
  let year3_amount = add(year2_amount, multiply(year2_amount, rate) / 100)  // 11576
  
  assert_eq(11576, year3_amount)
}

test "physics_kinematics_calculations" {
  // 测试物理运动学计算
  // 计算自由落体距离：s = 0.5 × g × t²
  // 简化使用整数：s = 5 × t²
  let gravity = 5  // 简化的重力加速度
  let time = 4  // 时间
  
  let distance = multiply(gravity, multiply(time, time))
  assert_eq(80, distance)
  
  // 计算斜面滑动距离：s = v₀ × t + 0.5 × a × t²
  let initial_velocity = 10
  let acceleration = 2
  let slide_time = 3
  
  let distance1 = multiply(initial_velocity, slide_time)  // 30
  let distance2 = multiply(acceleration, multiply(slide_time, slide_time)) / 2  // 9
  let total_distance = add(distance1, distance2)  // 39
  
  assert_eq(39, total_distance)
}

test "data_structure_simulation" {
  // 模拟数据结构操作
  // 模拟栈操作：push, push, pop, push
  let stack_size = 0
  
  // push 10
  stack_size = add(stack_size, 1)  // 1
  // push 20
  stack_size = add(stack_size, 1)  // 2
  // pop
  stack_size = add(stack_size, -1)  // 1
  // push 30
  stack_size = add(stack_size, 1)  // 2
  
  assert_eq(2, stack_size)
  
  // 模拟队列操作：enqueue, dequeue, enqueue
  let queue_size = 0
  
  // enqueue
  queue_size = add(queue_size, 1)  // 1
  // enqueue
  queue_size = add(queue_size, 1)  // 2
  // dequeue
  queue_size = add(queue_size, -1)  // 1
  // enqueue
  queue_size = add(queue_size, 1)  // 2
  
  assert_eq(2, queue_size)
}

test "algorithm_binary_search_simulation" {
  // 模拟二分查找的索引计算
  let array_size = 16
  let left = 0
  let right = add(array_size, -1)  // 15
  
  // 第一次查找：mid = (left + right) / 2
  let mid1 = add(left, right) / 2  // 7
  assert_eq(7, mid1)
  
  // 假设目标在右半部分：left = mid + 1
  let left2 = add(mid1, 1)  // 8
  let mid2 = add(left2, right) / 2  // 11
  assert_eq(11, mid2)
  
  // 假设目标在左半部分：right = mid - 1
  let right3 = add(mid2, -1)  // 10
  let mid3 = add(left2, right3) / 2  // 9
  assert_eq(9, mid3)
}

test "cryptography_simple_caesar_cipher" {
  // 简单的凯撒密码模拟（仅数字）
  let original_number = 1234
  let shift = 3
  
  // 加密：每个数字位加上shift
  let encrypted = add(original_number, multiply(shift, 1111))  // 1234 + 3333 = 4567
  assert_eq(4567, encrypted)
  
  // 解密：每个数字位减去shift
  let decrypted = add(encrypted, multiply(-shift, 1111))  // 4567 - 3333 = 1234
  assert_eq(1234, decrypted)
}

test "game_logic_turn_based_strategy" {
  // 回合制游戏逻辑测试
  let player_health = 100
  let enemy_health = 80
  let player_attack = 15
  let enemy_attack = 12
  let turns = 3
  
  // 模拟3回合战斗
  let current_player_health = player_health
  let current_enemy_health = enemy_health
  
  // 第1回合
  current_enemy_health = add(current_enemy_health, -player_attack)  // 65
  current_player_health = add(current_player_health, -enemy_attack)  // 88
  
  // 第2回合
  current_enemy_health = add(current_enemy_health, -player_attack)  // 50
  current_player_health = add(current_player_health, -enemy_attack)  // 76
  
  // 第3回合
  current_enemy_health = add(current_enemy_health, -player_attack)  // 35
  current_player_health = add(current_player_health, -enemy_attack)  // 64
  
  assert_eq(35, current_enemy_health)
  assert_eq(64, current_player_health)
  
  // 判断胜负
  let player_wins = current_enemy_health <= 0
  let enemy_wins = current_player_health <= 0
  let game_over = player_wins || enemy_wins
  
  assert_false(player_wins)
  assert_false(enemy_wins)
  assert_false(game_over)
}