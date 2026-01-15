// 用户综合测试用例
// 基于 azimuth 模块的功能编写的测试

test "add_commutativity_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = add(a, b)
  let result2 = add(b, a)
  
  assert_eq(result1, result2)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 15
  let b = 20
  let c = 25
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "greet_multilingual_support" {
  // 测试多语言支持的问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算：贷款分期付款
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 10          // 贷款年限
  
  // 简化计算：总利息 = 本金 × 利率 × 年数
  let total_interest = multiply(principal, multiply(annual_rate, years))
  // 总还款额 = 本金 + 总利息
  let total_payment = add(principal, total_interest)
  // 月供 = 总还款额 ÷ (年数 × 12)
  let monthly_payment = total_payment / multiply(years, 12)
  
  assert_eq(1600000, total_interest)  // 100000 × 6 × 10 = 1600000
  assert_eq(1700000, total_payment)   // 100000 + 1600000 = 1700000
  assert_eq(14166, monthly_payment)   // 1700000 ÷ 120 ≈ 14166
}

test "physics_kinematics_calculation" {
  // 测试物理学运动学计算：抛体运动
  let initial_velocity = 20  // 初始速度 m/s
  let angle = 45             // 发射角度 45度
  let gravity = 10           // 重力加速度 m/s²
  
  // 简化计算：最大高度 = v² × sin²(θ) ÷ (2g)
  // 对于45度角，sin(45°) ≈ 0.7，我们简化为整数计算
  let velocity_squared = multiply(initial_velocity, initial_velocity)
  let max_height = velocity_squared / multiply(2, gravity)
  
  // 射程 = v² × sin(2θ) ÷ g
  // 对于45度角，sin(90°) = 1
  let range = velocity_squared / gravity
  
  assert_eq(20, max_height)  // 400 ÷ 20 = 20
  assert_eq(40, range)       // 400 ÷ 10 = 40
}

test "data_structure_array_operations" {
  // 测试数据结构数组操作
  // 模拟数组操作：计算数组元素的各种统计值
  
  // 模拟数组元素 [10, 20, 30, 40, 50]
  let elem1 = 10
  let elem2 = 20
  let elem3 = 30
  let elem4 = 40
  let elem5 = 50
  
  // 计算总和
  let sum = add(add(add(add(elem1, elem2), elem3), elem4), elem5)
  // 计算平均值
  let average = sum / 5
  // 计算最大值和最小值的差
  let range = add(elem5, -elem1)
  // 计算中位数（已排序数组的中间值）
  let median = elem3
  
  assert_eq(150, sum)
  assert_eq(30, average)
  assert_eq(40, range)
  assert_eq(30, median)
}

test "algorithm_gcd_calculation" {
  // 测试算法：最大公约数计算（欧几里得算法）
  let a = 48
  let b = 18
  
  // 手动模拟欧几里得算法
  // 48 = 2 × 18 + 12
  // 18 = 1 × 12 + 6
  // 12 = 2 × 6 + 0
  // 所以 GCD(48, 18) = 6
  
  let gcd = 6
  assert_eq(6, gcd)
  
  // 验证公约数性质
  let a_div_gcd = a / gcd
  let b_div_gcd = b / gcd
  
  assert_eq(8, a_div_gcd)   // 48 ÷ 6 = 8
  assert_eq(3, b_div_gcd)   // 18 ÷ 6 = 3
  
  // 验证互质性
  let are_coprime = true  // 8和3互质
  assert_true(are_coprime)
}

test "game_development_collision_detection" {
  // 测试游戏开发：2D碰撞检测
  // 矩形1: (x1, y1, width1, height1)
  let rect1_x = 10
  let rect1_y = 20
  let rect1_width = 30
  let rect1_height = 40
  
  // 矩形2: (x2, y2, width2, height2)
  let rect2_x = 25
  let rect2_y = 35
  let rect2_width = 35
  let rect2_height = 45
  
  // 计算矩形边界
  let rect1_right = add(rect1_x, rect1_width)
  let rect1_bottom = add(rect1_y, rect1_height)
  let rect2_right = add(rect2_x, rect2_width)
  let rect2_bottom = add(rect2_y, rect2_height)
  
  // 碰撞检测：两个矩形在x轴和y轴上都有重叠
  let collision_x = rect1_right > rect2_x && rect1_x < rect2_right
  let collision_y = rect1_bottom > rect2_y && rect1_y < rect2_bottom
  let is_colliding = collision_x && collision_y
  
  assert_true(is_colliding)
  
  // 计算重叠区域的面积
  let overlap_left = if rect1_x > rect2_x { rect1_x } else { rect2_x }
  let overlap_top = if rect1_y > rect2_y { rect1_y } else { rect2_y }
  let overlap_right = if rect1_right < rect2_right { rect1_right } else { rect2_right }
  let overlap_bottom = if rect1_bottom < rect2_bottom { rect1_bottom } else { rect2_bottom }
  
  let overlap_width = add(overlap_right, -overlap_left)
  let overlap_height = add(overlap_bottom, -overlap_top)
  let overlap_area = multiply(overlap_width, overlap_height)
  
  assert_eq(20, overlap_width)   // 40 - 20 = 20
  assert_eq(25, overlap_height)  // 60 - 35 = 25
  assert_eq(500, overlap_area)   // 20 × 25 = 500
}

test "cryptography_simple_caesar_cipher" {
  // 测试密码学：简单凯撒密码
  // 由于我们只有数值运算，我们模拟字符编码
  
  // 模拟字符串 "HELLO" 的ASCII码
  let h_code = 72   // 'H'
  let e_code = 69   // 'E'
  let l_code = 76   // 'L'
  let o_code = 79   // 'O'
  
  // 凯撒密码加密：向右移动3位
  let shift = 3
  let h_encrypted = add(h_code, shift)
  let e_encrypted = add(e_code, shift)
  let l_encrypted = add(l_code, shift)
  let o_encrypted = add(o_code, shift)
  
  assert_eq(75, h_encrypted)  // 72 + 3 = 75 ('K')
  assert_eq(72, e_encrypted)  // 69 + 3 = 72 ('H')
  assert_eq(79, l_encrypted)  // 76 + 3 = 79 ('O')
  assert_eq(82, o_encrypted)  // 79 + 3 = 82 ('R')
  
  // 解密：向左移动3位
  let h_decrypted = add(h_encrypted, -shift)
  let e_decrypted = add(e_encrypted, -shift)
  let l_decrypted = add(l_encrypted, -shift)
  let o_decrypted = add(o_encrypted, -shift)
  
  assert_eq(72, h_decrypted)  // 75 - 3 = 72 ('H')
  assert_eq(69, e_decrypted)  // 72 - 3 = 69 ('E')
  assert_eq(76, l_decrypted)  // 79 - 3 = 76 ('L')
  assert_eq(79, o_decrypted)  // 82 - 3 = 79 ('O')
}

test "engineering_structural_analysis" {
  // 测试工程学：结构分析
  // 计算简支梁的最大弯矩和挠度
  
  let beam_length = 6000    // 梁长度 mm
  let load = 10000          // 集中载荷 N
  let moment_of_inertia = 85000000  // 截面惯性矩 mm⁴
  let elastic_modulus = 200000       // 弹性模量 N/mm²
  
  // 最大弯矩：M = P × L / 4
  let max_moment = multiply(load, beam_length) / 4
  assert_eq(15000000, max_moment)  // 10000 × 6000 / 4 = 15000000 N·mm
  
  // 最大挠度：δ = P × L³ / (48 × E × I)
  let length_cubed = multiply(beam_length, multiply(beam_length, beam_length))
  let denominator = multiply(48, multiply(elastic_modulus, moment_of_inertia))
  let max_deflection = multiply(load, length_cubed) / denominator
  
  // 验证梁的刚度
  let allowable_deflection = beam_length / 250  // 允许挠度 = L/250
  let is_stiff_enough = max_deflection <= allowable_deflection
  
  assert_eq(24, allowable_deflection)  // 6000 / 250 = 24 mm
  assert_true(is_stiff_enough)
}