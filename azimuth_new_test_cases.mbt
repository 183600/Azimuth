// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，覆盖更多场景

test "add_boundary_values" {
  // 测试加法边界值
  
  // 最大值测试
  assert_eq(2147483647, azimuth::add(2147483646, 1))
  
  // 最小值测试
  assert_eq(-2147483648, azimuth::add(-2147483647, -1))
  
  // 零值测试
  assert_eq(0, azimuth::add(0, 0))
  assert_eq(100, azimuth::add(0, 100))
  assert_eq(-50, azimuth::add(-50, 0))
}

test "multiply_boundary_values" {
  // 测试乘法边界值
  
  // 单位元素测试
  assert_eq(100, azimuth::multiply(100, 1))
  assert_eq(100, azimuth::multiply(1, 100))
  assert_eq(0, azimuth::multiply(100, 0))
  assert_eq(0, azimuth::multiply(0, 100))
  
  // 负数乘法测试
  assert_eq(-100, azimuth::multiply(-10, 10))
  assert_eq(100, azimuth::multiply(-10, -10))
}

test "greet_special_characters" {
  // 测试特殊字符的问候功能
  
  assert_eq("Hello, @user!", azimuth::greet("@user"))
  assert_eq("Hello, #hashtag!", azimuth::greet("#hashtag"))
  assert_eq("Hello, $price!", azimuth::greet("$price"))
  assert_eq("Hello, %percent!", azimuth::greet("%percent"))
  assert_eq("Hello, &amp!", azimuth::greet("&amp"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  
  // 复利计算: 本金 * (1 + 利率)^年数
  let principal = 1000
  let rate = 5  // 5%
  let years = 2
  
  // 第一年: 1000 * (1 + 0.05) = 1050
  let year1 = azimuth::add(principal, azimuth::multiply(principal, rate) / 100)
  assert_eq(1050, year1)
  
  // 第二年: 1050 * (1 + 0.05) = 1102.5 ≈ 1102 (整数)
  let year2 = azimuth::add(year1, azimuth::multiply(year1, rate) / 100)
  assert_eq(1102, year2)
}

test "physics_calculations" {
  // 测试物理计算
  
  // 速度计算: 距离 / 时间
  let distance = 300  // 米
  let time = 10       // 秒
  let speed = azimuth::multiply(distance, 1) / time
  assert_eq(30, speed)
  
  // 动能计算: 0.5 * 质量 * 速度^2
  let mass = 10       // kg
  let velocity = 5    // m/s
  let kinetic_energy = azimuth::multiply(azimuth::multiply(mass, velocity), velocity) / 2
  assert_eq(125, kinetic_energy)
}

test "statistical_calculations" {
  // 测试统计计算
  
  // 平均值计算: (1 + 2 + 3 + 4 + 5) / 5 = 3
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(1, 2), 3), 4), 5)
  let average = azimuth::multiply(sum, 1) / 5
  assert_eq(3, average)
  
  // 加权平均: (90*3 + 80*2 + 70*1) / (3+2+1) = 83.33 ≈ 83
  let weighted_sum = azimuth::add(azimuth::add(azimuth::multiply(90, 3), azimuth::multiply(80, 2)), azimuth::multiply(70, 1))
  let total_weight = azimuth::add(azimuth::add(3, 2), 1)
  let weighted_average = azimuth::multiply(weighted_sum, 1) / total_weight
  assert_eq(83, weighted_average)
}

test "number_theory" {
  // 测试数论计算
  
  // 最大公约数模拟 (欧几里得算法简化版)
  let a = 48
  let b = 18
  
  // 48 % 18 = 12
  let remainder1 = azimuth::multiply(a, 1) - azimuth::multiply(azimuth::multiply(a, 1) / b, b)
  assert_eq(12, remainder1)
  
  // 18 % 12 = 6
  let remainder2 = azimuth::multiply(b, 1) - azimuth::multiply(azimuth::multiply(b, 1) / remainder1, remainder1)
  assert_eq(6, remainder2)
  
  // 12 % 6 = 0
  let remainder3 = azimuth::multiply(remainder1, 1) - azimuth::multiply(azimuth::multiply(remainder1, 1) / remainder2, remainder2)
  assert_eq(0, remainder3)
}

test "combinatorial_calculations" {
  // 测试组合计算
  
  // 排列数 P(n,k) = n!/(n-k)!
  // P(5,2) = 5!/(5-2)! = 5*4 = 20
  let n = 5
  let k = 2
  let permutation = azimuth::multiply(azimuth::multiply(azimuth::multiply(azimuth::multiply(5, 4), 3), 2), 1) / azimuth::multiply(azimuth::multiply(azimuth::multiply(3, 2), 1), 1)
  assert_eq(20, azimuth::multiply(5, 4))
  
  // 组合数 C(n,k) = P(n,k)/k!
  // C(5,2) = 20/2! = 10
  let combination = azimuth::multiply(5, 4) / azimuth::multiply(2, 1)
  assert_eq(10, combination)
}

test "algorithm_simulation" {
  // 测试算法模拟
  
  // 二分查找模拟 (在有序数组中查找)
  let array_size = 16
  let target_index = 7
  let steps = 0
  
  // 模拟二分查找步骤
  let left = 0
  let right = azimuth::add(array_size, -1)
  
  // 第一次查找: (0 + 15) / 2 = 7
  let mid1 = azimuth::multiply(azimuth::add(left, right), 1) / 2
  steps = azimuth::add(steps, 1)
  assert_eq(7, mid1)
  
  // 如果找到目标，总共需要1步
  assert_eq(1, steps)
  
  // 如果目标在边界，需要更多步骤
  let target_at_edge = 15
  steps = 0
  
  // 第一次: (0 + 15) / 2 = 7
  steps = azimuth::add(steps, 1)
  // 第二次: (8 + 15) / 2 = 11
  steps = azimuth::add(steps, 1)
  // 第三次: (12 + 15) / 2 = 13
  steps = azimuth::add(steps, 1)
  // 第四次: (14 + 15) / 2 = 14
  steps = azimuth::add(steps, 1)
  // 第五次: (15 + 15) / 2 = 15
  steps = azimuth::add(steps, 1)
  
  assert_eq(5, steps)
}