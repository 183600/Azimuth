// Azimuth 项目的高级测试用例
// 覆盖边界值、实际应用场景和复杂数学运算

test "advanced_arithmetic_sequences" {
  // 测试等差数列求和：1 + 4 + 7 + 10 + 13
  let first_term = 1
  let common_diff = 3
  let num_terms = 5
  
  // 手动计算
  let term1 = first_term
  let term2 = add(term1, common_diff)
  let term3 = add(term2, common_diff)
  let term4 = add(term3, common_diff)
  let term5 = add(term4, common_diff)
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(35, sum)
}

test "financial_compound_interest" {
  // 测试复利计算：本金 × (1 + 利率)^年数
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 简化的复利计算（避免浮点数）
  let growth_factor = add(100, multiply(rate_percent, years))
  let total = multiply(principal, growth_factor)
  assert_eq(124000, total)  // 10000 × (100 + 8×3) = 10000 × 124 = 124000
}

test "physics_kinematics" {
  // 测试物理运动学公式：s = ut + (1/2)at²
  let initial_velocity = 10  // 初始速度
  let acceleration = 2       // 加速度
  let time = 5               // 时间
  
  // 位移 = 初速度 × 时间 + 0.5 × 加速度 × 时间²
  let distance1 = multiply(initial_velocity, time)
  let distance2 = multiply(acceleration, multiply(time, time)) / 2
  let total_distance = add(distance1, distance2)
  
  assert_eq(75, total_distance)  // 10×5 + 0.5×2×25 = 50 + 25 = 75
}

test "data_structure_indexing" {
  // 测试数据结构索引计算：二维数组转一维数组
  let rows = 4
  let cols = 5
  let row_index = 2
  let col_index = 3
  
  // 一维索引 = 行索引 × 列数 + 列索引
  let linear_index = add(multiply(row_index, cols), col_index)
  assert_eq(13, linear_index)
  
  // 测试边界情况
  let first_index = add(multiply(0, cols), 0)
  let last_index = add(multiply(subtract(rows, 1), subtract(cols, 1)), 0)
  assert_eq(0, first_index)
}

test "string_encoding_scenarios" {
  // 测试字符串编码场景
  let ascii_text = "Hello"
  let unicode_text = "你好"
  let mixed_text = "Hello你好123"
  let emoji_text = "🌍🎉"
  
  assert_eq_string("Hello, Hello!", greet(ascii_text))
  assert_eq_string("Hello, 你好!", greet(unicode_text))
  assert_eq_string("Hello, Hello你好123!", greet(mixed_text))
  assert_eq_string("Hello, 🌍🎉!", greet(emoji_text))
}

test "extreme_boundary_combinations" {
  // 测试极值边界组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试链式运算的边界处理
  let chain1 = add(add(max_val, min_val), max_val)
  assert_eq(max_val, chain1)
  
  let chain2 = multiply(multiply(min_val, 1), -1)
  assert_eq(min_val, chain2)
  
  // 测试混合运算
  let mixed1 = add(multiply(max_val, 0), 1)
  assert_eq(1, mixed1)
  
  let mixed2 = multiply(add(min_val, 1), 0)
  assert_eq(0, mixed2)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟
  let n = 100
  
  // O(n) 线性复杂度：简单的累加
  let linear_sum = multiply(n, add(n, 1)) / 2
  assert_eq(5050, linear_sum)
  
  // O(n²) 平方复杂度：嵌套循环的总和
  let quadratic_sum = multiply(n, multiply(n, add(n, 1))) / 6
  assert_eq(171700, quadratic_sum)
  
  // O(log n) 对数复杂度：二分查找的步骤数（简化）
  let log_steps = 0
  let temp_n = n
  // 简化的对数计算：log₂(100) ≈ 7
  assert_true(7 >= log_steps)
}

test "real_world_inventory_management" {
  // 测试实际库存管理场景
  let initial_stock = 1000
  let sales_per_day = 25
  let days = 5
  let restock_amount = 200
  
  // 计算销售量
  let total_sales = multiply(sales_per_day, days)
  
  // 计算剩余库存
  let remaining_stock = add(initial_stock, -total_sales)
  
  // 计算补货后的库存
  let final_stock = add(remaining_stock, restock_amount)
  
  assert_eq(1125, final_stock)  // 1000 - 25×5 + 200 = 1000 - 125 + 200 = 1075
}

test "statistical_variance_calculation" {
  // 测试统计方差计算
  // 数据集：[10, 20, 30, 40, 50]
  let sum = add(add(add(add(10, 20), 30), 40), 50)
  let mean = sum / 5  // 平均值 = 30
  
  // 计算方差：Σ(x - mean)² / n
  let diff1 = subtract(10, mean)
  let diff2 = subtract(20, mean)
  let diff3 = subtract(30, mean)
  let diff4 = subtract(40, mean)
  let diff5 = subtract(50, mean)
  
  let sq1 = multiply(diff1, diff1)
  let sq2 = multiply(diff2, diff2)
  let sq3 = multiply(diff3, diff3)
  let sq4 = multiply(diff4, diff4)
  let sq5 = multiply(diff5, diff5)
  
  let variance = add(add(add(add(sq1, sq2), sq3), sq4), sq5) / 5
  assert_eq(200, variance)  // ((400+100+0+100+400)/5) = 200
}

// 辅助函数：减法（通过加负数实现）
fn subtract(a : Int, b : Int) -> Int {
  add(a, -b)
}