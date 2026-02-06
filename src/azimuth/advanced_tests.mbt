// 高级 MoonBit 测试用例 - 针对 azimuth 库的综合测试
// 包含数字系统转换、统计分析、资源优化等高级应用场景

// 导入 azimuth 库
use "azimuth"

test "advanced_number_system_conversions" {
  // 模拟数字系统转换计算
  let decimal_value = 255
  let hex_weight = 16
  
  // 计算二进制表示的权重和
  let binary_sum = azimuth.add(azimuth.multiply(1, 128), azimuth.multiply(1, 64))
  let binary_total = azimuth.add(binary_sum, azimuth.add(azimuth.multiply(1, 32), azimuth.multiply(1, 16)))
  
  // 计算十六进制表示
  let hex_high = azimuth.divide_with_ceil(decimal_value, hex_weight)
  let hex_low = decimal_value - azimuth.multiply(hex_high - 1, hex_weight)
  
  azimuth.assert_eq(240, binary_total)
  azimuth.assert_eq(16, hex_high)
  azimuth.assert_eq(15, hex_low)
}

test "advanced_statistical_computation" {
  // 统计分析测试
  let sum_manual = azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(12, 15), 18), 21), 24), 27), 30)
  
  // 计算平均值（向上取整）
  let count = 7
  let mean_value = azimuth.divide_with_ceil(sum_manual, count)
  
  // 计算中位数近似值
  let median_approx = azimuth.divide_with_ceil(azimuth.add(21, 24), 2)
  
  azimuth.assert_eq(147, sum_manual)
  azimuth.assert_eq(21, mean_value)
  azimuth.assert_eq(23, median_approx)
}

test "advanced_resource_allocation" {
  // 资源优化算法测试
  let total_resources = 100
  let task1_requirement = 23
  let task2_requirement = 31
  let task3_requirement = 19
  
  // 计算已分配资源
  let allocated = azimuth.add(azimuth.add(task1_requirement, task2_requirement), task3_requirement)
  
  // 计算剩余资源
  let remaining = azimuth.subtract(total_resources, allocated)
  
  // 计算可以支持的新任务数量（每个任务需要8个资源）
  let new_tasks_possible = azimuth.divide_with_ceil(remaining, 8)
  
  azimuth.assert_eq(73, allocated)
  azimuth.assert_eq(27, remaining)
  azimuth.assert_eq(4, new_tasks_possible)
}

test "advanced_geometric_computations" {
  // 几何计算测试
  let length = 8
  let width = 6
  let height = 4
  
  // 计算表面积
  let area1 = azimuth.multiply(length, width)
  let area2 = azimuth.multiply(length, height)
  let area3 = azimuth.multiply(width, height)
  let surface_area = azimuth.multiply(2, azimuth.add(azimuth.add(area1, area2), area3))
  
  // 计算体积
  let volume = azimuth.multiply(area1, height)
  
  // 计算空间利用率（假设物品体积为150）
  let item_volume = 150
  let utilization = azimuth.divide_with_ceil(azimuth.multiply(item_volume, 100), volume)
  
  azimuth.assert_eq(208, surface_area)
  azimuth.assert_eq(192, volume)
  azimuth.assert_eq(78, utilization)
}

test "advanced_temporal_analysis" {
  // 时间序列分析测试
  let baseline = 100
  let growth_rate = 15
  
  // 计算复合增长
  let period1 = azimuth.add(baseline, azimuth.divide_with_ceil(azimuth.multiply(baseline, growth_rate), 100))
  let period2 = azimuth.add(period1, azimuth.divide_with_ceil(azimuth.multiply(period1, growth_rate), 100))
  let period3 = azimuth.add(period2, azimuth.divide_with_ceil(azimuth.multiply(period2, growth_rate), 100))
  
  // 计算总增长
  let total_growth = azimuth.subtract(period3, baseline)
  let growth_percentage = azimuth.divide_with_ceil(azimuth.multiply(total_growth, 100), baseline)
  
  azimuth.assert_eq(115, period1)
  azimuth.assert_eq(133, period2)
  azimuth.assert_eq(153, period3)
  azimuth.assert_eq(53, total_growth)
  azimuth.assert_eq(53, growth_percentage)
}

test "advanced_boundary_validation" {
  // 错误边界条件测试
  let max_safe_int = 1000000
  let min_safe_int = -1000000
  
  // 测试接近边界的运算
  let boundary_add1 = azimuth.add(max_safe_int, 1)
  let boundary_add2 = azimuth.add(min_safe_int, -1)
  let boundary_mult = azimuth.divide_with_ceil(max_safe_int, 1000)
  let boundary_div = azimuth.divide_with_ceil(max_safe_int, 1)
  
  // 测试零的边界情况
  let zero_by_positive = azimuth.divide_with_ceil(0, 100)
  let zero_by_negative = azimuth.divide_with_ceil(0, -100)
  let zero_by_zero = azimuth.divide_with_ceil(0, 0)
  
  azimuth.assert_eq(1000001, boundary_add1)
  azimuth.assert_eq(-1000001, boundary_add2)
  azimuth.assert_eq(1000, boundary_mult)
  azimuth.assert_eq(1000000, boundary_div)
  azimuth.assert_eq(0, zero_by_positive)
  azimuth.assert_eq(0, zero_by_negative)
  azimuth.assert_eq(0, zero_by_zero)
}

test "advanced_encryption_simulation" {
  // 密码学模拟测试
  let message = 12345
  let key1 = 7
  let key2 = 13
  let modulus = 1000
  
  // 简单的加密过程
  let step1 = azimuth.multiply(message, key1)
  let step2 = azimuth.add(step1, key2)
  let encrypted = step2 % modulus
  
  // 解密过程（简化）
  let decrypted_step1 = azimuth.subtract(encrypted, key2)
  let decrypted_approx = azimuth.divide_with_ceil(azimuth.multiply(decrypted_step1, 1), key1)
  
  azimuth.assert_eq(86425, step1)
  azimuth.assert_eq(86438, step2)
  azimuth.assert_eq(438, encrypted)
  azimuth.assert_eq(425, decrypted_step1)
  azimuth.assert_eq(61, decrypted_approx)
}

test "advanced_physics_modeling" {
  // 物理模拟测试
  let initial_velocity = 10
  let acceleration = 2
  let time = 5
  let mass = 50
  
  // 计算最终速度: v = u + at
  let final_velocity = azimuth.add(initial_velocity, azimuth.multiply(acceleration, time))
  
  // 计算位移: s = ut + 0.5at²
  let displacement = azimuth.add(azimuth.multiply(initial_velocity, time), azimuth.divide_with_ceil(azimuth.multiply(acceleration, azimuth.multiply(time, time)), 2))
  
  // 计算动能: KE = 0.5mv²
  let kinetic_energy = azimuth.divide_with_ceil(azimuth.multiply(mass, azimuth.multiply(final_velocity, final_velocity)), 2)
  
  // 计算需要的燃料单位（每单位燃料提供100能量）
  let fuel_units = azimuth.divide_with_ceil(kinetic_energy, 100)
  
  azimuth.assert_eq(20, final_velocity)
  azimuth.assert_eq(75, displacement)
  azimuth.assert_eq(10000, kinetic_energy)
  azimuth.assert_eq(100, fuel_units)
}