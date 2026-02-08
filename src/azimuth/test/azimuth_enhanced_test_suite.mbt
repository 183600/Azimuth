// Azimuth 项目增强测试套件
// 包含 10 个高质量 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖多种实际应用场景

test "financial_compound_interest" {
  // 复利计算测试 - 银行存款场景
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, rate), 100)
  let year1_balance = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_balance, rate), 100)
  let year2_balance = add(year1_balance, year2_interest)
  
  // 第三年
  let year3_interest = divide_with_ceil(multiply(year2_balance, rate), 100)
  let final_balance = add(year2_balance, year3_interest)
  
  let total_profit = subtract(final_balance, principal)
  
  assert_eq(10500, year1_balance)
  assert_eq(11025, year2_balance)
  assert_eq(11576, final_balance)
  assert_eq(1576, total_profit)
}

test "inventory_optimization" {
  // 库存优化测试 - 零售场景
  let monthly_demand = 450
  let package_size = 24
  let safety_stock = 50
  
  // 计算需要的包数
  let packages_needed = divide_with_ceil(monthly_demand, package_size)
  
  // 计算总库存
  let total_inventory = add(multiply(packages_needed, package_size), safety_stock)
  
  // 计算过剩库存
  let excess_inventory = subtract(total_inventory, add(monthly_demand, safety_stock))
  
  assert_eq(19, packages_needed)
  assert_eq(506, total_inventory)
  assert_eq(6, excess_inventory)
}

test "data_structure_array_operations" {
  // 数据结构数组操作模拟
  let array_length = 8
  let element_size = 4
  
  // 计算数组总大小
  let total_size = multiply(array_length, element_size)
  
  // 模拟索引访问（第3个元素的偏移量）
  let index = 2  // 0-based
  let offset = multiply(index, element_size)
  
  // 计算剩余元素数量
  let remaining_elements = subtract(array_length, add(index, 1))
  
  assert_eq(32, total_size)
  assert_eq(8, offset)
  assert_eq(5, remaining_elements)
}

test "time_zone_conversion" {
  // 时区转换测试 - 国际应用场景
  let local_time = 14  // 下午2点
  let utc_offset = -8  // PST时区
  let target_offset = 1  // CET时区
  
  // 转换为UTC时间
  let utc_time = add(local_time, -utc_offset)
  
  // 转换为目标时区
  let target_time = add(utc_time, target_offset)
  
  // 处理24小时制
  let adjusted_time = target_time
  let final_time = subtract(adjusted_time, multiply(divide_with_ceil(adjusted_time, 24), 24))
  
  assert_eq(22, utc_time)
  assert_eq(23, target_time)
  assert_eq(23, final_time)
}

test "engineering_stress_calculation" {
  // 工程压力计算测试
  let force = 1000  // 牛顿
  let area = 50     // 平方厘米
  let safety_factor = 2
  
  // 计算压力
  let stress = divide_with_ceil(force, area)
  
  // 计算最大安全压力
  let max_safe_stress = multiply(stress, safety_factor)
  
  // 计算材料等级（每100单位一个等级）
  let material_grade = divide_with_ceil(max_safe_stress, 100)
  
  assert_eq(20, stress)
  assert_eq(40, max_safe_stress)
  assert_eq(1, material_grade)
}

test "game_physics_simulation" {
  // 游戏物理模拟测试
  let initial_velocity = 5
  let gravity = -2
  let time_steps = 4
  
  // 模拟每秒的速度变化
  let velocity1 = add(initial_velocity, multiply(gravity, 1))
  let velocity2 = add(initial_velocity, multiply(gravity, 2))
  let velocity3 = add(initial_velocity, multiply(gravity, 3))
  let velocity4 = add(initial_velocity, multiply(gravity, 4))
  
  // 计算总位移
  let displacement = add(add(add(velocity1, velocity2), velocity3), velocity4)
  
  assert_eq(3, velocity1)
  assert_eq(1, velocity2)
  assert_eq(-1, velocity3)
  assert_eq(-3, velocity4)
  assert_eq(0, displacement)
}

test "network_bandwidth_calculation" {
  // 网络带宽计算测试
  let file_size = 2500  // KB
  let network_speed = 500  // KB/s
  let overhead_factor = 10  // 10% 开销
  
  // 计算基础传输时间
  let base_time = divide_with_ceil(file_size, network_speed)
  
  // 计算开销时间
  let overhead_time = divide_with_ceil(multiply(base_time, overhead_factor), 100)
  
  // 计算总时间
  let total_time = add(base_time, overhead_time)
  
  assert_eq(5, base_time)
  assert_eq(1, overhead_time)
  assert_eq(6, total_time)
}

test "cooking_recipe_scaling" {
  // 烹饪食谱缩放测试
  let original_servings = 4
  let target_servings = 7
  let flour_per_serving = 150  // 克
  let sugar_per_serving = 50   // 克
  
  // 计算缩放因子
  let scale_factor = divide_with_ceil(multiply(target_servings, 100), multiply(original_servings, 100))
  
  // 计算新配方用量
  let new_flour = multiply(flour_per_serving, target_servings)
  let new_sugar = multiply(sugar_per_serving, target_servings)
  
  // 计算总重量
  let total_weight = add(new_flour, new_sugar)
  
  assert_eq(1050, new_flour)
  assert_eq(350, new_sugar)
  assert_eq(1400, total_weight)
}

test "energy_efficiency_analysis" {
  // 能效分析测试
  let power_consumption = 1200  // 瓦特
  let operating_hours = 8      // 小时
  let output_power = 900       // 瓦特
  let days_in_month = 30       // 天
  
  // 计算日能耗
  let daily_energy = multiply(power_consumption, operating_hours)
  
  // 计算月能耗
  let monthly_energy = multiply(daily_energy, days_in_month)
  
  // 计算效率
  let efficiency = divide_with_ceil(multiply(output_power, 100), power_consumption)
  
  // 计算能量损失
  let energy_loss = subtract(power_consumption, output_power)
  
  assert_eq(9600, daily_energy)
  assert_eq(288000, monthly_energy)
  assert_eq(75, efficiency)
  assert_eq(300, energy_loss)
}

test "cryptographic_hash_simulation" {
  // 密码学哈希模拟测试
  let input_value = 12345
  let prime_modulus = 997
  let hash_constant = 31
  
  // 简单哈希函数模拟
  let step1 = multiply(input_value, hash_constant)
  let step2 = add(step1, input_value)
  let hash_result = step2 % prime_modulus
  
  // 二次哈希
  let step3 = multiply(hash_result, hash_constant)
  let final_hash = step3 % prime_modulus
  
  // 验证哈希值范围
  let hash_validity = divide_with_ceil(hash_result, prime_modulus)
  
  assert_eq(383010, step1)
  assert_eq(395355, step2)
  assert_eq(367, hash_result)
  assert_eq(11377, step3)
  assert_eq(383, final_hash)
  assert_eq(0, hash_validity)
}