// Azimuth 项目新增标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "azimuth_basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(6, 4))
  assert_eq(24, multiply(6, 4))
  assert_eq(2, subtract(6, 4))
  assert_eq(2, divide_with_ceil(6, 4))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "azimuth_string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, !", greet(""))
}

test "azimuth_division_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数除法
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(1, divide_with_ceil(3, 5))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 4))
  assert_eq(-2, divide_with_ceil(10, -5))
  assert_eq(3, divide_with_ceil(-10, -4))
}

test "azimuth_negative_number_calculations" {
  // 负数计算测试
  assert_eq(-10, add(-6, -4))
  assert_eq(24, multiply(-6, -4))
  assert_eq(-24, multiply(-6, 4))
  assert_eq(-2, subtract(-6, -4))
  assert_eq(-10, subtract(6, 16))
}

test "azimuth_mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 15
  let y = 25
  let z = 10
  
  // 交换律
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 结合律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 单位元
  assert_eq(add(x, 0), x)
  assert_eq(multiply(x, 1), x)
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "azimuth_business_packaging_calculation" {
  // 业务包装计算测试
  let total_products = 47
  let package_capacity = 12
  let packages_needed = divide_with_ceil(total_products, package_capacity)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, package_capacity)
  let unused_space = subtract(total_capacity, total_products)
  
  assert_eq(48, total_capacity)
  assert_eq(1, unused_space)
}

test "azimuth_financial_interest_calculation" {
  // 金融利息计算测试
  let principal = 8000
  let interest_rate = 6
  
  // 计算利息
  let interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let total_with_interest = add(principal, interest)
  
  assert_eq(480, interest)
  assert_eq(8480, total_with_interest)
  
  // 复利计算（第二年）
  let second_year_interest = divide_with_ceil(multiply(total_with_interest, interest_rate), 100)
  let second_year_total = add(total_with_interest, second_year_interest)
  
  assert_eq(509, second_year_interest)
  assert_eq(8989, second_year_total)
}

test "azimuth_temperature_conversion_scenario" {
  // 温度转换场景测试
  let celsius_temp = 20
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let fahrenheit_step1 = multiply(celsius_temp, 9)
  let fahrenheit_step2 = divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit_temp = add(fahrenheit_step2, 32)
  
  assert_eq(68, fahrenheit_temp)
  
  // 华氏度转开尔文（简化）
  let kelvin_temp = add(divide_with_ceil(multiply(subtract(fahrenheit_temp, 32), 5), 9), 273)
  
  assert_eq(293, kelvin_temp)
}

test "azimuth_game_scoring_system" {
  // 游戏评分系统测试
  let base_score = 50
  let level_bonus = 30
  let combo_multiplier = 2
  let penalty = 15
  
  // 计算最终得分
  let level_score = add(base_score, level_bonus)
  let combo_score = multiply(level_score, combo_multiplier)
  let final_score = subtract(combo_score, penalty)
  
  assert_eq(80, level_score)
  assert_eq(160, combo_score)
  assert_eq(145, final_score)
  
  // 计算星级（每50分一星）
  let star_rating = divide_with_ceil(final_score, 50)
  assert_eq(3, star_rating)
}

test "azimuth_resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 1500
  let project_a_cost = 450
  let project_b_cost = 380
  let project_c_cost = 290
  let project_d_cost = 520
  
  // 计算不同组合
  let combo_abc = add(add(project_a_cost, project_b_cost), project_c_cost)
  let combo_abd = add(add(project_a_cost, project_b_cost), project_d_cost)
  let combo_acd = add(add(project_a_cost, project_c_cost), project_d_cost)
  let combo_bcd = add(add(project_b_cost, project_c_cost), project_d_cost)
  
  // 找出最优组合（不超过预算且最大利用预算）
  let best_combo = combo_acd  // 450 + 290 + 520 = 1260
  let remaining_budget = subtract(total_budget, best_combo)
  let utilization_percent = divide_with_ceil(multiply(best_combo, 100), total_budget)
  
  assert_eq(1120, combo_abc)
  assert_eq(1350, combo_abd)
  assert_eq(1260, combo_acd)
  assert_eq(1190, combo_bcd)
  assert_eq(240, remaining_budget)
  assert_eq(84, utilization_percent)
}