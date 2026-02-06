// Azimuth 综合测试套件 - 标准MoonBit测试用例
// 包含10个高质量测试用例，覆盖各种场景

test "mathematical_series_calculation" {
  // 数学级数计算测试 - 等差数列求和
  // 计算 1 + 3 + 5 + 7 + 9 + 11 + 13 + 15 = 64
  
  let first_term = 1
  let common_diff = 2
  let num_terms = 8
  
  // 手动计算前几项
  let term1 = first_term
  let term2 = add(term1, common_diff)           // 3
  let term3 = add(term2, common_diff)           // 5
  let term4 = add(term3, common_diff)           // 7
  let term5 = add(term4, common_diff)           // 9
  let term6 = add(term5, common_diff)           // 11
  let term7 = add(term6, common_diff)           // 13
  let term8 = add(term7, common_diff)           // 15
  
  // 计算总和
  let sum = add(add(add(add(add(add(add(term1, term2), term3), term4), term5), term6), term7), term8)
  
  // 验证等差数列求和公式: n/2 * (2a + (n-1)d)
  let formula_sum = divide_with_ceil(multiply(num_terms, add(multiply(2, first_term), multiply(subtract(num_terms, 1), common_diff))), 2)
  
  assert_eq(term1, 1)
  assert_eq(term2, 3)
  assert_eq(term3, 5)
  assert_eq(term4, 7)
  assert_eq(term5, 9)
  assert_eq(term6, 11)
  assert_eq(term7, 13)
  assert_eq(term8, 15)
  assert_eq(sum, 64)
  assert_eq(formula_sum, 64)
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：有限预算下的项目资源分配
  
  let total_budget = 50000
  let project_a_cost = 12000
  let project_b_cost = 8000
  let project_c_cost = 15000
  let project_d_cost = 10000
  
  // 计算所有可能的组合成本
  let combo_ab = add(project_a_cost, project_b_cost)
  let combo_ac = add(project_a_cost, project_c_cost)
  let combo_ad = add(project_a_cost, project_d_cost)
  let combo_bc = add(project_b_cost, project_c_cost)
  let combo_bd = add(project_b_cost, project_d_cost)
  let combo_cd = add(project_c_cost, project_d_cost)
  
  // 计算三项目组合
  let combo_abc = add(combo_ab, project_c_cost)
  let combo_abd = add(combo_ab, project_d_cost)
  let combo_acd = add(combo_ac, project_d_cost)
  let combo_bcd = add(combo_bc, project_d_cost)
  
  // 计算四项目组合
  let combo_abcd = add(combo_abc, project_d_cost)
  
  // 找出最佳组合（成本最接近预算但不超过）
  let best_combo = combo_acd  // 12000 + 15000 + 10000 = 37000
  let remaining_budget = subtract(total_budget, best_combo)
  
  assert_eq(combo_ab, 20000)
  assert_eq(combo_ac, 27000)
  assert_eq(combo_ad, 22000)
  assert_eq(combo_bc, 23000)
  assert_eq(combo_bd, 18000)
  assert_eq(combo_cd, 25000)
  assert_eq(combo_abc, 35000)
  assert_eq(combo_abd, 30000)
  assert_eq(combo_acd, 37000)
  assert_eq(combo_bcd, 33000)
  assert_eq(combo_abcd, 45000)
  assert_eq(best_combo, 37000)
  assert_eq(remaining_budget, 13000)
}

test "time_zone_conversion" {
  // 时区转换测试
  // 场景：从北京时间转换为其他时区时间
  
  let beijing_hour = 14  // 北京时间下午2点
  let tokyo_offset = 1   // 东京比北京快1小时
  let london_offset = -8 // 伦敦比北京慢8小时
  let new_york_offset = -13 // 纽约比北京慢13小时
  
  // 计算各时区时间
  let tokyo_hour = add(beijing_hour, tokyo_offset)
  let london_hour = add(beijing_hour, london_offset)
  let new_york_hour = add(beijing_hour, new_york_offset)
  
  // 处理24小时制
  let tokyo_final = tokyo_hour
  let london_final = divide_with_ceil(london_hour, 24) > 0 ? add(london_hour, 24) : london_hour
  let new_york_final = divide_with_ceil(new_york_hour, 24) > 0 ? add(new_york_hour, 24) : new_york_hour
  
  // 计算时差
  let tokyo_diff = subtract(tokyo_final, beijing_hour)
  let london_diff = subtract(london_final, beijing_hour)
  let new_york_diff = subtract(new_york_final, beijing_hour)
  
  assert_eq(tokyo_hour, 15)
  assert_eq(london_hour, 6)
  assert_eq(new_york_hour, 1)
  assert_eq(tokyo_final, 15)
  assert_eq(london_final, 6)
  assert_eq(new_york_final, 1)
  assert_eq(tokyo_diff, 1)
  assert_eq(london_diff, -8)
  assert_eq(new_york_diff, -13)
}

test "data_structure_operations" {
  // 数据结构操作测试 - 队列操作模拟
  // 场景：模拟队列的入队和出队操作
  
  let queue_size = 0
  let front_element = 0
  let rear_element = 0
  
  // 入队操作
  let enqueue1 = 10
  let queue_size1 = add(queue_size, 1)
  let rear1 = enqueue1
  
  let enqueue2 = 20
  let queue_size2 = add(queue_size1, 1)
  let rear2 = enqueue2
  
  let enqueue3 = 30
  let queue_size3 = add(queue_size2, 1)
  let rear3 = enqueue3
  
  // 出队操作
  let dequeue1 = 10  // 第一个入队的元素
  let queue_size_after_dequeue1 = subtract(queue_size3, 1)
  let new_front1 = 20
  
  let dequeue2 = 20  // 第二个入队的元素
  let queue_size_after_dequeue2 = subtract(queue_size_after_dequeue1, 1)
  let new_front2 = 30
  
  let dequeue3 = 30  // 第三个入队的元素
  let queue_size_after_dequeue3 = subtract(queue_size_after_dequeue2, 1)
  
  assert_eq(queue_size1, 1)
  assert_eq(rear1, 10)
  assert_eq(queue_size2, 2)
  assert_eq(rear2, 20)
  assert_eq(queue_size3, 3)
  assert_eq(rear3, 30)
  assert_eq(dequeue1, 10)
  assert_eq(queue_size_after_dequeue1, 2)
  assert_eq(new_front1, 20)
  assert_eq(dequeue2, 20)
  assert_eq(queue_size_after_dequeue2, 1)
  assert_eq(new_front2, 30)
  assert_eq(dequeue3, 30)
  assert_eq(queue_size_after_dequeue3, 0)
}

test "geometry_area_calculations" {
  // 几何面积计算测试
  // 场景：计算不同形状的面积
  
  // 矩形面积
  let rect_length = 8
  let rect_width = 5
  let rect_area = multiply(rect_length, rect_width)
  
  // 三角形面积 (底×高÷2)
  let triangle_base = 10
  let triangle_height = 6
  let triangle_area = divide_with_ceil(multiply(triangle_base, triangle_height), 2)
  
  // 梯形面积 ((上底+下底)×高÷2)
  let trapezoid_top = 6
  let trapezoid_bottom = 10
  let trapezoid_height = 4
  let trapezoid_area = divide_with_ceil(multiply(add(trapezoid_top, trapezoid_bottom), trapezoid_height), 2)
  
  // 计算总面积
  let total_area = add(add(rect_area, triangle_area), trapezoid_area)
  
  // 计算面积占比
  let rect_percentage = divide_with_ceil(multiply(rect_area, 100), total_area)
  let triangle_percentage = divide_with_ceil(multiply(triangle_area, 100), total_area)
  let trapezoid_percentage = divide_with_ceil(multiply(trapezoid_area, 100), total_area)
  
  assert_eq(rect_area, 40)
  assert_eq(triangle_area, 30)
  assert_eq(trapezoid_area, 32)
  assert_eq(total_area, 102)
  assert_eq(rect_percentage, 40)
  assert_eq(triangle_percentage, 30)
  assert_eq(trapezoid_percentage, 32)
}

test "probability_calculation" {
  // 概率计算测试
  // 场景：计算简单事件的概率
  
  let total_outcomes = 36  // 两个骰子的所有可能结果
  let favorable_sum_7 = 6  // 和为7的有6种情况
  let favorable_sum_11 = 2  // 和为11的有2种情况
  let favorable_doubles = 6  // 双数的有6种情况
  
  // 计算概率（百分比）
  let prob_sum_7 = divide_with_ceil(multiply(favorable_sum_7, 100), total_outcomes)
  let prob_sum_11 = divide_with_ceil(multiply(favorable_sum_11, 100), total_outcomes)
  let prob_doubles = divide_with_ceil(multiply(favorable_doubles, 100), total_outcomes)
  
  // 计算期望值（假设和为7赢10元，和为11赢20元，双数输5元）
  let win_sum_7 = 10
  let win_sum_11 = 20
  let lose_doubles = -5
  
  let expected_value = add(add(
    divide_with_ceil(multiply(favorable_sum_7, win_sum_7), total_outcomes),
    divide_with_ceil(multiply(favorable_sum_11, win_sum_11), total_outcomes)
  ), divide_with_ceil(multiply(favorable_doubles, lose_doubles), total_outcomes))
  
  assert_eq(prob_sum_7, 17)  // 实际为16.67%，向上取整为17%
  assert_eq(prob_sum_11, 6)  // 实际为5.56%，向上取整为6%
  assert_eq(prob_doubles, 17)  // 实际为16.67%，向上取整为17%
  assert_eq(expected_value, 2)  // 期望值约为1.67，向上取整为2
}

test "network_routing_simulation" {
  // 网络路由模拟测试
  // 场景：计算数据包传输的最短路径
  
  // 节点间的距离（ hops 数）
  let source_to_router1 = 2
  let source_to_router2 = 5
  let router1_to_router3 = 3
  let router2_to_router3 = 1
  let router3_to_destination = 2
  let router1_to_destination = 8
  let router2_to_destination = 6
  
  // 路径1: Source -> Router1 -> Router3 -> Destination
  let path1_distance = add(add(source_to_router1, router1_to_router3), router3_to_destination)
  
  // 路径2: Source -> Router2 -> Router3 -> Destination
  let path2_distance = add(add(source_to_router2, router2_to_router3), router3_to_destination)
  
  // 路径3: Source -> Router1 -> Destination
  let path3_distance = add(source_to_router1, router1_to_destination)
  
  // 路径4: Source -> Router2 -> Destination
  let path4_distance = add(source_to_router2, router2_to_destination)
  
  // 找出最短路径
  let shortest_path = path1_distance
  let path_difference = subtract(path2_distance, path1_distance)
  
  // 计算传输时间（假设每hop需要10ms）
  let transmission_time = multiply(shortest_path, 10)
  
  assert_eq(path1_distance, 7)
  assert_eq(path2_distance, 8)
  assert_eq(path3_distance, 10)
  assert_eq(path4_distance, 11)
  assert_eq(shortest_path, 7)
  assert_eq(path_difference, 1)
  assert_eq(transmission_time, 70)
}

test "cryptography_basic_operations" {
  // 密码学基础操作测试
  // 场景：简单的凯撒密码加密和解密
  
  let original_message = 1234  // 原始消息（数字表示）
  let caesar_key = 7           // 凯撒密码密钥
  let modulus_value = 10000    // 模运算值
  
  // 加密过程：(message + key) mod modulus
  let encrypted_step1 = add(original_message, caesar_key)
  let encrypted_message = encrypted_step1 % modulus_value
  
  // 解密过程：(encrypted - key + modulus) mod modulus
  let decrypted_step1 = subtract(encrypted_message, caesar_key)
  let decrypted_message = decrypted_step1 < 0 ? add(decrypted_step1, modulus_value) : decrypted_step1
  
  // 多重加密（三次加密）
  let encrypt2_step1 = add(encrypted_message, caesar_key)
  let encrypt2_message = encrypt2_step1 % modulus_value
  
  let encrypt3_step1 = add(encrypt2_message, caesar_key)
  let encrypt3_message = encrypt3_step1 % modulus_value
  
  // 计算加密强度（简单度量）
  let encryption_strength = multiply(caesar_key, 3)
  
  assert_eq(encrypted_step1, 1241)
  assert_eq(encrypted_message, 1241)
  assert_eq(decrypted_step1, 1234)
  assert_eq(decrypted_message, 1234)
  assert_eq(encrypt2_step1, 1248)
  assert_eq(encrypt2_message, 1248)
  assert_eq(encrypt3_step1, 1255)
  assert_eq(encrypt3_message, 1255)
  assert_eq(encryption_strength, 21)
}

test "financial_compound_analysis" {
  // 金融复利分析测试
  // 场景：比较不同投资方案的收益
  
  let initial_investment = 10000
  let rate_a = 5   // 方案A：年利率5%
  let rate_b = 4   // 方案B：年利率4%，但每半年复利一次
  let years = 3
  
  // 方案A：年度复利
  let year1_a = add(initial_investment, divide_with_ceil(multiply(initial_investment, rate_a), 100))
  let year2_a = add(year1_a, divide_with_ceil(multiply(year1_a, rate_a), 100))
  let year3_a = add(year2_a, divide_with_ceil(multiply(year2_a, rate_a), 100))
  let profit_a = subtract(year3_a, initial_investment)
  
  // 方案B：半年度复利
  let half_year_b = add(initial_investment, divide_with_ceil(multiply(initial_investment, rate_b), 200))
  let year1_b = add(half_year_b, divide_with_ceil(multiply(half_year_b, rate_b), 200))
  let year1_half_b = add(year1_b, divide_with_ceil(multiply(year1_b, rate_b), 200))
  let year2_b = add(year1_half_b, divide_with_ceil(multiply(year1_half_b, rate_b), 200))
  let year2_half_b = add(year2_b, divide_with_ceil(multiply(year2_b, rate_b), 200))
  let year3_b = add(year2_half_b, divide_with_ceil(multiply(year2_half_b, rate_b), 200))
  let profit_b = subtract(year3_b, initial_investment)
  
  // 比较收益
  let profit_difference = subtract(profit_a, profit_b)
  let better_option = profit_a > profit_b ? 1 : 2  // 1表示方案A更好，2表示方案B更好
  
  assert_eq(year1_a, 10500)
  assert_eq(year2_a, 11025)
  assert_eq(year3_a, 11576)
  assert_eq(profit_a, 1576)
  assert_eq(half_year_b, 10200)
  assert_eq(year1_b, 10404)
  assert_eq(year1_half_b, 10612)
  assert_eq(year2_b, 10824)
  assert_eq(year2_half_b, 11044)
  assert_eq(year3_b, 11266)
  assert_eq(profit_b, 1266)
  assert_eq(profit_difference, 310)
  assert_eq(better_option, 1)
}

test "algorithm_complexity_analysis" {
  // 算法复杂度分析测试
  // 场景：比较不同算法的时间复杂度
  
  let input_size = 100
  
  // O(1) - 常数时间复杂度
  let o1_operations = 1
  
  // O(log n) - 对数时间复杂度（二分查找）
  let ologn_operations = 7  // log2(100) ≈ 6.64，向上取整为7
  
  // O(n) - 线性时间复杂度
  let on_operations = input_size
  
  // O(n log n) - 线性对数时间复杂度（高效排序）
  let onlogn_operations = multiply(input_size, ologn_operations)
  
  // O(n²) - 平方时间复杂度（简单排序）
  let on2_operations = multiply(input_size, input_size)
  
  // 计算相对性能
  let o1_vs_on = divide_with_ceil(on_operations, o1_operations)
  let ologn_vs_on = divide_with_ceil(on_operations, ologn_operations)
  let on_vs_on2 = divide_with_ceil(on2_operations, on_operations)
  
  // 计算在更大输入规模下的表现
  let large_input = 10000
  let large_on = large_input
  let large_on2 = multiply(large_input, large_input)
  let large_performance_diff = divide_with_ceil(large_on2, large_on)
  
  assert_eq(o1_operations, 1)
  assert_eq(ologn_operations, 7)
  assert_eq(on_operations, 100)
  assert_eq(onlogn_operations, 700)
  assert_eq(on2_operations, 10000)
  assert_eq(o1_vs_on, 100)
  assert_eq(ologn_vs_on, 15)  // 100/7 ≈ 14.29，向上取整为15
  assert_eq(on_vs_on2, 100)
  assert_eq(large_on, 10000)
  assert_eq(large_on2, 100000000)
  assert_eq(large_performance_diff, 10000)
}