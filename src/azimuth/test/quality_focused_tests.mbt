// 高质量聚焦测试用例 - 8个专业级测试
// 测试 azimuth 库的核心功能和边界情况

test "mathematical_series_summation" {
  // 数学级数求和测试
  // 计算等差数列: 3, 7, 11, 15, 19 (公差为4)
  let term1 = 3
  let term2 = @azimuth.add(term1, 4)
  let term3 = @azimuth.add(term2, 4)
  let term4 = @azimuth.add(term3, 4)
  let term5 = @azimuth.add(term4, 4)
  
  // 计算级数和
  let series_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  
  // 验证计算结果
  @azimuth.assert_eq(7, term2)
  @azimuth.assert_eq(11, term3)
  @azimuth.assert_eq(15, term4)
  @azimuth.assert_eq(19, term5)
  @azimuth.assert_eq(55, series_sum)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let annual_rate = 8
  let _years = 3
  
  // 第一年
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100)
  let year1_total = @azimuth.add(principal, year1_interest)
  
  // 第二年
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year1_total, annual_rate), 100)
  let year2_total = @azimuth.add(year1_total, year2_interest)
  
  // 第三年
  let year3_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year2_total, annual_rate), 100)
  let year3_total = @azimuth.add(year2_total, year3_interest)
  
  // 计算总收益
  let total_profit = @azimuth.subtract(year3_total, principal)
  
  @azimuth.assert_eq(10800, year1_total)
  @azimuth.assert_eq(11664, year2_total)
  @azimuth.assert_eq(12597, year3_total)
  @azimuth.assert_eq(2597, total_profit)
}

test "inventory_optimization" {
  // 库存优化算法测试
  let warehouse_capacity = 1000
  let small_boxes = 15
  let medium_boxes = 8
  let large_boxes = 4
  
  let small_volume = @azimuth.multiply(small_boxes, 20)  // 每个小箱20单位
  let medium_volume = @azimuth.multiply(medium_boxes, 50)  // 每个中箱50单位
  let large_volume = @azimuth.multiply(large_boxes, 100)  // 每个大箱100单位
  
  // 计算总体积
  let total_volume = @azimuth.add(@azimuth.add(small_volume, medium_volume), large_volume)
  
  // 计算剩余空间
  let remaining_space = @azimuth.subtract(warehouse_capacity, total_volume)
  
  // 计算空间利用率
  let utilization_rate = @azimuth.divide_with_ceil(@azimuth.multiply(total_volume, 100), warehouse_capacity)
  
  @azimuth.assert_eq(300, small_volume)
  @azimuth.assert_eq(400, medium_volume)
  @azimuth.assert_eq(400, large_volume)
  @azimuth.assert_eq(1100, total_volume)
  @azimuth.assert_eq(-100, remaining_space)  // 超出容量
  @azimuth.assert_eq(110, utilization_rate)
}

test "temperature_conversion_chain" {
  // 温度转换链测试
  let celsius = 30
  
  // 摄氏度转华氏度: F = C × 9/5 + 32
  let fahrenheit_step1 = @azimuth.multiply(celsius, 9)
  let fahrenheit_step2 = @azimuth.divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit = @azimuth.add(fahrenheit_step2, 32)
  
  // 华氏度转开尔文: K = (F - 32) × 5/9 + 273
  let kelvin_step1 = @azimuth.subtract(fahrenheit, 32)
  let kelvin_step2 = @azimuth.multiply(kelvin_step1, 5)
  let kelvin_step3 = @azimuth.divide_with_ceil(kelvin_step2, 9)
  let kelvin = @azimuth.add(kelvin_step3, 273)
  
  // 开尔文转兰金: R = K × 9/5
  let rankine_step1 = @azimuth.multiply(kelvin, 9)
  let rankine = @azimuth.divide_with_ceil(rankine_step1, 5)
  
  @azimuth.assert_eq(86, fahrenheit)
  @azimuth.assert_eq(303, kelvin)
  @azimuth.assert_eq(545, rankine)
}

test "game_score_system" {
  // 游戏评分系统测试
  let base_score = 1000
  let level_multiplier = 3
  let combo_count = 5
  let penalty_count = 2
  
  // 基础分数计算
  let leveled_score = @azimuth.multiply(base_score, level_multiplier)
  
  // 连击奖励: 每个连击+50分
  let combo_bonus = @azimuth.multiply(combo_count, 50)
  let score_with_combo = @azimuth.add(leveled_score, combo_bonus)
  
  // 惩罚扣除: 每个惩罚-100分
  let total_penalty = @azimuth.multiply(penalty_count, 100)
  let final_score = @azimuth.subtract(score_with_combo, total_penalty)
  
  // 计算星级 (每500分一星)
  let star_rating = @azimuth.divide_with_ceil(final_score, 500)
  
  @azimuth.assert_eq(3000, leveled_score)
  @azimuth.assert_eq(250, combo_bonus)
  @azimuth.assert_eq(3250, score_with_combo)
  @azimuth.assert_eq(200, total_penalty)
  @azimuth.assert_eq(3050, final_score)
  @azimuth.assert_eq(7, star_rating)
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_budget = 50000
  let team_members = 8
  let project_duration = 12  // 月
  
  // 计算人均预算
  let budget_per_person = @azimuth.divide_with_ceil(total_budget, team_members)
  
  // 计算月度预算
  let monthly_budget = @azimuth.divide_with_ceil(total_budget, project_duration)
  
  // 计算人均月度预算
  let monthly_per_person = @azimuth.divide_with_ceil(budget_per_person, project_duration)
  
  // 分配资源: 40%开发, 30%测试, 20%设计, 10%管理
  let dev_budget = @azimuth.divide_with_ceil(@azimuth.multiply(total_budget, 40), 100)
  let test_budget = @azimuth.divide_with_ceil(@azimuth.multiply(total_budget, 30), 100)
  let design_budget = @azimuth.divide_with_ceil(@azimuth.multiply(total_budget, 20), 100)
  let mgmt_budget = @azimuth.subtract(total_budget, @azimuth.add(@azimuth.add(@azimuth.add(dev_budget, test_budget), design_budget), 0))
  
  @azimuth.assert_eq(6250, budget_per_person)
  @azimuth.assert_eq(4167, monthly_budget)
  @azimuth.assert_eq(521, monthly_per_person)
  @azimuth.assert_eq(20000, dev_budget)
  @azimuth.assert_eq(15000, test_budget)
  @azimuth.assert_eq(10000, design_budget)
  @azimuth.assert_eq(5000, mgmt_budget)
}

test "data_compression_analysis" {
  // 数据压缩分析测试
  let original_data = 2500
  let compression_ratio = 4
  
  // 计算压缩后大小
  let compressed_size = @azimuth.divide_with_ceil(original_data, compression_ratio)
  
  // 计算节省空间
  let space_saved = @azimuth.subtract(original_data, compressed_size)
  
  // 计算压缩效率百分比
  let compression_efficiency = @azimuth.divide_with_ceil(@azimuth.multiply(space_saved, 100), original_data)
  
  // 二次压缩测试
  let second_compression = @azimuth.divide_with_ceil(compressed_size, compression_ratio)
  let total_compression_ratio = @azimuth.divide_with_ceil(original_data, second_compression)
  
  @azimuth.assert_eq(625, compressed_size)
  @azimuth.assert_eq(1875, space_saved)
  @azimuth.assert_eq(75, compression_efficiency)
  @azimuth.assert_eq(157, second_compression)
  @azimuth.assert_eq(16, total_compression_ratio)
}

test "network_routing_optimization" {
  // 网络路由优化测试
  let nodes = 6
  let bandwidth_per_edge = 100
  let max_hops = 3
  
  // 计算最大可能连接数
  let max_connections = @azimuth.divide_with_ceil(@azimuth.multiply(nodes, @azimuth.subtract(nodes, 1)), 2)
  
  // 计算总带宽需求
  let total_bandwidth = @azimuth.multiply(max_connections, bandwidth_per_edge)
  
  // 计算平均路径长度
  let avg_path_length = @azimuth.divide_with_ceil(@azimuth.add(1, max_hops), 2)
  
  // 计算延迟因子 (路径长度 × 负载因子)
  let load_factor = @azimuth.divide_with_ceil(max_connections, nodes)
  let delay_factor = @azimuth.multiply(avg_path_length, load_factor)
  
  // 计算网络效率 (反比于延迟)
  let network_efficiency = @azimuth.divide_with_ceil(100, delay_factor)
  
  @azimuth.assert_eq(15, max_connections)
  @azimuth.assert_eq(1500, total_bandwidth)
  @azimuth.assert_eq(2, avg_path_length)
  @azimuth.assert_eq(3, load_factor)
  @azimuth.assert_eq(6, delay_factor)
  @azimuth.assert_eq(17, network_efficiency)
}