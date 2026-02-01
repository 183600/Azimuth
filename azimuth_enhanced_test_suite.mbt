// Azimuth 增强测试套件
// 包含8个高质量的 MoonBit 测试用例

// 导入 azimuth 库
use azimuth

test "mathematical_distributive_property" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 4
  let c = 3
  
  let left_side = azimuth.multiply(a, azimuth.add(b, c))
  let right_side = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  
  azimuth.assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
}

test "complex_resource_allocation" {
  // 复杂资源分配场景测试
  // 场景：多项目资源分配优化
  let total_budget = 50000
  let project_a_priority = 3
  let project_b_priority = 2
  let project_c_priority = 1
  let total_priority = azimuth.add(project_a_priority, azimuth.add(project_b_priority, project_c_priority))
  
  // 按优先级分配预算
  let budget_a = azimuth.divide_with_ceil(
    azimuth.multiply(total_budget, project_a_priority), 
    total_priority
  )
  let budget_b = azimuth.divide_with_ceil(
    azimuth.multiply(total_budget, project_b_priority), 
    total_priority
  )
  let budget_c = azimuth.divide_with_ceil(
    azimuth.multiply(total_budget, project_c_priority), 
    total_priority
  )
  
  // 验证分配结果
  azimuth.assert_eq(25000, budget_a)  // ceil(50000*3/6) = 25000
  azimuth.assert_eq(16667, budget_b)  // ceil(50000*2/6) = 16667
  azimuth.assert_eq(8334, budget_c)   // ceil(50000*1/6) = 8334
  
  // 验证总分配不超过预算
  let total_allocated = azimuth.add(budget_a, azimuth.add(budget_b, budget_c))
  azimuth.assert_true(total_allocated <= total_budget)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算测试
  let initial_inventory = 1200
  let purchases = 800
  let final_inventory = 600
  let cost_per_unit = 25
  
  // 计算销售数量
  let goods_sold = azimuth.subtract(
    azimuth.add(initial_inventory, purchases), 
    final_inventory
  )
  
  // 计算销售成本
  let cost_of_goods_sold = azimuth.multiply(goods_sold, cost_per_unit)
  
  // 计算平均库存
  let average_inventory = azimuth.divide_with_ceil(
    azimuth.add(initial_inventory, final_inventory), 
    2
  )
  
  // 计算库存周转率
  let inventory_turnover = azimuth.divide_with_ceil(
    cost_of_goods_sold, 
    azimuth.multiply(average_inventory, cost_per_unit)
  )
  
  azimuth.assert_eq(1400, goods_sold)           // 1200 + 800 - 600 = 1400
  azimuth.assert_eq(35000, cost_of_goods_sold)  // 1400 * 25 = 35000
  azimuth.assert_eq(900, average_inventory)     // ceil((1200+600)/2) = 900
  azimuth.assert_eq(2, inventory_turnover)      // ceil(35000/(900*25)) = 2
}

test "time_zone_conversion_complex" {
  // 复杂时区转换测试
  let base_time_hours = 14  // 下午2点
  let base_time_minutes = 30
  let timezone_offset_hours = 8  // 东八区
  let target_timezone_offset = -5  // 西五区
  
  // 转换为分钟
  let base_total_minutes = azimuth.add(
    azimuth.multiply(base_time_hours, 60), 
    base_time_minutes
  )
  
  // 计算时区差异
  let timezone_difference = azimuth.subtract(
    timezone_offset_hours, 
    target_timezone_offset
  )
  
  // 转换时区
  let converted_minutes = azimuth.add(
    base_total_minutes, 
    azimuth.multiply(timezone_difference, 60)
  )
  
  // 处理跨日情况
  let adjusted_minutes = converted_minutes % (24 * 60)
  let final_hours = azimuth.divide_with_ceil(adjusted_minutes, 60) - 1
  let final_minutes = adjusted_minutes % 60
  
  azimuth.assert_eq(870, base_total_minutes)      // 14*60 + 30 = 870
  azimuth.assert_eq(13, timezone_difference)     // 8 - (-5) = 13
  azimuth.assert_eq(1650, converted_minutes)     // 870 + 13*60 = 1650
  azimuth.assert_eq(3, final_hours)              // floor(1650/60) % 24 = 3
  azimuth.assert_eq(30, final_minutes)           // 1650 % 60 = 30
}

test "project_critical_path_analysis" {
  // 项目关键路径分析测试
  let task_a_duration = 5
  let task_b_duration = 8
  let task_c_duration = 3
  let task_d_duration = 6
  
  // 并行任务A和B
  let parallel_ab_duration = azimuth.divide_with_ceil(
    azimuth.add(task_a_duration, task_b_duration), 
    2
  )
  
  // 串行任务C和D
  let serial_cd_duration = azimuth.add(task_c_duration, task_d_duration)
  
  // 关键路径长度
  let critical_path = azimuth.add(parallel_ab_duration, serial_cd_duration)
  
  // 计算项目缓冲时间（假设项目期限为20天）
  let project_deadline = 20
  let buffer_time = azimuth.subtract(project_deadline, critical_path)
  
  azimuth.assert_eq(7, parallel_ab_duration)    // ceil((5+8)/2) = 7
  azimuth.assert_eq(9, serial_cd_duration)      // 3 + 6 = 9
  azimuth.assert_eq(16, critical_path)          // 7 + 9 = 16
  azimuth.assert_eq(4, buffer_time)             // 20 - 16 = 4
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算（整数运算）
  let year_1_amount = azimuth.add(
    principal, 
    azimuth.divide_with_ceil(azimuth.multiply(principal, annual_rate), 100)
  )
  let year_2_amount = azimuth.add(
    year_1_amount, 
    azimuth.divide_with_ceil(azimuth.multiply(year_1_amount, annual_rate), 100)
  )
  let year_3_amount = azimuth.add(
    year_2_amount, 
    azimuth.divide_with_ceil(azimuth.multiply(year_2_amount, annual_rate), 100)
  )
  
  // 计算总收益
  let total_interest = azimuth.subtract(year_3_amount, principal)
  
  azimuth.assert_eq(10500, year_1_amount)        // 10000 + ceil(10000*5/100) = 10500
  azimuth.assert_eq(11025, year_2_amount)        // 10500 + ceil(10500*5/100) = 11025
  azimuth.assert_eq(11576, year_3_amount)        // 11025 + ceil(11025*5/100) = 11576
  azimuth.assert_eq(1576, total_interest)        // 11576 - 10000 = 1576
}

test "manufacturing_quality_control" {
  // 制造业质量控制测试
  let total_production = 1000
  let defect_rate = 3  // 3%
  let sample_size = 100
  
  // 计算预期缺陷数量
  let expected_defects = azimuth.divide_with_ceil(
    azimuth.multiply(total_production, defect_rate), 
    100
  )
  
  // 计算样本中的预期缺陷
  let sample_defects = azimuth.divide_with_ceil(
    azimuth.multiply(sample_size, defect_rate), 
    100
  )
  
  // 计算质量控制批次大小
  let batch_size = azimuth.divide_with_ceil(total_production, 10)
  
  // 计算需要的质检员数量（每人每天可检查50个）
  let inspectors_needed = azimuth.divide_with_ceil(total_production, 50)
  
  azimuth.assert_eq(30, expected_defects)         // ceil(1000*3/100) = 30
  azimuth.assert_eq(3, sample_defects)           // ceil(100*3/100) = 3
  azimuth.assert_eq(100, batch_size)             // ceil(1000/10) = 100
  azimuth.assert_eq(20, inspectors_needed)       // ceil(1000/50) = 20
}

test "logistics_routing_optimization" {
  // 物流路径优化测试
  let warehouse_locations = 5
  let delivery_points = 23
  let truck_capacity = 8
  let average_distance = 15
  
  // 计算需要的车辆数量
  let trucks_needed = azimuth.divide_with_ceil(delivery_points, truck_capacity)
  
  // 计算总配送距离
  let total_distance = azimuth.multiply(trucks_needed, average_distance)
  
  // 计算仓库覆盖范围
  let coverage_per_warehouse = azimuth.divide_with_ceil(
    delivery_points, 
    warehouse_locations
  )
  
  // 计算最优路径复杂度（简化模型）
  let route_complexity = azimuth.multiply(
    azimuth.divide_with_ceil(delivery_points, 2), 
    warehouse_locations
  )
  
  azimuth.assert_eq(3, trucks_needed)            // ceil(23/8) = 3
  azimuth.assert_eq(45, total_distance)          // 3 * 15 = 45
  azimuth.assert_eq(5, coverage_per_warehouse)   // ceil(23/5) = 5
  azimuth.assert_eq(35, route_complexity)        // ceil(23/2) * 5 = 35
}