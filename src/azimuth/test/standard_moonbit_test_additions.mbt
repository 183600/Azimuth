// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_with_negative_numbers" {
  // 测试使用负数的加法功能（模拟减法）
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-5, @azimuth.add(0, -5))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
}

test "complex_calculator_scenario" {
  // 复杂计算器场景测试
  let initial_value = 100
  let operations = @azimuth.add(@azimuth.multiply(5, 10), @azimuth.add(50, -25))
  let result = @azimuth.add(@azimuth.add(initial_value, operations), -75)
  
  @azimuth.assert_eq(150, result)  // 100 + (5*10 + (50-25)) - 75 = 100 + (50 + 25) - 75 = 150
}

test "temperature_conversion" {
  // 温度转换场景：摄氏度转华氏度
  let celsius = 25
  let fahrenheit = @azimuth.divide_with_ceil(@azimuth.add(@azimuth.multiply(celsius, 9), 160), 5)
  
  @azimuth.assert_eq(77, fahrenheit)  // ceil((25 * 9 + 160) / 5) = ceil(385 / 5) = 77
}

test "banking_interest_calculation" {
  // 银行利息计算
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  let interest_year1 = @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100)
  let interest_year2 = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(principal, interest_year1), rate_percent), 100)
  let interest_year3 = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(@azimuth.add(principal, interest_year1), interest_year2), rate_percent), 100)
  
  let total_amount = @azimuth.add(@azimuth.add(@azimuth.add(principal, interest_year1), interest_year2), interest_year3)
  
  @azimuth.assert_eq(500, interest_year1)
  @azimuth.assert_eq(525, interest_year2)
  @azimuth.assert_eq(551, interest_year3)
  @azimuth.assert_eq(11576, total_amount)
}

test "array_index_calculation" {
  // 数组索引计算模拟
  let page_size = 20
  let page_number = 3
  let item_offset = 5
  
  let start_index = @azimuth.add(@azimuth.multiply(page_number - 1, page_size), item_offset)
  
  @azimuth.assert_eq(45, start_index)  // (3-1)*20 + 5 = 40 + 5 = 45
}

test "resource_utilization" {
  // 资源利用率计算
  let total_resources = 1000
  let used_resources = 750
  let planned_resources = 200
  
  let utilization_after_planning = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(used_resources, planned_resources), 100), total_resources)
  
  @azimuth.assert_eq(95, utilization_after_planning)  // ceil((750+200)*100/1000) = ceil(95000/1000) = 95
}

test "game_score_calculation" {
  // 游戏分数计算
  let base_score = 1000
  let level_multiplier = 3
  let bonus_points = 500
  let penalty_points = 200
  
  let final_score = @azimuth.add(@azimuth.add(@azimuth.multiply(base_score, level_multiplier), bonus_points), -penalty_points)
  
  @azimuth.assert_eq(3300, final_score)  // 1000*3 + 500 - 200 = 3000 + 500 - 200 = 3300
}

test "data_structure_capacity" {
  // 数据结构容量计算
  let item_size = 24
  let total_items = 150
  let block_size = 1024
  
  let total_memory_needed = @azimuth.multiply(item_size, total_items)
  let blocks_needed = @azimuth.divide_with_ceil(total_memory_needed, block_size)
  
  @azimuth.assert_eq(3600, total_memory_needed)
  @azimuth.assert_eq(4, blocks_needed)  // ceil(3600/1024) = 4
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = -8  // 与目标时区的时差
  
  let target_time = @azimuth.add(local_time, time_difference)
  let adjusted_time = if target_time < 0 { @azimuth.add(target_time, 24) } else { target_time }
  
  @azimuth.assert_eq(6, adjusted_time)  // 14 + (-8) = 6
}

test "batch_processing_efficiency" {
  // 批处理效率计算
  let single_item_time = 5
  let batch_size = 50
  let batch_overhead = 100
  
  let sequential_time = @azimuth.multiply(single_item_time, batch_size)
  let batch_time = @azimuth.add(batch_overhead, @azimuth.multiply(single_item_time, @azimuth.divide_with_ceil(batch_size, 10)))
  let efficiency_gain = @azimuth.add(sequential_time, -batch_time)
  
  @azimuth.assert_eq(250, sequential_time)
  @azimuth.assert_eq(125, batch_time)
  @azimuth.assert_eq(125, efficiency_gain)
}