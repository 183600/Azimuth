// 新增的标准 MoonBit 测试用例
// 为 azimuth 库添加的实用测试场景

test "calendar_date_calculation" {
  // 日历日期计算：计算给定天数内的完整周数和剩余天数
  let total_days = 45
  let days_per_week = 7
  
  let complete_weeks = divide_with_ceil(total_days, days_per_week)
  let remaining_days = add(total_days, -multiply(complete_weeks - 1, days_per_week))
  
  assert_eq(7, complete_weeks)  // ceil(45/7) = 7
  assert_eq(3, remaining_days)  // 45 - 6*7 = 3
}

test "batch_processing_optimization" {
  // 批处理优化：计算最优批次大小
  let total_items = 234
  let max_batch_size = 50
  
  let batch_count = divide_with_ceil(total_items, max_batch_size)
  let last_batch_size = add(total_items, -multiply(batch_count - 1, max_batch_size))
  
  assert_eq(5, batch_count)  // ceil(234/50) = 5
  assert_eq(34, last_batch_size)  // 234 - 4*50 = 34
}

test "resource_allocation_fairness" {
  // 资源分配公平性测试
  let total_resources = 100
  let participants = 9
  
  let base_allocation = divide_with_ceil(total_resources, participants)
  let total_allocated = multiply(base_allocation, participants)
  let surplus = add(total_allocated, -total_resources)
  
  assert_eq(12, base_allocation)  // ceil(100/9) = 12
  assert_eq(108, total_allocated)  // 12 * 9 = 108
  assert_eq(8, surplus)  // 108 - 100 = 8
}

test "temperature_conversion_validation" {
  // 温度转换验证（简化版本）
  let celsius = 25
  let fahrenheit_approx = add(multiply(celsius, 9), 160) / 5
  
  // 验证转换公式的基本性质
  let freezing_c = 0
  let freezing_f = add(multiply(freezing_c, 9), 160) / 5
  let boiling_c = 100
  let boiling_f = add(multiply(boiling_c, 9), 160) / 5
  
  assert_eq(77, fahrenheit_approx)  // (25*9 + 160)/5 = 77
  assert_eq(32, freezing_f)  // (0*9 + 160)/5 = 32
  assert_eq(212, boiling_f)  // (100*9 + 160)/5 = 212
}

test "digital_storage_calculation" {
  // 数字存储计算：文件大小转换
  let file_size_mb = 2048
  let mb_per_gb = 1024
  
  let size_gb = divide_with_ceil(file_size_mb, mb_per_gb)
  let remainder_mb = add(file_size_mb, -multiply(size_gb - 1, mb_per_gb))
  
  assert_eq(2, size_gb)  // ceil(2048/1024) = 2
  assert_eq(1024, remainder_mb)  // 2048 - 1*1024 = 1024
}

test "network_packet_fragmentation" {
  // 网络数据包分片计算
  let data_size = 1500
  let mtu = 500  // Maximum Transmission Unit
  
  let fragment_count = divide_with_ceil(data_size, mtu)
  let last_fragment_size = add(data_size, -multiply(fragment_count - 1, mtu))
  
  assert_eq(3, fragment_count)  // ceil(1500/500) = 3
  assert_eq(500, last_fragment_size)  // 1500 - 2*500 = 500
}

test "construction_material_estimation" {
  // 建筑材料估算
  let wall_area = 120
  let tile_coverage = 15  // 每块瓷砖覆盖面积
  
  let tiles_needed = divide_with_ceil(wall_area, tile_coverage)
  let total_coverage = multiply(tiles_needed, tile_coverage)
  let waste_percentage = divide_with_ceil(multiply(add(total_coverage, -wall_area), 100), wall_area)
  
  assert_eq(8, tiles_needed)  // ceil(120/15) = 8
  assert_eq(120, total_coverage)  // 8 * 15 = 120
  assert_eq(0, waste_percentage)  // 无浪费
}

test "cryptography_key_generation" {
  // 密码学密钥生成模拟
  let base_key = 12345
  let multiplier = 31
  let modulo = 1000003  // 大质数
  
  let derived_key = multiply(base_key, multiplier) % modulo
  let key_validation = multiply(derived_key, 7) % 100
  
  assert_eq(382695, derived_key)  // (12345 * 31) % 1000003 = 382695
  assert_eq(65, key_validation)  // (382695 * 7) % 100 = 65
}

test "agricultural_planning" {
  // 农业规划：作物轮作计算
  let total_field_area = 500
  let crop_types = 4
  let fallow_area = 50  // 休耕面积
  
  let usable_area = add(total_field_area, -fallow_area)
  let area_per_crop = divide_with_ceil(usable_area, crop_types)
  let total_allocated = multiply(area_per_crop, crop_types)
  let remaining_area = add(total_allocated, -usable_area)
  
  assert_eq(113, area_per_crop)  // ceil(450/4) = 113
  assert_eq(452, total_allocated)  // 113 * 4 = 452
  assert_eq(2, remaining_area)  // 452 - 450 = 2
}

test "greeting_multilingual_support" {
  // 多语言问候支持测试
  let english_name = "World"
  let chinese_name = "世界"
  let spanish_name = "Mundo"
  let emoji_name = "🌍"
  
  let english_greeting = greet(english_name)
  let chinese_greeting = greet(chinese_name)
  let spanish_greeting = greet(spanish_name)
  let emoji_greeting = greet(emoji_name)
  
  assert_eq_string("Hello, World!", english_greeting)
  assert_eq_string("Hello, 世界!", chinese_greeting)
  assert_eq_string("Hello, Mundo!", spanish_greeting)
  assert_eq_string("Hello, 🌍!", emoji_greeting)
}