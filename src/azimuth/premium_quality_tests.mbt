// 高质量 MoonBit 测试用例 - 针对 azimuth 库
// 使用标准的 MoonBit 测试语法，包含实用场景和边界情况测试

test "statistical_analysis_mean_calculation" {
  // 统计分析：计算平均数
  let data1 = 85
  let data2 = 92
  let data3 = 78
  let data4 = 96
  let data5 = 89
  
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  let mean = divide_with_ceil(sum, 5)
  
  assert_eq(440, sum)
  assert_eq(88, mean)
}

test "cryptographic_hash_simulation" {
  // 模拟简单哈希算法
  let input_value = 12345
  let prime_multiplier = 31
  
  // 简化的哈希计算
  let hash_value = multiply(input_value, prime_multiplier)
  
  assert_eq(382695, hash_value)
}

test "game_physics_collision_detection" {
  // 游戏物理：碰撞检测计算
  let object1_x = 100
  let object1_y = 150
  let object1_width = 50
  let object1_height = 30
  
  let object2_x = 140
  let object2_y = 160
  let object2_width = 40
  let object2_height = 25
  
  // 计算边界
  let obj1_right = add(object1_x, object1_width)
  let obj1_bottom = add(object1_y, object1_height)
  let obj2_right = add(object2_x, object2_width)
  let obj2_bottom = add(object2_y, object2_height)
  
  assert_eq(150, obj1_right)
  assert_eq(180, obj1_bottom)
  assert_eq(180, obj2_right)
  assert_eq(185, obj2_bottom)
}

test "image_processing_pixel_manipulation" {
  // 图像处理：像素操作计算
  let image_width = 1920
  let image_height = 1080
  let bytes_per_pixel = 4  // RGBA
  
  // 计算图像总字节数
  let total_pixels = multiply(image_width, image_height)
  let total_bytes = multiply(total_pixels, bytes_per_pixel)
  
  // 计算需要的内存块数（每块1024字节）
  let memory_blocks = divide_with_ceil(total_bytes, 1024)
  
  assert_eq(2073600, total_pixels)
  assert_eq(8294400, total_bytes)
  assert_eq(8100, memory_blocks)
}

test "network_protocol_packet_calculation" {
  // 网络协议：数据包计算
  let header_size = 20
  let max_payload_size = 1460
  let total_data_size = 10000
  
  // 计算需要的数据包数量
  let packets_needed = divide_with_ceil(total_data_size, max_payload_size)
  
  // 计算总传输字节数（包含头部）
  let total_transferred = add(multiply(packets_needed, header_size), total_data_size)
  
  assert_eq(7, packets_needed)  // ceil(10000/1460) = 7
  assert_eq(10140, total_transferred)  // 7*20 + 10000 = 140 + 10000
}

test "financial_compound_interest_with_fees" {
  // 金融计算：包含费用的复利
  let initial_investment = 5000
  let annual_rate = 8  // 8%
  let management_fee_percent = 1  // 1%
  
  // 第一年
  let year1_gross = add(initial_investment, divide_with_ceil(multiply(initial_investment, annual_rate), 100))
  let year1_fee = divide_with_ceil(multiply(year1_gross, management_fee_percent), 100)
  let year1_net = add(year1_gross, -year1_fee)
  
  // 第二年
  let year2_gross = add(year1_net, divide_with_ceil(multiply(year1_net, annual_rate), 100))
  let year2_fee = divide_with_ceil(multiply(year2_gross, management_fee_percent), 100)
  let year2_net = add(year2_gross, -year2_fee)
  
  // 第三年
  let year3_gross = add(year2_net, divide_with_ceil(multiply(year2_net, annual_rate), 100))
  let year3_fee = divide_with_ceil(multiply(year3_gross, management_fee_percent), 100)
  let year3_net = add(year3_gross, -year3_fee)
  
  assert_eq(5400, year1_gross)
  assert_eq(54, year1_fee)
  assert_eq(5346, year1_net)
  assert_eq(5774, year2_gross)
  assert_eq(58, year2_fee)
  assert_eq(5716, year2_net)
  assert_eq(6173, year3_gross)
  assert_eq(62, year3_fee)
  assert_eq(6111, year3_net)
}

test "machine_learning_batch_processing" {
  // 机器学习：批处理计算
  let batch_size = 32
  let total_samples = 1000
  let epochs = 10
  
  // 计算每epoch的批次数
  let batches_per_epoch = divide_with_ceil(total_samples, batch_size)
  
  // 计算总批次数
  let total_batches = multiply(batches_per_epoch, epochs)
  
  assert_eq(32, batches_per_epoch)  // ceil(1000/32) = 32
  assert_eq(320, total_batches)     // 32 * 10 = 320
}

test "database_index_optimization" {
  // 数据库：索引优化计算
  let table_rows = 1000000
  let index_size_per_row = 8  // 每行索引8字节
  let page_size = 8192  // 8KB页面
  
  // 计算索引总大小
  let total_index_size = multiply(table_rows, index_size_per_row)
  
  // 计算需要的页面数
  let pages_needed = divide_with_ceil(total_index_size, page_size)
  
  // 计算索引级别（B树近似）
  let fan_out = 100  // 每个节点100个子节点
  let level1_pages = divide_with_ceil(pages_needed, fan_out)
  let level2_pages = divide_with_ceil(level1_pages, fan_out)
  let total_index_levels = add(level2_pages, add(level1_pages, pages_needed))
  
  assert_eq(8000000, total_index_size)
  assert_eq(977, pages_needed)  // ceil(8000000/8192) = 977
  assert_eq(10, level1_pages)   // ceil(977/100) = 10
  assert_eq(1, level2_pages)    // ceil(10/100) = 1
  assert_eq(988, total_index_levels)  // 1 + 10 + 977 = 988
}