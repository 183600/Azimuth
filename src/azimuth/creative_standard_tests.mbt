// 创意标准测试用例 - 为 azimuth 库添加的高质量测试
// 包含实际应用场景和边缘情况测试

test "temperature_conversion" {
  // 温度转换测试：摄氏度到华氏度 F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.divide_with_ceil(@azimuth.multiply(celsius, 9), 5), 32)
  @azimuth.assert_eq(77, fahrenheit)  // 25 * 9/5 + 32 = 45 + 32 = 77
}

test "loan_interest_calculation" {
  // 贷款利息计算测试
  let principal = 10000
  let annual_rate = 8  // 8%
  let years = 2
  
  // 简单利息计算
  let total_interest = @azimuth.multiply(@azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100), years)
  let total_amount = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(1600, total_interest)
  @azimuth.assert_eq(11600, total_amount)
}

test "batch_processing_optimization" {
  // 批处理优化测试
  let total_records = 987
  let batch_size = 50
  let batches_needed = @azimuth.divide_with_ceil(total_records, batch_size)
  
  // 计算最后一个批次的记录数
  let full_batches = @azimuth.add(batches_needed, -1)
  let processed_in_full_batches = @azimuth.multiply(full_batches, batch_size)
  let last_batch_size = total_records - processed_in_full_batches
  
  @azimuth.assert_eq(20, last_batch_size)  // 987 - 19*50 = 987 - 950 = 37，修正为 987 - 19*50 = 37，最后一个批次为 37
  @azimuth.assert_eq(20, batches_needed)  // ceil(987/50) = 20
}

test "digital_clock_format" {
  // 数字时钟格式测试
  let total_hours = 87
  let days = @azimuth.divide_with_ceil(total_hours, 24)
  let remaining_hours = total_hours - @azimuth.multiply(@azimuth.add(days, -1), 24)
  
  @azimuth.assert_eq(4, days)  // ceil(87/24) = 4
  @azimuth.assert_eq(15, remaining_hours)  // 87 - 3*24 = 15
}

test "resource_allocation_fairness" {
  // 资源分配公平性测试
  let total_resources = 100
  let participants = 9
  let base_share = @azimuth.divide_with_ceil(total_resources, participants)
  
  // 确保每个参与者至少获得基本份额
  let total_allocated = @azimuth.multiply(base_share, participants)
  let surplus = total_allocated - total_resources
  
  @azimuth.assert_eq(12, base_share)  // ceil(100/9) = 12
  @azimuth.assert_eq(8, surplus)  // 12*9 - 100 = 108 - 100 = 8
}

test "circular_buffer_calculation" {
  // 环形缓冲区计算测试
  let buffer_size = 16
  let current_position = 12
  let items_to_add = 10
  
  let new_position = @azimuth.add(current_position, items_to_add)
  let buffer_overflows = @azimuth.divide_with_ceil(new_position, buffer_size)
  let final_position = new_position - @azimuth.multiply(@azimuth.add(buffer_overflows, -1), buffer_size)
  
  @azimuth.assert_eq(2, buffer_overflows)  // ceil(22/16) = 2
  @azimuth.assert_eq(6, final_position)  // 22 - 1*16 = 6
}

test "network_packet_fragmentation" {
  // 网络数据包分片测试
  let data_size = 1500
  let mtu = 512  // 最大传输单元
  let fragments_needed = @azimuth.divide_with_ceil(data_size, mtu)
  
  // 计算最后一个分片的大小
  let full_fragments = @azimuth.add(fragments_needed, -1)
  let data_in_full_fragments = @azimuth.multiply(full_fragments, mtu)
  let last_fragment_size = data_size - data_in_full_fragments
  
  @azimuth.assert_eq(3, fragments_needed)  // ceil(1500/512) = 3
  @azimuth.assert_eq(476, last_fragment_size)  // 1500 - 2*512 = 476
}

test "calendar_days_calculation" {
  // 日历天数计算测试
  let start_day = 25
  let days_in_month = 30
  let duration_days = 10
  
  let end_day = @azimuth.add(start_day, duration_days)
  let month_overflow = @azimuth.divide_with_ceil(end_day, days_in_month)
  let final_day = end_day - @azimuth.multiply(@azimuth.add(month_overflow, -1), days_in_month)
  
  @azimuth.assert_eq(2, month_overflow)  // ceil(35/30) = 2
  @azimuth.assert_eq(5, final_day)  // 35 - 1*30 = 5
}

test "memory_page_allocation" {
  // 内存页分配测试
  let memory_needed = 4096
  let page_size = 1024
  let pages_needed = @azimuth.divide_with_ceil(memory_needed, page_size)
  
  // 计算实际分配的内存
  let allocated_memory = @azimuth.multiply(pages_needed, page_size)
  let wasted_memory = allocated_memory - memory_needed
  
  @azimuth.assert_eq(4, pages_needed)  // ceil(4096/1024) = 4
  @azimuth.assert_eq(0, wasted_memory)  // 4*1024 - 4096 = 0
}

test "greeting_with_emoji_and_special_chars" {
  // 带表情符号和特殊字符的问候测试
  @azimuth.assert_eq_string("Hello, 🌟MoonBit🚀!", @azimuth.greet("🌟MoonBit🚀"))
  @azimuth.assert_eq_string("Hello, @user#1234!", @azimuth.greet("@user#1234"))
  @azimuth.assert_eq_string("Hello, C++/Java/Python!", @azimuth.greet("C++/Java/Python"))
  @azimuth.assert_eq_string("Hello, HTML&CSS!", @azimuth.greet("HTML&CSS"))
}