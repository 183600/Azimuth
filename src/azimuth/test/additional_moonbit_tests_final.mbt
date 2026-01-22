// Azimuth 项目额外的 MoonBit 测试用例
// 包含10个新的测试场景，覆盖更多应用场景

test "number_system_conversion" {
  // 测试数字系统转换：二进制转十进制（简化版）
  // 二进制数 1011 = 1×2³ + 0×2² + 1×2¹ + 1×2⁰ = 8 + 0 + 2 + 1 = 11
  
  // 模拟二进制转十进制
  let bit3 = 1
  let bit2 = 0
  let bit1 = 1
  let bit0 = 1
  
  let decimal_value = @azimuth.add(@azimuth.add(
    @azimuth.multiply(bit3, 8),  // 1×8 = 8
    @azimuth.multiply(bit2, 4)   // 0×4 = 0
  ), @azimuth.add(
    @azimuth.multiply(bit1, 2),  // 1×2 = 2
    @azimuth.multiply(bit0, 1)   // 1×1 = 1
  ))
  
  @azimuth.assert_eq(11, decimal_value)
  
  // 测试另一个二进制数：1101 = 1×8 + 1×4 + 0×2 + 1×1 = 13
  let binary_1101 = @azimuth.add(@azimuth.add(
    @azimuth.multiply(1, 8),  // 8
    @azimuth.multiply(1, 4)   // 4
  ), @azimuth.add(
    @azimuth.multiply(0, 2),  // 0
    @azimuth.multiply(1, 1)   // 1
  ))
  
  @azimuth.assert_eq(13, binary_1101)
}

test "velocity_distance_calculation" {
  // �物理学计算：速度、距离和时间
  let initial_velocity = 10  // 初始速度 m/s
  let acceleration = 2       // 加速度 m/s²
  let time = 5               // 时间 s
  
  // 计算最终速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  @azimuth.assert_eq(20, final_velocity)
  
  // 计算距离：s = ut + 0.5×at²（简化为整数计算）
  let ut_distance = @azimuth.multiply(initial_velocity, time)  // 10×5 = 50
  let at_squared = @azimuth.multiply(acceleration, @azimuth.multiply(time, time))  // 2×25 = 50
  let total_distance = @azimuth.add(ut_distance, at_squared / 2)  // 50 + 25 = 75
  
  @azimuth.assert_eq(75, total_distance)
  
  // 计算平均速度：(初始速度 + 最终速度) / 2
  let average_velocity = @azimuth.add(initial_velocity, final_velocity) / 2
  @azimuth.assert_eq(15, average_velocity)
}

test "array_manipulation_operations" {
  // 测试数组操作模拟（使用单个变量表示数组索引操作）
  let array_size = 10
  let index = 3
  let value = 42
  
  // 模拟数组访问：计算内存地址
  let base_address = 1000
  let element_size = 4
  let memory_address = @azimuth.add(base_address, @azimuth.multiply(index, element_size))
  @azimuth.assert_eq(1012, memory_address)
  
  // 模拟数组边界检查
  let is_valid_index = index < array_size
  @azimuth.assert_eq(is_valid_index, true)
  
  // 测试越界情况
  let invalid_index = 15
  let is_invalid_index = invalid_index < array_size
  @azimuth.assert_eq(0, 1)(is_invalid_index)
  
  // 模拟数组元素求和：sum = a[0] + a[1] + ... + a[4]
  let sum_first_5 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  @azimuth.assert_eq(150, sum_first_5)
}

test "cryptographic_simple_hash" {
  // 测试简单的哈希函数模拟
  let input = 12345
  
  // 简单的哈希算法：hash = (input × prime1 + prime2) % mod
  let prime1 = 31
  let prime2 = 17
  let mod = 1000
  
  let hash_value = (@azimuth.add(@azimuth.multiply(input, prime1), prime2)) % mod
  @azimuth.assert_eq(482, hash_value)  // (12345×31 + 17) % 1000 = 482
  
  // 测试另一个输入
  let input2 = 67890
  let hash_value2 = (@azimuth.add(@azimuth.multiply(input2, prime1), prime2)) % mod
  @azimuth.assert_eq(527, hash_value2)  // (67890×31 + 17) % 1000 = 527
  
  // 验证哈希值在范围内
  @azimuth.assert_eq(true, hash_value >= 0 && hash_value < mod)
  @azimuth.assert_eq(true, hash_value2 >= 0 && hash_value2 < mod)
}

test "compression_ratio_calculation" {
  // 测试压缩比计算
  let original_size = 10240  // 原始大小 10KB
  let compressed_size = 3072  // 压缩后大小 3KB
  
  // 计算压缩比：(原始大小 - 压缩后大小) / 原始大小 × 100%
  let size_difference = @azimuth.add(original_size, -compressed_size)  // 7168
  let compression_ratio = @azimuth.multiply(size_difference, 100) / original_size
  
  @azimuth.assert_eq(70, compression_ratio)  // 7168/10240×100 = 70%
  
  // 计算空间节省：原始大小 - 压缩后大小
  @azimuth.assert_eq(7168, size_difference)
  
  // 测试另一个压缩场景
  let original_size2 = 5000
  let compressed_size2 = 2000
  let compression_ratio2 = @azimuth.multiply(@azimuth.add(original_size2, -compressed_size2), 100) / original_size2
  
  @azimuth.assert_eq(60, compression_ratio2)  // 3000/5000×100 = 60%
}

test "network_bandwidth_calculation" {
  // 测试网络带宽计算
  let file_size = 1048576  // 1MB = 1024×1024 bytes
  let bandwidth = 1048576  // 1MB/s = 1024×1024 bytes/s
  
  // 计算传输时间：文件大小 / 带宽
  let transfer_time = file_size / bandwidth
  @azimuth.assert_eq(1, transfer_time)  // 1秒
  
  // 计算不同带宽下的传输时间
  let bandwidth_slow = 262144  // 256KB/s
  let transfer_time_slow = file_size / bandwidth_slow
  @azimuth.assert_eq(4, transfer_time_slow)  // 4秒
  
  // 计算网络吞吐量：传输的数据量 / 时间
  let data_transferred = 5242880  // 5MB
  let transmission_time = 10  // 10秒
  let throughput = data_transferred / transmission_time
  @azimuth.assert_eq(524288, throughput)  // 524288 bytes/s
}

test "database_pagination_calculation" {
  // 测试数据库分页计算
  let total_records = 1000
  let page_size = 25
  
  // 计算总页数：(总记录数 + 页面大小 - 1) / 页面大小
  let total_pages = @azimuth.add(@azimuth.add(total_records, page_size), -1) / page_size
  @azimuth.assert_eq(40, total_pages)
  
  // 计算特定页的偏移量：(页码 - 1) × 页面大小
  let page_number = 3
  let offset = @azimuth.multiply(@azimuth.add(page_number, -1), page_size)
  @azimuth.assert_eq(50, offset)  // (3-1)×25 = 50
  
  // 计算最后一页的记录数
  let last_page_offset = @azimuth.multiply(@azimuth.add(total_pages, -1), page_size)
  let last_page_records = @azimuth.add(total_records, -last_page_offset)
  @azimuth.assert_eq(25, last_page_records)
  
  // 测试记录数不能整除页面大小的情况
  let total_records2 = 1023
  let total_pages2 = @azimuth.add(@azimuth.add(total_records2, page_size), -1) / page_size
  @azimuth.assert_eq(41, total_pages2)
}

test "color_rgb_conversion" {
  // 测试RGB颜色转换
  // RGB(255, 0, 0) = 红色
  let red = 255
  let green = 0
  let blue = 0
  
  // 转换为十六进制（简化计算）
  let hex_red = @azimuth.multiply(red, 65536)  // 255×65536 = 16711680
  let hex_green = @azimuth.multiply(green, 256)  // 0×256 = 0
  let hex_blue = blue  // 0
  let hex_color = @azimuth.add(@azimuth.add(hex_red, hex_green), hex_blue)
  
  @azimuth.assert_eq(16711680, hex_color)
  
  // 测试另一个颜色：RGB(0, 128, 255) = 蓝绿色
  let red2 = 0
  let green2 = 128
  let blue2 = 255
  
  let hex_red2 = @azimuth.multiply(red2, 65536)  // 0
  let hex_green2 = @azimuth.multiply(green2, 256)  // 128×256 = 32768
  let hex_blue2 = blue2  // 255
  let hex_color2 = @azimuth.add(@azimuth.add(hex_red2, hex_green2), hex_blue2)
  
  @azimuth.assert_eq(33023, hex_color2)
  
  // 计算颜色亮度：(R + G + B) / 3
  let brightness = @azimuth.add(@azimuth.add(red2, green2), blue2) / 3
  @azimuth.assert_eq(127, brightness)  // (0+128+255)/3 = 127
}

test "algorithm_binary_search_simulation" {
  // 测试二分查找算法模拟
  let sorted_array_size = 100
  let target = 42
  
  // 模拟二分查找的步骤
  let left = 0
  let right = @azimuth.add(sorted_array_size, -1)  // 99
  let mid = @azimuth.add(left, right) / 2  // 49
  
  // 第一次比较：target < mid
  let first_comparison = target < mid
  @azimuth.assert_eq(first_comparison, true)
  
  // 更新右边界
  let new_right = @azimuth.add(mid, -1)  // 48
  let new_mid = @azimuth.add(left, new_right) / 2  // 24
  
  // 第二次比较：target > new_mid
  let second_comparison = target > new_mid
  @azimuth.assert_eq(second_comparison, true)
  
  // 更新左边界
  let new_left = @azimuth.add(new_mid, 1)  // 25
  let final_mid = @azimuth.add(new_left, new_right) / 2  // 36
  
  // 第三次比较：target > final_mid
  let third_comparison = target > final_mid
  @azimuth.assert_eq(third_comparison, true)
  
  // 计算查找步数（简化）
  let max_steps = 10  // log₂(100) ≈ 6.6，向上取整为7，留余量
  @azimuth.assert_eq(true, 3 < max_steps)
}

test "game_inventory_management" {
  // 测试游戏物品栏管理
  let max_inventory_size = 20
  let current_items = 15
  let new_items = 3
  
  // 计算是否能添加新物品
  let available_space = @azimuth.add(max_inventory_size, -current_items)
  let can_add_items = new_items <= available_space
  @azimuth.assert_eq(can_add_items, true)
  
  // 更新物品数量
  let updated_items = @azimuth.add(current_items, new_items)
  @azimuth.assert_eq(18, updated_items)
  
  // 计算物品栏使用率
  let usage_percentage = @azimuth.multiply(updated_items, 100) / max_inventory_size
  @azimuth.assert_eq(90, usage_percentage)
  
  // 测试物品堆叠计算
  let stack_size = 64
  let item_count = 150
  let full_stacks = item_count / stack_size  // 2
  let remaining_items = item_count % stack_size  // 22
  
  @azimuth.assert_eq(2, full_stacks)
  @azimuth.assert_eq(22, remaining_items)
  
  // 计算所需槽位
  let required_slots = @azimuth.add(full_stacks, if (remaining_items > 0) { 1 } else { 0 })
  @azimuth.assert_eq(3, required_slots)
}