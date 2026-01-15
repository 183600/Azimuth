// 额外的 MoonBit 测试用例
// 针对 azimuth 包的高级功能和边界条件测试

// 测试 divide_with_ceil 函数
test "divide_with_ceil_basic" {
  // 基本向上取整除法测试
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))  // 10/4 = 2.5 → 3
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(12, 4))  // 12/4 = 3 → 3
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))   // 0/5 = 0 → 0
}

test "divide_with_ceil_negative" {
  // 负数向上取整除法测试
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-10, 4))  // -10/4 = -2.5 → -2
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -4))  // 10/-4 = -2.5 → -2
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -4))  // -10/-4 = 2.5 → 3
}

test "complex_calculations_with_ceil" {
  // 包含向上取整除法的复杂计算
  // 计算需要的容器数量：总物品数 / 每个容器容量（向上取整）
  let total_items = 23
  let container_capacity = 5
  let containers_needed = @azimuth.divide_with_ceil(total_items, container_capacity)
  @azimuth.assert_eq(5, containers_needed)  // 23/5 = 4.6 → 5个容器
}

test "temperature_conversion_advanced" {
  // 高级温度转换：精确的摄氏度转华氏度
  // F = C × 9/5 + 32，使用整数运算近似
  let celsius = 25
  // 近似计算：C × 2 - C/10 + 32
  let approx_fahrenheit = @azimuth.add(@azimuth.add(@azimuth.multiply(celsius, 2), @azimuth.multiply(celsius, -1) / 10), 32)
  @azimuth.assert_eq(77, approx_fahrenheit)  // 25 × 2 - 2 + 32 = 77
}

test "page_calculation" {
  // 分页计算测试
  let total_items = 95
  let items_per_page = 10
  
  // 计算总页数（向上取整）
  let total_pages = @azimuth.divide_with_ceil(total_items, items_per_page)
  @azimuth.assert_eq(10, total_pages)  // 95/10 = 9.5 → 10页
  
  // 计算最后一页的项目数
  let last_page_items = total_items - @azimuth.multiply(items_per_page, total_pages - 1)
  @azimuth.assert_eq(5, last_page_items)
}

test "batch_processing" {
  // 批处理计算测试
  let total_records = 1234
  let batch_size = 100
  
  // 计算需要的批次数（向上取整）
  let batches_needed = @azimuth.divide_with_ceil(total_records, batch_size)
  @azimuth.assert_eq(13, batches_needed)  // 1234/100 = 12.34 → 13批次
  
  // 计算最后一批的记录数
  let last_batch_size = total_records - @azimuth.multiply(batch_size, batches_needed - 1)
  @azimuth.assert_eq(34, last_batch_size)
}

test "resource_allocation" {
  // 资源分配测试
  let people = 17
  let rooms = 4
  
  // 计算每个房间的人数（向上取整，确保所有人都有位置）
  let people_per_room = @azimuth.divide_with_ceil(people, rooms)
  @azimuth.assert_eq(5, people_per_room)  // 17/4 = 4.25 → 5人/房间
  
  // 验证总容量足够
  let total_capacity = @azimuth.multiply(people_per_room, rooms)
  @azimuth.assert_true(total_capacity >= people)
}

test "time_slot_scheduling" {
  // 时间槽调度测试
  let total_minutes = 125
  let slot_duration = 30
  
  // 计算需要的时间槽数量（向上取整）
  let slots_needed = @azimuth.divide_with_ceil(total_minutes, slot_duration)
  @azimuth.assert_eq(5, slots_needed)  // 125/30 = 4.17 → 5个时间槽
  
  // 计算最后一个时间槽的剩余时间
  let last_slot_time = total_minutes - @azimuth.multiply(slot_duration, slots_needed - 1)
  @azimuth.assert_eq(5, last_slot_time)
}