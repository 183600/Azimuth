// 简洁高质量测试用例 - 针对 azimuth 库核心功能的测试

test "temperature_conversion" {
  // 温度转换场景：摄氏度转华氏度 (F = C * 9/5 + 32)
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  assert_eq(77, fahrenheit)
}

test "shopping_cart_total" {
  // 购物车总价计算
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(103, total)
}

test "page_layout_calculation" {
  // 页面布局计算：计算需要的列数
  let total_elements = 17
  let elements_per_row = 4
  let rows_needed = divide_with_ceil(total_elements, elements_per_row)
  assert_eq(5, rows_needed)
}

test "batch_processing" {
  // 批处理场景：计算需要的批次数
  let total_records = 1000
  let batch_size = 75
  let batches_needed = divide_with_ceil(total_records, batch_size)
  assert_eq(14, batches_needed)
}

test "area_calculation" {
  // 面积计算：矩形房间需要多少块地砖
  let room_length = 12
  let room_width = 8
  let tile_area = 3
  let total_tiles = divide_with_ceil(multiply(room_length, room_width), tile_area)
  assert_eq(32, total_tiles)
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_time = 14
  let time_difference = 8
  let utc_time = add(local_time, -time_difference)
  assert_eq(6, utc_time)
}

test "compound_interest" {
  // 复利计算（简化版）
  let principal = 1000
  let rate = 5
  let years = 3
  // 简化计算：每年向上取整
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  assert_eq(1158, year3)
}

test "team_greeting" {
  // 团队问候场景
  let team_name = "开发团队"
  let project_name = "Azimuth"
  let greeting = greet(team_name + " - " + project_name)
  assert_eq_string("Hello, 开发团队 - Azimuth!", greeting)
}

test "inventory_restock" {
  // 库存补货计算
  let current_stock = 45
  let min_required = 120
  let package_size = 12
  let need_to_order = divide_with_ceil(add(min_required, -current_stock), package_size)
  assert_eq(7, need_to_order)
}

test "digital_storage" {
  // 数字存储计算：文件需要的存储块数
  let file_size_mb = 850
  let block_size_mb = 64
  let blocks_needed = divide_with_ceil(file_size_mb, block_size_mb)
  assert_eq(14, blocks_needed)
}