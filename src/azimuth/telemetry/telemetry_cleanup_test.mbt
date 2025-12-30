// 遥测数据清理测试用例

test "telemetry_time_based_cleanup" {
  // 测试基于时间的数据清理
  
  let current_time = 1701423045000  // 2023-12-01 10:30:45 UTC
  let retention_days = 7           // 保留7天的数据
  let retention_ms = retention_days * 24 * 60 * 60 * 1000
  let cutoff_time = current_time - retention_ms
  
  let telemetry_data = [
    (1701326645000, "old_data_1"),    // 9天前，应该被清理
    (1701413045000, "old_data_2"),    // 1天前，应该保留
    (1701423045000, "current_data"),  // 当前时间，应该保留
    (1701330245000, "old_data_3"),    // 8天前，应该被清理
    (1701423044000, "recent_data")    // 1秒前，应该保留
  ]
  
  // 清理过期数据
  let cleaned_data = []
  
  let mut i = 0
  while i < telemetry_data.length() {
    let timestamp = telemetry_data[i].0
    let data = telemetry_data[i].1
    
    if timestamp >= cutoff_time {
      cleaned_data.push((timestamp, data))
    }
    
    i = i + 1
  }
  
  // 验证时间清理结果
  assert_eq(cleaned_data.length(), 3)  // 应该保留3条数据
  
  // 验证保留的数据
  assert_eq(cleaned_data[0], (1701413045000, "old_data_2"))    // 1天前
  assert_eq(cleaned_data[1], (1701423045000, "current_data"))  // 当前
  assert_eq(cleaned_data[2], (1701423044000, "recent_data"))   // 1秒前
  
  // 验证被清理的数据
  let mut found_old1 = false
  let mut found_old3 = false
  i = 0
  while i < cleaned_data.length() {
    if cleaned_data[i].1 == "old_data_1" {
      found_old1 = true
    }
    if cleaned_data[i].1 == "old_data_3" {
      found_old3 = true
    }
    i = i + 1
  }
  assert_eq(found_old1, false)  // old_data_1应该被清理
  assert_eq(found_old3, false)  // old_data_3应该被清理
}

test "telemetry_size_based_cleanup" {
  // 测试基于大小的数据清理
  
  let max_storage_size = 1000  // 最大存储1000个单位
  let telemetry_storage = [
    ("trace_001", 200),
    ("trace_002", 150),
    ("trace_003", 300),
    ("trace_004", 100),
    ("trace_005", 250),
    ("trace_006", 180),
    ("trace_007", 120),
    ("trace_008", 220)
  ]
  
  // 计算总存储大小
  let mut total_size = 0
  let mut i = 0
  while i < telemetry_storage.length() {
    total_size = total_size + telemetry_storage[i].1
    i = i + 1
  }
  
  // 如果超过最大大小，清理最旧的数据
  let cleaned_storage = []
  let mut current_size = 0
  
  // 从最新数据开始保留
  i = telemetry_storage.length() - 1
  while i >= 0 {
    let trace_id = telemetry_storage[i].0
    let size = telemetry_storage[i].1
    
    if current_size + size <= max_storage_size {
      cleaned_storage.push((trace_id, size))
      current_size = current_size + size
    }
    
    i = i - 1
  }
  
  // 反转数组以保持时间顺序
  cleaned_storage.reverse()
  
  // 验证大小清理结果
  assert_eq(current_size <= max_storage_size, true)
  
  // 计算清理后的总大小
  let mut cleaned_total = 0
  i = 0
  while i < cleaned_storage.length() {
    cleaned_total = cleaned_total + cleaned_storage[i].1
    i = i + 1
  }
  assert_eq(cleaned_total <= max_storage_size, true)
}

test "telemetry_duplicate_cleanup" {
  // 测试重复数据清理
  
  let telemetry_events = [
    ("event_001", "data_A"),
    ("event_002", "data_B"),
    ("event_003", "data_A"),  // 重复数据
    ("event_004", "data_C"),
    ("event_005", "data_B"),  // 重复数据
    ("event_006", "data_D"),
    ("event_007", "data_A"),  // 重复数据
    ("event_008", "data_E")
  ]
  
  // 清理重复数据，保留第一次出现的记录
  let seen_data = {}
  let deduplicated_events = []
  
  let mut i = 0
  while i < telemetry_events.length() {
    let event_id = telemetry_events[i].0
    let data = telemetry_events[i].1
    
    if !seen_data.contains(data) {
      deduplicated_events.push((event_id, data))
      seen_data[data] = true
    }
    
    i = i + 1
  }
  
  // 验证重复清理结果
  assert_eq(deduplicated_events.length(), 5)  // 原始8个，去重后5个
  assert_eq(deduplicated_events[0], ("event_001", "data_A"))  // 第一次出现的data_A
  assert_eq(deduplicated_events[1], ("event_002", "data_B"))  // 第一次出现的data_B
  assert_eq(deduplicated_events[2], ("event_004", "data_C"))
  assert_eq(deduplicated_events[3], ("event_006", "data_D"))
  assert_eq(deduplicated_events[4], ("event_008", "data_E"))
  
  // 验证重复数据被清理
  let mut found_duplicate_A = false
  let mut found_duplicate_B = false
  i = 0
  while i < deduplicated_events.length() {
    if deduplicated_events[i].0 == "event_003" {
      found_duplicate_A = true
    }
    if deduplicated_events[i].0 == "event_005" {
      found_duplicate_B = true
    }
    i = i + 1
  }
  assert_eq(found_duplicate_A, false)  // event_003应该被清理
  assert_eq(found_duplicate_B, false)  // event_005应该被清理
}

test "telemetry_invalid_data_cleanup" {
  // 测试无效数据清理
  
  let raw_telemetry_data = [
    ("metric_001", 75.5),      // 有效数据
    ("metric_002", -5.2),      // 负值，可能无效
    ("metric_003", 125.8),     // 有效数据
    ("", 45.7),                // 空键名，无效
    ("metric_004", 0.0),       // 零值，可能无效
    ("metric_005", 999999.9),  // 异常大值，可能无效
    ("metric_006", 82.3),      // 有效数据
    ("metric_007", -1.0)       // 负值，可能无效
  ]
  
  // 清理无效数据的标准
  let valid_data = []
  
  let mut i = 0
  while i < raw_telemetry_data.length() {
    let key = raw_telemetry_data[i].0
    let value = raw_telemetry_data[i].1
    
    let is_valid = true
    
    // 检查键名是否为空
    if key.length() == 0 {
      is_valid = false
    }
    
    // 检查值是否为负数
    if value < 0.0 {
      is_valid = false
    }
    
    // 检查值是否为零（某些情况下可能无效）
    if value == 0.0 {
      is_valid = false
    }
    
    // 检查值是否异常大
    if value > 1000.0 {
      is_valid = false
    }
    
    if is_valid {
      valid_data.push((key, value))
    }
    
    i = i + 1
  }
  
  // 验证无效数据清理结果
  assert_eq(valid_data.length(), 3)  // 应该只有3条有效数据
  
  // 验证保留的有效数据
  assert_eq(valid_data[0], ("metric_001", 75.5))
  assert_eq(valid_data[1], ("metric_003", 125.8))
  assert_eq(valid_data[2], ("metric_006", 82.3))
  
  // 验证无效数据被清理
  let mut found_invalid = false
  i = 0
  while i < valid_data.length() {
    if valid_data[i].0 == "" {
      found_invalid = true
    }
    i = i + 1
  }
  assert_eq(found_invalid, false)  // 空键名应该被清理
}