// 遥测数据采样测试用例

test "telemetry_random_sampling" {
  // 测试随机采样算法
  
  let telemetry_data = [
    ("trace_001", 150.5),
    ("trace_002", 89.3),
    ("trace_003", 210.7),
    ("trace_004", 45.2),
    ("trace_005", 178.9),
    ("trace_006", 92.4),
    ("trace_007", 234.1),
    ("trace_008", 67.8),
    ("trace_009", 156.3),
    ("trace_010", 123.6)
  ]
  
  // 10%采样率，使用简单哈希采样
  let sampling_rate = 0.1
  let sampled_data = []
  
  let mut i = 0
  while i < telemetry_data.length() {
    let trace_id = telemetry_data[i].0
    let hash_value = trace_id.length() % 10  // 简单哈希函数
    
    // 如果哈希值小于采样率*10，则采样
    if hash_value.to_double() < sampling_rate * 10.0 {
      sampled_data.push(telemetry_data[i])
    }
    i = i + 1
  }
  
  // 验证采样结果
  assert_eq(sampled_data.length() >= 0, true)
  assert_eq(sampled_data.length() <= telemetry_data.length(), true)
  assert_eq(sampled_data.length() <= 2, true)  // 10%采样率，10个数据最多2个
}

test "telemetry_priority_sampling" {
  // 测试优先级采样
  
  let priority_data = [
    ("critical_error", 1),
    ("warning", 2),
    ("info", 3),
    ("debug", 4),
    ("critical_error", 1),
    ("info", 3),
    ("warning", 2),
    ("debug", 4),
    ("critical_error", 1),
    ("warning", 2)
  ]
  
  // 根据优先级采样，优先级1-4，采样率分别为100%, 75%, 50%, 25%
  let sampled_priority = []
  
  let mut i = 0
  while i < priority_data.length() {
    let priority = priority_data[i].1
    let should_sample = false
    
    if priority == 1 {
      should_sample = true  // 100%采样
    } else if priority == 2 {
      should_sample = i % 4 != 3  // 75%采样
    } else if priority == 3 {
      should_sample = i % 2 == 0  // 50%采样
    } else if priority == 4 {
      should_sample = i % 4 == 0  // 25%采样
    }
    
    if should_sample {
      sampled_priority.push(priority_data[i])
    }
    i = i + 1
  }
  
  // 验证优先级采样结果
  let mut critical_count = 0
  let mut warning_count = 0
  let mut info_count = 0
  let mut debug_count = 0
  
  i = 0
  while i < sampled_priority.length() {
    let priority = sampled_priority[i].1
    if priority == 1 {
      critical_count = critical_count + 1
    } else if priority == 2 {
      warning_count = warning_count + 1
    } else if priority == 3 {
      info_count = info_count + 1
    } else if priority == 4 {
      debug_count = debug_count + 1
    }
    i = i + 1
  }
  
  assert_eq(critical_count, 3)  // 所有critical_error都应该被采样
  assert_eq(warning_count <= 2, true)  // warning最多75%采样
  assert_eq(info_count <= 2, true)  // info最多50%采样
  assert_eq(debug_count <= 1, true)  // debug最多25%采样
}

test "telemetry_adaptive_sampling" {
  // 测试自适应采样（基于负载调整采样率）
  
  let load_data = [
    (10, 0.2),   // 低负载，20%采样
    (50, 0.4),   // 中负载，40%采样
    (100, 0.6),  // 高负载，60%采样
    (200, 0.8),  // 很高负载，80%采样
    (500, 0.9)   // 极高负载，90%采样
  ]
  
  let adaptive_samples = []
  
  let mut i = 0
  while i < load_data.length() {
    let load = load_data[i].0
    let sampling_rate = load_data[i].1
    
    // 模拟生成100个数据点，根据采样率采样
    let mut sampled_count = 0
    let mut j = 0
    while j < 100 {
      // 使用简单随机采样
      if j % 10 < (sampling_rate * 10.0).to_int() {
        sampled_count = sampled_count + 1
      }
      j = j + 1
    }
    
    adaptive_samples.push((load, sampled_count))
    i = i + 1
  }
  
  // 验证自适应采样结果
  assert_eq(adaptive_samples[0].1 >= 15 && adaptive_samples[0].1 <= 25, true)  // 20%采样
  assert_eq(adaptive_samples[1].1 >= 35 && adaptive_samples[1].1 <= 45, true)  // 40%采样
  assert_eq(adaptive_samples[2].1 >= 55 && adaptive_samples[2].1 <= 65, true)  // 60%采样
  assert_eq(adaptive_samples[3].1 >= 75 && adaptive_samples[3].1 <= 85, true)  // 80%采样
  assert_eq(adaptive_samples[4].1 >= 85 && adaptive_samples[4].1 <= 95, true)  // 90%采样
}

test "telemetry_reservoir_sampling" {
  // 测试蓄水池采样算法（适用于流数据）
  
  let stream_data = [
    "item_001", "item_002", "item_003", "item_004", "item_005",
    "item_006", "item_007", "item_008", "item_009", "item_010",
    "item_011", "item_012", "item_013", "item_014", "item_015"
  ]
  
  let reservoir_size = 5
  let reservoir = []
  
  let mut i = 0
  while i < stream_data.length() {
    if i < reservoir_size {
      // 填充蓄水池
      reservoir.push(stream_data[i])
    } else {
      // 随机替换
      let replace_index = i % (i + 1)  // 简化的随机选择
      if replace_index < reservoir_size {
        reservoir[replace_index] = stream_data[i]
      }
    }
    i = i + 1
  }
  
  // 验证蓄水池采样结果
  assert_eq(reservoir.length(), reservoir_size)
  assert_eq(reservoir.length() <= stream_data.length(), true)
  
  // 检查蓄水池中的元素都是来自原始数据
  i = 0
  while i < reservoir.length() {
    let mut found = false
    let mut j = 0
    while j < stream_data.length() {
      if reservoir[i] == stream_data[j] {
        found = true
        break
      }
      j = j + 1
    }
    assert_eq(found, true)
    i = i + 1
  }
}