// 遥测并发安全测试用例

test "telemetry_concurrent_span_creation" {
  // 测试并发span创建
  
  let concurrent_threads = 10
  let spans_per_thread = 100
  let mut all_spans = []
  
  // 模拟并发span创建
  let mut thread_id = 0
  while thread_id < concurrent_threads {
    let mut span_id = 0
    while span_id < spans_per_thread {
      let span_name = "thread_" + thread_id.to_string() + "_span_" + span_id.to_string()
      let trace_id = "trace_" + thread_id.to_string()
      let span_info = (span_name, trace_id, thread_id, span_id)
      all_spans.push(span_info)
      span_id = span_id + 1
    }
    thread_id = thread_id + 1
  }
  
  // 验证并发创建结果
  let expected_total_spans = concurrent_threads * spans_per_thread
  assert_eq(all_spans.length(), expected_total_spans)
  assert_eq(all_spans.length(), 1000) // 10 * 100
  
  // 验证span命名唯一性
  let mut i = 0
  while i < all_spans.length() {
    let span_name = all_spans[i].0
    let expected_name = "thread_" + all_spans[i].2.to_string() + "_span_" + all_spans[i].3.to_string()
    assert_eq(span_name, expected_name)
    i = i + 1
  }
  
  // 验证trace_id分组
  let mut trace_counts = []
  thread_id = 0
  while thread_id < concurrent_threads {
    let expected_trace_id = "trace_" + thread_id.to_string()
    let mut count = 0
    i = 0
    while i < all_spans.length() {
      if all_spans[i].1 == expected_trace_id {
        count = count + 1
      }
      i = i + 1
    }
    trace_counts.push((expected_trace_id, count))
    thread_id = thread_id + 1
  }
  
  // 验证每个trace的span数量
  assert_eq(trace_counts.length(), concurrent_threads)
  i = 0
  while i < trace_counts.length() {
    assert_eq(trace_counts[i].1, spans_per_thread)
    i = i + 1
  }
}

test "telemetry_concurrent_metric_update" {
  // 测试并发metric更新
  
  let metric_name = "concurrent_counter"
  let initial_value = 0
  let concurrent_updaters = 20
  let updates_per_updater = 50
  let increment_value = 1
  
  // 模拟并发metric更新
  let mut current_value = initial_value
  let mut update_log = []
  
  let mut updater_id = 0
  while updater_id < concurrent_updaters {
    let mut update_id = 0
    while update_id < updates_per_updater {
      // 模拟原子更新操作
      let old_value = current_value
      current_value = current_value + increment_value
      let new_value = current_value
      
      let update_record = "updater_" + updater_id.to_string() + 
                         "_update_" + update_id.to_string() + 
                         ":" + old_value.to_string() + "->" + new_value.to_string()
      update_log.push(update_record)
      
      update_id = update_id + 1
    }
    updater_id = updater_id + 1
  }
  
  // 验证最终值
  let expected_final_value = initial_value + (concurrent_updaters * updates_per_updater * increment_value)
  assert_eq(current_value, expected_final_value)
  assert_eq(current_value, 1000) // 0 + 20 * 50 * 1
  
  // 验证更新日志
  assert_eq(update_log.length(), concurrent_updaters * updates_per_updater)
  assert_eq(update_log.length(), 1000)
  
  // 验证更新记录格式
  assert_eq(update_log[0], "updater_0_update_0:0->1")
  assert_eq(update_log[update_log.length() - 1], "updater_19_update_49:998->999")
  
  // 验证每个updater的更新次数
  let mut updater_counts = []
  updater_id = 0
  while updater_id < concurrent_updaters {
    let mut count = 0
    let mut i = 0
    while i < update_log.length() {
      if update_log[i].contains("updater_" + updater_id.to_string() + "_") {
        count = count + 1
      }
      i = i + 1
    }
    updater_counts.push(("updater_" + updater_id.to_string(), count))
    updater_id = updater_id + 1
  }
  
  // 验证每个updater的更新次数
  assert_eq(updater_counts.length(), concurrent_updaters)
  updater_id = 0
  while updater_id < updater_counts.length() {
    assert_eq(updater_counts[updater_id].1, updates_per_updater)
    updater_id = updater_id + 1
  }
}

test "telemetry_concurrent_buffer_access" {
  // 测试并发buffer访问
  
  let buffer_size = 500
  let concurrent_readers = 5
  let concurrent_writers = 3
  let operations_per_thread = 100
  
  // 初始化共享buffer
  let mut shared_buffer = []
  let mut i = 0
  while i < buffer_size {
    shared_buffer.push("slot_" + i.to_string())
    i = i + 1
  }
  
  // 模拟并发读取操作
  let mut read_operations = []
  let mut reader_id = 0
  while reader_id < concurrent_readers {
    let mut op_id = 0
    while op_id < operations_per_thread {
      let read_index = (reader_id * operations_per_thread + op_id) % buffer_size
      let read_value = shared_buffer[read_index]
      let read_record = "reader_" + reader_id.to_string() + 
                       "_read_" + op_id.to_string() + 
                       ":index_" + read_index.to_string() + 
                       "=" + read_value
      read_operations.push(read_record)
      op_id = op_id + 1
    }
    reader_id = reader_id + 1
  }
  
  // 模拟并发写入操作（写入新buffer）
  let mut write_buffer = []
  let mut writer_id = 0
  while writer_id < concurrent_writers {
    let mut op_id = 0
    while op_id < operations_per_thread {
      let write_value = "writer_" + writer_id.to_string() + "_data_" + op_id.to_string()
      write_buffer.push(write_value)
      let write_record = "writer_" + writer_id.to_string() + 
                        "_write_" + op_id.to_string() + 
                        "=" + write_value
      read_operations.push(write_record) // 复用同一个数组记录操作
      op_id = op_id + 1
    }
    writer_id = writer_id + 1
  }
  
  // 验证并发操作结果
  let total_reads = concurrent_readers * operations_per_thread
  let total_writes = concurrent_writers * operations_per_thread
  let total_operations = total_reads + total_writes
  
  assert_eq(read_operations.length(), total_operations)
  assert_eq(total_operations, 800) // (5+3)*100
  assert_eq(write_buffer.length(), total_writes)
  assert_eq(write_buffer.length(), 300) // 3*100
  
  // 验证读取操作的索引范围
  let mut i = 0
  while i < total_reads {
    assert_eq(read_operations[i].contains("reader_"), true)
    assert_eq(read_operations[i].contains("_read_"), true)
    assert_eq(read_operations[i].contains(":index_"), true)
    assert_eq(read_operations[i].contains("=slot_"), true)
    i = i + 1
  }
  
  // 验证写入操作
  i = total_reads
  while i < read_operations.length() {
    assert_eq(read_operations[i].contains("writer_"), true)
    assert_eq(read_operations[i].contains("_write_"), true)
    i = i + 1
  }
  
  // 验证数据一致性
  let mut read_index_count = []
  i = 0
  while i < buffer_size {
    read_index_count.push((i, 0))
    i = i + 1
  }
  
  // 统计每个索引被读取的次数
  i = 0
  while i < total_reads {
    let record = read_operations[i]
    let parts = record.split("=")
    let index_part = parts[0]
    let index_str = index_part.split("index_")[1]
    let index = index_str.to_int()
    read_index_count[index] = (index, read_index_count[index].1 + 1)
    i = i + 1
  }
  
  // 验证读取分布（应该相对均匀）
  let mut min_reads = read_index_count[0].1
  let mut max_reads = read_index_count[0].1
  i = 0
  while i < read_index_count.length() {
    if read_index_count[i].1 < min_reads {
      min_reads = read_index_count[i].1
    }
    if read_index_count[i].1 > max_reads {
      max_reads = read_index_count[i].1
    }
    i = i + 1
  }
  
  // 验证读取分布的均匀性（最大差异不应太大）
  let read_variance = max_reads - min_reads
  assert_eq(read_variance >= 0, true)
  assert_eq(read_variance < 10, true) // 允许一定的差异
}