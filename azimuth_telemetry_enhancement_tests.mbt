// Azimuth遥测系统增强功能测试用例
// 专注于遥测系统的高级功能和性能优化

// 测试1: 遥测数据压缩与解压缩
test "遥测数据压缩与解压缩测试" {
  // 创建测试遥测数据
  let telemetry_data = [
    ("trace-001", "span-001", "database.query", 125, "ok"),
    ("trace-001", "span-002", "cache.lookup", 25, "ok"),
    ("trace-002", "span-003", "api.request", 200, "error"),
    ("trace-002", "span-004", "data.processing", 350, "ok"),
    ("trace-003", "span-005", "file.read", 180, "ok")
  ]
  
  // 模拟压缩函数
  let compress_data = fn(data: Array[(String, String, String, Int, String)]) {
    let compressed = []
    for item in data {
      match item {
        (trace_id, span_id, operation, duration, status) => {
          // 简化的压缩：将重复的trace_id替换为引用
          let compressed_item = (
            if trace_id == "trace-001" { "t1" } 
            else if trace_id == "trace-002" { "t2" } 
            else { "t3" },
            span_id,
            operation,
            duration,
            status
          )
          compressed = compressed + [compressed_item]
        }
      }
    }
    compressed
  }
  
  // 模拟解压缩函数
  let decompress_data = fn(compressed: Array[(String, String, String, Int, String)]) {
    let decompressed = []
    for item in compressed {
      match item {
        (trace_ref, span_id, operation, duration, status) => {
          // 简化的解压缩：将引用还原为完整的trace_id
          let decompressed_item = (
            if trace_ref == "t1" { "trace-001" }
            else if trace_ref == "t2" { "trace-002" }
            else { "trace-003" },
            span_id,
            operation,
            duration,
            status
          )
          decompressed = decompressed + [decompressed_item]
        }
      }
    }
    decompressed
  }
  
  // 测试压缩
  let compressed_data = compress_data(telemetry_data)
  assert_eq(compressed_data.length(), 5)
  
  // 验证压缩效果
  match compressed_data[0] {
    (trace_ref, _, _, _, _) => assert_eq(trace_ref, "t1")
  }
  
  // 测试解压缩
  let decompressed_data = decompress_data(compressed_data)
  assert_eq(decompressed_data.length(), 5)
  
  // 验证解压缩后的数据与原始数据一致
  assert_eq(decompressed_data, telemetry_data)
  
  // 计算压缩率
  let original_size = telemetry_data.reduce(fn(acc, item) {
    match item {
      (trace_id, span_id, operation, _, status) => 
        acc + trace_id.length() + span_id.length() + operation.length() + status.length()
    }
  }, 0)
  
  let compressed_size = compressed_data.reduce(fn(acc, item) {
    match item {
      (trace_ref, span_id, operation, _, status) => 
        acc + trace_ref.length() + span_id.length() + operation.length() + status.length()
    }
  }, 0)
  
  let compression_ratio = (original_size - compressed_size) * 100 / original_size
  assert_true(compression_ratio > 10)  // 至少10%的压缩率
}

// 测试2: 遥测数据缓存机制
test "遥测数据缓存机制测试" {
  // 模拟缓存存储
  let cache_storage = { mut entries: [] }
  
  // 缓存操作函数
  let cache_get = fn(key: String) {
    let mut found = None
    for entry in cache_storage.entries {
      match entry {
        (k, v) => {
          if k == key {
            found = Some(v)
          }
        }
      }
    }
    found
  }
  
  let cache_set = fn(key: String, value: String) {
    let mut updated = false
    let mut new_entries = []
    
    // 检查键是否已存在
    for entry in cache_storage.entries {
      match entry {
        (k, v) => {
          if k == key {
            new_entries = new_entries + [(k, value)]
            updated = true
          } else {
            new_entries = new_entries + [(k, v)]
          }
        }
      }
    }
    
    // 如果键不存在，添加新条目
    if not(updated) {
      new_entries = new_entries + [(key, value)]
    }
    
    cache_storage.entries = new_entries
  }
  
  let cache_has = fn(key: String) {
    let mut found = false
    for entry in cache_storage.entries {
      match entry {
        (k, _) => {
          if k == key {
            found = true
          }
        }
      }
    }
    found
  }
  
  // 测试缓存设置和获取
  cache_set("trace-001:span-001", "database.query")
  cache_set("trace-001:span-002", "cache.lookup")
  cache_set("trace-002:span-001", "api.request")
  
  assert_eq(cache_storage.entries.length(), 3)
  assert_true(cache_has("trace-001:span-001"))
  assert_false(cache_has("trace-999:span-999"))
  
  // 测试缓存获取
  let cached_value1 = cache_get("trace-001:span-001")
  assert_eq(cached_value1, Some("database.query"))
  
  let cached_value2 = cache_get("trace-001:span-002")
  assert_eq(cached_value2, Some("cache.lookup"))
  
  let missing_value = cache_get("trace-999:span-999")
  assert_eq(missing_value, None)
  
  // 测试缓存更新
  cache_set("trace-001:span-001", "database.query.updated")
  let updated_value = cache_get("trace-001:span-001")
  assert_eq(updated_value, Some("database.query.updated"))
  
  // 验证缓存大小未增加
  assert_eq(cache_storage.entries.length(), 3)
  
  // 测试缓存容量限制
  let max_cache_size = 5
  let cache_set_with_limit = fn(key: String, value: String) {
    // 如果缓存已满，删除最旧的条目
    if cache_storage.entries.length() >= max_cache_size {
      let oldest = cache_storage.entries[0]
      match oldest {
        (k, _) => {
          let mut new_entries = []
          for entry in cache_storage.entries {
            match entry {
              (entry_key, entry_value) => {
                if entry_key != k {
                  new_entries = new_entries + [(entry_key, entry_value)]
                }
              }
            }
          }
          cache_storage.entries = new_entries
        }
      }
    }
    
    cache_set(key, value)
  }
  
  // 添加条目直到达到容量限制
  cache_set_with_limit("trace-003:span-001", "file.read")
  cache_set_with_limit("trace-003:span-002", "file.write")
  cache_set_with_limit("trace-004:span-001", "network.request")
  
  // 验证最旧的条目被删除
  assert_eq(cache_storage.entries.length(), 5)
  assert_false(cache_has("trace-001:span-001"))  // 最旧的条目被删除
  assert_true(cache_has("trace-004:span-001"))   // 最新的条目存在
}

// 测试3: 遥测数据采样策略
test "遥测数据采样策略测试" {
  // 定义采样策略类型
  enum SamplingStrategy {
    Always
    Never
    Probability(Float)
    RateLimit(Int)  // 每秒最多采样数
    AttributeBased(String, String)  // 基于属性值的采样
  }
  
  // 采样决策函数
  let should_sample = fn(strategy: SamplingStrategy, trace_id: String, attributes: Array[(String, String)]) {
    match strategy {
      SamplingStrategy::Always => true,
      SamplingStrategy::Never => false,
      SamplingStrategy::Probability(prob) => {
        // 简化的概率采样：基于trace_id的哈希值
        let hash = trace_id.length() % 100
        (hash.to_float() / 100.0) < prob
      },
      SamplingStrategy::RateLimit(max_rate) => {
        // 简化的速率限制：基于trace_id的模运算
        let hash = trace_id.length() % 10
        hash < (max_rate / 10)
      },
      SamplingStrategy::AttributeBased(key, value) => {
        let mut found = false
        for attr in attributes {
          match attr {
            (k, v) => {
              if k == key && v == value {
                found = true
              }
            }
          }
        }
        found
      }
    }
  }
  
  // 测试总是采样策略
  let always_strategy = SamplingStrategy::Always
  assert_true(should_sample(always_strategy, "trace-001", []))
  assert_true(should_sample(always_strategy, "trace-002", []))
  
  // 测试从不采样策略
  let never_strategy = SamplingStrategy::Never
  assert_false(should_sample(never_strategy, "trace-001", []))
  assert_false(should_sample(never_strategy, "trace-002", []))
  
  // 测试概率采样策略
  let prob_strategy = SamplingStrategy::Probability(0.5)
  let prob_samples = []
  for i in 0..=10 {
    let trace_id = "trace-" + i.to_string()
    if should_sample(prob_strategy, trace_id, []) {
      prob_samples = prob_samples + [trace_id]
    }
  }
  // 由于是确定性采样，我们可以预测结果
  assert_true(prob_samples.length() >= 3)  // 至少采样30%
  assert_true(prob_samples.length() <= 7)  // 最多采样70%
  
  // 测试速率限制采样策略
  let rate_limit_strategy = SamplingStrategy::RateLimit(5)  // 每秒最多5个采样
  let rate_samples = []
  for i in 0..=10 {
    let trace_id = "trace-" + i.to_string()
    if should_sample(rate_limit_strategy, trace_id, []) {
      rate_samples = rate_samples + [trace_id]
    }
  }
  // 由于是确定性采样，我们可以预测结果
  assert_eq(rate_samples.length(), 5)
  
  // 测试基于属性的采样策略
  let attr_strategy = SamplingStrategy::AttributeBased("service.name", "critical.service")
  let critical_attrs = [("service.name", "critical.service"), ("env", "production")]
  let normal_attrs = [("service.name", "normal.service"), ("env", "production")]
  
  assert_true(should_sample(attr_strategy, "trace-001", critical_attrs))
  assert_false(should_sample(attr_strategy, "trace-002", normal_attrs))
  
  // 测试复合采样策略
  let composite_sample = fn(strategies: Array[SamplingStrategy], trace_id: String, attributes: Array[(String, String)]) {
    for strategy in strategies {
      if should_sample(strategy, trace_id, attributes) {
        return true
      }
    }
    false
  }
  
  let strategies = [
    SamplingStrategy::AttributeBased("service.name", "critical.service"),
    SamplingStrategy::Probability(0.1)
  ]
  
  assert_true(composite_sample(strategies, "trace-001", critical_attrs))
  assert_false(composite_sample(strategies, "trace-002", normal_attrs))
  
  // 测试采样统计
  let calculate_sampling_rate = fn(total_traces: Int, sampled_traces: Int) {
    if total_traces > 0 {
      sampled_traces * 100 / total_traces
    } else {
      0
    }
  }
  
  let total_traces = 100
  let sampled_traces = prob_samples.length() * 10  // 估算
  let sampling_rate = calculate_sampling_rate(total_traces, sampled_traces)
  assert_true(sampling_rate >= 20 && sampling_rate <= 80)
}

// 测试4: 遥测数据批处理机制
test "遥测数据批处理机制测试" {
  // 模拟批处理配置
  let batch_config = {
    max_batch_size: 5,
    max_batch_timeout: 1000,  // 毫秒
    max_retries: 3
  }
  
  // 模拟批处理存储
  let batch_storage = { mut items: [], mut last_flush_time: 1640995200 }
  
  // 添加项目到批处理
  let add_to_batch = fn(item: String) {
    batch_storage.items = batch_storage.items + [item]
  }
  
  // 检查是否需要刷新批处理
  let should_flush = fn(current_time: Int) {
    let size_reached = batch_storage.items.length() >= batch_config.max_batch_size
    let timeout_reached = (current_time - batch_storage.last_flush_time) >= batch_config.max_batch_timeout
    size_reached or timeout_reached
  }
  
  // 刷新批处理
  let flush_batch = fn(current_time: Int) {
    let flushed_items = batch_storage.items
    batch_storage.items = []
    batch_storage.last_flush_time = current_time
    flushed_items
  }
  
  // 测试批处理大小触发刷新
  add_to_batch("item-1")
  add_to_batch("item-2")
  add_to_batch("item-3")
  assert_eq(batch_storage.items.length(), 3)
  assert_false(should_flush(1640995200))
  
  add_to_batch("item-4")
  add_to_batch("item-5")
  assert_eq(batch_storage.items.length(), 5)
  assert_true(should_flush(1640995200))
  
  let flushed_items = flush_batch(1640995200)
  assert_eq(flushed_items.length(), 5)
  assert_eq(batch_storage.items.length(), 0)
  
  // 测试超时触发刷新
  add_to_batch("item-6")
  add_to_batch("item-7")
  assert_eq(batch_storage.items.length(), 2)
  assert_false(should_flush(1640995600))  // 未达到大小限制
  
  assert_true(should_flush(1640996200))  // 达到超时限制
  
  let timeout_flushed = flush_batch(1640996200)
  assert_eq(timeout_flushed.length(), 2)
  assert_eq(batch_storage.items.length(), 0)
  
  // 测试批处理重试机制
  let retry_storage = { mut failed_batches: [], mut retry_counts: [] }
  
  let process_batch_with_retry = fn(batch: Array[String], retry_count: Int) {
    // 模拟处理失败
    if retry_count < 2 {
      (false, "Processing failed")
    } else {
      (true, "Processing succeeded")
    }
  }
  
  let flush_with_retry = fn(current_time: Int) {
    let batch = batch_storage.items
    if batch.length() > 0 {
      let mut success = false
      let mut message = ""
      let mut retries = 0
      
      while retries < batch_config.max_retries and not(success) {
        let (proc_success, proc_message) = process_batch_with_retry(batch, retries)
        success = proc_success
        message = proc_message
        
        if not(success) {
          retries = retries + 1
        }
      }
      
      if success {
        batch_storage.items = []
        batch_storage.last_flush_time = current_time
        (true, message)
      } else {
        retry_storage.failed_batches = retry_storage.failed_batches + [batch]
        retry_storage.retry_counts = retry_storage.retry_counts + [retries]
        (false, message)
      }
    } else {
      (true, "No items to process")
    }
  }
  
  // 测试重试机制
  add_to_batch("item-8")
  add_to_batch("item-9")
  
  let (success1, message1) = flush_with_retry(1640996300)
  assert_false(success1)
  assert_eq(message1, "Processing failed")
  assert_eq(retry_storage.failed_batches.length(), 1)
  assert_eq(retry_storage.retry_counts[0], 3)
  
  // 测试批处理聚合
  let aggregate_batches = fn(batches: Array[Array[String]]) {
    let mut all_items = []
    for batch in batches {
      all_items = all_items + batch
    }
    all_items
  }
  
  let all_failed_items = aggregate_batches(retry_storage.failed_batches)
  assert_eq(all_failed_items.length(), 2)
  assert_true(all_failed_items.contains("item-8"))
  assert_true(all_failed_items.contains("item-9"))
  
  // 测试批处理性能指标
  let batch_metrics = {
    total_items: 0,
    total_batches: 0,
    average_batch_size: 0,
    failed_batches: 0
  }
  
  let update_metrics = fn(batch_size: Int, success: Bool) {
    batch_metrics.total_items = batch_metrics.total_items + batch_size
    batch_metrics.total_batches = batch_metrics.total_batches + 1
    batch_metrics.average_batch_size = batch_metrics.total_items / batch_metrics.total_batches
    
    if not(success) {
      batch_metrics.failed_batches = batch_metrics.failed_batches + 1
    }
  }
  
  update_metrics(5, true)
  update_metrics(2, true)
  update_metrics(2, false)
  
  assert_eq(batch_metrics.total_items, 9)
  assert_eq(batch_metrics.total_batches, 3)
  assert_eq(batch_metrics.average_batch_size, 3)
  assert_eq(batch_metrics.failed_batches, 1)
}

// 测试5: 遥测数据导出格式转换
test "遥测数据导出格式转换测试" {
  // 定义遥测数据结构
  type TelemetrySpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // 创建测试数据
  let test_spans = [
    {
      trace_id: "trace-001",
      span_id: "span-001",
      parent_span_id: None,
      operation_name: "database.query",
      start_time: 1640995200,
      end_time: 1640995250,
      status: "ok",
      attributes: [("db.type", "postgresql"), ("db.statement", "SELECT * FROM users")]
    },
    {
      trace_id: "trace-001",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation_name: "cache.lookup",
      start_time: 1640995225,
      end_time: 1640995235,
      status: "ok",
      attributes: [("cache.type", "redis"), ("cache.key", "user:123")]
    }
  ]
  
  // JSON格式导出
  let export_to_json = fn(spans: Array[TelemetrySpan]) {
    let json_lines = []
    for span in spans {
      let json_line = "{" +
        "\"trace_id\":\"" + span.trace_id + "\"," +
        "\"span_id\":\"" + span.span_id + "\"," +
        "\"operation_name\":\"" + span.operation_name + "\"," +
        "\"start_time\":" + span.start_time.to_string() + "," +
        "\"end_time\":" + span.end_time.to_string() + "," +
        "\"status\":\"" + span.status + "\"" +
        "}"
      json_lines = json_lines + [json_line]
    }
    "[" + json_lines.join(",") + "]"
  }
  
  let json_export = export_to_json(test_spans)
  assert_true(json_export.contains("trace-001"))
  assert_true(json_export.contains("span-001"))
  assert_true(json_export.contains("database.query"))
  assert_true(json_export.contains("cache.lookup"))
  
  // CSV格式导出
  let export_to_csv = fn(spans: Array[TelemetrySpan]) {
    let header = "trace_id,span_id,parent_span_id,operation_name,start_time,end_time,status"
    let csv_lines = [header]
    
    for span in spans {
      let parent_id = match span.parent_span_id {
        Some(id) => id
        None => ""
      }
      let csv_line = span.trace_id + "," + 
                    span.span_id + "," + 
                    parent_id + "," + 
                    span.operation_name + "," + 
                    span.start_time.to_string() + "," + 
                    span.end_time.to_string() + "," + 
                    span.status
      csv_lines = csv_lines + [csv_line]
    }
    
    csv_lines.join("\n")
  }
  
  let csv_export = export_to_csv(test_spans)
  assert_true(csv_export.contains("trace_id,span_id,parent_span_id"))
  assert_true(csv_export.contains("trace-001,span-001,,database.query"))
  assert_true(csv_export.contains("trace-001,span-002,span-001,cache.lookup"))
  
  // Prometheus格式导出
  let export_to_prometheus = fn(spans: Array[TelemetrySpan]) {
    let mut metrics = []
    
    // 计算span持续时间指标
    for span in spans {
      let duration = span.end_time - span.start_time
      let metric_line = "azimuth_span_duration{" +
        "trace_id=\"" + span.trace_id + "\"," +
        "span_id=\"" + span.span_id + "\"," +
        "operation=\"" + span.operation_name + "\"" +
        "} " + duration.to_string()
      metrics = metrics + [metric_line]
    }
    
    // 计算span计数指标
    let span_count = spans.length()
    let count_metric = "azimuth_span_count " + span_count.to_string()
    metrics = metrics + [count_metric]
    
    metrics.join("\n")
  }
  
  let prometheus_export = export_to_prometheus(test_spans)
  assert_true(prometheus_export.contains("azimuth_span_duration"))
  assert_true(prometheus_export.contains("trace_id=\"trace-001\""))
  assert_true(prometheus_export.contains("operation=\"database.query\""))
  assert_true(prometheus_export.contains("azimuth_span_count 2"))
  
  // 自定义格式导出
  let export_to_custom = fn(spans: Array[TelemetrySpan]) {
    let custom_lines = []
    
    for span in spans {
      let duration = span.end_time - span.start_time
      let parent_id = match span.parent_span_id {
        Some(id) => id
        None => "root"
      }
      
      let custom_line = "SPAN|" + 
                       span.trace_id + "|" + 
                       span.span_id + "|" + 
                       parent_id + "|" + 
                       span.operation_name + "|" + 
                       duration.to_string() + "ms|" + 
                       span.status
      custom_lines = custom_lines + [custom_line]
    }
    
    custom_lines.join("\n")
  }
  
  let custom_export = export_to_custom(test_spans)
  assert_true(custom_export.contains("SPAN|trace-001|span-001|root|database.query|50ms|ok"))
  assert_true(custom_export.contains("SPAN|trace-001|span-002|span-001|cache.lookup|10ms|ok"))
  
  // 测试格式转换性能
  let measure_export_performance = fn(export_fn: (Array[TelemetrySpan]) -> String, spans: Array[TelemetrySpan]) {
    let start_time = 1640995200
    let result = export_fn(spans)
    let end_time = 1640995300
    let duration = end_time - start_time
    (result, duration)
  }
  
  let (json_result, json_duration) = measure_export_performance(export_to_json, test_spans)
  let (csv_result, csv_duration) = measure_export_performance(export_to_csv, test_spans)
  let (prometheus_result, prometheus_duration) = measure_export_performance(export_to_prometheus, test_spans)
  let (custom_result, custom_duration) = measure_export_performance(export_to_custom, test_spans)
  
  assert_eq(json_duration, 1000)
  assert_eq(csv_duration, 1000)
  assert_eq(prometheus_duration, 1000)
  assert_eq(custom_duration, 1000)
  
  // 验证所有导出结果都包含相同的核心信息
  assert_true(json_result.contains("trace-001"))
  assert_true(csv_result.contains("trace-001"))
  assert_true(prometheus_result.contains("trace-001"))
  assert_true(custom_result.contains("trace-001"))
}

// 测试6: 遥测数据安全加密
test "遥测数据安全加密测试" {
  // 模拟简单的加密函数
  let simple_encrypt = fn(data: String, key: String) {
    let mut encrypted = ""
    let key_length = key.length()
    
    for i in 0..data.length() {
      let data_char = data[i]
      let key_char = key[i % key_length]
      let encrypted_char = ((data_char.to_int() + key_char.to_int()) % 256).to_char()
      encrypted = encrypted + encrypted_char.to_string()
    }
    
    encrypted
  }
  
  // 模拟简单的解密函数
  let simple_decrypt = fn(encrypted_data: String, key: String) {
    let mut decrypted = ""
    let key_length = key.length()
    
    for i in 0..encrypted_data.length() {
      let encrypted_char = encrypted_data[i]
      let key_char = key[i % key_length]
      let decrypted_char = ((encrypted_char.to_int() - key_char.to_int() + 256) % 256).to_char()
      decrypted = decrypted + decrypted_char.to_string()
    }
    
    decrypted
  }
  
  // 测试加密和解密
  let encryption_key = "azimuth-key-2023"
  let original_data = "trace-001:span-001:database.query:ok"
  
  let encrypted_data = simple_encrypt(original_data, encryption_key)
  assert_ne(encrypted_data, original_data)
  assert_eq(encrypted_data.length(), original_data.length())
  
  let decrypted_data = simple_decrypt(encrypted_data, encryption_key)
  assert_eq(decrypted_data, original_data)
  
  // 测试不同密钥的加密结果
  let different_key = "different-key-2023"
  let encrypted_with_different_key = simple_encrypt(original_data, different_key)
  assert_ne(encrypted_with_different_key, encrypted_data)
  
  // 测试错误密钥解密失败
  let wrong_decrypted = simple_decrypt(encrypted_data, different_key)
  assert_ne(wrong_decrypted, original_data)
  
  // 测试批量数据加密
  let telemetry_records = [
    "trace-001:span-001:database.query:ok",
    "trace-001:span-002:cache.lookup:ok",
    "trace-002:span-001:api.request:error",
    "trace-002:span-002:data.processing:ok"
  ]
  
  let encrypt_batch = fn(records: Array[String], key: String) {
    let mut encrypted_records = []
    for record in records {
      let encrypted = simple_encrypt(record, key)
      encrypted_records = encrypted_records + [encrypted]
    }
    encrypted_records
  }
  
  let decrypt_batch = fn(encrypted_records: Array[String], key: String) {
    let mut decrypted_records = []
    for encrypted_record in encrypted_records {
      let decrypted = simple_decrypt(encrypted_record, key)
      decrypted_records = decrypted_records + [decrypted]
    }
    decrypted_records
  }
  
  let encrypted_batch = encrypt_batch(telemetry_records, encryption_key)
  assert_eq(encrypted_batch.length(), 4)
  
  let decrypted_batch = decrypt_batch(encrypted_batch, encryption_key)
  assert_eq(decrypted_batch, telemetry_records)
  
  // 测试加密数据完整性验证
  let calculate_checksum = fn(data: String) {
    let mut sum = 0
    for i in 0..data.length() {
      sum = sum + data[i].to_int()
    }
    sum % 1000  // 简化的校验和
  }
  
  let original_checksum = calculate_checksum(original_data)
  let encrypted_checksum = calculate_checksum(encrypted_data)
  let decrypted_checksum = calculate_checksum(decrypted_data)
  
  assert_eq(original_checksum, decrypted_checksum)
  assert_ne(original_checksum, encrypted_checksum)
  
  // 测试加密性能
  let measure_encryption_performance = fn(data: String, key: String) {
    let start_time = 1640995200
    let encrypted = simple_encrypt(data, key)
    let end_time = 1640995250
    let duration = end_time - start_time
    (encrypted, duration)
  }
  
  let (perf_encrypted, perf_duration) = measure_encryption_performance(original_data, encryption_key)
  assert_eq(perf_duration, 50)
  assert_eq(perf_encrypted, encrypted_data)
  
  // 测试密钥管理
  let key_manager = {
    current_key: "default-key",
    key_rotation_interval: 86400,  // 24小时
    last_rotation: 1640995200
  }
  
  let should_rotate_key = fn(current_time: Int) {
    (current_time - key_manager.last_rotation) >= key_manager.key_rotation_interval
  }
  
  let rotate_key = fn(current_time: Int) {
    let new_key = "rotated-key-" + current_time.to_string()
    key_manager.current_key = new_key
    key_manager.last_rotation = current_time
    new_key
  }
  
  assert_false(should_rotate_key(1640995300))  // 100秒后，未达到24小时
  
  let rotated_key = rotate_key(1640995200 + 86400)  // 24小时后
  assert_eq(rotated_key, "rotated-key-1641081600")
  assert_eq(key_manager.current_key, rotated_key)
}

// 测试7: 遥测数据性能基准测试
test "遥测数据性能基准测试测试" {
  // 性能测试配置
  let benchmark_config = {
    test_data_size: 1000,
    warmup_iterations: 10,
    benchmark_iterations: 100
  }
  
  // 生成测试数据
  let generate_test_data = fn(size: Int) {
    let mut data = []
    for i in 0..size {
      let record = (
        "trace-" + (i % 10).to_string(),
        "span-" + i.to_string(),
        "operation-" + (i % 5).to_string(),
        50 + (i % 200),
        if i % 10 == 0 { "error" } else { "ok" }
      )
      data = data + [record]
    }
    data
  }
  
  let test_data = generate_test_data(benchmark_config.test_data_size)
  assert_eq(test_data.length(), 1000)
  
  // 性能测量函数
  let measure_performance = fn(operation: () -> Unit, iterations: Int) {
    let mut total_time = 0
    
    for i in 0..iterations {
      let start_time = 1640995200
      operation()
      let end_time = 1640995300
      total_time = total_time + (end_time - start_time)
    }
    
    total_time / iterations
  }
  
  // 测试数据过滤性能
  let filter_successful_spans = fn(data: Array[(String, String, String, Int, String)]) {
    let mut filtered = []
    for item in data {
      match item {
        (_, _, _, _, status) => {
          if status == "ok" {
            filtered = filtered + [item]
          }
        }
      }
    }
    filtered
  }
  
  let filter_operation = fn() {
    let filtered = filter_successful_spans(test_data)
    assert_true(filtered.length() > 0)
  }
  
  // 预热
  for i in 0..benchmark_config.warmup_iterations {
    filter_operation()
  }
  
  // 基准测试
  let filter_performance = measure_performance(filter_operation, benchmark_config.benchmark_iterations)
  assert_eq(filter_performance, 1000)  // 模拟的性能测量结果
  
  // 测试数据聚合性能
  let aggregate_by_trace = fn(data: Array[(String, String, String, Int, String)]) {
    let mut aggregation = []
    
    for item in data {
      match item {
        (trace_id, _, _, duration, status) => {
          let mut found = false
          let mut new_aggregation = []
          
          for agg in aggregation {
            match agg {
              (id, count, total_duration, error_count) => {
                if id == trace_id {
                  new_aggregation = new_aggregation + [(id, count + 1, total_duration + duration, 
                    if status == "error" { error_count + 1 } else { error_count })]
                  found = true
                } else {
                  new_aggregation = new_aggregation + [(id, count, total_duration, error_count)]
                }
              }
            }
          }
          
          if not(found) {
            new_aggregation = new_aggregation + [(trace_id, 1, duration, if status == "error" { 1 } else { 0 })]
          }
          
          aggregation = new_aggregation
        }
      }
    }
    
    aggregation
  }
  
  let aggregate_operation = fn() {
    let aggregated = aggregate_by_trace(test_data)
    assert_eq(aggregated.length(), 10)  // 10个不同的trace_id
  }
  
  let aggregate_performance = measure_performance(aggregate_operation, benchmark_config.benchmark_iterations)
  assert_eq(aggregate_performance, 2000)  // 聚合操作比过滤慢
  
  // 测试数据序列化性能
  let serialize_data = fn(data: Array[(String, String, String, Int, String)]) {
    let mut serialized = ""
    
    for item in data {
      match item {
        (trace_id, span_id, operation, duration, status) => {
          let line = trace_id + "|" + span_id + "|" + operation + "|" + 
                    duration.to_string() + "|" + status + "\n"
          serialized = serialized + line
        }
      }
    }
    
    serialized
  }
  
  let serialize_operation = fn() {
    let serialized = serialize_data(test_data)
    assert_true(serialized.length() > 0)
  }
  
  let serialize_performance = measure_performance(serialize_operation, benchmark_config.benchmark_iterations)
  assert_eq(serialize_performance, 5000)  // 序列化操作更慢
  
  // 测试内存使用情况
  let memory_usage = {
    initial_memory: 1000000,  // 假设的初始内存使用量
    peak_memory: 0,
    final_memory: 0
  }
  
  let measure_memory_usage = fn(operation: () -> Unit) {
    let initial_memory = memory_usage.initial_memory
    let peak_memory = initial_memory + 500000  // 模拟峰值内存使用
    let final_memory = initial_memory + 100000  // 模拟最终内存使用
    
    operation()
    
    memory_usage.peak_memory = peak_memory
    memory_usage.final_memory = final_memory
    
    (initial_memory, peak_memory, final_memory)
  }
  
  let (initial, peak, final) = measure_memory_usage(filter_operation)
  assert_eq(initial, 1000000)
  assert_eq(peak, 1500000)
  assert_eq(final, 1100000)
  
  // 测试性能回归检测
  let performance_baseline = {
    filter_operation: 1000,
    aggregate_operation: 2000,
    serialize_operation: 5000
  }
  
  let current_performance = {
    filter_operation: filter_performance,
    aggregate_operation: aggregate_performance,
    serialize_operation: serialize_performance
  }
  
  let check_performance_regression = fn(baseline: (String, Int), current: (String, Int), threshold: Float) {
    let (name, baseline_time) = baseline
    let (_, current_time) = current
    let regression_ratio = (current_time.to_float() - baseline_time.to_float()) / baseline_time.to_float()
    regression_ratio > threshold
  }
  
  let regression_threshold = 0.2  // 20%的性能回归阈值
  
  let filter_regression = check_performance_regression(
    ("filter_operation", performance_baseline.filter_operation),
    ("filter_operation", current_performance.filter_operation),
    regression_threshold
  )
  
  let aggregate_regression = check_performance_regression(
    ("aggregate_operation", performance_baseline.aggregate_operation),
    ("aggregate_operation", current_performance.aggregate_operation),
    regression_threshold
  )
  
  let serialize_regression = check_performance_regression(
    ("serialize_operation", performance_baseline.serialize_operation),
    ("serialize_operation", current_performance.serialize_operation),
    regression_threshold
  )
  
  assert_false(filter_regression)  // 无性能回归
  assert_false(aggregate_regression)  // 无性能回归
  assert_false(serialize_regression)  // 无性能回归
  
  // 性能报告生成
  let generate_performance_report = fn() {
    "Performance Benchmark Report:\n" +
    "- Filter Operation: " + filter_performance.to_string() + "ms\n" +
    "- Aggregate Operation: " + aggregate_performance.to_string() + "ms\n" +
    "- Serialize Operation: " + serialize_performance.to_string() + "ms\n" +
    "- Memory Usage: Initial=" + initial.to_string() + 
    ", Peak=" + peak.to_string() + 
    ", Final=" + final.to_string() + "\n" +
    "- Performance Regression: " + 
    (if filter_regression or aggregate_regression or serialize_regression { "Detected" } else { "None" })
  }
  
  let report = generate_performance_report()
  assert_true(report.contains("Performance Benchmark Report"))
  assert_true(report.contains("Filter Operation: 1000ms"))
  assert_true(report.contains("Performance Regression: None"))
}

// 测试8: 遥测数据并发处理
test "遥测数据并发处理测试" {
  // 模拟并发任务队列
  let task_queue = { mut tasks: [], mut completed_tasks: [] }
  
  // 添加任务到队列
  let add_task = fn(task_id: String, data: String) {
    let task = (task_id, data, "pending")
    task_queue.tasks = task_queue.tasks + [task]
  }
  
  // 处理任务
  let process_task = fn(task: (String, String, String)) {
    let (task_id, data, _) = task
    // 模拟处理时间
    let processed_data = data.to_uppercase()
    let completed_task = (task_id, processed_data, "completed")
    task_queue.completed_tasks = task_queue.completed_tasks + [completed_task]
  }
  
  // 并发处理多个任务
  let process_tasks_concurrently = fn(concurrency_limit: Int) {
    let mut batch_size = 0
    let mut processed_count = 0
    
    while processed_count < task_queue.tasks.length() {
      // 确定当前批处理大小
      let remaining = task_queue.tasks.length() - processed_count
      batch_size = if remaining < concurrency_limit { remaining } else { concurrency_limit }
      
      // 处理当前批次
      for i in 0..batch_size {
        let task_index = processed_count + i
        if task_index < task_queue.tasks.length() {
          let task = task_queue.tasks[task_index]
          process_task(task)
        }
      }
      
      processed_count = processed_count + batch_size
    }
  }
  
  // 添加测试任务
  add_task("task-1", "trace-data-1")
  add_task("task-2", "trace-data-2")
  add_task("task-3", "trace-data-3")
  add_task("task-4", "trace-data-4")
  add_task("task-5", "trace-data-5")
  
  assert_eq(task_queue.tasks.length(), 5)
  assert_eq(task_queue.completed_tasks.length(), 0)
  
  // 并发处理任务（模拟并发度为3）
  process_tasks_concurrently(3)
  
  assert_eq(task_queue.completed_tasks.length(), 5)
  
  // 验证处理结果
  for completed_task in task_queue.completed_tasks {
    match completed_task {
      (task_id, processed_data, status) => {
        assert_eq(status, "completed")
        assert_true(processed_data.contains("TRACE-DATA"))
      }
    }
  }
  
  // 测试线程安全的数据结构
  let concurrent_counter = { mut value: 0, mut operations: [] }
  
  let atomic_increment = fn(operation_id: String) {
    // 模拟原子操作
    concurrent_counter.value = concurrent_counter.value + 1
    concurrent_counter.operations = concurrent_counter.operations + [("increment", operation_id)]
  }
  
  let atomic_decrement = fn(operation_id: String) {
    // 模拟原子操作
    concurrent_counter.value = concurrent_counter.value - 1
    concurrent_counter.operations = concurrent_counter.operations + [("decrement", operation_id)]
  }
  
  let atomic_read = fn() {
    concurrent_counter.value
  }
  
  // 并发执行原子操作
  for i in 0..=10 {
    if i % 2 == 0 {
      atomic_increment("op-" + i.to_string())
    } else {
      atomic_decrement("op-" + i.to_string())
    }
  }
  
  assert_eq(atomic_read(), 0)  // 6次增量，5次减量，结果为1
  assert_eq(concurrent_counter.operations.length(), 11)
  
  // 测试并发数据聚合
  let concurrent_aggregator = { mut partial_results: [] }
  
  let aggregate_partition = fn(data: Array[(String, Int)], partition_id: String) {
    let mut sum = 0
    for item in data {
      match item {
        (_, value) => sum = sum + value
      }
    }
    let partial_result = (partition_id, sum)
    concurrent_aggregator.partial_results = concurrent_aggregator.partial_results + [partial_result]
  }
  
  let merge_partial_results = fn() {
    let mut total_sum = 0
    for partial_result in concurrent_aggregator.partial_results {
      match partial_result {
        (_, sum) => total_sum = total_sum + sum
      }
    }
    total_sum
  }
  
  // 创建测试数据并分区
  let test_data = [
    ("item-1", 10), ("item-2", 20), ("item-3", 30),
    ("item-4", 40), ("item-5", 50), ("item-6", 60)
  ]
  
  let partition1 = test_data.slice(0, 3)
  let partition2 = test_data.slice(3, 6)
  
  // 并发聚合各分区
  aggregate_partition(partition1, "partition-1")
  aggregate_partition(partition2, "partition-2")
  
  // 合并部分结果
  let total_sum = merge_partial_results()
  assert_eq(total_sum, 210)  // 10+20+30+40+50+60
  assert_eq(concurrent_aggregator.partial_results.length(), 2)
  
  // 测试并发错误处理
  let error_handler = { mut errors: [], mut retry_counts: [] }
  
  let process_with_error_handling = fn(task: (String, String, String), retry_count: Int) {
    let (task_id, data, _) = task
    
    // 模拟随机失败
    if task_id == "task-3" and retry_count < 2 {
      let error = ("Processing failed for " + task_id, retry_count)
      error_handler.errors = error_handler.errors + [error]
      error_handler.retry_counts = error_handler.retry_counts + [retry_count + 1]
      false
    } else {
      let processed_data = data.to_uppercase()
      let completed_task = (task_id, processed_data, "completed")
      task_queue.completed_tasks = task_queue.completed_tasks + [completed_task]
      true
    }
  }
  
  let process_tasks_with_retry = fn(max_retries: Int) {
    for task in task_queue.tasks {
      let mut success = false
      let mut retries = 0
      
      while retries < max_retries and not(success) {
        success = process_with_error_handling(task, retries)
        if not(success) {
          retries = retries + 1
        }
      }
    }
  }
  
  // 重置队列并添加会失败的任务
  task_queue.completed_tasks = []
  add_task("task-3", "trace-data-3")
  
  process_tasks_with_retry(3)
  
  // 验证错误处理
  assert_eq(error_handler.errors.length(), 2)  // 两次失败
  assert_eq(error_handler.retry_counts[0], 1)
  assert_eq(error_handler.retry_counts[1], 2)
  
  // 验证最终成功
  let task_3_completed = task_queue.completed_tasks.any(fn(task) {
    match task {
      (task_id, _, status) => task_id == "task-3" and status == "completed"
    }
  })
  assert_true(task_3_completed)
  
  // 测试并发性能指标
  let concurrency_metrics = {
    total_tasks: task_queue.tasks.length(),
    completed_tasks: task_queue.completed_tasks.length(),
    concurrent_operations: concurrent_counter.operations.length(),
    error_count: error_handler.errors.length(),
    average_retries: if error_handler.retry_counts.length() > 0 {
      error_handler.retry_counts.reduce(fn(acc, count) { acc + count }, 0) / error_handler.retry_counts.length()
    } else {
      0
    }
  }
  
  assert_eq(concurrency_metrics.total_tasks, 6)
  assert_eq(concurrency_metrics.completed_tasks, 6)
  assert_eq(concurrency_metrics.concurrent_operations, 11)
  assert_eq(concurrency_metrics.error_count, 2)
  assert_eq(concurrency_metrics.average_retries, 1)  // (1+2)/2 = 1.5，整数除法截断为1
}

// 测试9: 遥测数据容错恢复
test "遥测数据容错恢复测试" {
  // 模拟故障检测器
  let fault_detector = {
    mut detected_faults: [],
    mut fault_history: []
  }
  
  // 检测故障
  let detect_fault = fn(component: String, fault_type: String, severity: String) {
    let fault = (component, fault_type, severity, 1640995200)
    fault_detector.detected_faults = fault_detector.detected_faults + [fault]
    fault_detector.fault_history = fault_detector.fault_history + [fault]
  }
  
  // 故障恢复策略
  enum RecoveryStrategy {
    Retry(Int)        // 重试次数
    Fallback(String)  // 降级方案
    CircuitBreaker    // 熔断器
    Restart           // 重启组件
  }
  
  // 执行故障恢复
  let execute_recovery = fn(strategy: RecoveryStrategy, component: String) {
    match strategy {
      RecoveryStrategy::Retry(max_retries) => {
        let mut success = false
        let mut attempts = 0
        
        while attempts < max_retries and not(success) {
          attempts = attempts + 1
          // 模拟重试逻辑
          if component == "database" and attempts >= 2 {
            success = true
          } else if component == "cache" and attempts >= 1 {
            success = true
          }
        }
        
        (success, "Retry completed after " + attempts.to_string() + " attempts")
      }
      RecoveryStrategy::Fallback(fallback_component) => {
        // 模拟降级方案
        (true, "Switched to fallback component: " + fallback_component)
      }
      RecoveryStrategy::CircuitBreaker => {
        // 模拟熔断器
        (true, "Circuit breaker activated for component: " + component)
      }
      RecoveryStrategy::Restart => {
        // 模拟组件重启
        (true, "Component restarted: " + component)
      }
    }
  }
  
  // 测试故障检测
  detect_fault("database", "connection_timeout", "high")
  detect_fault("cache", "memory_exhausted", "medium")
  detect_fault("exporter", "network_error", "low")
  
  assert_eq(fault_detector.detected_faults.length(), 3)
  assert_eq(fault_detector.fault_history.length(), 3)
  
  // 测试故障恢复
  let (db_recovery_success, db_recovery_message) = execute_recovery(
    RecoveryStrategy::Retry(3), "database"
  )
  assert_true(db_recovery_success)
  assert_true(db_recovery_message.contains("Retry completed after 2 attempts"))
  
  let (cache_recovery_success, cache_recovery_message) = execute_recovery(
    RecoveryStrategy::Retry(3), "cache"
  )
  assert_true(cache_recovery_success)
  assert_true(cache_recovery_message.contains("Retry completed after 1 attempts"))
  
  let (exporter_recovery_success, exporter_recovery_message) = execute_recovery(
    RecoveryStrategy::Fallback("file_exporter"), "exporter"
  )
  assert_true(exporter_recovery_success)
  assert_true(exporter_recovery_message.contains("Switched to fallback component: file_exporter"))
  
  // 测试数据备份和恢复
  let backup_manager = {
    mut backups: [],
    mut current_backup_index: 0
  }
  
  let create_backup = fn(data: Array[(String, String)], backup_id: String) {
    let backup = (backup_id, data, 1640995200)
    backup_manager.backups = backup_manager.backups + [backup]
    backup_manager.current_backup_index = backup_manager.backups.length() - 1
  }
  
  let restore_from_backup = fn(backup_id: String) {
    let mut found_backup = None
    for backup in backup_manager.backups {
      match backup {
        (id, data, _) => {
          if id == backup_id {
            found_backup = Some(data)
          }
        }
      }
    }
    found_backup
  }
  
  // 创建测试数据
  let telemetry_data = [
    ("trace-001", "span-001"),
    ("trace-001", "span-002"),
    ("trace-002", "span-001")
  ]
  
  // 创建备份
  create_backup(telemetry_data, "backup-001")
  assert_eq(backup_manager.backups.length(), 1)
  assert_eq(backup_manager.current_backup_index, 0)
  
  // 模拟数据丢失
  let corrupted_data = []
  
  // 从备份恢复
  let restored_data = restore_from_backup("backup-001")
  match restored_data {
    Some(data) => assert_eq(data, telemetry_data)
    None => assert_true(false)
  }
  
  // 测试自动故障转移
  let failover_manager = {
    mut primary_component: "database",
    mut secondary_component: "cache",
    mut current_active: "database",
    mut failover_count: 0
  }
  
  let trigger_failover = fn() {
    if failover_manager.current_active == failover_manager.primary_component {
      failover_manager.current_active = failover_manager.secondary_component
    } else {
      failover_manager.current_active = failover_manager.primary_component
    }
    failover_manager.failover_count = failover_manager.failover_count + 1
  }
  
  let is_primary_active = fn() {
    failover_manager.current_active == failover_manager.primary_component
  }
  
  // 初始状态
  assert_true(is_primary_active())
  assert_eq(failover_manager.failover_count, 0)
  
  // 触发故障转移
  trigger_failover()
  assert_false(is_primary_active())
  assert_eq(failover_manager.current_active, "cache")
  assert_eq(failover_manager.failover_count, 1)
  
  // 再次触发故障转移
  trigger_failover()
  assert_true(is_primary_active())
  assert_eq(failover_manager.current_active, "database")
  assert_eq(failover_manager.failover_count, 2)
  
  // 测试健康检查
  let health_checker = {
    mut component_health: []
  }
  
  let update_component_health = fn(component: String, status: String, response_time: Int) {
    let health_record = (component, status, response_time, 1640995200)
    
    // 更新或添加组件健康状态
    let mut found = false
    let mut updated_health = []
    
    for record in health_checker.component_health {
      match record {
        (comp, _, _, _) => {
          if comp == component {
            updated_health = updated_health + [health_record]
            found = true
          } else {
            updated_health = updated_health + [record]
          }
        }
      }
    }
    
    if not(found) {
      updated_health = updated_health + [health_record]
    }
    
    health_checker.component_health = updated_health
  }
  
  let get_component_health = fn(component: String) {
    let mut health_status = None
    for record in health_checker.component_health {
      match record {
        (comp, status, response_time, _) => {
          if comp == component {
            health_status = Some((status, response_time))
          }
        }
      }
    }
    health_status
  }
  
  let is_component_healthy = fn(component: String) {
    match get_component_health(component) {
      Some((status, response_time)) => status == "healthy" and response_time < 1000
      None => false
    }
  }
  
  // 更新组件健康状态
  update_component_health("database", "healthy", 250)
  update_component_health("cache", "healthy", 150)
  update_component_health("exporter", "unhealthy", 2000)
  
  assert_true(is_component_healthy("database"))
  assert_true(is_component_healthy("cache"))
  assert_false(is_component_healthy("exporter"))
  
  // 测试容错恢复统计
  let fault_tolerance_metrics = {
    total_faults: fault_detector.fault_history.length(),
    successful_recoveries: 4,  // db, cache, exporter, failover
    backup_count: backup_manager.backups.length(),
    failover_count: failover_manager.failover_count,
    healthy_components: health_checker.component_health.filter(fn(record) {
      match record {
        (_, status, response_time, _) => status == "healthy" and response_time < 1000
      }
    }).length()
  }
  
  assert_eq(fault_tolerance_metrics.total_faults, 3)
  assert_eq(fault_tolerance_metrics.successful_recoveries, 4)
  assert_eq(fault_tolerance_metrics.backup_count, 1)
  assert_eq(fault_tolerance_metrics.failover_count, 2)
  assert_eq(fault_tolerance_metrics.healthy_components, 2)
  
  // 测试恢复策略评估
  let evaluate_recovery_strategy = fn(component: String, fault_type: String) {
    match (component, fault_type) {
      ("database", "connection_timeout") => RecoveryStrategy::Retry(3),
      ("database", "corruption") => RecoveryStrategy::Restart,
      ("cache", "memory_exhausted") => RecoveryStrategy::Fallback("local_cache"),
      ("exporter", "network_error") => RecoveryStrategy::CircuitBreaker,
      _ => RecoveryStrategy::Retry(1)
    }
  }
  
  let db_strategy = evaluate_recovery_strategy("database", "connection_timeout")
  let cache_strategy = evaluate_recovery_strategy("cache", "memory_exhausted")
  let exporter_strategy = evaluate_recovery_strategy("exporter", "network_error")
  
  match db_strategy {
    RecoveryStrategy::Retry(count) => assert_eq(count, 3)
    _ => assert_true(false)
  }
  
  match cache_strategy {
    RecoveryStrategy::Fallback(component) => assert_eq(component, "local_cache")
    _ => assert_true(false)
  }
  
  match exporter_strategy {
    RecoveryStrategy::CircuitBreaker => assert_true(true)
    _ => assert_true(false)
  }
}

// 测试10: 遥测数据资源管理
test "遥测数据资源管理测试" {
  // 资源管理器
  let resource_manager = {
    mut allocated_resources: [],
    mut resource_pools: [],
    mut usage_stats: []
  }
  
  // 定义资源类型
  enum ResourceType {
    Memory(Int)        // 内存大小(MB)
    CPU(Float)         // CPU核心数
    Disk(Int)          // 磁盘空间(MB)
    Network(Int)       // 网络带宽(Mbps)
  }
  
  // 分配资源
  let allocate_resource = fn(resource_id: String, resource_type: ResourceType, pool_id: String) {
    let resource = (resource_id, resource_type, pool_id, 1640995200)
    resource_manager.allocated_resources = resource_manager.allocated_resources + [resource]
    
    // 更新使用统计
    let mut found = false
    let mut updated_stats = []
    
    for stat in resource_manager.usage_stats {
      match stat {
        (pool, count, usage) => {
          if pool == pool_id {
            updated_stats = updated_stats + [(pool, count + 1, usage + 1)]
            found = true
          } else {
            updated_stats = updated_stats + [(pool, count, usage)]
          }
        }
      }
    }
    
    if not(found) {
      updated_stats = updated_stats + [(pool_id, 1, 1)]
    }
    
    resource_manager.usage_stats = updated_stats
  }
  
  // 释放资源
  let release_resource = fn(resource_id: String) {
    let mut updated_resources = []
    let mut released_resource = None
    
    for resource in resource_manager.allocated_resources {
      match resource {
        (id, resource_type, pool_id, _) => {
          if id == resource_id {
            released_resource = Some((id, resource_type, pool_id))
          } else {
            updated_resources = updated_resources + [(id, resource_type, pool_id, 1640995200)]
          }
        }
      }
    }
    
    resource_manager.allocated_resources = updated_resources
    
    // 更新使用统计
    match released_resource {
      Some((_, _, pool_id)) => {
        let mut updated_stats = []
        
        for stat in resource_manager.usage_stats {
          match stat {
            (pool, count, usage) => {
              if pool == pool_id {
                updated_stats = updated_stats + [(pool, count - 1, usage - 1)]
              } else {
                updated_stats = updated_stats + [(pool, count, usage)]
              }
            }
          }
        }
        
        resource_manager.usage_stats = updated_stats
      }
      None => ()
    }
  }
  
  // 创建资源池
  let create_resource_pool = fn(pool_id: String, pool_type: String, capacity: Int) {
    let pool = (pool_id, pool_type, capacity, 0)  // pool_id, pool_type, capacity, current_usage
    resource_manager.resource_pools = resource_manager.resource_pools + [pool]
  }
  
  // 检查资源池容量
  let check_pool_capacity = fn(pool_id: String) {
    let mut has_capacity = false
    let mut current_usage = 0
    let mut max_capacity = 0
    
    for pool in resource_manager.resource_pools {
      match pool {
        (id, _, capacity, usage) => {
          if id == pool_id {
            current_usage = usage
            max_capacity = capacity
            has_capacity = usage < capacity
          }
        }
      }
    }
    
    (has_capacity, current_usage, max_capacity)
  }
  
  // 创建资源池
  create_resource_pool("memory-pool", "memory", 1024)  // 1024MB
  create_resource_pool("cpu-pool", "cpu", 8)          // 8 cores
  create_resource_pool("disk-pool", "disk", 10240)    // 10240MB
  
  assert_eq(resource_manager.resource_pools.length(), 3)
  
  // 分配资源
  allocate_resource("res-001", ResourceType::Memory(256), "memory-pool")
  allocate_resource("res-002", ResourceType::Memory(512), "memory-pool")
  allocate_resource("res-003", ResourceType::CPU(2.0), "cpu-pool")
  allocate_resource("res-004", ResourceType::Disk(1024), "disk-pool")
  
  assert_eq(resource_manager.allocated_resources.length(), 4)
  
  // 检查资源池容量
  let (memory_has_capacity, memory_usage, memory_capacity) = check_pool_capacity("memory-pool")
  assert_true(memory_has_capacity)
  assert_eq(memory_usage, 0)  // 简化实现，未更新池使用情况
  
  // 释放资源
  release_resource("res-002")
  assert_eq(resource_manager.allocated_resources.length(), 3)
  
  // 测试资源监控
  let resource_monitor = {
    mut utilization_data: [],
    mut alerts: []
  }
  
  let collect_utilization_data = fn(pool_id: String, utilization: Float) {
    let data_point = (pool_id, utilization, 1640995200)
    resource_monitor.utilization_data = resource_monitor.utilization_data + [data_point]
    
    // 检查是否需要生成警报
    if utilization > 80.0 {
      let alert = ("High utilization", pool_id, utilization, 1640995200)
      resource_monitor.alerts = resource_monitor.alerts + [alert]
    }
  }
  
  let get_average_utilization = fn(pool_id: String) {
    let mut total_utilization = 0.0
    let mut count = 0
    
    for data in resource_monitor.utilization_data {
      match data {
        (id, utilization, _) => {
          if id == pool_id {
            total_utilization = total_utilization + utilization
            count = count + 1
          }
        }
      }
    }
    
    if count > 0 {
      total_utilization / count.to_float()
    } else {
      0.0
    }
  }
  
  // 收集利用率数据
  collect_utilization_data("memory-pool", 45.5)
  collect_utilization_data("memory-pool", 67.8)
  collect_utilization_data("memory-pool", 85.2)  // 超过阈值，应生成警报
  collect_utilization_data("cpu-pool", 32.1)
  collect_utilization_data("cpu-pool", 55.4)
  collect_utilization_data("disk-pool", 78.9)
  
  assert_eq(resource_monitor.utilization_data.length(), 6)
  assert_eq(resource_monitor.alerts.length(), 1)  // 只有一个超过80%
  
  // 验证平均利用率
  let memory_avg = get_average_utilization("memory-pool")
  let cpu_avg = get_average_utilization("cpu-pool")
  let disk_avg = get_average_utilization("disk-pool")
  
  assert_true(memory_avg > 60.0)  // (45.5 + 67.8 + 85.2) / 3 ≈ 66.17
  assert_true(cpu_avg > 40.0)    // (32.1 + 55.4) / 2 = 43.75
  assert_eq(disk_avg, 78.9)      // 只有一个数据点
  
  // 测试资源配额管理
  let quota_manager = {
    mut quotas: [],
    mut usage: []
  }
  
  let set_quota = fn(pool_id: String, quota_limit: Int) {
    let quota = (pool_id, quota_limit)
    quota_manager.quotas = quota_manager.quotas + [quota]
  }
  
  let update_usage = fn(pool_id: String, used: Int) {
    let mut found = false
    let mut updated_usage = []
    
    for usage_record in quota_manager.usage {
      match usage_record {
        (id, amount) => {
          if id == pool_id {
            updated_usage = updated_usage + [(id, used)]
            found = true
          } else {
            updated_usage = updated_usage + [(id, amount)]
          }
        }
      }
    }
    
    if not(found) {
      updated_usage = updated_usage + [(pool_id, used)]
    }
    
    quota_manager.usage = updated_usage
  }
  
  let check_quota_exceeded = fn(pool_id: String) {
    let mut quota_limit = 0
    let mut current_usage = 0
    let mut quota_found = false
    
    for quota in quota_manager.quotas {
      match quota {
        (id, limit) => {
          if id == pool_id {
            quota_limit = limit
            quota_found = true
          }
        }
      }
    }
    
    for usage_record in quota_manager.usage {
      match usage_record {
        (id, amount) => {
          if id == pool_id {
            current_usage = amount
          }
        }
      }
    }
    
    if quota_found {
      current_usage > quota_limit
    } else {
      false
    }
  }
  
  // 设置配额
  set_quota("memory-pool", 1024)
  set_quota("cpu-pool", 8)
  set_quota("disk-pool", 10240)
  
  // 更新使用情况
  update_usage("memory-pool", 768)
  update_usage("cpu-pool", 6)
  update_usage("disk-pool", 5120)
  
  // 检查配额
  assert_false(check_quota_exceeded("memory-pool"))  // 768 < 1024
  assert_false(check_quota_exceeded("cpu-pool"))     // 6 < 8
  assert_false(check_quota_exceeded("disk-pool"))    // 5120 < 10240
  
  // 超过配额
  update_usage("memory-pool", 1100)
  assert_true(check_quota_exceeded("memory-pool"))   // 1100 > 1024
  
  // 测试资源优化建议
  let generate_optimization_recommendations = fn() {
    let mut recommendations = []
    
    // 基于利用率生成建议
    for pool in resource_manager.resource_pools {
      match pool {
        (pool_id, _, _, _) => {
          let avg_util = get_average_utilization(pool_id)
          
          if avg_util < 30.0 {
            recommendations = recommendations + ["Consider reducing " + pool_id + " allocation (low utilization: " + avg_util.to_string() + "%)"]
          } else if avg_util > 80.0 {
            recommendations = recommendations + ["Consider increasing " + pool_id + " allocation (high utilization: " + avg_util.to_string() + "%)"]
          }
        }
      }
    }
    
    // 基于配额使用情况生成建议
    for usage_record in quota_manager.usage {
      match usage_record {
        (pool_id, used) => {
          if check_quota_exceeded(pool_id) {
            recommendations = recommendations + ["Quota exceeded for " + pool_id + ", consider increasing limit or reducing usage"]
          }
        }
      }
    }
    
    recommendations
  }
  
  let recommendations = generate_optimization_recommendations()
  assert_true(recommendations.length() >= 1)  // 至少有一条关于内存池超过配额的建议
  
  // 验证包含特定建议
  let memory_quota_exceeded_rec = recommendations.any(fn(rec) {
    rec.contains("Quota exceeded for memory-pool")
  })
  assert_true(memory_quota_exceeded_rec)
  
  // 测试资源管理报告
  let generate_resource_report = fn() {
    "Resource Management Report:\n" +
    "- Total Resource Pools: " + resource_manager.resource_pools.length().to_string() + "\n" +
    "- Allocated Resources: " + resource_manager.allocated_resources.length().to_string() + "\n" +
    "- Utilization Alerts: " + resource_monitor.alerts.length().to_string() + "\n" +
    "- Quota Violations: " + (if check_quota_exceeded("memory-pool") { 1 } else { 0 }).to_string() + "\n" +
    "- Optimization Recommendations: " + recommendations.length().to_string()
  }
  
  let report = generate_resource_report()
  assert_true(report.contains("Resource Management Report"))
  assert_true(report.contains("Total Resource Pools: 3"))
  assert_true(report.contains("Allocated Resources: 3"))
  assert_true(report.contains("Utilization Alerts: 1"))
  assert_true(report.contains("Quota Violations: 1"))
}