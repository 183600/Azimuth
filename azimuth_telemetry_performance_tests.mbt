// Azimuth Telemetry Performance Test Suite
// This file contains comprehensive test cases for telemetry system performance

// Test 1: High-Volume Span Processing
test "high-volume span processing throughput" {
  // Define span structure
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation: String,
    start_time: Int,
    end_time: Int,
    tags: Array<(String, String)>
  }
  
  // Generate high volume of spans
  let generate_spans = fn(count: Int) {
    let mut spans = []
    for i in 0..count {
      let span = {
        trace_id: "trace-" + (i % 1000).to_string(),
        span_id: "span-" + i.to_string(),
        parent_span_id: if i > 0 { Some("span-" + (i - 1).to_string()) } else { None },
        operation: "operation_" + (i % 10).to_string(),
        start_time: 1640995200 + i * 10,
        end_time: 1640995200 + i * 10 + 5,
        tags: [("service", "service-" + (i % 5).to_string()), ("env", "prod")]
      }
      spans = spans.push(span)
    }
    spans
  }
  
  // Generate 1000 spans
  let spans = generate_spans(1000)
  assert_eq(spans.length(), 1000)
  
  // Test batch processing performance
  let process_batch = fn(batch: Array[Span>, batch_size: Int) {
    let mut processed = 0
    let start_time = 1640999999  // Mock current time
    
    for i in 0..batch.length() {
      if i % batch_size == 0 {
        // Process batch
        processed = processed + batch_size
      }
    }
    
    let end_time = 1650999999  // Mock end time
    let duration = end_time - start_time
    let throughput = (processed as Float) / (duration as Float)
    
    { processed, duration, throughput }
  }
  
  let batch_result = process_batch(spans, 100)
  assert_eq(batch_result.processed, 1000)
  assert_eq(batch_result.duration, 10000000)  // 10,000,000 seconds (mock)
  assert_eq(batch_result.throughput, 1000.0 / 10000000.0)
  
  // Test memory usage estimation
  let estimate_memory_usage = fn(spans: Array[Span>) {
    let base_span_size = 100  // Base size in bytes
    let tag_size = 20         // Size per tag
    let total_size = spans.reduce(0, fn(acc, span) {
      acc + base_span_size + span.tags.length() * tag_size
    })
    total_size
  }
  
  let memory_usage = estimate_memory_usage(spans)
  assert_eq(memory_usage, 1000 * (100 + 2 * 20))  // 1000 spans * (100 base + 2 tags * 20 bytes)
}

// Test 2: Concurrent Span Collection
test "concurrent span collection performance" {
  // Define concurrent collector
  type ConcurrentCollector = {
    buffer: Array[Span>,
    buffer_size: Int,
    flush_count: Int
  }
  
  // Simulate concurrent span collection
  let simulate_concurrent_collection = fn(num_collectors: Int, spans_per_collector: Int) {
    let mut total_spans = 0
    let mut total_buffers = []
    
    for collector_id in 0..num_collectors {
      let mut collector_buffer = []
      
      for span_id in 0..spans_per_collector {
        let span = {
          trace_id: "trace-" + collector_id.to_string() + "-" + span_id.to_string(),
          span_id: "span-" + collector_id.to_string() + "-" + span_id.to_string(),
          parent_span_id: None,
          operation: "concurrent_operation",
          start_time: 1640995200 + span_id,
          end_time: 1640995200 + span_id + 1,
          tags: [("collector", collector_id.to_string())]
        }
        collector_buffer = collector_buffer.push(span)
      }
      
      total_buffers = total_buffers.push(collector_buffer)
      total_spans = total_spans + spans_per_collector
    }
    
    { total_spans, buffers: total_buffers }
  }
  
  let collection_result = simulate_concurrent_collection(10, 100)
  assert_eq(collection_result.total_spans, 1000)
  assert_eq(collection_result.buffers.length(), 10)
  
  // Test buffer merging performance
  let merge_buffers = fn(buffers: Array[Array[Span>>) {
    let start_time = 1640999999
    let mut merged = []
    
    for buffer in buffers {
      merged = merged + buffer
    }
    
    let end_time = 1650999999
    let duration = end_time - start_time
    
    { merged_count: merged.length(), duration }
  }
  
  let merge_result = merge_buffers(collection_result.buffers)
  assert_eq(merge_result.merged_count, 1000)
  assert_eq(merge_result.duration, 10000000)
  
  // Test concurrent flush simulation
  let simulate_concurrent_flush = fn(buffers: Array[Array[Span>>, flush_size: Int) {
    let mut flush_operations = 0
    
    for buffer in buffers {
      let mut remaining = buffer.length()
      while remaining > 0 {
        let current_flush = if remaining < flush_size { remaining } else { flush_size }
        remaining = remaining - current_flush
        flush_operations = flush_operations + 1
      }
    }
    
    flush_operations
  }
  
  let flush_count = simulate_concurrent_flush(collection_result.buffers, 50)
  assert_eq(flush_count, 20)  // 10 collectors * (100 spans / 50 flush_size)
}

// Test 3: Metric Aggregation Performance
test "metric aggregation performance under load" {
  // Define metric structure
  type Metric = {
    name: String,
    value: Float,
    labels: Array<(String, String)>,
    timestamp: Int
  }
  
  // Generate high volume of metrics
  let generate_metrics = fn(count: Int) {
    let mut metrics = []
    for i in 0..count {
      let metric = {
        name: "metric_" + (i % 10).to_string(),
        value: (i as Float) * 1.5,
        labels: [
          ("service", "service-" + (i % 5).to_string()),
          ("env", "prod"),
          ("instance", "instance-" + (i % 20).to_string())
        ],
        timestamp: 1640995200 + i
      }
      metrics = metrics.push(metric)
    }
    metrics
  }
  
  let metrics = generate_metrics(10000)
  assert_eq(metrics.length(), 10000)
  
  // Test aggregation by labels
  let aggregate_by_labels = fn(metrics: Array[Metric], label_keys: Array[String>) {
    let start_time = 1640999999
    let groups = Map::empty()
    
    for metric in metrics {
      let label_values = label_keys.map(fn(key) {
        match metric.labels.find(fn(l) { l.0 == key }) {
          Some((_, value)) => value
          None => "unknown"
        }
      })
      let group_key = label_values.join("|")
      
      let existing = match Map::get(groups, group_key) {
        Some(values) => values
        None => []
      }
      
      let _ = Map::insert(groups, group_key, existing.push(metric.value))
    }
    
    let end_time = 1650999999
    let duration = end_time - start_time
    
    { groups, duration }
  }
  
  let aggregation_result = aggregate_by_labels(metrics, ["service", "env"])
  assert_true(aggregation_result.groups.size() > 0)
  assert_eq(aggregation_result.duration, 10000000)
  
  // Test percentile calculation performance
  let calculate_percentiles = fn(values: Array[Float], percentiles: Array<Float]) {
    let start_time = 1640999999
    
    let sorted_values = values.sort(fn(a, b) {
      if a < b { -1 } else if a > b { 1 } else { 0 }
    })
    
    let results = percentiles.map(fn(p) {
      let index = ((p / 100.0) * (sorted_values.length() as Float)) as Int
      if index >= sorted_values.length() {
        sorted_values[sorted_values.length() - 1]
      } else {
        sorted_values[index]
      }
    })
    
    let end_time = 1650999999
    let duration = end_time - start_time
    
    { percentiles: results, duration }
  }
  
  let test_values = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
  let percentile_result = calculate_percentiles(test_values, [50.0, 90.0, 95.0, 99.0])
  assert_eq(percentile_result.percentiles.length(), 4)
  assert_eq(percentile_result.duration, 10000000)
  
  // Test time-series aggregation
  let aggregate_time_series = fn(metrics: Array[Metric], window_size: Int) {
    let start_time = 1640999999
    let time_windows = Map::empty()
    
    for metric in metrics {
      let window_start = (metric.timestamp / window_size) * window_size
      let window_key = window_start.to_string()
      
      let existing = match Map::get(time_windows, window_key) {
        Some(values) => values
        None => []
      }
      
      let _ = Map::insert(time_windows, window_key, existing.push(metric.value))
    }
    
    let end_time = 1650999999
    let duration = end_time - start_time
    
    { windows: time_windows, duration }
  }
  
  let time_series_result = aggregate_time_series(metrics, 60)  // 60-second windows
  assert_true(time_series_result.windows.size() > 0)
  assert_eq(time_series_result.duration, 10000000)
}

// Test 4: Serialization Performance
test "telemetry data serialization performance" {
  // Define telemetry data structure
  type TelemetryData = {
    spans: Array[Span],
    metrics: Array[Metric],
    metadata: Array<(String, String)>
  }
  
  // Create test data
  let spans = []
  for i in 0..1000 {
    spans = spans.push({
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      parent_span_id: None,
      operation: "test_operation",
      start_time: 1640995200 + i,
      end_time: 1640995200 + i + 5,
      tags: [("test", "true")]
    })
  }
  
  let metrics = []
  for i in 0..1000 {
    metrics = metrics.push({
      name: "test_metric",
      value: (i as Float),
      labels: [("index", i.to_string())],
      timestamp: 1640995200 + i
    })
  }
  
  let telemetry_data = {
    spans,
    metrics,
    metadata: [("version", "1.0.0"), ("source", "performance_test")]
  }
  
  // Test JSON serialization performance
  let serialize_to_json = fn(data: TelemetryData) {
    let start_time = 1640999999
    
    // Simulate JSON serialization
    let json_string = "{"
    json_string = json_string + "\"spans\":["
    for i in 0..data.spans.length() {
      if i > 0 { json_string = json_string + "," }
      let span = data.spans[i]
      json_string = json_string + "{"
      json_string = json_string + "\"trace_id\":\"" + span.trace_id + "\"," 
      json_string = json_string + "\"span_id\":\"" + span.span_id + "\"," 
      json_string = json_string + "\"operation\":\"" + span.operation + "\""
      json_string = json_string + "}"
    }
    json_string = json_string + "],"
    json_string = json_string + "\"metrics_count\":" + data.metrics.length().to_string()
    json_string = json_string + "}"
    
    let end_time = 1650999999
    let duration = end_time - start_time
    
    { json_size: json_string.length(), duration }
  }
  
  let json_result = serialize_to_json(telemetry_data)
  assert_true(json_result.json_size > 0)
  assert_eq(json_result.duration, 10000000)
  
  // Test binary serialization performance
  let serialize_to_binary = fn(data: TelemetryData) {
    let start_time = 1640999999
    
    // Simulate binary serialization
    let mut binary_size = 0
    binary_size = binary_size + data.spans.length() * 50  // 50 bytes per span
    binary_size = binary_size + data.metrics.length() * 30  // 30 bytes per metric
    binary_size = binary_size + data.metadata.length() * 20  // 20 bytes per metadata
    
    let end_time = 1650999999
    let duration = end_time - start_time
    
    { binary_size, duration }
  }
  
  let binary_result = serialize_to_binary(telemetry_data)
  assert_eq(binary_result.binary_size, 1000 * 50 + 1000 * 30 + 2 * 20)
  assert_eq(binary_result.duration, 10000000)
  
  // Compare serialization efficiency
  let compression_ratio = (binary_result.binary_size as Float) / (json_result.json_size as Float)
  assert_true(compression_ratio < 1.0)  // Binary should be more compact
}

// Test 5: Memory Usage Optimization
test "memory usage optimization for telemetry data" {
  // Define memory pool for span allocation
  type SpanPool = {
    pool: Array[Span>,
    allocated: Int,
    capacity: Int
  }
  
  // Create span pool
  let create_span_pool = fn(capacity: Int) {
    let pool = []
    for i in 0..capacity {
      pool = pool.push({
        trace_id: "",
        span_id: "",
        parent_span_id: None,
        operation: "",
        start_time: 0,
        end_time: 0,
        tags: []
      })
    }
    { pool, allocated: 0, capacity }
  }
  
  let span_pool = create_span_pool(1000)
  assert_eq(span_pool.capacity, 1000)
  assert_eq(span_pool.allocated, 0)
  
  // Test span allocation from pool
  let allocate_span = fn(pool: SpanPool, trace_id: String, span_id: String, operation: String) {
    if pool.allocated < pool.capacity {
      let span = pool.pool[pool.allocated]
      let updated_span = {
        trace_id,
        span_id,
        parent_span_id: None,
        operation,
        start_time: 1640995200,
        end_time: 1640995210,
        tags: []
      }
      { span: updated_span, updated_pool: { pool | allocated: pool.allocated + 1 } }
    } else {
      { span: pool.pool[0], updated_pool: pool }  // Return default span
    }
  }
  
  let allocation_result = allocate_span(span_pool, "trace-123", "span-456", "test_operation")
  assert_eq(allocation_result.span.trace_id, "trace-123")
  assert_eq(allocation_result.span.span_id, "span-456")
  assert_eq(allocation_result.span.operation, "test_operation")
  assert_eq(allocation_result.updated_pool.allocated, 1)
  
  // Test string interning for common values
  type StringInterner = {
    strings: Map::empty(),
    next_id: Int
  }
  
  let create_interner = fn() {
    { strings: Map::empty(), next_id: 0 }
  }
  
  let intern_string = fn(interner: StringInterner, value: String) {
    match Map::get(interner.strings, value) {
      Some(id) => { id, interner }
      None => {
        let new_id = interner.next_id
        let updated_interner = {
          strings: Map::insert(interner.strings, value, new_id),
          next_id: new_id + 1
        }
        (new_id, updated_interner)
      }
    }
  }
  
  let string_interner = create_interner()
  
  // Intern common strings
  let (service_id, interner1) = intern_string(string_interner, "payment-service")
  let (env_id, interner2) = intern_string(interner1, "production")
  let (service_id2, interner3) = intern_string(interner2, "payment-service")  // Should reuse
  
  assert_eq(service_id, service_id2)  // Same string should have same ID
  assert_eq(interner3.next_id, 2)    // Only 2 unique strings
  
  // Test memory usage estimation with optimization
  let estimate_optimized_memory = fn(spans: Array[Span>, interner: StringInterner) {
    let base_span_size = 100
    let tag_size = 20
    let string_interning_savings = 10  // Savings per interned string
    
    spans.reduce(0, fn(acc, span) {
      let interned_tags = span.tags.filter(fn(tag) {
        Map::contains_key(interner.strings, tag.0) || Map::contains_key(interner.strings, tag.1)
      })
      let savings = interned_tags.length() * string_interning_savings
      acc + base_span_size + span.tags.length() * tag_size - savings
    })
  }
  
  let optimized_memory = estimate_optimized_memory([{
    trace_id: "trace-123",
    span_id: "span-456",
    parent_span_id: None,
    operation: "test_operation",
    start_time: 1640995200,
    end_time: 1640995210,
    tags: [("service", "payment-service"), ("env", "production")]
  }], interner3)
  
  assert_eq(optimized_memory, 100 + 2 * 20 - 2 * 10)  // Base + 2 tags - 2 interned savings
}

// Test 6: Cache Performance for Telemetry Data
test "cache performance for frequently accessed telemetry data" {
  // Define cache structure
  type Cache = {
    entries: Map::empty(),
    max_size: Int,
    hits: Int,
    misses: Int
  }
  
  // Create cache
  let create_cache = fn(max_size: Int) {
    { entries: Map::empty(), max_size, hits: 0, misses: 0 }
  }
  
  // Test cache get operation
  let cache_get = fn(cache: Cache, key: String) {
    match Map::get(cache.entries, key) {
      Some(value) => {
        ({ cache | hits: cache.hits + 1 }, Some(value))
      }
      None => {
        ({ cache | misses: cache.misses + 1 }, None)
      }
    }
  }
  
  // Test cache put operation
  let cache_put = fn(cache: Cache, key: String, value: String) {
    if Map::size(cache.entries) >= cache.max_size {
      // Simple eviction: remove first entry (in real implementation, use LRU)
      let keys = Map::keys(cache.entries)
      if keys.length() > 0 {
        let first_key = keys[0]
        let entries_without_first = Map::remove(cache.entries, first_key)
        { entries: Map::insert(entries_without_first, key, value), max_size: cache.max_size, hits: cache.hits, misses: cache.misses }
      } else {
        { entries: Map::insert(cache.entries, key, value), max_size: cache.max_size, hits: cache.hits, misses: cache.misses }
      }
    } else {
      { entries: Map::insert(cache.entries, key, value), max_size: cache.max_size, hits: cache.hits, misses: cache.misses }
    }
  }
  
  let cache = create_cache(100)
  
  // Test cache operations
  let (cache1, _) = cache_get(cache, "trace-123")  // Miss
  assert_eq(cache1.misses, 1)
  assert_eq(cache1.hits, 0)
  
  let cache2 = cache_put(cache1, "trace-123", "cached_trace_data")
  assert_eq(Map::size(cache2.entries), 1)
  
  let (cache3, result) = cache_get(cache2, "trace-123")  // Hit
  assert_eq(cache3.hits, 1)
  assert_eq(cache3.misses, 1)
  assert_eq(result, Some("cached_trace_data"))
  
  // Test cache performance with multiple operations
  let simulate_cache_workload = fn(cache: Cache, operations: Array<(String, String)>) {
    let mut final_cache = cache
    
    for (key, value) in operations {
      // Try to get from cache first
      let (updated_cache, cached_value) = cache_get(final_cache, key)
      
      match cached_value {
        Some(_) => {
          // Cache hit, use cached value
          final_cache = updated_cache
        }
        None => {
          // Cache miss, put value in cache
          final_cache = cache_put(updated_cache, key, value)
        }
      }
    }
    
    final_cache
  }
  
  let cache_operations = [
    ("trace-001", "data-001"),
    ("trace-002", "data-002"),
    ("trace-001", "data-001"),  // Should hit cache
    ("trace-003", "data-003"),
    ("trace-002", "data-002"),  // Should hit cache
    ("trace-004", "data-004"),
    ("trace-001", "data-001"),  // Should hit cache
    ("trace-005", "data-005")
  ]
  
  let final_cache = simulate_cache_workload(cache, cache_operations)
  
  // Calculate hit rate
  let total_requests = final_cache.hits + final_cache.misses
  let hit_rate = (final_cache.hits as Float) / (total_requests as Float) * 100.0
  
  assert_eq(final_cache.hits, 3)  // 3 cache hits
  assert_eq(final_cache.misses, 5)  // 5 cache misses
  assert_eq(hit_rate, 37.5)  // 3/8 * 100
}

// Test 7: Batch Processing Performance
test "batch processing performance for telemetry data" {
  // Define batch processor
  type BatchProcessor = {
    batch_size: Int,
    max_wait_time: Int,
    processed_batches: Int,
    total_items: Int
  }
  
  // Create batch processor
  let create_processor = fn(batch_size: Int, max_wait_time: Int) {
    {
      batch_size,
      max_wait_time,
      processed_batches: 0,
      total_items: 0
    }
  }
  
  // Test batch processing
  let process_batches = fn(processor: BatchProcessor, items: Array[String>) {
    let mut current_processor = processor
    let mut current_batch = []
    let mut last_flush_time = 1640995200
    
    for item in items {
      current_batch = current_batch.push(item)
      
      // Check if batch should be flushed
      let current_time = 1640995200 + current_batch.length() * 10
      let should_flush = current_batch.length() >= current_processor.batch_size ||
                        (current_time - last_flush_time) >= current_processor.max_wait_time
      
      if should_flush && current_batch.length() > 0 {
        // Process batch
        current_processor = {
          processed_batches: current_processor.processed_batches + 1,
          total_items: current_processor.total_items + current_batch.length(),
          batch_size: current_processor.batch_size,
          max_wait_time: current_processor.max_wait_time
        }
        current_batch = []
        last_flush_time = current_time
      }
    }
    
    // Process remaining items
    if current_batch.length() > 0 {
      current_processor = {
        processed_batches: current_processor.processed_batches + 1,
        total_items: current_processor.total_items + current_batch.length(),
        batch_size: current_processor.batch_size,
        max_wait_time: current_processor.max_wait_time
      }
    }
    
    current_processor
  }
  
  // Generate test items
  let test_items = []
  for i in 0..1000 {
    test_items = test_items.push("item-" + i.to_string())
  }
  
  // Test with different batch sizes
  let processor1 = create_processor(100, 5000)  // 100 items per batch, 5s max wait
  let result1 = process_batches(processor1, test_items)
  
  assert_eq(result1.total_items, 1000)
  assert_eq(result1.processed_batches, 10)  // 1000 / 100
  
  let processor2 = create_processor(150, 5000)  // 150 items per batch, 5s max wait
  let result2 = process_batches(processor2, test_items)
  
  assert_eq(result2.total_items, 1000)
  assert_eq(result2.processed_batches, 7)  // ceil(1000 / 150)
  
  let processor3 = create_processor(1000, 1)  // 1000 items per batch, 1s max wait
  let result3 = process_batches(processor3, test_items)
  
  assert_eq(result3.total_items, 1000)
  assert_eq(result3.processed_batches, 1000)  // Each item triggers flush due to time
  
  // Test batch processing efficiency
  let calculate_efficiency = fn(result: BatchProcessor) {
    let avg_batch_size = (result.total_items as Float) / (result.processed_batches as Float)
    let efficiency = avg_batch_size / 1000.0 * 100.0  // Percentage of ideal batch size
    { avg_batch_size, efficiency }
  }
  
  let efficiency1 = calculate_efficiency(result1)
  let efficiency2 = calculate_efficiency(result2)
  let efficiency3 = calculate_efficiency(result3)
  
  assert_eq(efficiency1.avg_batch_size, 100.0)
  assert_eq(efficiency1.efficiency, 10.0)
  
  assert_eq(efficiency2.avg_batch_size.round(), 143.0)  // 1000 / 7
  assert_eq(efficiency2.efficiency.round(), 14.3)
  
  assert_eq(efficiency3.avg_batch_size, 1.0)
  assert_eq(efficiency3.efficiency, 0.1)
}

// Test 8: Performance Monitoring and Metrics
test "performance monitoring and metrics collection" {
  // Define performance metrics
  type PerformanceMetrics = {
    operation_count: Int,
    total_duration: Int,
    min_duration: Int,
    max_duration: Int,
    memory_usage: Int,
    cpu_usage: Float
  }
  
  // Create performance monitor
  let create_performance_monitor = fn() {
    {
      operation_count: 0,
      total_duration: 0,
      min_duration: 999999999,
      max_duration: 0,
      memory_usage: 0,
      cpu_usage: 0.0
    }
  }
  
  // Simulate operation execution with monitoring
  let execute_with_monitoring = fn(monitor: PerformanceMetrics, operation: () -> Unit) {
    let start_time = 1640995200
    let start_memory = 1024 * 1024 * 100  // 100MB
    let start_cpu = 25.5
    
    // Execute operation
    operation()
    
    let end_time = 1640995210
    let end_memory = 1024 * 1024 * 105  // 105MB
    let end_cpu = 35.2
    
    let duration = end_time - start_time
    let memory_delta = end_memory - start_memory
    let cpu_delta = end_cpu - start_cpu
    
    {
      operation_count: monitor.operation_count + 1,
      total_duration: monitor.total_duration + duration,
      min_duration: if duration < monitor.min_duration { duration } else { monitor.min_duration },
      max_duration: if duration > monitor.max_duration { duration } else { monitor.max_duration },
      memory_usage: monitor.memory_usage + memory_delta,
      cpu_usage: monitor.cpu_usage + cpu_delta
    }
  }
  
  // Test performance monitoring
  let monitor = create_performance_monitor()
  
  let monitor1 = execute_with_monitoring(monitor, fn() {
    // Simulate operation 1
  })
  
  let monitor2 = execute_with_monitoring(monitor1, fn() {
    // Simulate operation 2
  })
  
  let monitor3 = execute_with_monitoring(monitor2, fn() {
    // Simulate operation 3
  })
  
  assert_eq(monitor3.operation_count, 3)
  assert_eq(monitor3.total_duration, 30)  // 3 operations * 10 seconds each
  assert_eq(monitor3.min_duration, 10)
  assert_eq(monitor3.max_duration, 10)
  assert_eq(monitor3.memory_usage, 15 * 1024 * 1024)  // 3 operations * 5MB each
  assert_eq(monitor3.cpu_usage, 29.1)  // Sum of CPU deltas
  
  // Calculate performance statistics
  let calculate_performance_stats = fn(metrics: PerformanceMetrics) {
    if metrics.operation_count == 0 {
      { avg_duration: 0.0, avg_memory: 0.0, avg_cpu: 0.0 }
    } else {
      {
        avg_duration: (metrics.total_duration as Float) / (metrics.operation_count as Float),
        avg_memory: (metrics.memory_usage as Float) / (metrics.operation_count as Float),
        avg_cpu: metrics.cpu_usage / (metrics.operation_count as Float)
      }
    }
  }
  
  let stats = calculate_performance_stats(monitor3)
  assert_eq(stats.avg_duration, 10.0)
  assert_eq(stats.avg_memory, 5.0 * 1024.0 * 1024.0)  // 5MB per operation
  assert_eq(stats.avg_cpu, 9.7)  // 29.1 / 3
  
  // Test performance threshold detection
  let check_performance_thresholds = fn(metrics: PerformanceMetrics, max_avg_duration: Float, max_avg_memory: Int, max_avg_cpu: Float) {
    let stats = calculate_performance_stats(metrics)
    
    let duration_ok = stats.avg_duration <= max_avg_duration
    let memory_ok = stats.avg_memory <= (max_avg_memory as Float)
    let cpu_ok = stats.avg_cpu <= max_avg_cpu
    
    {
      within_thresholds: duration_ok && memory_ok && cpu_ok,
      duration_violation: not(duration_ok),
      memory_violation: not(memory_ok),
      cpu_violation: not(cpu_ok)
    }
  }
  
  let threshold_check = check_performance_thresholds(monitor3, 15.0, 10 * 1024 * 1024, 15.0)
  assert_true(threshold_check.within_thresholds)
  assert_false(threshold_check.duration_violation)
  assert_false(threshold_check.memory_violation)
  assert_false(threshold_check.cpu_violation)
  
  let strict_threshold_check = check_performance_thresholds(monitor3, 5.0, 3 * 1024 * 1024, 5.0)
  assert_false(strict_threshold_check.within_thresholds)
  assert_true(strict_threshold_check.duration_violation)
  assert_true(strict_threshold_check.memory_violation)
  assert_true(strict_threshold_check.cpu_violation)
}