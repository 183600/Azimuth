// Azimuth Performance and Concurrent Safety Tests
// 性能和并发安全测试，验证系统在高负载和并发场景下的表现

// 测试1: 批量操作性能优化
test "batch operations performance optimization" {
  // 创建大量属性的批量操作测试
  let start_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  
  // 批量创建属性
  let mut batch_attributes = []
  for i = 0; i < 1000; i = i + 1 {
    batch_attributes = batch_attributes + [("batch.attr." + i.to_string(), @azimuth.StringValue("batch.value." + i.to_string()))]
  }
  
  let attributes_creation_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  let attributes_creation_duration = attributes_creation_time - start_time
  
  // 验证批量操作时间在合理范围内（假设小于100ms）
  assert_true(attributes_creation_duration < 100000000L) // 100ms in nanoseconds
  
  // 批量创建Baggage条目
  let mut batch_baggage = @azimuth.Baggage::new()
  for i = 0; i < 500; i = i + 1 {
    batch_baggage = @azimuth.Baggage::set_entry(batch_baggage, "batch.baggage." + i.to_string(), "batch.value." + i.to_string())
  }
  
  let baggage_creation_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  let baggage_creation_duration = baggage_creation_time - attributes_creation_time
  
  // 验证Baggage批量操作时间
  assert_true(baggage_creation_duration < 50000000L) // 50ms in nanoseconds
  
  // 批量创建Span事件
  let mut batch_events = []
  for i = 0; i < 100; i = i + 1 {
    batch_events = batch_events + [Event({
      name: "batch.event." + i.to_string(),
      timestamp: 1704067200000000000L + i.to_long(),
      attributes: [("event.index", IntValue(i)), ("event.data", StringValue("batch.data." + i.to_string()))]
    })]
  }
  
  let events_creation_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  let events_creation_duration = events_creation_time - baggage_creation_time
  
  // 验证事件批量操作时间
  assert_true(events_creation_duration < 20000000L) // 20ms in nanoseconds
  
  // 验证批量操作结果
  assert_eq(batch_attributes.length(), 1000)
  assert_true(@azimuth.Baggage::get_entry(batch_baggage, "batch.baggage.499") != None)
  assert_eq(batch_events.length(), 100)
  
  // 验证性能指标
  let total_duration = events_creation_time - start_time
  let attributes_per_ms = 1000.0 / (attributes_creation_duration.to_double() / 1000000.0)
  let baggage_per_ms = 500.0 / (baggage_creation_duration.to_double() / 1000000.0)
  let events_per_ms = 100.0 / (events_creation_duration.to_double() / 1000000.0)
  
  // 验证每毫秒处理的操作数
  assert_true(attributes_per_ms > 10.0) // 至少每毫秒处理10个属性
  assert_true(baggage_per_ms > 10.0)   // 至少每毫秒处理10个baggage条目
  assert_true(events_per_ms > 5.0)     // 至少每毫秒处理5个事件
}

// 测试2: 内存使用优化
test "memory usage optimization" {
  // 测试内存高效的数据结构
  let memory_efficient_attributes = @azimuth.Attributes::with_capacity(1000)
  
  // 预分配容量的属性设置
  for i = 0; i < 1000; i = i + 1 {
    @azimuth.Attributes::set(memory_efficient_attributes, "memory.attr." + i.to_string(), @azimuth.StringValue("memory.value." + i.to_string()))
  }
  
  // 验证内存高效属性
  assert_true(@azimuth.Attributes::size(memory_efficient_attributes) == 1000)
  
  // 测试内存高效的Baggage
  let memory_efficient_baggage = @azimuth.Baggage::with_capacity(500)
  
  for i = 0; i < 500; i = i + 1 {
    memory_efficient_baggage = @azimuth.Baggage::set_entry(memory_efficient_baggage, "memory.baggage." + i.to_string(), "memory.value." + i.to_string())
  }
  
  // 验证内存高效Baggage
  assert_true(@azimuth.Baggage::size(memory_efficient_baggage) == 500)
  
  // 测试内存高效的Span
  let memory_efficient_span = Span({
    name: "memory.efficient.span",
    kind: Internal,
    recording: true,
    span_context: SpanContext({
      trace_id: "1234567890abcdef1234567890abcdef",
      span_id: "1234567890abcdef",
      sampled: true,
      trace_state: ""
    }),
    parent_span_id: None,
    attributes: memory_efficient_attributes,
    events: [],
    links: [],
    status: Unset,
    start_time: 1704067200000000000L,
    end_time: None,
    duration: None
  })
  
  // 验证内存高效Span
  assert_eq(memory_efficient_span.name, "memory.efficient.span")
  assert_eq(memory_efficient_span.attributes.length(), 1000)
  
  // 测试内存清理和释放
  let large_attributes = @azimuth.Attributes::new()
  for i = 0; i < 10000; i = i + 1 {
    @azimuth.Attributes::set(large_attributes, "large.attr." + i.to_string(), @azimuth.StringValue("large.value." + i.to_string()))
  }
  
  // 验证大属性集
  assert_true(@azimuth.Attributes::size(large_attributes) == 10000)
  
  // 清理大属性集（模拟内存释放）
  @azimuth.Attributes::clear(large_attributes)
  
  // 验证清理结果
  assert_true(@azimuth.Attributes::size(large_attributes) == 0)
  
  // 测试对象池模式（如果支持）
  let span_pool = @azimuth.SpanPool::new(100) // 预创建100个Span对象
  
  // 从池中获取Span
  let pooled_span = @azimuth.SpanPool::acquire(span_pool)
  
  // 使用Span
  let used_span = Span({
    name: "pooled.span",
    kind: Internal,
    recording: true,
    span_context: SpanContext({
      trace_id: "pooled1234567890abcdef1234567890",
      span_id: "pooled1234567890",
      sampled: true,
      trace_state: ""
    }),
    parent_span_id: None,
    attributes: [],
    events: [],
    links: [],
    status: Unset,
    start_time: 1704067200000000000L,
    end_time: None,
    duration: None
  })
  
  // 归还Span到池中
  @azimuth.SpanPool::release(span_pool, used_span)
  
  // 验证池状态
  assert_true(@azimuth.SpanPool::available_count(span_pool) > 0)
}

// 测试3: 并发安全的数据访问
test "concurrent safe data access" {
  // 创建共享资源
  let shared_attributes = @azimuth.Attributes::new()
  let shared_baggage = @azimuth.Baggage::new()
  let shared_resource = @azimuth.Resource::new()
  
  // 模拟并发写入操作
  let concurrent_writes = 100
  
  // 并发设置属性（模拟）
  for i = 0; i < concurrent_writes; i = i + 1 {
    // 在真实环境中，这些操作会在不同线程中执行
    let thread_id = i.to_string()
    @azimuth.Attributes::set(shared_attributes, "thread." + thread_id + ".attr", @azimuth.IntValue(i))
  }
  
  // 并发设置Baggage条目（模拟）
  for i = 0; i < concurrent_writes; i = i + 1 {
    let thread_id = i.to_string()
    shared_baggage = @azimuth.Baggage::set_entry(shared_baggage, "thread." + thread_id + ".baggage", "value." + thread_id)
  }
  
  // 验证并发写入结果
  assert_true(@azimuth.Attributes::size(shared_attributes) == concurrent_writes)
  assert_true(@azimuth.Baggage::size(shared_baggage) == concurrent_writes)
  
  // 模拟并发读取操作
  let concurrent_reads = 50
  
  // 并发读取属性（模拟）
  for i = 0; i < concurrent_reads; i = i + 1 {
    let read_index = (i * 2) % concurrent_writes // 确保读取存在的键
    let attr_key = "thread." + read_index.to_string() + ".attr"
    
    match @azimuth.Attributes::get(shared_attributes, attr_key) {
      Some(@azimuth.IntValue(value)) => assert_eq(value, read_index)
      _ => assert_true(false)
    }
  }
  
  // 并发读取Baggage条目（模拟）
  for i = 0; i < concurrent_reads; i = i + 1 {
    let read_index = (i * 2) % concurrent_writes
    let baggage_key = "thread." + read_index.to_string() + ".baggage"
    
    match @azimuth.Baggage::get_entry(shared_baggage, baggage_key) {
      Some(value) => assert_eq(value, "value." + read_index.to_string())
      None => assert_true(false)
    }
  }
  
  // 模拟并发读写混合操作
  let mixed_operations = 25
  
  for i = 0; i < mixed_operations; i = i + 1 {
    // 写操作
    let write_key = "mixed.attr." + i.to_string()
    @azimuth.Attributes::set(shared_attributes, write_key, @azimuth.StringValue("mixed.value." + i.to_string()))
    
    // 读操作
    let read_index = i % concurrent_writes
    let read_key = "thread." + read_index.to_string() + ".attr"
    
    match @azimuth.Attributes::get(shared_attributes, read_key) {
      Some(@azimuth.IntValue(value)) => assert_eq(value, read_index)
      _ => assert_true(false)
    }
  }
  
  // 验证混合操作结果
  assert_true(@azimuth.Attributes::size(shared_attributes) == concurrent_writes + mixed_operations)
  
  // 测试原子操作
  let atomic_counter = @azimuth.AtomicCounter::new(0)
  
  // 并发原子递增（模拟）
  for i = 0; i < 1000; i = i + 1 {
    @azimuth.AtomicCounter::increment(atomic_counter)
  }
  
  // 验证原子操作结果
  assert_eq(@azimuth.AtomicCounter::get(atomic_counter), 1000)
  
  // 测试原子比较和交换
  let atomic_value = @azimuth.AtomicValue::new(42)
  
  // 原子比较和交换（模拟）
  let swap_result = @azimuth.AtomicValue::compare_and_swap(atomic_value, 42, 100)
  assert_true(swap_result) // 应该成功交换
  
  let current_value = @azimuth.AtomicValue::get(atomic_value)
  assert_eq(current_value, 100)
  
  // 再次尝试交换（应该失败）
  let swap_result2 = @azimuth.AtomicValue::compare_and_swap(atomic_value, 42, 200)
  assert_false(swap_result2) // 应该失败，因为当前值不是42
}

// 测试4: 高频操作性能
test "high frequency operations performance" {
  // 测试高频属性操作
  let high_freq_attributes = @azimuth.Attributes::new()
  let operation_count = 10000
  
  let start_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  
  // 高频设置属性
  for i = 0; i < operation_count; i = i + 1 {
    @azimuth.Attributes::set(high_freq_attributes, "high.freq.attr." + i.to_string(), @azimuth.IntValue(i))
  }
  
  let set_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  let set_duration = set_time - start_time
  
  // 高频获取属性
  for i = 0; i < operation_count; i = i + 1 {
    let attr_key = "high.freq.attr." + i.to_string()
    match @azimuth.Attributes::get(high_freq_attributes, attr_key) {
      Some(@azimuth.IntValue(value)) => assert_eq(value, i)
      _ => assert_true(false)
    }
  }
  
  let get_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  let get_duration = get_time - set_time
  
  // 高频删除属性
  for i = 0; i < operation_count; i = i + 1 {
    let attr_key = "high.freq.attr." + i.to_string()
    @azimuth.Attributes::remove(high_freq_attributes, attr_key)
  }
  
  let remove_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  let remove_duration = remove_time - get_time
  
  // 验证高频操作性能
  let total_duration = remove_time - start_time
  let ops_per_second = operation_count.to_double() / (total_duration.to_double() / 1000000000.0) * 3.0 // 3种操作
  
  // 验证每秒操作数（应该至少1000 ops/sec）
  assert_true(ops_per_second > 1000.0)
  
  // 验证删除结果
  assert_true(@azimuth.Attributes::size(high_freq_attributes) == 0)
  
  // 测试高频Span操作
  let high_freq_span_count = 1000
  let high_freq_spans = []
  
  let span_start_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  
  // 高频创建Span
  for i = 0; i < high_freq_span_count; i = i + 1 {
    let span = Span({
      name: "high.freq.span." + i.to_string(),
      kind: Internal,
      recording: true,
      span_context: SpanContext({
        trace_id: "1234567890abcdef1234567890abcdef",
        span_id: "12345678" + i.to_string().pad_left(8, '0'),
        sampled: true,
        trace_state: ""
      }),
      parent_span_id: None,
      attributes: [("span.index", IntValue(i))],
      events: [],
      links: [],
      status: Unset,
      start_time: 1704067200000000000L + i.to_long(),
      end_time: None,
      duration: None
    })
    
    high_freq_spans = high_freq_spans + [span]
  }
  
  let span_creation_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  let span_creation_duration = span_creation_time - span_start_time
  
  // 验证高频Span创建性能
  let span_ops_per_second = high_freq_span_count.to_double() / (span_creation_duration.to_double() / 1000000000.0)
  
  // 验证每秒Span创建数（应该至少100 spans/sec）
  assert_true(span_ops_per_second > 100.0)
  
  // 验证Span创建结果
  assert_eq(high_freq_spans.length(), high_freq_span_count)
}

// 测试5: 资源池化和重用
test "resource pooling and reuse" {
  // 创建Span对象池
  let span_pool = @azimuth.SpanPool::new(50)
  
  // 验证初始池状态
  assert_eq(@azimuth.SpanPool::total_count(span_pool), 50)
  assert_eq(@azimuth.SpanPool::available_count(span_pool), 50)
  
  // 获取多个Span对象
  let acquired_spans = []
  
  for i = 0; i < 30; i = i + 1 {
    let span = @azimuth.SpanPool::acquire(span_pool)
    acquired_spans = acquired_spans + [span]
  }
  
  // 验证获取后的池状态
  assert_eq(@azimuth.SpanPool::available_count(span_pool), 20)
  assert_eq(acquired_spans.length(), 30)
  
  // 使用获取的Span对象
  for i = 0; i < acquired_spans.length(); i = i + 1 {
    let span = acquired_spans[i]
    // 重置Span状态
    @azimuth.Span::reset(span)
    
    // 设置新的Span属性
    @azimuth.Span::set_name(span, "reused.span." + i.to_string())
    @azimuth.Span::set_kind(span, @azimuth.Internal)
  }
  
  // 归还Span对象到池中
  for span in acquired_spans {
    @azimuth.SpanPool::release(span_pool, span)
  }
  
  // 验证归还后的池状态
  assert_eq(@azimuth.SpanPool::available_count(span_pool), 50)
  
  // 创建属性对象池
  let attributes_pool = @azimuth.AttributesPool::new(100)
  
  // 验证属性池初始状态
  assert_eq(@azimuth.AttributesPool::total_count(attributes_pool), 100)
  assert_eq(@azimuth.AttributesPool::available_count(attributes_pool), 100)
  
  // 获取并使用属性对象
  let acquired_attributes = []
  
  for i = 0; i < 50; i = i + 1 {
    let attrs = @azimuth.AttributesPool::acquire(attributes_pool)
    @azimuth.Attributes::clear(attrs) // 清理之前的状态
    @azimuth.Attributes::set(attrs, "pool.attr." + i.to_string(), @azimuth.StringValue("pool.value." + i.to_string()))
    acquired_attributes = acquired_attributes + [attrs]
  }
  
  // 验证属性池状态
  assert_eq(@azimuth.AttributesPool::available_count(attributes_pool), 50)
  
  // 归还属性对象
  for attrs in acquired_attributes {
    @azimuth.Attributes::clear(attrs) // 清理状态
    @azimuth.AttributesPool::release(attributes_pool, attrs)
  }
  
  // 验证归还后的属性池状态
  assert_eq(@azimuth.AttributesPool::available_count(attributes_pool), 100)
  
  // 测试池化性能优势
  let pool_start_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  
  // 使用池化对象创建大量Span
  for i = 0; i < 200; i = i + 1 {
    let span = @azimuth.SpanPool::acquire(span_pool)
    @azimuth.Span::reset(span)
    @azimuth.Span::set_name(span, "pooled.span." + i.to_string())
    @azimuth.SpanPool::release(span_pool, span)
  }
  
  let pool_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  let pool_duration = pool_time - pool_start_time
  
  // 直接创建大量Span（不使用池）
  let direct_start_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  
  for i = 0; i < 200; i = i + 1 {
    let span = Span({
      name: "direct.span." + i.to_string(),
      kind: Internal,
      recording: true,
      span_context: SpanContext({
        trace_id: "1234567890abcdef1234567890abcdef",
        span_id: "12345678" + i.to_string().pad_left(8, '0'),
        sampled: true,
        trace_state: ""
      }),
      parent_span_id: None,
      attributes: [],
      events: [],
      links: [],
      status: Unset,
      start_time: 1704067200000000000L,
      end_time: None,
      duration: None
    })
    // Span对象会被垃圾回收
  }
  
  let direct_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  let direct_duration = direct_time - direct_start_time
  
  // 验证池化性能优势（池化应该更快）
  assert_true(pool_duration < direct_duration)
  
  // 计算性能提升百分比
  let performance_improvement = (direct_duration.to_double() - pool_duration.to_double()) / direct_duration.to_double() * 100.0
  
  // 验证至少有10%的性能提升
  assert_true(performance_improvement > 10.0)
}

// 测试6: 内存泄漏检测
test "memory leak detection" {
  // 创建大量临时对象并释放
  let leak_test_iterations = 1000
  
  for iteration = 0; iteration < leak_test_iterations; iteration = iteration + 1 {
    // 创建临时属性
    let temp_attributes = @azimuth.Attributes::new()
    
    for i = 0; i < 100; i = i + 1 {
      @azimuth.Attributes::set(temp_attributes, "temp.attr." + i.to_string(), @azimuth.StringValue("temp.value." + i.to_string()))
    }
    
    // 使用属性
    match @azimuth.Attributes::get(temp_attributes, "temp.attr.50") {
      Some(@azimuth.StringValue(value)) => assert_eq(value, "temp.value.50")
      _ => assert_true(false)
    }
    
    // 清理属性（应该释放内存）
    @azimuth.Attributes::clear(temp_attributes)
    
    // 创建临时Baggage
    let temp_baggage = @azimuth.Baggage::new()
    
    for i = 0; i < 50; i = i + 1 {
      temp_baggage = @azimuth.Baggage::set_entry(temp_baggage, "temp.baggage." + i.to_string(), "temp.value." + i.to_string())
    }
    
    // 使用Baggage
    match @azimuth.Baggage::get_entry(temp_baggage, "temp.baggage.25") {
      Some(value) => assert_eq(value, "temp.value.25")
      None => assert_true(false)
    }
    
    // 清理Baggage（应该释放内存）
    temp_baggage = @azimuth.Baggage::new() // 创建新的空Baggage替换旧的
    
    // 创建临时Span
    let temp_span = Span({
      name: "temp.span." + iteration.to_string(),
      kind: Internal,
      recording: true,
      span_context: SpanContext({
        trace_id: "temp1234567890abcdef1234567890",
        span_id: "temp1234567890",
        sampled: true,
        trace_state: ""
      }),
      parent_span_id: None,
      attributes: temp_attributes,
      events: [],
      links: [],
      status: Unset,
      start_time: 1704067200000000000L + iteration.to_long(),
      end_time: None,
      duration: None
    })
    
    // 使用Span
    assert_eq(temp_span.name, "temp.span." + iteration.to_string())
    
    // Span会在作用域结束时被垃圾回收
  }
  
  // 验证内存使用情况（在真实环境中会使用内存监控工具）
  // 这里我们只是确保操作完成而没有崩溃
  assert_true(true)
  
  // 测试循环引用检测（如果适用）
  let span_a = Span({
    name: "span.a",
    kind: Internal,
    recording: true,
    span_context: SpanContext({
      trace_id: "cycle1234567890abcdef1234567890",
      span_id: "span1234567890",
      sampled: true,
      trace_state: ""
    }),
    parent_span_id: None,
    attributes: [],
    events: [],
    links: [],
    status: Unset,
    start_time: 1704067200000000000L,
    end_time: None,
    duration: None
  })
  
  let span_b = Span({
    name: "span.b",
    kind: Internal,
    recording: true,
    span_context: SpanContext({
      trace_id: "cycle1234567890abcdef1234567890",
      span_id: "span0987654321",
      sampled: true,
      trace_state: ""
    }),
    parent_span_id: None,
    attributes: [],
    events: [],
    links: [Link({
      linked_span_context: span_a.span_context,
      attributes: [("link.type", StringValue("circular"))]
    })],
    status: Unset,
    start_time: 1704067200000000001L,
    end_time: None,
    duration: None
  })
  
  // 创建循环引用（如果支持）
  span_a.links = span_a.links + [Link({
    linked_span_context: span_b.span_context,
    attributes: [("link.type", StringValue("circular"))]
  })]
  
  // 验证循环引用不会导致内存泄漏
  assert_eq(span_a.links.length(), 1)
  assert_eq(span_b.links.length(), 1)
  
  // 在真实环境中，这里会检查垃圾回收器是否能正确处理循环引用
}

// 测试7: 缓存性能优化
test "cache performance optimization" {
  // 创建LRU缓存
  let lru_cache = @azimuth.LRUCache::new(100) // 容量100
  
  // 验证初始缓存状态
  assert_eq(@azimuth.LRUCache::size(lru_cache), 0)
  assert_true(@azimuth.LRUCache::is_empty(lru_cache))
  
  // 填充缓存
  for i = 0; i < 150; i = i + 1 {
    let key = "cache.key." + i.to_string()
    let value = "cache.value." + i.to_string()
    @azimuth.LRUCache::put(lru_cache, key, value)
  }
  
  // 验证缓存大小（应该不超过容量）
  assert_eq(@azimuth.LRUCache::size(lru_cache), 100)
  assert_false(@azimuth.LRUCache::is_empty(lru_cache))
  
  // 验证最近的项目在缓存中
  match @azimuth.LRUCache::get(lru_cache, "cache.key.149") {
    Some(value) => assert_eq(value, "cache.value.149")
    None => assert_true(false)
  }
  
  // 验证最旧的项目不在缓存中
  match @azimuth.LRUCache::get(lru_cache, "cache.key.49") {
    Some(_) => assert_true(false) // 应该已被淘汰
    None => assert_true(true)
  }
  
  // 测试缓存命中率
  let cache_hits = 0
  let cache_misses = 0
  
  // 访问缓存中的项目
  for i = 100; i < 150; i = i + 1 {
    let key = "cache.key." + i.to_string()
    match @azimuth.LRUCache::get(lru_cache, key) {
      Some(_) => cache_hits = cache_hits + 1
      None => cache_misses = cache_misses + 1
    }
  }
  
  // 访问不在缓存中的项目
  for i = 0; i < 50; i = i + 1 {
    let key = "cache.key." + i.to_string()
    match @azimuth.LRUCache::get(lru_cache, key) {
      Some(_) => cache_hits = cache_hits + 1
      None => cache_misses = cache_misses + 1
    }
  }
  
  // 验证缓存命中率
  assert_eq(cache_hits, 50) // 100-149在缓存中
  assert_eq(cache_misses, 50) // 0-49不在缓存中
  
  // 测试缓存性能
  let cache_start_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  
  // 大量缓存操作
  for i = 0; i < 10000; i = i + 1 {
    let key = "perf.key." + (i % 200).to_string() // 重复访问200个不同的键
    let value = "perf.value." + i.to_string()
    
    match @azimuth.LRUCache::get(lru_cache, key) {
      Some(_) => {
        // 缓存命中，更新值
        @azimuth.LRUCache::put(lru_cache, key, value)
      }
      None => {
        // 缓存未命中，添加新值
        @azimuth.LRUCache::put(lru_cache, key, value)
      }
    }
  }
  
  let cache_end_time = @azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())
  let cache_duration = cache_end_time - cache_start_time
  
  // 验证缓存性能（应该至少10000 ops/sec）
  let cache_ops_per_second = 10000.0 / (cache_duration.to_double() / 1000000000.0)
  assert_true(cache_ops_per_second > 10000.0)
  
  // 测试缓存清理
  @azimuth.LRUCache::clear(lru_cache)
  
  // 验证清理结果
  assert_eq(@azimuth.LRUCache::size(lru_cache), 0)
  assert_true(@azimuth.LRUCache::is_empty(lru_cache))
  
  // 测试带过期时间的缓存
  let ttl_cache = @azimuth.TTLCache::new(100, 5000) // 容量100，TTL 5秒
  
  // 添加项目到TTL缓存
  @azimuth.TTLCache::put(ttl_cache, "ttl.key.1", "ttl.value.1")
  @azimuth.TTLCache::put(ttl_cache, "ttl.key.2", "ttl.value.2")
  
  // 验证TTL缓存中的项目
  match @azimuth.TTLCache::get(ttl_cache, "ttl.key.1") {
    Some(value) => assert_eq(value, "ttl.value.1")
    None => assert_true(false)
  }
  
  // 模拟时间流逝（在真实环境中会使用时间模拟）
  // 这里我们只是验证TTL缓存的基本功能
  
  // 手动清理过期项目
  @azimuth.TTLCache::cleanup_expired(ttl_cache)
  
  // 验证缓存状态
  assert_eq(@azimuth.TTLCache::size(ttl_cache), 2) // 项目应该仍然存在（假设未过期）
}