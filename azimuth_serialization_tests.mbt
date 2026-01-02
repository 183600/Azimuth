// 遥测数据序列化和反序列化测试用例
// 测试Azimuth遥测系统的数据序列化和反序列化功能

test "JSON序列化和反序列化" {
  // 测试JSON格式的序列化和反序列化
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "json.test")
  
  // 创建span
  let span = Tracer::start_span(tracer, "json.serialization.test")
  Span::set_attribute(span, "operation.type", "test")
  Span::set_attribute(span, "operation.duration", 150)
  Span::set_attribute(span, "operation.success", true)
  Span::add_event(span, "operation.started", [("timestamp", "2025-01-02T10:00:00Z")])
  Span::add_event(span, "operation.completed", [("result", "success")])
  
  // 序列化为JSON
  let json_serializer = JsonSerializer::new()
  let json_data = JsonSerializer::serialize_span(json_serializer, span)
  assert_true(json_data.length() > 0)
  assert_true(json_data.contains("\"name\":\"json.serialization.test\""))
  assert_true(json_data.contains("\"operation.type\":\"test\""))
  
  // 反序列化JSON
  let deserialized_span = JsonSerializer::deserialize_span(json_serializer, json_data)
  assert_true(deserialized_span != None)
  
  match deserialized_span {
    Some(span) => {
      assert_eq(Span::name(span), "json.serialization.test")
      assert_eq(Span::get_attribute(span, "operation.type"), Some("test"))
      assert_eq(Span::get_attribute(span, "operation.duration"), Some(150))
      assert_eq(Span::get_attribute(span, "operation.success"), Some(true))
    }
    None => assert_true(false)
  }
  
  // 测试度量数据的JSON序列化
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "json.metrics.test")
  
  let counter = Meter::create_counter(meter, "requests.total", Some("Total requests"), Some("count"))
  Counter::add_with_attributes(counter, 42.0, [("method", "GET"), ("status", "200")])
  
  let json_metrics = JsonSerializer::serialize_counter(json_serializer, counter)
  assert_true(json_metrics.contains("\"name\":\"requests.total\""))
  assert_true(json_metrics.contains("\"value\":42.0"))
  assert_true(json_metrics.contains("\"method\":\"GET\""))
  
  // 反序列化度量数据
  let deserialized_counter = JsonSerializer::deserialize_counter(json_serializer, json_metrics)
  assert_true(deserialized_counter != None)
  
  match deserialized_counter {
    Some(counter) => {
      assert_eq(Counter::get_name(counter), "requests.total")
      assert_eq(Counter::get_value(counter), 42.0)
      assert_eq(Counter::get_attribute(counter, "method"), Some("GET"))
    }
    None => assert_true(false)
  }
  
  Span::end(span)
  assert_true(true)
}

test "Protocol Buffers序列化" {
  // 测试Protocol Buffers格式的序列化
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "protobuf.test")
  
  // 创建span
  let span = Tracer::start_span(tracer, "protobuf.serialization.test")
  Span::set_attribute(span, "service.name", "payment-service")
  Span::set_attribute(span, "service.version", "1.2.3")
  Span::set_attribute(span, "trace.id", "trace-123456")
  Span::set_attribute(span, "span.id", "span-789012")
  
  // 序列化为Protocol Buffers
  let protobuf_serializer = ProtobufSerializer::new()
  let protobuf_data = ProtobufSerializer::serialize_span(protobuf_serializer, span)
  assert_true(protobuf_data.length() > 0)
  
  // 反序列化Protocol Buffers
  let deserialized_span = ProtobufSerializer::deserialize_span(protobuf_serializer, protobuf_data)
  assert_true(deserialized_span != None)
  
  match deserialized_span {
    Some(span) => {
      assert_eq(Span::name(span), "protobuf.serialization.test")
      assert_eq(Span::get_attribute(span, "service.name"), Some("payment-service"))
      assert_eq(Span::get_attribute(span, "service.version"), Some("1.2.3"))
      assert_eq(Span::get_attribute(span, "trace.id"), Some("trace-123456"))
      assert_eq(Span::get_attribute(span, "span.id"), Some("span-789012"))
    }
    None => assert_true(false)
  }
  
  // 测试批量span序列化
  let spans = []
  for i in 1..=5 {
    let batch_span = Tracer::start_span(tracer, "batch.span." + i.to_string())
    Span::set_attribute(batch_span, "batch.id", "batch-001")
    Span::set_attribute(batch_span, "span.index", i)
    spans = spans.push(batch_span)
  }
  
  let batch_protobuf = ProtobufSerializer::serialize_span_batch(protobuf_serializer, spans)
  assert_true(batch_protobuf.length() > protobuf_data.length()) // 批量数据应该更大
  
  // 反序列化批量span
  let deserialized_batch = ProtobufSerializer::deserialize_span_batch(protobuf_serializer, batch_protobuf)
  assert_eq(deserialized_batch.length(), 5)
  
  for i in 0..=4 {
    let span = deserialized_batch[i]
    assert_eq(Span::name(span), "batch.span." + (i + 1).to_string())
    assert_eq(Span::get_attribute(span, "batch.id"), Some("batch-001"))
    assert_eq(Span::get_attribute(span, "span.index"), Some(i + 1))
  }
  
  // 结束所有span
  for span in spans {
    Span::end(span)
  }
  Span::end(span)
  
  assert_true(true)
}

test "跨格式序列化兼容性" {
  // 测试跨格式序列化兼容性
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross_format.test")
  
  // 创建带复杂属性的span
  let span = Tracer::start_span(tracer, "cross.format.test")
  Span::set_attribute(span, "string.attr", "test value")
  Span::set_attribute(span, "int.attr", 42)
  Span::set_attribute(span, "float.attr", 3.14)
  Span::set_attribute(span, "bool.attr", true)
  Span::set_attribute(span, "array.attr", [1, 2, 3])
  Span::set_attribute(span, "map.attr", [("key1", "value1"), ("key2", "value2")])
  
  Span::add_event(span, "test.event", [
    ("event.type", "test"),
    ("event.data", "{\"nested\": \"json\"}"),
    ("timestamp", "2025-01-02T10:00:00Z")
  ])
  
  // 序列化为JSON
  let json_serializer = JsonSerializer::new()
  let json_data = JsonSerializer::serialize_span(json_serializer, span)
  
  // 从JSON反序列化
  let json_deserialized = JsonSerializer::deserialize_span(json_serializer, json_data)
  
  // 重新序列化为Protocol Buffers
  let protobuf_serializer = ProtobufSerializer::new()
  match json_deserialized {
    Some(deserialized_span) => {
      let protobuf_data = ProtobufSerializer::serialize_span(protobuf_serializer, deserialized_span)
      
      // 从Protocol Buffers反序列化
      let final_span = ProtobufSerializer::deserialize_span(protobuf_serializer, protobuf_data)
      
      match final_span {
        Some(span) => {
          // 验证跨序列化的数据完整性
          assert_eq(Span::name(span), "cross.format.test")
          assert_eq(Span::get_attribute(span, "string.attr"), Some("test value"))
          assert_eq(Span::get_attribute(span, "int.attr"), Some(42))
          assert_eq(Span::get_attribute(span, "float.attr"), Some(3.14))
          assert_eq(Span::get_attribute(span, "bool.attr"), Some(true))
          
          // 验证事件数据
          let events = Span::get_events(span)
          assert_true(events.length() > 0)
          
          let event = events[0]
          assert_eq(Event::get_name(event), "test.event")
          assert_eq(Event::get_attribute(event, "event.type"), Some("test"))
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  Span::end(span)
  assert_true(true)
}

test "压缩序列化数据" {
  // 测试压缩序列化数据
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "compression.test")
  
  // 创建大量span用于压缩测试
  let spans = []
  for i in 1..=100 {
    let span = Tracer::start_span(tracer, "compression.test.span." + i.to_string())
    Span::set_attribute(span, "service.name", "compression-test-service")
    Span::set_attribute(span, "service.version", "1.0.0")
    Span::set_attribute(span, "span.index", i)
    Span::set_attribute(span, "large.data", "x".repeat(1000)) // 每个span包含1KB的重复数据
    
    // 添加多个事件
    for j in 1..=5 {
      Span::add_event(span, "event." + j.to_string(), [
        ("event.index", j),
        ("event.data", "event data " + j.to_string())
      ])
    }
    
    spans = spans.push(span)
  }
  
  // 序列化未压缩的数据
  let json_serializer = JsonSerializer::new()
  let uncompressed_json = JsonSerializer::serialize_span_batch(json_serializer, spans)
  let uncompressed_size = uncompressed_json.length()
  
  // 压缩序列化数据
  let compressed_json = JsonSerializer::serialize_span_batch_compressed(json_serializer, spans)
  let compressed_size = compressed_json.length()
  
  // 验证压缩效果
  assert_true(compressed_size < uncompressed_size)
  let compression_ratio = compressed_size.to_float() / uncompressed_size.to_float()
  assert_true(compression_ratio < 0.8) // 至少20%的压缩率
  
  // 解压缩并反序列化
  let decompressed_spans = JsonSerializer::deserialize_span_batch_decompressed(json_serializer, compressed_json)
  assert_eq(decompressed_spans.length(), 100)
  
  // 验证解压缩后的数据完整性
  for i in 0..=99 {
    let original_span = spans[i]
    let decompressed_span = decompressed_spans[i]
    
    assert_eq(Span::name(decompressed_span), Span::name(original_span))
    assert_eq(Span::get_attribute(decompressed_span, "service.name"), Span::get_attribute(original_span, "service.name"))
    assert_eq(Span::get_attribute(decompressed_span, "span.index"), Span::get_attribute(original_span, "span.index"))
    
    let original_events = Span::get_events(original_span)
    let decompressed_events = Span::get_events(decompressed_span)
    assert_eq(original_events.length(), decompressed_events.length())
  }
  
  // 测试不同压缩算法
  let gzip_compressed = JsonSerializer::serialize_span_batch_with_compression(json_serializer, spans, "gzip")
  let deflate_compressed = JsonSerializer::serialize_span_batch_with_compression(json_serializer, spans, "deflate")
  
  assert_true(gzip_compressed.length() != deflate_compressed.length())
  
  // 结束所有span
  for span in spans {
    Span::end(span)
  }
  
  assert_true(true)
}

test "序列化版本兼容性" {
  // 测试序列化版本兼容性
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "version.test")
  
  // 创建当前版本的span
  let span = Tracer::start_span(tracer, "version.compatibility.test")
  Span::set_attribute(span, "service.name", "version-test-service")
  Span::set_attribute(span, "service.version", "1.0.0")
  Span::set_attribute(span, "new.field", "new value") // 新版本特有的字段
  
  // 使用当前版本序列化
  let current_serializer = JsonSerializer::new_with_version("2.0.0")
  let current_data = current_serializer.serialize_span(span)
  
  // 使用旧版本反序列化（模拟向后兼容）
  let old_serializer = JsonSerializer::new_with_version("1.0.0")
  let backward_compatible = old_serializer.deserialize_span(current_data)
  
  match backward_compatible {
    Some(span) => {
      // 旧版本应该能够读取基本字段
      assert_eq(Span::name(span), "version.compatibility.test")
      assert_eq(Span::get_attribute(span, "service.name"), Some("version-test-service"))
      assert_eq(Span::get_attribute(span, "service.version"), Some("1.0.0"))
      
      // 新字段可能被忽略或处理为默认值
      let new_field = Span::get_attribute(span, "new.field")
      // 旧版本可能无法处理新字段，这是可以接受的
    }
    None => assert_true(false)
  }
  
  // 测试向前兼容（旧版本数据在新版本中读取）
  let old_span = Tracer::start_span(tracer, "old.version.test")
  Span::set_attribute(old_span, "service.name", "old-version-service")
  // 不包含新字段
  
  let old_serializer_legacy = JsonSerializer::new_with_version("1.0.0")
  let old_data = old_serializer_legacy.serialize_span(old_span)
  
  let new_serializer = JsonSerializer::new_with_version("2.0.0")
  let forward_compatible = new_serializer.deserialize_span(old_data)
  
  match forward_compatible {
    Some(span) => {
      // 新版本应该能够读取旧版本数据
      assert_eq(Span::name(span), "old.version.test")
      assert_eq(Span::get_attribute(span, "service.name"), Some("old-version-service"))
      
      // 新字段应该有默认值
      let new_field = Span::get_attribute(span, "new.field")
      // 新字段应该有默认值或为None
    }
    None => assert_true(false)
  }
  
  // 测试版本迁移
  let migrator = VersionMigrator::new("1.0.0", "2.0.0")
  VersionMigrator::add_field_mapping(migrator, "old_field", "new_field")
  VersionMigrator::add_default_value(migrator, "new_field", "default value")
  
  let migrated_span = VersionMigrator::migrate_span(migrator, old_data)
  assert_true(migrated_span != None)
  
  Span::end(span)
  Span::end(old_span)
  assert_true(true)
}

test "序列化性能和内存优化" {
  // 测试序列化性能和内存优化
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance.test")
  
  // 创建大量span用于性能测试
  let large_spans = []
  for i in 1..=1000 {
    let span = Tracer::start_span(tracer, "performance.test.span." + i.to_string())
    Span::set_attribute(span, "service.name", "performance-test-service")
    Span::set_attribute(span, "span.index", i)
    Span::set_attribute(span, "payload.data", "performance test data " + i.to_string())
    
    large_spans = large_spans.push(span)
  }
  
  // 测试序列化性能
  let serializer = JsonSerializer::new()
  let start_time = Time::now()
  
  let serialized_data = serializer.serialize_span_batch_optimized(large_spans)
  
  let end_time = Time::now()
  let serialization_time = end_time - start_time
  
  // 验证序列化在合理时间内完成（例如小于1秒）
  assert_true(serialization_time < 1000000000L) // 1秒
  
  // 测试内存使用优化
  let memory_before = Memory::get_usage()
  let deserialized_spans = serializer.deserialize_span_batch_streaming(serialized_data)
  let memory_after = Memory::get_usage()
  
  // 流式反序列化应该使用更少的内存
  let memory_used = memory_after - memory_before
  let expected_memory = serialized_data.length() * 2 // 估计的内存使用上限
  assert_true(memory_used < expected_memory)
  
  // 验证反序列化结果
  assert_eq(deserialized_spans.length(), 1000)
  
  // 测试增量序列化
  let incremental_serializer = IncrementalSerializer::new()
  for i in 0..=999 {
    let span = large_spans[i]
    incremental_serializer.add_span(incremental_serializer, span)
  }
  
  let incremental_data = incremental_serializer.finalize(incremental_serializer)
  assert_true(incremental_data.length() > 0)
  
  // 测试序列化缓存
  let cached_serializer = CachedSerializer::new(100) // 缓存100个序列化结果
  let test_span = Tracer::start_span(tracer, "cached.span")
  Span::set_attribute(test_span, "cache.test", "value")
  
  // 第一次序列化（应该缓存结果）
  let first_serialization = cached_serializer.serialize_span(cached_serializer, test_span)
  
  // 第二次序列化相同span（应该使用缓存）
  let second_serialization = cached_serializer.serialize_span(cached_serializer, test_span)
  
  assert_eq(first_serialization, second_serialization)
  
  // 验证缓存命中
  let cache_stats = cached_serializer.get_cache_stats(cached_serializer)
  assert_true(cache_stats.hits > 0)
  
  // 结束所有span
  for span in large_spans {
    Span::end(span)
  }
  Span::end(test_span)
  
  assert_true(true)
}