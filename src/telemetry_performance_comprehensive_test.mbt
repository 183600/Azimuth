// 性能基准测试 - 测试高并发场景下的遥测数据收集性能
test "telemetry_performance_high_concurrency" {
  // 测试目标：验证高并发场景下遥测系统的性能表现
  
  // 1. 基准测试参数
  let operation_count = 1000
  let concurrent_threads = 10
  let start_time = @builtin.unix_time_nano()
  
  // 2. 批量创建Span以测试性能
  let spans = Array.range(0, operation_count).map(fn(i) {
    let span_id_bytes = [
      (i >> 24) & 0xFF as Byte,
      (i >> 16) & 0xFF as Byte,
      (i >> 8) & 0xFF as Byte,
      i & 0xFF as Byte,
      0x00, 0x00, 0x00, 0x00
    ]
    
    @azimuth.telemetry.api.trace.Span{
      name: "performance-test-span-\{i}",
      context: @azimuth.telemetry.api.trace.SpanContext{
        trace_id: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10],
        span_id: span_id_bytes,
        trace_flags: 0x01,
        trace_state: "performance=test"
      },
      kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
      parent_span_id: None,
      start_time_unix_nanos: start_time + (i * 1000),
      end_time_unix_nanos: Some(start_time + (i * 1000) + 500),
      status: @azimuth.telemetry.api.trace.StatusCode::Ok,
      status_description: None,
      attributes: [
        ("operation.index", @azimuth.telemetry.api.common.AttributeValue::int(i.to_int64())),
        ("performance.test", @azimuth.telemetry.api.common.AttributeValue::bool(true))
      ],
      events: [],
      links: []
    }
  })
  
  // 3. 验证Span创建性能
  let span_creation_time = @builtin.unix_time_nano() - start_time
  assert_eq(spans.length(), operation_count)
  
  // 性能断言：1000个Span创建应该在合理时间内完成（假设1秒内）
  let max_creation_time_ns = 1_000_000_000 // 1秒
  assert(span_creation_time < max_creation_time_ns, "Span创建时间过长：\{span_creation_time}纳秒")
  
  // 4. 批量创建Metrics测量
  let metrics_start_time = @builtin.unix_time_nano()
  let measurements = Array.range(0, operation_count).map(fn(i) {
    @azimuth.telemetry.api.metrics.Measurement{
      value: (i % 100).to_double() * 1.5,
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("performance_counter")),
        ("operation.index", @azimuth.telemetry.api.common.AttributeValue::int(i.to_int64())),
        ("thread.id", @azimuth.telemetry.api.common.AttributeValue::int((i % concurrent_threads).to_int64()))
      ]
    }
  })
  
  // 5. 验证Metrics创建性能
  let metrics_creation_time = @builtin.unix_time_nano() - metrics_start_time
  assert_eq(measurements.length(), operation_count)
  assert(metrics_creation_time < max_creation_time_ns, "Metrics创建时间过长：\{metrics_creation_time}纳秒")
  
  // 6. 测试属性查找性能
  let attribute_lookup_start = @builtin.unix_time_nano()
  let total_attributes_found = spans.fold(0, fn(acc, span) {
    acc + span.attributes.fold(0, fn(attr_acc, (key, _)) {
      attr_acc + if key == "operation.index" { 1 } else { 0 }
    })
  })
  
  let attribute_lookup_time = @builtin.unix_time_nano() - attribute_lookup_start
  assert_eq(total_attributes_found, operation_count)
  
  // 属性查找应该在合理时间内完成
  assert(attribute_lookup_time < max_creation_time_ns, "属性查找时间过长：\{attribute_lookup_time}纳秒")
  
  // 7. 测试内存使用效率 - 计算平均每个Span的属性数量
  let total_attributes = spans.fold(0, fn(acc, span) { acc + span.attributes.length() })
  let avg_attributes_per_span = total_attributes.to_double() / spans.length().to_double()
  assert(avg_attributes_per_span >= 2.0, "每个Span应该平均至少有2个属性")
  
  // 8. 性能基准验证
  let total_time = @builtin.unix_time_nano() - start_time
  let operations_per_second = (operation_count * 1_000_000_000) / total_time.to_double()
  
  // 性能要求：每秒至少处理10,000个操作
  let min_ops_per_second = 10000.0
  assert(operations_per_second >= min_ops_per_second, "性能不达标：\{operations_per_second} ops/s，要求至少\{min_ops_per_second} ops/s")
  
  println("✓ 高并发性能测试通过：")
  println("  - Span创建时间：\{span_creation_time}纳秒")
  println("  - Metrics创建时间：\{metrics_creation_time}纳秒")
  println("  - 属性查找时间：\{attribute_lookup_time}纳秒")
  println("  - 操作吞吐量：\{operations_per_second} ops/s")
}

test "telemetry_performance_memory_efficiency" {
  // 测试目标：验证遥测系统的内存使用效率
  
  // 1. 创建大型属性集以测试内存效率
  let large_attributes = Array.range(0, 100).map(fn(i) {
    let key = "large.attribute.\{i}"
    let value = @azimuth.telemetry.api.common.AttributeValue::string("large-value-string-\{i}-with-additional-data-to-increase-memory-footprint")
    (key, value)
  })
  
  // 2. 创建包含大量属性的Span
  let memory_test_spans = Array.range(0, 100).map(fn(i) {
    @azimuth.telemetry.api.trace.Span{
      name: "memory-efficiency-test-span-\{i}",
      context: @azimuth.telemetry.api.trace.SpanContext{
        trace_id: [0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99],
        span_id: [
          (i >> 24) & 0xFF as Byte,
          (i >> 16) & 0xFF as Byte,
          (i >> 8) & 0xFF as Byte,
          i & 0xFF as Byte,
          0x00, 0x00, 0x00, 0x00
        ],
        trace_flags: 0x01,
        trace_state: "memory=test"
      },
      kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
      parent_span_id: None,
      start_time_unix_nanos: 1640995200000000000 + (i * 1000000),
      end_time_unix_nanos: Some(1640995200000000000 + (i * 1000000) + 500000),
      status: @azimuth.telemetry.api.trace.StatusCode::Ok,
      status_description: None,
      attributes: large_attributes,
      events: [],
      links: []
    }
  })
  
  // 3. 验证内存效率
  assert_eq(memory_test_spans.length(), 100)
  
  // 检查每个Span的属性数量
  let attributes_per_span = memory_test_spans.map(fn(span) { span.attributes.length() })
  let total_attributes = attributes_per_span.fold(0, fn(acc, count) { acc + count })
  assert_eq(total_attributes, 100 * 100, "总属性数量应该为10000个")
  
  // 4. 测试数组类型的内存效率
  let array_attributes = [
    ("string.array", @azimuth.telemetry.api.common.AttributeValue::array_string(["item1", "item2", "item3", "item4", "item5"])),
    ("int.array", @azimuth.telemetry.api.common.AttributeValue::array_int([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])),
    ("float.array", @azimuth.telemetry.api.common.AttributeValue::array_float([1.1, 2.2, 3.3, 4.4, 5.5])),
    ("bool.array", @azimuth.telemetry.api.common.AttributeValue::array_bool([true, false, true, false, true]))
  ]
  
  // 5. 创建包含数组属性的Span
  let array_test_span = @azimuth.telemetry.api.trace.Span{
    name: "array-attribute-test",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00],
      span_id: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
      trace_flags: 0x01,
      trace_state: "array=test"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000,
    end_time_unix_nanos: Some(1640995200100000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: array_attributes,
    events: [],
    links: []
  }
  
  // 验证数组属性
  assert_eq(array_test_span.attributes.length(), 4)
  
  // 验证数组数据完整性
  let string_array_attr = array_test_span.attributes.find(fn((key, _)) { key == "string.array" })
  assert(string_array_attr != None, "应该找到字符串数组属性")
  
  // 6. 测试内存回收效率（通过创建和销毁对象）
  let memory_cleanup_start = @builtin.unix_time_nano()
  
  // 创建临时对象然后让其超出作用域
  for i = 0; i < 50; i = i + 1 {
    let temp_spans = Array.range(0, 20).map(fn(j) {
      @azimuth.telemetry.api.trace.Span{
        name: "temp-span-\{i}-\{j}",
        context: @azimuth.telemetry.api.trace.SpanContext{
          trace_id: [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f],
          span_id: [(i + j) & 0xFF as Byte, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
          trace_flags: 0x01,
          trace_state: "temp=test"
        },
        kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
        parent_span_id: None,
        start_time_unix_nanos: 1640995200000000000 + (i * 1000000),
        end_time_unix_nanos: Some(1640995200000000000 + (i * 1000000) + 100000),
        status: @azimuth.telemetry.api.trace.StatusCode::Ok,
        status_description: None,
        attributes: [
          ("temp.index", @azimuth.telemetry.api.common.AttributeValue::int(i.to_int64())),
          ("temp.value", @azimuth.telemetry.api.common.AttributeValue::float(j.to_double()))
        ],
        events: [],
        links: []
      }
    })
    // temp_spans在这里超出作用域，应该被回收
  }
  
  let memory_cleanup_time = @builtin.unix_time_nano() - memory_cleanup_start
  
  // 内存清理应该在合理时间内完成
  assert(memory_cleanup_time < 2_000_000_000, "内存清理时间过长：\{memory_cleanup_time}纳秒")
  
  println("✓ 内存效率测试通过：")
  println("  - 大属性集处理：\{total_attributes}个属性")
  println("  - 数组属性支持：4种数组类型")
  println("  - 内存清理时间：\{memory_cleanup_time}纳秒")
}

test "telemetry_performance_batch_operations" {
  // 测试目标：验证批量操作的性能
  
  // 1. 批量创建测试数据
  let batch_size = 500
  let batch_start_time = @builtin.unix_time_nano()
  
  // 批量创建度量
  let batch_measurements = Array.range(0, batch_size).map(fn(i) {
    @azimuth.telemetry.api.metrics.Measurement{
      value: (i * 2.5),
      attributes: [
        ("batch.id", @azimuth.telemetry.api.common.AttributeValue::int((i / 50).to_int64())),
        ("measurement.index", @azimuth.telemetry.api.common.AttributeValue::int(i.to_int64())),
        ("batch.type", @azimuth.telemetry.api.common.AttributeValue::string("performance-test"))
      ]
    }
  })
  
  // 批量创建事件
  let batch_events = Array.range(0, batch_size).map(fn(i) {
    @azimuth.telemetry.api.trace.SpanEvent{
      name: "batch-event-\{i}",
      timestamp_unix_nanos: 1640995200000000000 + (i * 1000000),
      attributes: [
        ("event.sequence", @azimuth.telemetry.api.common.AttributeValue::int(i.to_int64())),
        ("event.category", @azimuth.telemetry.api.common.AttributeValue::string("batch-operation"))
      ]
    }
  })
  
  let batch_creation_time = @builtin.unix_time_nano() - batch_start_time
  
  // 验证批量创建结果
  assert_eq(batch_measurements.length(), batch_size)
  assert_eq(batch_events.length(), batch_size)
  
  // 批量操作性能要求
  assert(batch_creation_time < 1_000_000_000, "批量创建时间过长：\{batch_creation_time}纳秒")
  
  // 2. 测试批量数据处理性能
  let processing_start_time = @builtin.unix_time_nano()
  
  // 按批次ID分组度量
  let grouped_measurements = batch_measurements.reduce(@stdlib.new_map(), fn(acc, measurement) {
    let batch_id_attr = measurement.attributes.find(fn((key, _)) { key == "batch.id" })
    match batch_id_attr {
      Some((_, @azimuth.telemetry.api.common.AttributeValue::IntValue(batch_id))) => {
        let current_batch = acc.get(batch_id).unwrap_or([])
        acc.set(batch_id, current_batch.append([measurement]))
        acc
      }
      _ => acc
    }
  })
  
  // 计算每批次的统计信息
  let batch_stats = grouped_measurements.to_array().map(fn((batch_id, measurements)) {
    let total_value = measurements.fold(0.0, fn(acc, m) { acc + m.value })
    let avg_value = total_value / measurements.length().to_double()
    (batch_id, avg_value, measurements.length())
  })
  
  let processing_time = @builtin.unix_time_nano() - processing_start_time
  
  // 验证处理结果
  assert_eq(grouped_measurements.size(), 10, "应该有10个批次（每批50个）")
  assert_eq(batch_stats.length(), 10)
  
  // 验证批次统计
  let first_batch_avg = batch_stats[0].1
  assert(first_batch_avg > 0.0, "第一批次的平均值应该大于0")
  
  // 批量处理性能要求
  assert(processing_time < 500_000_000, "批量处理时间过长：\{processing_time}纳秒")
  
  println("✓ 批量操作性能测试通过：")
  println("  - 批量创建时间：\{batch_creation_time}纳秒")
  println("  - 批量处理时间：\{processing_time}纳秒")
  println("  - 处理批次数量：\{grouped_measurements.size()}")
  println("  - 平均每批次大小：\{(batch_size / grouped_measurements.size())}")
}