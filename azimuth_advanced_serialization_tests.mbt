// Azimuth Advanced Serialization and Deserialization Test Suite
// 高级序列化和反序列化测试套件

// Test 1: 高性能二进制序列化
test "high-performance binary serialization" {
  let binary_serializer = @azimuth.BinarySerializer::new()
  
  // 创建复杂的遥测数据结构
  let telemetry_batch = @azimuth.TelemetryBatch::new()
  
  for i in 0..=500 {
    let span = @azimuth.Span::new("operation-#{i}", @azimuth.SpanKind::Internal, @azimuth.SpanContext::random())
    
    // 添加各种类型的属性
    @azimuth.Span::set_attribute(span, "string.attr", @azimuth.StringValue("value-#{i}"))
    @azimuth.Span::set_attribute(span, "int.attr", @azimuth.IntValue(i))
    @azimuth.Span::set_attribute(span, "float.attr", @azimuth.FloatValue(3.14 * i))
    @azimuth.Span::set_attribute(span, "bool.attr", @azimuth.BoolValue(i % 2 == 0))
    @azimuth.Span::set_attribute(span, "array.attr", @azimuth.ArrayStringValue(["item1", "item2", "item3"]))
    
    // 添加事件
    @azimuth.Span::add_event(span, "event-#{i}", Some([
      ("event.data", @azimuth.StringValue("event-data-#{i}")),
      ("event.timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos()))
    ]))
    
    @azimuth.TelemetryBatch::add_span(telemetry_batch, span)
  }
  
  // 测试二进制序列化
  let serialization_start = @azimuth.Time::now_unix_nanos()
  let binary_data = @azimuth.BinarySerializer::serialize(binary_serializer, telemetry_batch)
  let serialization_end = @azimuth.Time::now_unix_nanos()
  let serialization_time = serialization_end - serialization_start
  
  // 验证序列化结果
  assert_true(binary_data.length() > 0)
  assert_true(serialization_time < 100000000) // 应该在100ms内完成
  
  // 测试二进制反序列化
  let deserialization_start = @azimuth.Time::now_unix_nanos()
  let deserialized_batch = @azimuth.BinarySerializer::deserialize(binary_serializer, binary_data)
  let deserialization_end = @azimuth.Time::now_unix_nanos()
  let deserialization_time = deserialization_end - deserialization_start
  
  // 验证反序列化结果
  assert_eq(@azimuth.TelemetryBatch::span_count(deserialized_batch), @azimuth.TelemetryBatch::span_count(telemetry_batch))
  assert_true(deserialization_time < 100000000) // 应该在100ms内完成
  
  // 验证数据完整性
  let original_spans = @azimuth.TelemetryBatch::spans(telemetry_batch)
  let deserialized_spans = @azimuth.TelemetryBatch::spans(deserialized_batch)
  
  for i in 0..=original_spans.length() - 1 {
    let original_span = original_spans[i]
    let deserialized_span = deserialized_spans[i]
    
    assert_eq(@azimuth.Span::name(original_span), @azimuth.Span::name(deserialized_span))
    assert_eq(@azimuth.Span::kind(original_span), @azimuth.Span::kind(deserialized_span))
    
    let original_attrs = @azimuth.Span::attributes(original_span)
    let deserialized_attrs = @azimuth.Span::attributes(deserialized_span)
    assert_eq(original_attrs.length(), deserialized_attrs.length())
  }
}

// Test 2: 多格式序列化兼容性
test "multi-format serialization compatibility" {
  let telemetry_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("string.key", @azimuth.StringValue("test value")),
      ("int.key", @azimuth.IntValue(42)),
      ("float.key", @azimuth.FloatValue(3.14159)),
      ("bool.key", @azimuth.BoolValue(true)),
      ("array.string", @azimuth.ArrayStringValue(["a", "b", "c"])),
      ("array.int", @azimuth.ArrayIntValue([1, 2, 3])),
      ("nested.object", @azimuth.ObjectValue([
        ("inner.string", @azimuth.StringValue("inner value")),
        ("inner.int", @azimuth.IntValue(100))
      ]))
    ]
  )
  
  // 测试JSON序列化
  let json_serializer = @azimuth.JSONSerializer::new()
  let json_data = @azimuth.JSONSerializer::serialize(json_serializer, telemetry_data)
  assert_true(json_data.length() > 0)
  
  // 验证JSON格式
  assert_true(json_data.starts_with("{"))
  assert_true(json_data.ends_with("}"))
  
  // 测试JSON反序列化
  let json_deserialized = @azimuth.JSONSerializer::deserialize(json_serializer, json_data)
  let json_attrs = @azimuth.TelemetryData::attributes(json_deserialized)
  let original_attrs = @azimuth.TelemetryData::attributes(telemetry_data)
  assert_eq(json_attrs.length(), original_attrs.length())
  
  // 测试MessagePack序列化
  let msgpack_serializer = @azimuth.MessagePackSerializer::new()
  let msgpack_data = @azimuth.MessagePackSerializer::serialize(msgpack_serializer, telemetry_data)
  assert_true(msgpack_data.length() > 0)
  
  // 测试MessagePack反序列化
  let msgpack_deserialized = @azimuth.MessagePackSerializer::deserialize(msgpack_serializer, msgpack_data)
  let msgpack_attrs = @azimuth.TelemetryData::attributes(msgpack_deserialized)
  assert_eq(msgpack_attrs.length(), original_attrs.length())
  
  // 测试Protocol Buffers序列化
  let protobuf_serializer = @azimuth.ProtobufSerializer::new()
  let protobuf_data = @azimuth.ProtobufSerializer::serialize(protobuf_serializer, telemetry_data)
  assert_true(protobuf_data.length() > 0)
  
  // 测试Protocol Buffers反序列化
  let protobuf_deserialized = @azimuth.ProtobufSerializer::deserialize(protobuf_serializer, protobuf_data)
  let protobuf_attrs = @azimuth.TelemetryData::attributes(protobuf_deserialized)
  assert_eq(protobuf_attrs.length(), original_attrs.length())
  
  // 比较序列化效率
  let json_size = json_data.length()
  let msgpack_size = msgpack_data.length()
  let protobuf_size = protobuf_data.length()
  
  // MessagePack和Protocol Buffers应该比JSON更紧凑
  assert_true(msgpack_size < json_size)
  assert_true(protobuf_size < json_size)
}

// Test 3: 增量序列化和差异处理
test "incremental serialization and diff processing" {
  let incremental_serializer = @azimuth.IncrementalSerializer::new()
  
  // 创建初始数据集
  let initial_batch = @azimuth.TelemetryBatch::new()
  for i in 0..=100 {
    let span = @azimuth.Span::new("initial-span-#{i}", @azimuth.SpanKind::Internal, @azimuth.SpanContext::random())
    @azimuth.Span::set_attribute(span, "batch.id", @azimuth.StringValue("initial"))
    @azimuth.Span::set_attribute(span, "index", @azimuth.IntValue(i))
    @azimuth.TelemetryBatch::add_span(initial_batch, span)
  }
  
  // 序列化初始数据集
  let initial_snapshot = @azimuth.IncrementalSerializer::create_snapshot(incremental_serializer, initial_batch)
  
  // 创建更新的数据集（添加、修改、删除）
  let updated_batch = @azimuth.TelemetryBatch::new()
  
  // 保留部分原始数据
  for i in 0..=50 {
    let span = @azimuth.Span::new("initial-span-#{i}", @azimuth.SpanKind::Internal, @azimuth.SpanContext::random())
    @azimuth.Span::set_attribute(span, "batch.id", @azimuth.StringValue("updated"))
    @azimuth.Span::set_attribute(span, "index", @azimuth.IntValue(i))
    @azimuth.Span::set_attribute(span, "status", @azimuth.StringValue("modified"))
    @azimuth.TelemetryBatch::add_span(updated_batch, span)
  }
  
  // 添加新数据
  for i in 101..=150 {
    let span = @azimuth.Span::new("new-span-#{i}", @azimuth.SpanKind::Internal, @azimuth.SpanContext::random())
    @azimuth.Span::set_attribute(span, "batch.id", @azimuth.StringValue("updated"))
    @azimuth.Span::set_attribute(span, "index", @azimuth.IntValue(i))
    @azimuth.Span::set_attribute(span, "status", @azimuth.StringValue("new"))
    @azimuth.TelemetryBatch::add_span(updated_batch, span)
  }
  
  // 计算差异
  let diff = @azimuth.IncrementalSerializer::compute_diff(incremental_serializer, initial_batch, updated_batch)
  
  // 验证差异
  assert_true(@azimuth.Diff::added_count(diff) > 0)
  assert_true(@azimuth.Diff::modified_count(diff) > 0)
  assert_true(@azimuth.Diff::removed_count(diff) > 0)
  
  // 序列化差异
  let diff_data = @azimuth.IncrementalSerializer::serialize_diff(incremental_serializer, diff)
  assert_true(diff_data.length() > 0)
  
  // 应用差异到原始数据
  let reconstructed_batch = @azimuth.IncrementalSerializer::apply_diff(incremental_serializer, initial_batch, diff_data)
  
  // 验证重构结果
  assert_eq(@azimuth.TelemetryBatch::span_count(reconstructed_batch), @azimuth.TelemetryBatch::span_count(updated_batch))
  
  let reconstructed_spans = @azimuth.TelemetryBatch::spans(reconstructed_batch)
  let updated_spans = @azimuth.TelemetryBatch::spans(updated_batch)
  
  for i in 0..=reconstructed_spans.length() - 1 {
    let reconstructed_span = reconstructed_spans[i]
    let updated_span = updated_spans[i]
    
    assert_eq(@azimuth.Span::name(reconstructed_span), @azimuth.Span::name(updated_span))
    
    let reconstructed_attrs = @azimuth.Span::attributes(reconstructed_span)
    let updated_attrs = @azimuth.Span::attributes(updated_span)
    assert_eq(reconstructed_attrs.length(), updated_attrs.length())
  }
}

// Test 4: 循环引用和复杂图结构序列化
test "circular reference and complex graph structure serialization" {
  let graph_serializer = @azimuth.GraphSerializer::new()
  
  // 创建具有循环引用的复杂图结构
  let node1 = @azimuth.GraphNode::new("node1")
  let node2 = @azimuth.GraphNode::new("node2")
  let node3 = @azimuth.GraphNode::new("node3")
  
  // 建立连接（包括循环引用）
  @azimuth.GraphNode::add_connection(node1, node2, "parent-child")
  @azimuth.GraphNode::add_connection(node2, node3, "parent-child")
  @azimuth.GraphNode::add_connection(node3, node1, "circular") // 循环引用
  
  // 添加自引用
  @azimuth.GraphNode::add_connection(node1, node1, "self-reference")
  
  // 添加遥测数据到节点
  for i in 0..=10 {
    let telemetry_data = @azimuth.TelemetryData::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanId::generate(),
      [
        ("node.id", @azimuth.StringValue("node1")),
        ("metric.name", @azimuth.StringValue("cpu.usage")),
        ("metric.value", @azimuth.FloatValue(0.1 + i * 0.05)),
        ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos()))
      ]
    )
    @azimuth.GraphNode::add_telemetry(node1, telemetry_data)
  }
  
  // 创建图结构
  let graph = @azimuth.TelemetryGraph::new()
  @azimuth.TelemetryGraph::add_node(graph, node1)
  @azimuth.TelemetryGraph::add_node(graph, node2)
  @azimuth.TelemetryGraph::add_node(graph, node3)
  
  // 测试图结构序列化
  let graph_data = @azimuth.GraphSerializer::serialize(graph_serializer, graph)
  assert_true(graph_data.length() > 0)
  
  // 测试图结构反序列化
  let deserialized_graph = @azimuth.GraphSerializer::deserialize(graph_serializer, graph_data)
  
  // 验证图结构完整性
  assert_eq(@azimuth.TelemetryGraph::node_count(deserialized_graph), @azimuth.TelemetryGraph::node_count(graph))
  
  let original_nodes = @azimuth.TelemetryGraph::nodes(graph)
  let deserialized_nodes = @azimuth.TelemetryGraph::nodes(deserialized_graph)
  
  for i in 0..=original_nodes.length() - 1 {
    let original_node = original_nodes[i]
    let deserialized_node = deserialized_nodes[i]
    
    assert_eq(@azimuth.GraphNode::id(original_node), @azimuth.GraphNode::id(deserialized_node))
    assert_eq(@azimuth.GraphNode::connection_count(original_node), @azimuth.GraphNode::connection_count(deserialized_node))
    assert_eq(@azimuth.GraphNode::telemetry_count(original_node), @azimuth.GraphNode::telemetry_count(deserialized_node))
  }
  
  // 验证循环引用处理
  let deserialized_node1 = @azimuth.TelemetryGraph::get_node(deserialized_graph, "node1")
  let connections = @azimuth.GraphNode::connections(deserialized_node1)
  assert_true(connections.length() >= 2) // 至少有到node2和自引用的连接
}

// Test 5: 压缩序列化和流式处理
test "compressed serialization and stream processing" {
  let stream_serializer = @azimuth.StreamSerializer::new(@azimuth.CompressionType::Zstd)
  
  // 创建大量遥测数据
  let telemetry_stream = @azimuth.TelemetryStream::new()
  
  for batch_id in 0..=10 {
    let batch = @azimuth.TelemetryBatch::new()
    
    for span_id in 0..=100 {
      let span = @azimuth.Span::new("span-#{batch_id}-#{span_id}", @azimuth.SpanKind::Internal, @azimuth.SpanContext::random())
      
      // 添加重复的属性以提高压缩效果
      @azimuth.Span::set_attribute(span, "service.name", @azimuth.StringValue("payment-service"))
      @azimuth.Span::set_attribute(span, "service.version", @azimuth.StringValue("1.2.3"))
      @azimuth.Span::set_attribute(span, "environment", @azimuth.StringValue("production"))
      @azimuth.Span::set_attribute(span, "batch.id", @azimuth.IntValue(batch_id))
      @azimuth.Span::set_attribute(span, "span.id", @azimuth.IntValue(span_id))
      
      @azimuth.TelemetryBatch::add_span(batch, span)
    }
    
    @azimuth.TelemetryStream::add_batch(telemetry_stream, batch)
  }
  
  // 测试流式序列化
  let serialization_start = @azimuth.Time::now_unix_nanos()
  let compressed_stream = @azimuth.StreamSerializer::serialize_stream(stream_serializer, telemetry_stream)
  let serialization_end = @azimuth.Time::now_unix_nanos()
  let stream_serialization_time = serialization_end - serialization_start
  
  // 验证压缩效果
  let original_size = @azimuth.TelemetryStream::calculate_size(telemetry_stream)
  let compressed_size = @azimuth.CompressedStream::size(compressed_stream)
  
  assert_true(compressed_size < original_size)
  assert_true(stream_serialization_time < 500000000) // 应该在500ms内完成
  
  // 测试流式反序列化
  let deserialization_start = @azimuth.Time::now_unix_nanos()
  let deserialized_stream = @azimuth.StreamSerializer::deserialize_stream(stream_serializer, compressed_stream)
  let deserialization_end = @azimuth.Time::now_unix_nanos()
  let stream_deserialization_time = deserialization_end - deserialization_start
  
  // 验证流式反序列化结果
  assert_eq(@azimuth.TelemetryStream::batch_count(deserialized_stream), @azimuth.TelemetryStream::batch_count(telemetry_stream))
  assert_true(stream_deserialization_time < 500000000) // 应该在500ms内完成
  
  // 测试部分流读取
  let partial_stream = @azimuth.StreamSerializer::deserialize_partial(stream_serializer, compressed_stream, 5) // 只读取前5个批次
  assert_eq(@azimuth.TelemetryStream::batch_count(partial_stream), 5)
  
  // 验证部分流数据完整性
  let original_batches = @azimuth.TelemetryStream::batches(telemetry_stream)
  let partial_batches = @azimuth.TelemetryStream::batches(partial_stream)
  
  for i in 0..=4 {
    let original_batch = original_batches[i]
    let partial_batch = partial_batches[i]
    
    assert_eq(@azimuth.TelemetryBatch::span_count(original_batch), @azimuth.TelemetryBatch::span_count(partial_batch))
  }
}

// Test 6: 版本兼容性和模式演进
test "version compatibility and schema evolution" {
  let versioned_serializer = @azimuth.VersionedSerializer::new()
  
  // 定义不同版本的数据模式
  let v1_schema = @azimuth.Schema::new("1.0.0", [
    @azimuth.SchemaField::new("trace_id", @azimuth.FieldType::String, true),
    @azimuth.SchemaField::new("span_id", @azimuth.FieldType::String, true),
    @azimuth.SchemaField::new("operation_name", @azimuth.FieldType::String, true),
    @azimuth.SchemaField::new("start_time", @azimuth.FieldType::Timestamp, true),
    @azimuth.SchemaField::new("duration", @azimuth.FieldType::Int, true)
  ])
  
  let v2_schema = @azimuth.Schema::new("2.0.0", [
    @azimuth.SchemaField::new("trace_id", @azimuth.FieldType::String, true),
    @azimuth.SchemaField::new("span_id", @azimuth.FieldType::String, true),
    @azimuth.SchemaField::new("operation_name", @azimuth.FieldType::String, true),
    @azimuth.SchemaField::new("start_time", @azimuth.FieldType::Timestamp, true),
    @azimuth.SchemaField::new("duration", @azimuth.FieldType::Int, true),
    @azimuth.SchemaField::new("service_name", @azimuth.FieldType::String, true), // 新增字段
    @azimuth.SchemaField::new("status", @azimuth.FieldType::String, false) // 可选字段
  ])
  
  let v3_schema = @azimuth.Schema::new("3.0.0", [
    @azimuth.SchemaField::new("trace_id", @azimuth.FieldType::String, true),
    @azimuth.SchemaField::new("span_id", @azimuth.FieldType::String, true),
    @azimuth.SchemaField::new("operation_name", @azimuth.FieldType::String, true),
    @azimuth.SchemaField::new("start_time", @azimuth.FieldType::Timestamp, true),
    @azimuth.SchemaField::new("duration_ms", @azimuth.FieldType::Int, true), // 字段重命名
    @azimuth.SchemaField::new("service_name", @azimuth.FieldType::String, true),
    @azimuth.SchemaField::new("status", @azimuth.FieldType::Enum(["ok", "error", "timeout"]), false), // 类型变更
    @azimuth.SchemaField::new("tags", @azimuth.FieldType::Map(@azimuth.FieldType::String), false) // 新增复杂类型
  ])
  
  // 注册模式
  @azimuth.VersionedSerializer::register_schema(versioned_serializer, v1_schema)
  @azimuth.VersionedSerializer::register_schema(versioned_serializer, v2_schema)
  @azimuth.VersionedSerializer::register_schema(versioned_serializer, v3_schema)
  
  // 创建v1格式数据
  let v1_data = @azimuth.VersionedData::new("1.0.0", [
    ("trace_id", @azimuth.StringValue("trace-123456")),
    ("span_id", @azimuth.StringValue("span-123456")),
    ("operation_name", @azimuth.StringValue("http.request")),
    ("start_time", @azimuth.IntValue(1640995200000L)),
    ("duration", @azimuth.IntValue(150))
  ])
  
  // 序列化v1数据
  let v1_serialized = @azimuth.VersionedSerializer::serialize(versioned_serializer, v1_data)
  
  // 测试向后兼容性：v3反序列化v1数据
  let v1_to_v3_data = @azimuth.VersionedSerializer::deserialize(versioned_serializer, v1_serialized, "3.0.0")
  
  // 验证字段映射
  let v1_to_v3_fields = @azimuth.VersionedData::fields(v1_to_v3_data)
  assert_true(@azimuth.VersionedData::has_field(v1_to_v3_data, "trace_id"))
  assert_true(@azimuth.VersionedData::has_field(v1_to_v3_data, "duration_ms")) // duration映射到duration_ms
  assert_false(@azimuth.VersionedData::has_field(v1_to_v3_data, "service_name")) // v1没有此字段
  
  // 创建v3格式数据
  let v3_data = @azimuth.VersionedData::new("3.0.0", [
    ("trace_id", @azimuth.StringValue("trace-789012")),
    ("span_id", @azimuth.StringValue("span-789012")),
    ("operation_name", @azimuth.StringValue("database.query")),
    ("start_time", @azimuth.IntValue(1640995300000L)),
    ("duration_ms", @azimuth.IntValue(200)),
    ("service_name", @azimuth.StringValue("auth-service")),
    ("status", @azimuth.EnumValue("error")),
    ("tags", @azimuth.MapValue([
      ("db.type", @azimuth.StringValue("postgresql")),
      ("query.type", @azimuth.StringValue("select"))
    ]))
  ])
  
  // 序列化v3数据
  let v3_serialized = @azimuth.VersionedSerializer::serialize(versioned_serializer, v3_data)
  
  // 测试向前兼容性：v1反序列化v3数据
  let v3_to_v1_data = @azimuth.VersionedSerializer::deserialize(versioned_serializer, v3_serialized, "1.0.0")
  
  // 验证字段过滤
  let v3_to_v1_fields = @azimuth.VersionedData::fields(v3_to_v1_data)
  assert_true(@azimuth.VersionedData::has_field(v3_to_v1_data, "trace_id"))
  assert_true(@azimuth.VersionedData::has_field(v3_to_v1_data, "duration")) // duration_ms映射到duration
  assert_false(@azimuth.VersionedData::has_field(v3_to_v1_data, "service_name")) // v1不支持此字段
  assert_false(@azimuth.VersionedData::has_field(v3_to_v1_data, "tags")) // v1不支持复杂类型
  
  // 测试模式兼容性检查
  let v1_to_v2_compatible = @azimuth.VersionedSerializer::check_compatibility(versioned_serializer, "1.0.0", "2.0.0")
  let v2_to_v3_compatible = @azimuth.VersionedSerializer::check_compatibility(versioned_serializer, "2.0.0", "3.0.0")
  let v1_to_v3_compatible = @azimuth.VersionedSerializer::check_compatibility(versioned_serializer, "1.0.0", "3.0.0")
  
  assert_true(v1_to_v2_compatible)
  assert_true(v2_to_v3_compatible)
  assert_true(v1_to_v3_compatible) // 支持跨版本兼容
}

// Test 7: 安全序列化和加密
test "secure serialization and encryption" {
  let secure_serializer = @azimuth.SecureSerializer::new()
  
  // 配置加密
  let encryption_config = @azimuth.EncryptionConfig::new(
    @azimuth.EncryptionAlgorithm::AES256_GCM,
    @azimuth.KeyDerivationFunction::PBKDF2,
    "secure-password-12345",
    @azimuth.Salt::generate()
  )
  
  @azimuth.SecureSerializer::configure_encryption(secure_serializer, encryption_config)
  
  // 创建敏感遥测数据
  let sensitive_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("user.email", @azimuth.StringValue("user@example.com")),
      ("user.ssn", @azimuth.StringValue("123-45-6789")),
      ("credit.card", @azimuth.StringValue("4111-1111-1111-1111")),
      ("api.key", @azimuth.StringValue("sk-1234567890abcdef")),
      ("session.token", @azimuth.StringValue("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...")),
      ("transaction.amount", @azimuth.FloatValue(1000.0)),
      ("account.balance", @azimuth.FloatValue(5000.0))
    ]
  )
  
  // 测试安全序列化
  let encrypted_data = @azimuth.SecureSerializer::serialize(secure_serializer, sensitive_data)
  
  // 验证加密效果
  assert_true(encrypted_data.length() > 0)
  assert_false(@azimuth.String::from_bytes(encrypted_data).contains("user@example.com"))
  assert_false(@azimuth.String::from_bytes(encrypted_data).contains("123-45-6789"))
  assert_false(@azimuth.String::from_bytes(encrypted_data).contains("4111-1111-1111-1111"))
  
  // 测试安全反序列化
  let decrypted_data = @azimuth.SecureSerializer::deserialize(secure_serializer, encrypted_data)
  
  // 验证解密正确性
  let decrypted_attrs = @azimuth.TelemetryData::attributes(decrypted_data)
  let original_attrs = @azimuth.TelemetryData::attributes(sensitive_data)
  assert_eq(decrypted_attrs.length(), original_attrs.length())
  
  // 验证敏感数据完整性
  let decrypted_email = @azimuth.TelemetryData::get_attribute(decrypted_data, "user.email")
  match decrypted_email {
    Some(@azimuth.StringValue(email)) => assert_eq(email, "user@example.com")
    _ => assert_true(false)
  }
  
  let decrypted_ssn = @azimuth.TelemetryData::get_attribute(decrypted_data, "user.ssn")
  match decrypted_ssn {
    Some(@azimuth.StringValue(ssn)) => assert_eq(ssn, "123-45-6789")
    _ => assert_true(false)
  }
  
  // 测试数据完整性验证
  let integrity_check = @azimuth.SecureSerializer::verify_integrity(secure_serializer, encrypted_data)
  assert_true(integrity_check)
  
  // 测试篡改检测
  let tampered_data = @azimuth.ByteVector::copy(encrypted_data)
  if tampered_data.length() > 10 {
    tampered_data[10] = tampered_data[10] + 1 // 篡改一个字节
  }
  
  let tampered_check = @azimuth.SecureSerializer::verify_integrity(secure_serializer, tampered_data)
  assert_false(tampered_check)
  
  // 测试错误密钥解密
  let wrong_encryption_config = @azimuth.EncryptionConfig::new(
    @azimuth.EncryptionAlgorithm::AES256_GCM,
    @azimuth.KeyDerivationFunction::PBKDF2,
    "wrong-password",
    @azimuth.Salt::generate()
  )
  
  let wrong_serializer = @azimuth.SecureSerializer::new()
  @azimuth.SecureSerializer::configure_encryption(wrong_serializer, wrong_encryption_config)
  
  let wrong_decryption_result = @azimuth.SecureSerializer::try_deserialize(wrong_serializer, encrypted_data)
  match wrong_decryption_result {
    @azimuth.Result::Err(msg) => assert_true(msg.contains("decryption") || msg.contains("authentication"))
    _ => assert_true(false)
  }
}