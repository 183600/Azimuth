// Azimuth 上下文传播测试
// 专注于测试分布式系统中上下文信息的传播、转换和一致性

// 测试1: 跨进程上下文传播
test "跨进程上下文传播完整性验证" {
  // 定义进程间上下文载体
  type ProcessContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)],
    trace_flags: Int,
    trace_state: String
  }
  
  // 定义序列化格式
  enum SerializationFormat {
    Binary
    JSON
    Text
    Custom
  }
  
  // 定义上下文序列化器
  type ContextSerializer = {
    format: SerializationFormat,
    serialize: fn(ProcessContext) -> String,
    deserialize: fn(String) -> Option[ProcessContext],
    get_content_type: fn() -> String
  }
  
  // 创建JSON序列化器
  let create_json_serializer = fn() {
    let serialize = fn(context: ProcessContext) {
      let json_obj = {
        "trace_id": context.trace_id,
        "span_id": context.span_id,
        "parent_span_id": match context.parent_span_id {
          Some(id) => id
          None => ""
        },
        "baggage": context.baggage,
        "trace_flags": context.trace_flags,
        "trace_state": context.trace_state
      }
      JSON::stringify(json_obj)
    }
    
    let deserialize = fn(data: String) {
      match JSON::parse(data) {
        Some(obj) => {
          let trace_id = match obj["trace_id"] { String(s) => s, _ => "" }
          let span_id = match obj["span_id"] { String(s) => s, _ => "" }
          let parent_span_id_str = match obj["parent_span_id"] { String(s) => s, _ => "" }
          let parent_span_id = if parent_span_id_str != "" { Some(parent_span_id_str) } else { None }
          
          let baggage_items = match obj["baggage"] {
            Array(items) => items.filter_map(fn(item) {
              match item {
                Object(kv_pairs) => {
                  let key = match kv_pairs[0] { (k, _) => k }
                  let value = match kv_pairs[1] { (_, v) => match v { String(s) => s, _ => "" } }
                  Some((key, value))
                }
                _ => None
              }
            })
            _ => []
          }
          
          let trace_flags = match obj["trace_flags"] { Int(i) => i, _ => 0 }
          let trace_state = match obj["trace_state"] { String(s) => s, _ => "" }
          
          Some({
            trace_id: trace_id,
            span_id: span_id,
            parent_span_id: parent_span_id,
            baggage: baggage_items,
            trace_flags: trace_flags,
            trace_state: trace_state
          })
        }
        None => None
      }
    }
    
    let get_content_type = fn() { "application/json" }
    
    {
      format: SerializationFormat::JSON,
      serialize,
      deserialize,
      get_content_type
    }
  }
  
  // 创建文本序列化器（类似W3C TraceContext格式）
  let create_text_serializer = fn() {
    let serialize = fn(context: ProcessContext) {
      let traceparent = "00-" + context.trace_id + "-" + context.span_id + "-" + 
                       (if context.trace_flags == 1 { "01" } else { "00" })
      
      let baggage_string = if context.baggage.length() > 0 {
        context.baggage.map(fn(item) { item.0 + "=" + item.1 }).join(",")
      } else {
        ""
      }
      
      let tracestate = if context.trace_state != "" {
        context.trace_state
      } else {
        ""
      }
      
      let parts = [traceparent]
      if baggage_string != "" {
        parts = parts.push("baggage=" + baggage_string)
      }
      if tracestate != "" {
        parts = parts.push("tracestate=" + tracestate)
      }
      
      parts.join("\n")
    }
    
    let deserialize = fn(data: String) {
      let lines = data.split("\n")
      
      match lines.find(fn(line) { line.starts_with("00-") }) {
        Some(traceparent_line) => {
          let parts = traceparent_line.split("-")
          if parts.length() >= 4 {
            let trace_id = parts[1]
            let span_id = parts[2]
            let trace_flags = if parts[3] == "01" { 1 } else { 0 }
            
            let baggage = match lines.find(fn(line) { line.starts_with("baggage=") }) {
              Some(baggage_line) => {
                let baggage_data = baggage_line.substring(8)  // 移除"baggage="前缀
                baggage_data.split(",").map(fn(item) {
                  let kv = item.split("=")
                  if kv.length() == 2 {
                    (kv[0], kv[1])
                  } else {
                    (item, "")
                  }
                })
              }
              None => []
            }
            
            let trace_state = match lines.find(fn(line) { line.starts_with("tracestate=") }) {
              Some(tracestate_line) => tracestate_line.substring(11)  // 移除"tracestate="前缀
              None => ""
            }
            
            Some({
              trace_id: trace_id,
              span_id: span_id,
              parent_span_id: None,  // 文本格式不包含父span信息
              baggage: baggage,
              trace_flags: trace_flags,
              trace_state: trace_state
            })
          } else {
            None
          }
        }
        None => None
      }
    }
    
    let get_content_type = fn() { "text/plain" }
    
    {
      format: SerializationFormat::Text,
      serialize,
      deserialize,
      get_content_type
    }
  }
  
  // 创建二进制序列化器
  let create_binary_serializer = fn() {
    let serialize = fn(context: ProcessContext) {
      // 简化的二进制序列化（实际实现会更复杂）
      let buffer = ""
      
      // 添加长度前缀的字符串
      let add_string = fn(str: String, buf: String) {
        buf + str.length().to_string(16).pad_start(4, "0") + str
      }
      
      let buffer_with_trace_id = add_string(context.trace_id, buffer)
      let buffer_with_span_id = add_string(context.span_id, buffer_with_trace_id)
      
      let parent_span_id_str = match context.parent_span_id {
        Some(id) => id
        None => ""
      }
      let buffer_with_parent = add_string(parent_span_id_str, buffer_with_span_id)
      
      // 添加baggage项数量
      let buffer_with_baggage_count = buffer_with_parent + context.baggage.length().to_string(16).pad_start(2, "0")
      
      // 添加每个baggage项
      let mut buffer_with_baggage = buffer_with_baggage_count
      for item in context.baggage {
        buffer_with_baggage = add_string(item.0, buffer_with_baggage)
        buffer_with_baggage = add_string(item.1, buffer_with_baggage)
      }
      
      // 添加trace_flags和trace_state
      let buffer_with_flags = buffer_with_baggage + context.trace_flags.to_string(16).pad_start(2, "0")
      let final_buffer = add_string(context.trace_state, buffer_with_flags)
      
      // 转换为Base64以避免二进制数据问题
      Base64::encode(final_buffer.as_bytes())
    }
    
    let deserialize = fn(data: String) {
      match Base64::decode(data) {
        Some(bytes) => {
          let str_data = String::from_utf8(bytes)
          let mut pos = 0
          
          let read_string = fn(s: String, p: Int) {
            let length_str = s.substring(p, p + 4)
            let length = Int::parse(length_str, 16).unwrap_or(0)
            let string_value = s.substring(p + 4, p + 4 + length)
            (string_value, p + 4 + length)
          }
          
          let (trace_id, new_pos1) = read_string(str_data, pos)
          let (span_id, new_pos2) = read_string(str_data, new_pos1)
          let (parent_span_id_str, new_pos3) = read_string(str_data, new_pos2)
          let parent_span_id = if parent_span_id_str != "" { Some(parent_span_id_str) } else { None }
          
          let baggage_count_str = str_data.substring(new_pos3, new_pos3 + 2)
          let baggage_count = Int::parse(baggage_count_str, 16).unwrap_or(0)
          
          let mut pos = new_pos3 + 2
          let mut baggage = []
          
          for i in 0..baggage_count {
            let (key, new_pos_key) = read_string(str_data, pos)
            let (value, new_pos_value) = read_string(str_data, new_pos_key)
            baggage = baggage.push((key, value))
            pos = new_pos_value
          }
          
          let trace_flags_str = str_data.substring(pos, pos + 2)
          let trace_flags = Int::parse(trace_flags_str, 16).unwrap_or(0)
          
          let (trace_state, _) = read_string(str_data, pos + 2)
          
          Some({
            trace_id: trace_id,
            span_id: span_id,
            parent_span_id: parent_span_id,
            baggage: baggage,
            trace_flags: trace_flags,
            trace_state: trace_state
          })
        }
        None => None
      }
    }
    
    let get_content_type = fn() { "application/octet-stream" }
    
    {
      format: SerializationFormat::Binary,
      serialize,
      deserialize,
      get_content_type
    }
  }
  
  // 创建测试上下文
  let create_test_context = fn() {
    {
      trace_id: "0af7651916cd43dd8448eb211c80319c",
      span_id: "b9c7c989f97918e1",
      parent_span_id: Some("2512a3b1e6c7a4b8"),
      baggage: [
        ("user.id", "user-12345"),
        ("request.id", "req-67890"),
        ("session.id", "sess-abc123"),
        ("service.version", "1.2.3")
      ],
      trace_flags: 1,
      trace_state: "vendor1=value1,vendor2=value2"
    }
  }
  
  // 测试不同序列化器
  let serializers = [
    create_json_serializer(),
    create_text_serializer(),
    create_binary_serializer()
  ]
  
  let test_context = create_test_context()
  
  for serializer in serializers {
    // 序列化
    let serialized = serializer.serialize(test_context)
    assert_true(serialized.length() > 0)
    
    // 反序列化
    let deserialized = serializer.deserialize(serialized)
    assert_true(deserialized.is_some())
    
    match deserialized {
      Some(context) => {
        // 验证基本字段
        assert_eq(context.trace_id, test_context.trace_id)
        assert_eq(context.span_id, test_context.span_id)
        assert_eq(context.trace_flags, test_context.trace_flags)
        assert_eq(context.trace_state, test_context.trace_state)
        
        // 验证baggage
        assert_eq(context.baggage.length(), test_context.baggage.length())
        for original_item in test_context.baggage {
          assert_true(context.baggage.any_fn(item) {
            item.0 == original_item.0 && item.1 == original_item.1
          })
        }
        
        // 注意：文本序列化器不包含parent_span_id
        if serializer.format != SerializationFormat::Text {
          assert_eq(context.parent_span_id, test_context.parent_span_id)
        }
      }
      None => assert_true(false)
    }
  }
  
  // 测试跨格式转换
  let json_serializer = create_json_serializer()
  let text_serializer = create_text_serializer()
  
  // JSON -> Text -> JSON
  let json_serialized = json_serializer.serialize(test_context)
  let json_deserialized = json_serializer.deserialize(json_serialized).unwrap()
  let text_serialized = text_serializer.serialize(json_deserialized)
  let text_deserialized = text_serializer.deserialize(text_serialized).unwrap()
  let final_json_serialized = json_serializer.serialize(text_deserialized)
  let final_json_deserialized = json_serializer.deserialize(final_json_serialized).unwrap()
  
  // 验证跨格式转换后的一致性
  assert_eq(final_json_deserialized.trace_id, test_context.trace_id)
  assert_eq(final_json_deserialized.span_id, test_context.span_id)
  assert_eq(final_json_deserialized.trace_flags, test_context.trace_flags)
  assert_eq(final_json_deserialized.baggage.length(), test_context.baggage.length())
}

// 测试2: 异步上下文传播
test "异步上下文传播一致性验证" {
  // 定义异步上下文
  type AsyncContext = {
    process_context: ProcessContext,
    correlation_id: String,
    causation_id: String,
    message_id: String,
    conversation_id: Option[String]
  }
  
  // 定义异步消息类型
  enum MessageType {
    Request
    Response
    Event
    Command
    Query
  }
  
  // 定义异步消息
  type AsyncMessage = {
    id: String,
    type: MessageType,
    payload: String,
    context: AsyncContext,
    timestamp: Int,
    source: String,
    destination: String,
    headers: Array[(String, String)]
  }
  
  // 创建异步上下文管理器
  let create_async_context_manager = fn() {
    let create_context = fn(parent_context: Option[AsyncContext], message_type: MessageType) {
      let process_context = match parent_context {
        Some(parent) => {
          {
            trace_id: parent.process_context.trace_id,
            span_id: UUID::v4(),
            parent_span_id: Some(parent.process_context.span_id),
            baggage: parent.process_context.baggage,
            trace_flags: parent.process_context.trace_flags,
            trace_state: parent.process_context.trace_state
          }
        }
        None => {
          {
            trace_id: UUID::v4(),
            span_id: UUID::v4(),
            parent_span_id: None,
            baggage: [],
            trace_flags: 1,
            trace_state: ""
          }
        }
      }
      
      let correlation_id = match parent_context {
        Some(parent) => parent.correlation_id
        None => UUID::v4()
      }
      
      let causation_id = UUID::v4()
      let message_id = UUID::v4()
      
      let conversation_id = match message_type {
        MessageType::Request => Some(UUID::v4())
        MessageType::Response => {
          match parent_context {
            Some(parent) => parent.conversation_id
            None => None
          }
        }
        _ => {
          match parent_context {
            Some(parent) => parent.conversation_id
            None => None
          }
        }
      }
      
      {
        process_context: process_context,
        correlation_id: correlation_id,
        causation_id: causation_id,
        message_id: message_id,
        conversation_id: conversation_id
      }
    }
    
    let create_message = fn(context: AsyncContext, message_type: MessageType, payload: String, source: String, destination: String) {
      {
        id: context.message_id,
        type: message_type,
        payload: payload,
        context: context,
        timestamp: Time::now(),
        source: source,
        destination: destination,
        headers: [
          ("trace-id", context.process_context.trace_id),
          ("span-id", context.process_context.span_id),
          ("correlation-id", context.correlation_id),
          ("causation-id", context.causation_id),
          ("message-id", context.message_id)
        ]
      }
    }
    
    let extract_context_from_message = fn(message: AsyncMessage) {
      message.context
    }
    
    {
      create_context,
      create_message,
      extract_context_from_message
    }
  }
  
  // 创建消息传播器
  let create_message_propagator = fn() {
    let propagate_context = fn(message: AsyncMessage, headers: Array[(String, String)]) {
      let context = message.context
      
      let context_headers = [
        ("trace-id", context.process_context.trace_id),
        ("span-id", context.process_context.span_id),
        ("correlation-id", context.correlation_id),
        ("causation-id", context.causation_id),
        ("message-id", context.message_id)
      ]
      
      let baggage_headers = context.process_context.baggage.map(fn(item) {
        ("baggage-" + item.0, item.1)
      })
      
      let all_headers = headers + context_headers + baggage_headers
      
      if context.process_context.parent_span_id.is_some() {
        let parent_span_id = context.process_context.parent_span_id.unwrap()
        all_headers.push(("parent-span-id", parent_span_id))
      }
      
      if context.conversation_id.is_some() {
        let conversation_id = context.conversation_id.unwrap()
        all_headers.push(("conversation-id", conversation_id))
      }
      
      all_headers
    }
    
    let extract_context_from_headers = fn(headers: Array[(String, String)]) {
      match headers.find(fn(header) { header.0 == "trace-id" }) {
        Some((_, trace_id)) => {
          match headers.find(fn(header) { header.0 == "span-id" }) {
            Some((_, span_id)) => {
              match headers.find(fn(header) { header.0 == "correlation-id" }) {
                Some((_, correlation_id)) => {
                  match headers.find(fn(header) { header.0 == "causation-id" }) {
                    Some((_, causation_id)) => {
                      match headers.find(fn(header) { header.0 == "message-id" }) {
                        Some((_, message_id)) => {
                          let parent_span_id = headers.find(fn(header) { header.0 == "parent-span-id" })
                            .map(fn(header) { header.1 })
                          
                          let conversation_id = headers.find(fn(header) { header.0 == "conversation-id" })
                            .map(fn(header) { header.1 })
                          
                          let baggage_headers = headers.filter(fn(header) { header.0.starts_with("baggage-") })
                          let baggage = baggage_headers.map(fn(header) {
                            let key = header.0.substring(8)  // 移除"baggage-"前缀
                            (key, header.1)
                          })
                          
                          let process_context = {
                            trace_id: trace_id,
                            span_id: span_id,
                            parent_span_id: parent_span_id,
                            baggage: baggage,
                            trace_flags: 1,
                            trace_state: ""
                          }
                          
                          Some({
                            process_context: process_context,
                            correlation_id: correlation_id,
                            causation_id: causation_id,
                            message_id: message_id,
                            conversation_id: conversation_id
                          })
                        }
                        None => None
                      }
                    }
                    None => None
                  }
                }
                None => None
              }
            }
            None => None
          }
        }
        None => None
      }
    }
    
    {
      propagate_context,
      extract_context_from_headers
    }
  }
  
  // 测试异步上下文传播
  let context_manager = create_async_context_manager()
  let propagator = create_message_propagator()
  
  // 创建根上下文
  let root_context = context_manager.create_context(None, MessageType::Request)
  
  // 创建请求消息
  let request_message = context_manager.create_message(
    root_context,
    MessageType::Request,
    "Get user information",
    "client",
    "user-service"
  )
  
  // 传播上下文到头部
  let propagated_headers = propagator.propagate_context(request_message, [])
  
  // 验证传播的头部
  assert_true(propagated_headers.any(fn(header) { header.0 == "trace-id" }))
  assert_true(propagated_headers.any(fn(header) { header.0 == "span-id" }))
  assert_true(propagated_headers.any(fn(header) { header.0 == "correlation-id" }))
  assert_true(propagated_headers.any(fn(header) { header.0 == "causation-id" }))
  assert_true(propagated_headers.any(fn(header) { header.0 == "message-id" }))
  assert_true(propagated_headers.any(fn(header) { header.0 == "conversation-id" }))
  
  // 从头部提取上下文
  let extracted_context = propagator.extract_context_from_headers(propagated_headers)
  assert_true(extracted_context.is_some())
  
  match extracted_context {
    Some(context) => {
      // 验证提取的上下文
      assert_eq(context.process_context.trace_id, root_context.process_context.trace_id)
      assert_eq(context.process_context.span_id, root_context.process_context.span_id)
      assert_eq(context.correlation_id, root_context.correlation_id)
      assert_eq(context.conversation_id, root_context.conversation_id)
      
      // 创建响应上下文（基于请求上下文）
      let response_context = context_manager.create_context(Some(context), MessageType::Response)
      
      // 验证响应上下文的关系
      assert_eq(response_context.process_context.trace_id, root_context.process_context.trace_id)
      assert_eq(response_context.process_context.parent_span_id, Some(root_context.process_context.span_id))
      assert_eq(response_context.correlation_id, root_context.correlation_id)
      assert_eq(response_context.conversation_id, root_context.conversation_id)
      assert_ne(response_context.process_context.span_id, root_context.process_context.span_id)
      
      // 创建响应消息
      let response_message = context_manager.create_message(
        response_context,
        MessageType::Response,
        "User information retrieved",
        "user-service",
        "client"
      )
      
      // 验证响应消息的上下文关系
      assert_eq(response_message.context.process_context.trace_id, request_message.context.process_context.trace_id)
      assert_eq(response_message.context.process_context.parent_span_id, Some(request_message.context.process_context.span_id))
      assert_eq(response_message.context.correlation_id, request_message.context.correlation_id)
      assert_eq(response_message.context.conversation_id, request_message.context.conversation_id)
    }
    None => assert_true(false)
  }
  
  // 测试复杂消息链
  let mut contexts = [root_context]
  let mut messages = [request_message]
  
  // 模拟服务链调用
  let services = ["auth-service", "profile-service", "notification-service"]
  
  for service in services {
    let last_context = contexts[contexts.length() - 1]
    let new_context = context_manager.create_context(Some(last_context), MessageType::Request)
    
    // 添加服务特定的baggage
    let updated_context = {
      process_context: {
        trace_id: new_context.process_context.trace_id,
        span_id: new_context.process_context.span_id,
        parent_span_id: new_context.process_context.parent_span_id,
        baggage: new_context.process_context.baggage + [("service", service)],
        trace_flags: new_context.process_context.trace_flags,
        trace_state: new_context.process_context.trace_state
      },
      correlation_id: new_context.correlation_id,
      causation_id: new_context.causation_id,
      message_id: new_context.message_id,
      conversation_id: new_context.conversation_id
    }
    
    contexts = contexts.push(updated_context)
    
    let service_message = context_manager.create_message(
      updated_context,
      MessageType::Request,
      "Call " + service,
      "previous-service",
      service
    )
    
    messages = messages.push(service_message)
  }
  
  // 验证消息链的上下文一致性
  for i in 1..contexts.length() {
    let current = contexts[i]
    let parent = contexts[i - 1]
    
    // 验证trace_id一致性
    assert_eq(current.process_context.trace_id, parent.process_context.trace_id)
    
    // 验证父子关系
    assert_eq(current.process_context.parent_span_id, Some(parent.process_context.span_id))
    
    // 验证correlation_id一致性
    assert_eq(current.correlation_id, parent.correlation_id)
    
    // 验证span_id唯一性
    assert_ne(current.process_context.span_id, parent.process_context.span_id)
    
    // 验证causation_id唯一性
    assert_ne(current.causation_id, parent.causation_id)
  }
}

// 测试3: 跨线程上下文传播
test "跨线程上下文传播安全性验证" {
  // 定义线程本地存储键
  type ThreadLocalKey = String
  
  // 定义线程本地上下文存储
  type ThreadLocalStorage = {
    get_context: fn() -> Option[AsyncContext],
    set_context: fn(AsyncContext) -> Unit,
    clear_context: fn() -> Unit,
    with_context: fn(AsyncContext, () -> Unit) -> Unit
  }
  
  // 创建线程本地存储实现
  let create_thread_local_storage = fn() {
    let mut storage = Map::empty()
    
    let get_context = fn() {
      let thread_id = Thread::current_id().to_string()
      Map::get(storage, thread_id)
    }
    
    let set_context = fn(context: AsyncContext) {
      let thread_id = Thread::current_id().to_string()
      let _ = Map::insert(storage, thread_id, context)
    }
    
    let clear_context = fn() {
      let thread_id = Thread::current_id().to_string()
      let _ = Map::remove(storage, thread_id)
    }
    
    let with_context = fn(context: AsyncContext, operation: () -> Unit) {
      let previous_context = get_context()
      set_context(context)
      operation()
      match previous_context {
        Some(prev) => set_context(prev)
        None => clear_context()
      }
    }
    
    {
      get_context,
      set_context,
      clear_context,
      with_context
    }
  }
  
  // 创建线程安全的上下文传播器
  let create_thread_safe_propagator = fn(storage: ThreadLocalStorage) {
    let propagate_to_thread = fn(context: AsyncContext, target_thread_id: String) {
      // 在实际实现中，这会使用线程间通信机制
      // 这里简化为直接存储到共享存储中
      let _ = Map::insert(storage, target_thread_id, context)
    }
    
    let capture_from_thread = fn(source_thread_id: String) {
      // 在实际实现中，这会使用线程间通信机制
      Map::get(storage, source_thread_id)
    }
    
    let execute_in_context = fn(context: AsyncContext, operation: () -> Unit) {
      storage.with_context(context, operation)
    }
    
    {
      propagate_to_thread,
      capture_from_thread,
      execute_in_context
    }
  }
  
  // 测试线程本地存储
  let storage = create_thread_local_storage()
  let propagator = create_thread_safe_propagator(storage)
  
  // 创建测试上下文
  let test_context = {
    process_context: {
      trace_id: "trace-thread-test-12345",
      span_id: "span-thread-test-67890",
      parent_span_id: Some("span-parent-test"),
      baggage: [("thread.id", Thread::current_id().to_string())],
      trace_flags: 1,
      trace_state: ""
    },
    correlation_id: "corr-thread-test",
    causation_id: "cause-thread-test",
    message_id: "msg-thread-test",
    conversation_id: Some("conv-thread-test")
  }
  
  // 测试线程本地存储
  assert_eq(storage.get_context(), None)  // 初始状态为空
  
  storage.set_context(test_context)
  let retrieved_context = storage.get_context()
  assert_true(retrieved_context.is_some())
  
  match retrieved_context {
    Some(context) => {
      assert_eq(context.process_context.trace_id, test_context.process_context.trace_id)
      assert_eq(context.process_context.span_id, test_context.process_context.span_id)
    }
    None => assert_true(false)
  }
  
  // 测试with_context
  let mut operation_executed = false
  let mut context_in_operation = None
  
  let operation = fn() {
    operation_executed = true
    context_in_operation = storage.get_context()
  }
  
  storage.with_context(test_context, operation)
  
  assert_true(operation_executed)
  assert_true(context_in_operation.is_some())
  
  match context_in_operation {
    Some(context) => {
      assert_eq(context.process_context.trace_id, test_context.process_context.trace_id)
    }
    None => assert_true(false)
  }
  
  // 验证with_context后恢复原状态
  assert_eq(storage.get_context(), None)  // 应该恢复为空
  
  // 测试跨线程传播（模拟）
  let main_thread_id = Thread::current_id().to_string()
  let worker_thread_id = "worker-thread-1"
  
  // 传播上下文到工作线程
  propagator.propagate_to_thread(test_context, worker_thread_id)
  
  // 从工作线程捕获上下文
  let captured_context = propagator.capture_from_thread(worker_thread_id)
  assert_true(captured_context.is_some())
  
  match captured_context {
    Some(context) => {
      assert_eq(context.process_context.trace_id, test_context.process_context.trace_id)
      assert_eq(context.correlation_id, test_context.correlation_id)
    }
    None => assert_true(false)
  }
  
  // 测试在上下文中执行操作
  let mut operation_result = ""
  
  let contextual_operation = fn() {
    let current_context = storage.get_context()
    match current_context {
      Some(context) => {
        operation_result = context.process_context.trace_id
      }
      None => {
        operation_result = "no-context"
      }
    }
  }
  
  propagator.execute_in_context(test_context, contextual_operation)
  assert_eq(operation_result, test_context.process_context.trace_id)
  
  // 验证操作后恢复原状态
  let context_after_operation = storage.get_context()
  assert_eq(context_after_operation, None)
}

// 测试4: 上下文转换和适配
test "上下文转换和适配机制验证" {
  // 定义不同系统的上下文格式
  type SystemAContext = {
    traceId: String,
    spanId: String,
    parentSpanId: String,
    metadata: Map[String, String]
  }
  
  type SystemBContext = {
    correlationId: String,
    messageId: String,
    parentId: Option[String],
    properties: Array[(String, String)]
  }
  
  type SystemCContext = {
    requestContext: {
      traceId: String,
      spanId: String
    },
    userContext: {
      userId: String,
      sessionId: String
    },
    additionalInfo: Array[String]
  }
  
  // 定义上下文适配器
  type ContextAdapter = {
    source_format: String,
    target_format: String,
    convert: fn(String) -> String,
    validate: fn(String) -> Bool
  }
  
  // 创建系统A到标准格式的适配器
  let create_system_a_adapter = fn() {
    let convert = fn(data: String) {
      match JSON::parse(data) {
        Some(obj) => {
          let trace_id = match obj["traceId"] { String(s) => s, _ => "" }
          let span_id = match obj["spanId"] { String(s) => s, _ => "" }
          let parent_span_id_str = match obj["parentSpanId"] { String(s) => s, _ => "" }
          let parent_span_id = if parent_span_id_str != "" { Some(parent_span_id_str) } else { None }
          
          let metadata = match obj["metadata"] {
            Object(map) => map.map(fn(kv) { kv })
            _ => []
          }
          
          let baggage = metadata.filter(fn(kv) { 
            !["traceId", "spanId", "parentSpanId"].contains(kv.0) 
          })
          
          let standard_context = {
            trace_id: trace_id,
            span_id: span_id,
            parent_span_id: parent_span_id,
            baggage: baggage,
            trace_flags: 1,
            trace_state: ""
          }
          
          JSON::stringify({
            "trace_id": standard_context.trace_id,
            "span_id": standard_context.span_id,
            "parent_span_id": match standard_context.parent_span_id {
              Some(id) => id
              None => ""
            },
            "baggage": standard_context.baggage,
            "trace_flags": standard_context.trace_flags,
            "trace_state": standard_context.trace_state
          })
        }
        None => ""
      }
    }
    
    let validate = fn(data: String) {
      match JSON::parse(data) {
        Some(obj) => {
          match obj["traceId"] {
            String(trace_id) => trace_id.length() > 0,
            _ => false
          }
        }
        None => false
      }
    }
    
    {
      source_format: "system_a",
      target_format: "standard",
      convert,
      validate
    }
  }
  
  // 创建标准格式到系统B的适配器
  let create_standard_to_system_b_adapter = fn() {
    let convert = fn(data: String) {
      match JSON::parse(data) {
        Some(obj) => {
          let trace_id = match obj["trace_id"] { String(s) => s, _ => "" }
          let span_id = match obj["span_id"] { String(s) => s, _ => "" }
          let parent_span_id = match obj["parent_span_id"] { String(s) => s, _ => "" }
          let baggage = match obj["baggage"] { Array(items) => items, _ => [] }
          
          let system_b_context = {
            correlationId: trace_id,
            messageId: span_id,
            parentId: if parent_span_id != "" { Some(parent_span_id) } else { None },
            properties: baggage
          }
          
          JSON::stringify(system_b_context)
        }
        None => ""
      }
    }
    
    let validate = fn(data: String) {
      match JSON::parse(data) {
        Some(obj) => {
          match obj["trace_id"] {
            String(trace_id) => trace_id.length() > 0,
            _ => false
          }
        }
        None => false
      }
    }
    
    {
      source_format: "standard",
      target_format: "system_b",
      convert,
      validate
    }
  }
  
  // 创建系统B到系统C的适配器
  let create_system_b_to_system_c_adapter = fn() {
    let convert = fn(data: String) {
      match JSON::parse(data) {
        Some(obj) => {
          let correlation_id = match obj["correlationId"] { String(s) => s, _ => "" }
          let message_id = match obj["messageId"] { String(s) => s, _ => "" }
          let properties = match obj["properties"] { Array(items) => items, _ => [] }
          
          let user_id = properties.find_fn(prop) {
            match prop { (k, v) => k == "user.id" }
          }.map_fn(prop) {
            match prop { (_, v) => v }
          }.unwrap_or("unknown")
          
          let session_id = properties.find_fn(prop) {
            match prop { (k, v) => k == "session.id" }
          }.map_fn(prop) {
            match prop { (_, v) => v }
          }.unwrap_or("unknown")
          
          let additional_info = properties.filter_fn(prop) {
            match prop { (k, _) => k != "user.id" && k != "session.id" }
          }.map_fn(prop) {
            match prop { (k, v) => k + ":" + v }
          }
          
          let system_c_context = {
            requestContext: {
              traceId: correlation_id,
              spanId: message_id
            },
            userContext: {
              userId: user_id,
              sessionId: session_id
            },
            additionalInfo: additional_info
          }
          
          JSON::stringify(system_c_context)
        }
        None => ""
      }
    }
    
    let validate = fn(data: String) {
      match JSON::parse(data) {
        Some(obj) => {
          match obj["correlationId"] {
            String(correlation_id) => correlation_id.length() > 0,
            _ => false
          }
        }
        None => false
      }
    }
    
    {
      source_format: "system_b",
      target_format: "system_c",
      convert,
      validate
    }
  }
  
  // 创建上下文转换管道
  let create_transformation_pipeline = fn(adapters: Array[ContextAdapter]) {
    let transform = fn(data: String, source_format: String, target_format: String) {
      let mut current_data = data
      let mut current_format = source_format
      let mut transformation_path = []
      
      while current_format != target_format {
        let matching_adapters = adapters.filter_fn(adapter) {
          adapter.source_format == current_format
        }
        
        if matching_adapters.length() == 0 {
          return {
            success: false,
            data: "",
            path: transformation_path,
            error: "No adapter found for format: " + current_format
          }
        }
        
        let adapter = matching_adapters[0]
        
        if not adapter.validate(current_data) {
          return {
            success: false,
            data: "",
            path: transformation_path,
            error: "Invalid data for format: " + current_format
          }
        }
        
        current_data = adapter.convert(current_data)
        transformation_path = transformation_path.push(current_format + "->" + adapter.target_format)
        current_format = adapter.target_format
      }
      
      {
        success: true,
        data: current_data,
        path: transformation_path,
        error: ""
      }
    }
    
    { transform }
  }
  
  // 测试上下文转换
  let system_a_adapter = create_system_a_adapter()
  let standard_to_system_b_adapter = create_standard_to_system_b_adapter()
  let system_b_to_system_c_adapter = create_system_b_to_system_c_adapter()
  
  let adapters = [
    system_a_adapter,
    standard_to_system_b_adapter,
    system_b_to_system_c_adapter
  ]
  
  let pipeline = create_transformation_pipeline(adapters)
  
  // 创建系统A的测试数据
  let system_a_data = JSON::stringify({
    "traceId": "trace-system-a-12345",
    "spanId": "span-system-a-67890",
    "parentSpanId": "span-parent-system-a",
    "metadata": {
      "user.id": "user-123",
      "session.id": "session-456",
      "service.name": "service-A",
      "operation.name": "operation-A"
    }
  })
  
  // 测试系统A -> 标准格式转换
  let a_to_standard = system_a_adapter.convert(system_a_data)
  assert_true(a_to_standard.length() > 0)
  assert_true(system_a_adapter.validate(system_a_data))
  
  // 测试标准格式 -> 系统B转换
  let standard_to_b = standard_to_system_b_adapter.convert(a_to_standard)
  assert_true(standard_to_b.length() > 0)
  assert_true(standard_to_system_b_adapter.validate(a_to_standard))
  
  // 测试系统B -> 系统C转换
  let b_to_c = system_b_to_system_c_adapter.convert(standard_to_b)
  assert_true(b_to_c.length() > 0)
  assert_true(system_b_to_system_c_adapter.validate(standard_to_b))
  
  // 验证最终结果
  match JSON::parse(b_to_c) {
    Some(obj) => {
      // 验证请求上下文
      match obj["requestContext"] {
        Object(request_ctx) => {
          assert_eq(match request_ctx["traceId"] { String(s) => s, _ => "" }, "trace-system-a-12345")
          assert_eq(match request_ctx["spanId"] { String(s) => s, _ => "" }, "span-system-a-67890")
        }
        _ => assert_true(false)
      }
      
      // 验证用户上下文
      match obj["userContext"] {
        Object(user_ctx) => {
          assert_eq(match user_ctx["userId"] { String(s) => s, _ => "" }, "user-123")
          assert_eq(match user_ctx["sessionId"] { String(s) => s, _ => "" }, "session-456")
        }
        _ => assert_true(false)
      }
      
      // 验证附加信息
      match obj["additionalInfo"] {
        Array(additional_info) => {
          assert_true(additional_info.length() >= 2)  // 至少有service.name和operation.name
          assert_true(additional_info.any_fn(info) { 
            match info { String(s) => s.contains("service.name") || s.contains("operation.name"), _ => false } 
          })
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试完整管道转换
  let pipeline_result = pipeline.transform(system_a_data, "system_a", "system_c")
  
  assert_true(pipeline_result.success)
  assert_eq(pipeline_result.error, "")
  assert_eq(pipeline_result.path.length(), 3)  // system_a->standard, standard->system_b, system_b->system_c
  
  // 验证管道结果与手动转换结果一致
  match JSON::parse(pipeline_result.data) {
    Some(pipeline_obj) => {
      match JSON::parse(b_to_c) {
        Some(manual_obj) => {
          // 验证关键字段一致
          match (pipeline_obj["requestContext"], manual_obj["requestContext"]) {
            (Object(pipeline_req), Object(manual_req)) => {
              assert_eq(
                match pipeline_req["traceId"] { String(s) => s, _ => "" },
                match manual_req["traceId"] { String(s) => s, _ => "" }
              )
              assert_eq(
                match pipeline_req["spanId"] { String(s) => s, _ => "" },
                match manual_req["spanId"] { String(s) => s, _ => "" }
              )
            }
            _ => assert_true(false)
          }
          
          match (pipeline_obj["userContext"], manual_obj["userContext"]) {
            (Object(pipeline_user), Object(manual_user)) => {
              assert_eq(
                match pipeline_user["userId"] { String(s) => s, _ => "" },
                match manual_user["userId"] { String(s) => s, _ => "" }
              )
              assert_eq(
                match pipeline_user["sessionId"] { String(s) => s, _ => "" },
                match manual_user["sessionId"] { String(s) => s, _ => "" }
              )
            }
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试无效数据
  let invalid_data = JSON::stringify({
    "invalidField": "invalidValue"
  })
  
  let invalid_result = pipeline.transform(invalid_data, "system_a", "system_c")
  assert_false(invalid_result.success)
  assert_true(invalid_result.error.contains("Invalid data"))
  
  // 测试不支持的格式
  let unsupported_result = pipeline.transform(system_a_data, "unknown_format", "system_c")
  assert_false(unsupported_result.success)
  assert_true(unsupported_result.error.contains("No adapter found"))
}