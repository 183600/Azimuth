// Azimuth 分布式追踪上下文传播测试用例
// 包含跨服务、跨进程的追踪上下文传播和注入提取

// 测试1: Trace Context基本注入和提取
test "Trace Context基本注入和提取" {
  // 定义Trace Context结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int,
    trace_state: String
  }
  
  // 生成随机Trace ID
  let generate_trace_id = fn() {
    let mut trace_id = ""
    for i in 0..32 {
      let digit = (Time::now() + i) % 16
      trace_id = trace_id + "0123456789abcdef"[digit]
    }
    trace_id
  }
  
  // 生成随机Span ID
  let generate_span_id = fn() {
    let mut span_id = ""
    for i in 0..16 {
      let digit = (Time::now() + i + 100) % 16
      span_id = span_id + "0123456789abcdef"[digit]
    }
    span_id
  }
  
  // 创建Trace Context
  let create_trace_context = fn(trace_id: Option[String], parent_span_id: Option[String]) {
    let trace = match trace_id {
      Some(id) => id
      None => generate_trace_id()
    }
    
    let span = match parent_span_id {
      Some(id) => generate_span_id()  // 在实际实现中会基于父Span ID生成
      None => generate_span_id()
    }
    
    {
      trace_id: trace,
      span_id: span,
      trace_flags: 1,  // 采样标志
      trace_state: ""
    }
  }
  
  // 将Trace Context注入HTTP头
  let inject_http_headers = fn(context: TraceContext) {
    let headers = []
    
    // 注入traceparent头
    let traceparent = "00-" + context.trace_id + "-" + context.span_id + "-" + 
                      (context.trace_flags % 16).to_string()
    headers = headers.push(("traceparent", traceparent))
    
    // 注入tracestate头（如果有值）
    if context.trace_state.length() > 0 {
      headers = headers.push(("tracestate", context.trace_state))
    }
    
    headers
  }
  
  // 从HTTP头提取Trace Context
  let extract_http_headers = fn(headers: Array<(String, String)>) {
    let mut trace_id = ""
    let mut span_id = ""
    let mut trace_flags = 0
    let mut trace_state = ""
    
    for (key, value) in headers {
      match key {
        "traceparent" => {
          // 格式: 00-trace_id-span_id-trace_flags
          let parts = value.split("-")
          if parts.length() >= 4 {
            trace_id = parts[1]
            span_id = parts[2]
            trace_flags = str_to_int(parts[3], 16)  // 十六进制转十进制
          }
        }
        "tracestate" => {
          trace_state = value
        }
        _ => {}
      }
    }
    
    if trace_id.length() > 0 && span_id.length() > 0 {
      Some({
        trace_id,
        span_id,
        trace_flags,
        trace_state
      })
    } else {
      None
    }
  }
  
  // 辅助函数：十六进制字符串转整数
  let str_to_int = fn(s: String, base: Int) {
    let mut result = 0
    let hex_chars = "0123456789abcdef"
    
    for c in s.to_char_array() {
      let digit = hex_chars.find(c.to_lowercase().to_string())
      match digit {
        Some(d) => result = result * base + d
        None => {}
      }
    }
    
    result
  }
  
  // 测试创建Trace Context
  let root_context = create_trace_context(None, None)
  assert_eq(root_context.trace_id.length(), 32)
  assert_eq(root_context.span_id.length(), 16)
  assert_eq(root_context.trace_flags, 1)
  assert_eq(root_context.trace_state, "")
  
  // 测试子Span Context创建
  let child_context = create_trace_context(Some(root_context.trace_id), Some(root_context.span_id))
  assert_eq(child_context.trace_id, root_context.trace_id)
  assert_ne(child_context.span_id, root_context.span_id)
  assert_eq(child_context.trace_flags, 1)
  
  // 测试HTTP头注入
  let headers = inject_http_headers(root_context)
  assert_eq(headers.length(), 1)
  
  let traceparent_header = headers.find(fn(h) { h.0 == "traceparent" })
  match traceparent_header {
    Some((_, value)) => {
      assert_true(value.starts_with("00-"))
      assert_true(value.contains(root_context.trace_id))
      assert_true(value.contains(root_context.span_id))
      assert_true(value.ends_with("1"))
    }
    None => assert_true(false)
  }
  
  // 测试HTTP头提取
  let extracted_context = extract_http_headers(headers)
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, root_context.trace_id)
      assert_eq(context.span_id, root_context.span_id)
      assert_eq(context.trace_flags, root_context.trace_flags)
    }
    None => assert_true(false)
  }
  
  // 测试带有tracestate的上下文
  let context_with_state = {
    trace_id: root_context.trace_id,
    span_id: root_context.span_id,
    trace_flags: root_context.trace_flags,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  let headers_with_state = inject_http_headers(context_with_state)
  assert_eq(headers_with_state.length(), 2)
  
  let extracted_with_state = extract_http_headers(headers_with_state)
  match extracted_with_state {
    Some(context) => {
      assert_eq(context.trace_state, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
    }
    None => assert_true(false)
  }
  
  // 测试无效头处理
  let invalid_headers = [
    ("traceparent", "invalid-format"),
    ("traceparent", "00-invalid-trace-id-span-id-01"),
    ("other-header", "some-value")
  ]
  
  let invalid_extracted = extract_http_headers(invalid_headers)
  assert_eq(invalid_extracted, None)
}

// 测试2: 跨服务上下文传播
test "跨服务上下文传播" {
  // 定义服务调用结构
  type ServiceCall = {
    service_name: String,
    operation_name: String,
    incoming_context: Option[TraceContext],
    outgoing_context: Option[TraceContext],
    duration_ms: Int
  }
  
  // 模拟服务调用
  let simulate_service_call = fn(
    service_name: String, 
    operation_name: String, 
    incoming_context: Option[TraceContext>
  ) {
    let start_time = Time::now()
    
    // 创建当前服务的Context
    let current_context = match incoming_context {
      Some(ctx) => create_trace_context(Some(ctx.trace_id), Some(ctx.span_id))
      None => create_trace_context(None, None)
    }
    
    // 模拟服务处理时间
    let processing_time = 50 + (Time::now() % 100)
    
    // 创建下一服务的Context
    let outgoing_context = create_trace_context(
      Some(current_context.trace_id), 
      Some(current_context.span_id)
    )
    
    let end_time = start_time + processing_time
    
    {
      service_name,
      operation_name,
      incoming_context,
      outgoing_context: Some(outgoing_context),
      duration_ms: processing_time
    }
  }
  
  // 使用前一个测试的函数
  let create_trace_context = fn(trace_id: Option[String], parent_span_id: Option[String]) {
    let trace = match trace_id {
      Some(id) => id
      None => generate_trace_id()
    }
    
    let span = match parent_span_id {
      Some(id) => generate_span_id()
      None => generate_span_id()
    }
    
    {
      trace_id: trace,
      span_id: span,
      trace_flags: 1,
      trace_state: ""
    }
  }
  
  let generate_trace_id = fn() {
    let mut trace_id = ""
    for i in 0..32 {
      let digit = (Time::now() + i) % 16
      trace_id = trace_id + "0123456789abcdef"[digit]
    }
    trace_id
  }
  
  let generate_span_id = fn() {
    let mut span_id = ""
    for i in 0..16 {
      let digit = (Time::now() + i + 100) % 16
      span_id = span_id + "0123456789abcdef"[digit]
    }
    span_id
  }
  
  // 模拟分布式调用链：API Gateway -> Auth Service -> User Service -> Database
  let api_call = simulate_service_call("api-gateway", "authenticate_user", None)
  
  let auth_call = simulate_service_call(
    "auth-service", 
    "validate_token", 
    api_call.outgoing_context
  )
  
  let user_call = simulate_service_call(
    "user-service", 
    "get_user_profile", 
    auth_call.outgoing_context
  )
  
  let db_call = simulate_service_call(
    "database", 
    "query_user", 
    user_call.outgoing_context
  )
  
  // 验证Trace ID在整个调用链中保持一致
  assert_eq(api_call.outgoing_context.unwrap().trace_id, 
             auth_call.incoming_context.unwrap().trace_id)
  assert_eq(auth_call.outgoing_context.unwrap().trace_id, 
             user_call.incoming_context.unwrap().trace_id)
  assert_eq(user_call.outgoing_context.unwrap().trace_id, 
             db_call.incoming_context.unwrap().trace_id)
  
  // 验证每个服务都有唯一的Span ID
  assert_ne(api_call.outgoing_context.unwrap().span_id, 
            auth_call.outgoing_context.unwrap().span_id)
  assert_ne(auth_call.outgoing_context.unwrap().span_id, 
            user_call.outgoing_context.unwrap().span_id)
  assert_ne(user_call.outgoing_context.unwrap().span_id, 
            db_call.outgoing_context.unwrap().span_id)
  
  // 验证父子关系
  assert_eq(api_call.outgoing_context.unwrap().span_id, 
            auth_call.incoming_context.unwrap().span_id)
  assert_eq(auth_call.outgoing_context.unwrap().span_id, 
            user_call.incoming_context.unwrap().span_id)
  assert_eq(user_call.outgoing_context.unwrap().span_id, 
            db_call.incoming_context.unwrap().span_id)
  
  // 计算总调用时间
  let total_duration = api_call.duration_ms + auth_call.duration_ms + 
                       user_call.duration_ms + db_call.duration_ms
  assert_true(total_duration > 0)
  
  // 验证调用链结构
  let call_chain = [api_call, auth_call, user_call, db_call]
  assert_eq(call_chain.length(), 4)
  
  // 验证每个服务都有传入和传出的上下文（除了第一个服务没有传入上下文）
  assert_eq(api_call.incoming_context, None)
  assert_true(api_call.outgoing_context.is_some())
  
  assert_true(auth_call.incoming_context.is_some())
  assert_true(auth_call.outgoing_context.is_some())
  
  assert_true(user_call.incoming_context.is_some())
  assert_true(user_call.outgoing_context.is_some())
  
  assert_true(db_call.incoming_context.is_some())
  assert_true(db_call.outgoing_context.is_some())
}

// 测试3: 异步消息传递中的上下文传播
test "异步消息传递中的上下文传播" {
  // 定义消息结构
  type Message = {
    message_id: String,
    topic: String,
    payload: String,
    trace_context: Option[TraceContext],
    timestamp: Int
  }
  
  // 定义生产者结构
  type Producer = {
    name: String,
    context: Option[TraceContext]
  }
  
  // 定义消费者结构
  type Consumer = {
    name: String,
    processed_messages: Array[Message]
  }
  
  // 生产者发送消息
  let produce_message = fn(producer: Producer, topic: String, payload: String) {
    let message_context = match producer.context {
      Some(ctx) => create_trace_context(Some(ctx.trace_id), Some(ctx.span_id))
      None => create_trace_context(None, None)
    }
    
    {
      message_id: generate_message_id(),
      topic,
      payload,
      trace_context: Some(message_context),
      timestamp: Time::now()
    }
  }
  
  // 消费者处理消息
  let consume_message = fn(consumer: Consumer, message: Message) {
    let processing_context = match message.trace_context {
      Some(ctx) => create_trace_context(Some(ctx.trace_id), Some(ctx.span_id))
      None => create_trace_context(None, None)
    }
    
    // 模拟消息处理
    let updated_consumer = {
      name: consumer.name,
      processed_messages: consumer.processed_messages.push(message)
    }
    
    (updated_consumer, processing_context)
  }
  
  // 辅助函数
  let generate_message_id = fn() {
    "msg-" + (Time::now() % 10000).to_string()
  }
  
  let create_trace_context = fn(trace_id: Option[String], parent_span_id: Option[String]) {
    let trace = match trace_id {
      Some(id) => id
      None => generate_trace_id()
    }
    
    let span = match parent_span_id {
      Some(id) => generate_span_id()
      None => generate_span_id()
    }
    
    {
      trace_id: trace,
      span_id: span,
      trace_flags: 1,
      trace_state: ""
    }
  }
  
  let generate_trace_id = fn() {
    let mut trace_id = ""
    for i in 0..32 {
      let digit = (Time::now() + i) % 16
      trace_id = trace_id + "0123456789abcdef"[digit]
    }
    trace_id
  }
  
  let generate_span_id = fn() {
    let mut span_id = ""
    for i in 0..16 {
      let digit = (Time::now() + i + 100) % 16
      span_id = span_id + "0123456789abcdef"[digit]
    }
    span_id
  }
  
  // 创建生产者
  let web_producer = {
    name: "web-service",
    context: None  // 开始新的追踪
  }
  
  // 创建消费者
  let email_consumer = {
    name: "email-service",
    processed_messages: []
  }
  
  let notification_consumer = {
    name: "notification-service",
    processed_messages: []
  }
  
  // Web服务产生用户注册事件
  let user_registered_msg = produce_message(
    web_producer, 
    "user.events", 
    "{\"user_id\": \"123\", \"email\": \"user@example.com\"}"
  )
  
  // 验证消息包含Trace Context
  assert_true(user_registered_msg.trace_context.is_some())
  let original_context = user_registered_msg.trace_context.unwrap()
  
  // Email服务处理消息
  let (updated_email_consumer, email_context) = consume_message(email_consumer, user_registered_msg)
  
  // 验证Email服务继承了Trace Context
  assert_eq(email_context.trace_id, original_context.trace_id)
  assert_ne(email_context.span_id, original_context.span_id)
  
  // Notification服务也处理同一条消息
  let (updated_notification_consumer, notification_context) = consume_message(
    notification_consumer, 
    user_registered_msg
  )
  
  // 验证Notification服务也继承了Trace Context
  assert_eq(notification_context.trace_id, original_context.trace_id)
  assert_ne(notification_context.span_id, original_context.span_id)
  assert_ne(notification_context.span_id, email_context.span_id)  // 每个消费者有唯一Span ID
  
  // Email服务发送确认消息
  let email_producer = {
    name: "email-service",
    context: Some(email_context)
  }
  
  let email_confirmation_msg = produce_message(
    email_producer, 
    "email.confirmations", 
    "{\"message_id\": \"" + user_registered_msg.message_id + "\", \"status\": \"sent\"}"
  )
  
  // 验证确认消息的Trace Context
  assert_true(email_confirmation_msg.trace_context.is_some())
  let email_confirmation_context = email_confirmation_msg.trace_context.unwrap()
  
  // 确认消息应该与原始消息共享相同的Trace ID
  assert_eq(email_confirmation_context.trace_id, original_context.trace_id)
  assert_ne(email_confirmation_context.span_id, email_context.span_id)
  
  // 验证消息处理历史
  assert_eq(updated_email_consumer.processed_messages.length(), 1)
  assert_eq(updated_notification_consumer.processed_messages.length(), 1)
  
  // 验证消息关联
  assert_eq(updated_email_consumer.processed_messages[0].message_id, 
            user_registered_msg.message_id)
  assert_eq(updated_notification_consumer.processed_messages[0].message_id, 
            user_registered_msg.message_id)
  
  // 测试消息链追踪
  let message_chain = [
    ("web-service", user_registered_msg),
    ("email-service", email_confirmation_msg)
  ]
  
  // 验证所有消息共享相同的Trace ID
  for i in 0..message_chain.length() {
    let (service, msg) = message_chain[i]
    match msg.trace_context {
      Some(ctx) => {
        assert_eq(ctx.trace_id, original_context.trace_id)
      }
      None => assert_true(false)
    }
  }
}

// 测试4: 跨进程上下文传播
test "跨进程上下文传播" {
  // 定义进程间通信结构
  type ProcessContext = {
    process_id: Int,
    process_name: String,
    trace_context: Option[TraceContext],
    parent_process: Option[Int]
  }
  
  // 定义进程间调用
  type ProcessCall = {
    caller_process: Int,
    callee_process: Int,
    operation: String,
    context_transmitted: Bool,
    transmission_method: String
  }
  
  // 创建进程
  let create_process = fn(process_id: Int, process_name: String, parent_process: Option[Int]) {
    {
      process_id,
      process_name,
      trace_context: None,
      parent_process
    }
  }
  
  // 进程间传递上下文
  let transmit_context = fn(
    caller: ProcessContext, 
    callee: ProcessContext, 
    operation: String, 
    method: String
  ) {
    let context_transmitted = caller.trace_context.is_some()
    
    if context_transmitted {
      // 被调用进程继承调用者的上下文
      let callee_with_context = {
        process_id: callee.process_id,
        process_name: callee.process_name,
        trace_context: caller.trace_context,
        parent_process: Some(caller.process_id)
      }
      
      (callee_with_context, {
        caller_process: caller.process_id,
        callee_process: callee.process_id,
        operation,
        context_transmitted: true,
        transmission_method: method
      })
    } else {
      // 没有上下文传递
      (callee, {
        caller_process: caller.process_id,
        callee_process: callee.process_id,
        operation,
        context_transmitted: false,
        transmission_method: method
      })
    }
  }
  
  // 辅助函数
  let create_trace_context = fn(trace_id: Option[String], parent_span_id: Option[String]) {
    let trace = match trace_id {
      Some(id) => id
      None => generate_trace_id()
    }
    
    let span = match parent_span_id {
      Some(id) => generate_span_id()
      None => generate_span_id()
    }
    
    {
      trace_id: trace,
      span_id: span,
      trace_flags: 1,
      trace_state: ""
    }
  }
  
  let generate_trace_id = fn() {
    let mut trace_id = ""
    for i in 0..32 {
      let digit = (Time::now() + i) % 16
      trace_id = trace_id + "0123456789abcdef"[digit]
    }
    trace_id
  }
  
  let generate_span_id = fn() {
    let mut span_id = ""
    for i in 0..16 {
      let digit = (Time::now() + i + 100) % 16
      span_id = span_id + "0123456789abcdef"[digit]
    }
    span_id
  }
  
  // 创建进程层次结构
  let main_process = create_process(1001, "main", None)
  
  // 主进程创建Trace Context
  let main_with_context = {
    process_id: main_process.process_id,
    process_name: main_process.process_name,
    trace_context: Some(create_trace_context(None, None)),
    parent_process: main_process.parent_process
  }
  
  // 主进程fork子进程
  let worker_process = create_process(1002, "worker", Some(1001))
  
  // 通过环境变量传递上下文
  let (worker_with_env_context, env_call) = transmit_context(
    main_with_context, 
    worker_process, 
    "initialize_worker", 
    "environment_variables"
  )
  
  // 验证环境变量传递
  assert_true(env_call.context_transmitted)
  assert_eq(env_call.transmission_method, "environment_variables")
  assert_true(worker_with_env_context.trace_context.is_some())
  assert_eq(worker_with_env_context.parent_process, Some(1001))
  
  // Worker进程通过命令行参数调用另一个进程
  let helper_process = create_process(1003, "helper", Some(1002))
  
  let (helper_with_cli_context, cli_call) = transmit_context(
    worker_with_env_context, 
    helper_process, 
    "execute_helper_task", 
    "command_line_arguments"
  )
  
  // 验证命令行参数传递
  assert_true(cli_call.context_transmitted)
  assert_eq(helper_with_cli_context.parent_process, Some(1002))
  
  // Helper进程通过IPC调用另一个进程
  let service_process = create_process(1004, "service", Some(1003))
  
  let (service_with_ipc_context, ipc_call) = transmit_context(
    helper_with_cli_context, 
    service_process, 
    "request_service", 
    "inter_process_communication"
  )
  
  // 验证IPC传递
  assert_true(ipc_call.context_transmitted)
  assert_eq(service_with_ipc_context.parent_process, Some(1003))
  
  // 验证Trace Context在整个进程链中保持一致
  let original_trace_id = main_with_context.trace_context.unwrap().trace_id
  
  assert_eq(worker_with_env_context.trace_context.unwrap().trace_id, original_trace_id)
  assert_eq(helper_with_cli_context.trace_context.unwrap().trace_id, original_trace_id)
  assert_eq(service_with_ipc_context.trace_context.unwrap().trace_id, original_trace_id)
  
  // 验证每个进程有唯一的Span ID
  assert_ne(main_with_context.trace_context.unwrap().span_id, 
            worker_with_env_context.trace_context.unwrap().span_id)
  assert_ne(worker_with_env_context.trace_context.unwrap().span_id, 
            helper_with_cli_context.trace_context.unwrap().span_id)
  assert_ne(helper_with_cli_context.trace_context.unwrap().span_id, 
            service_with_ipc_context.trace_context.unwrap().span_id)
  
  // 验证进程调用链
  let process_calls = [env_call, cli_call, ipc_call]
  
  for i in 0..process_calls.length() {
    let call = process_calls[i]
    assert_true(call.context_transmitted)
    
    // 验证调用者-被调用者关系
    if i > 0 {
      let prev_call = process_calls[i - 1]
      assert_eq(call.caller_process, prev_call.callee_process)
    }
  }
  
  // 测试没有上下文的情况
  let isolated_process = create_process(1005, "isolated", None)
  let target_process = create_process(1006, "target", Some(1005))
  
  let (target_still_isolated, no_context_call) = transmit_context(
    isolated_process, 
    target_process, 
    "run_without_context", 
    "none"
  )
  
  // 验证没有上下文传递
  assert_false(no_context_call.context_transmitted)
  assert_eq(target_still_isolated.trace_context, None)
}

// 测试5: 上下文传播中的 baggage 传递
test "上下文传播中的 baggage 传递" {
  // 定义 Baggage 结构
  type BaggageEntry = {
    key: String,
    value: String,
    metadata: Option[String]
  }
  
  // 定义增强的 Trace Context
  type EnhancedTraceContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int,
    trace_state: String,
    baggage: Array[BaggageEntry]
  }
  
  // 创建带有 baggage 的上下文
  let create_context_with_baggage = fn(
    trace_id: Option[String], 
    parent_span_id: Option[String],
    baggage: Array[BaggageEntry>
  ) {
    let trace = match trace_id {
      Some(id) => id
      None => generate_trace_id()
    }
    
    let span = match parent_span_id {
      Some(id) => generate_span_id()
      None => generate_span_id()
    }
    
    {
      trace_id: trace,
      span_id: span,
      trace_flags: 1,
      trace_state: "",
      baggage
    }
  }
  
  // 添加 baggage 项
  let add_baggage_item = fn(context: EnhancedTraceContext, key: String, value: String, metadata: Option[String]) {
    let existing_index = context.baggage.find_index(fn(entry) { entry.key == key })
    
    let updated_baggage = match existing_index {
      Some(index) => {
        let mut new_baggage = context.baggage
        new_baggage[index] = { key, value, metadata }
        new_baggage
      }
      None => {
        context.baggage.push({ key, value, metadata })
      }
    }
    
    {
      trace_id: context.trace_id,
      span_id: context.span_id,
      trace_flags: context.trace_flags,
      trace_state: context.trace_state,
      baggage: updated_baggage
    }
  }
  
  // 获取 baggage 项
  let get_baggage_item = fn(context: EnhancedTraceContext, key: String) {
    context.baggage.find(fn(entry) { entry.key == key })
  }
  
  // 移除 baggage 项
  let remove_baggage_item = fn(context: EnhancedTraceContext, key: String) {
    let filtered_baggage = context.baggage.filter(fn(entry) { entry.key != key })
    
    {
      trace_id: context.trace_id,
      span_id: context.span_id,
      trace_flags: context.trace_flags,
      trace_state: context.trace_state,
      baggage: filtered_baggage
    }
  }
  
  // 将 baggage 序列化为 HTTP 头
  let serialize_baggage = fn(baggage: Array[BaggageEntry]) {
    let mut parts = []
    
    for entry in baggage {
      let part = entry.key + "=" + entry.value
      let part_with_metadata = match entry.metadata {
        Some(meta) => part + ";" + meta
        None => part
      }
      parts = parts.push(part_with_metadata)
    }
    
    parts.join(",")
  }
  
  // 从 HTTP 头反序列化 baggage
  let deserialize_baggage = fn(baggage_header: String) {
    let entries = baggage_header.split(",")
    let mut baggage = []
    
    for entry in entries {
      let parts = entry.split(";")
      if parts.length() > 0 {
        let key_value = parts[0].split("=")
        if key_value.length() == 2 {
          let key = key_value[0]
          let value = key_value[1]
          
          let metadata = if parts.length() > 1 {
            let mut meta_parts = []
            for i in 1..parts.length() {
              meta_parts = meta_parts.push(parts[i])
            }
            Some(meta_parts.join(";"))
          } else {
            None
          }
          
          baggage = baggage.push({ key, value, metadata })
        }
      }
    }
    
    baggage
  }
  
  // 跨服务传递 baggage
  let propagate_baggage = fn(context: EnhancedTraceContext, allowed_keys: Array[String]) {
    let filtered_baggage = context.baggage.filter(fn(entry) {
      allowed_keys.contains(entry.key)
    })
    
    {
      trace_id: context.trace_id,
      span_id: context.span_id,
      trace_flags: context.trace_flags,
      trace_state: context.trace_state,
      baggage: filtered_baggage
    }
  }
  
  // 辅助函数
  let generate_trace_id = fn() {
    let mut trace_id = ""
    for i in 0..32 {
      let digit = (Time::now() + i) % 16
      trace_id = trace_id + "0123456789abcdef"[digit]
    }
    trace_id
  }
  
  let generate_span_id = fn() {
    let mut span_id = ""
    for i in 0..16 {
      let digit = (Time::now() + i + 100) % 16
      span_id = span_id + "0123456789abcdef"[digit]
    }
    span_id
  }
  
  // 创建初始上下文
  let initial_context = create_context_with_baggage(None, None, [])
  
  // 添加 baggage 项
  let context_with_user = add_baggage_item(
    initial_context, 
    "user.id", 
    "12345", 
    Some("propagation=downstream")
  )
  
  let context_with_session = add_baggage_item(
    context_with_user, 
    "session.id", 
    "sess-abc-123", 
    None
  )
  
  let context_with_tenant = add_baggage_item(
    context_with_session, 
    "tenant.id", 
    "tenant-xyz", 
    Some("sensitive=true")
  )
  
  // 验证 baggage 添加
  assert_eq(context_with_tenant.baggage.length(), 3)
  
  let user_entry = get_baggage_item(context_with_tenant, "user.id")
  match user_entry {
    Some(entry) => {
      assert_eq(entry.key, "user.id")
      assert_eq(entry.value, "12345")
      assert_eq(entry.metadata, Some("propagation=downstream"))
    }
    None => assert_true(false)
  }
  
  // 测试 baggage 序列化
  let serialized = serialize_baggage(context_with_tenant.baggage)
  assert_true(serialized.contains("user.id=12345;propagation=downstream"))
  assert_true(serialized.contains("session.id=sess-abc-123"))
  assert_true(serialized.contains("tenant.id=tenant-xyz;sensitive=true"))
  
  // 测试 baggage 反序列化
  let deserialized = deserialize_baggage(serialized)
  assert_eq(deserialized.length(), 3)
  
  // 验证反序列化的 baggage 项
  let deserialized_user = deserialized.find(fn(entry) { entry.key == "user.id" })
  match deserialized_user {
    Some(entry) => {
      assert_eq(entry.key, "user.id")
      assert_eq(entry.value, "12345")
      assert_eq(entry.metadata, Some("propagation=downstream"))
    }
    None => assert_true(false)
  }
  
  // 测试 baggage 移除
  let context_without_tenant = remove_baggage_item(context_with_tenant, "tenant.id")
  assert_eq(context_without_tenant.baggage.length(), 2)
  assert_eq(get_baggage_item(context_without_tenant, "tenant.id"), None)
  assert_eq(get_baggage_item(context_without_tenant, "user.id"), Some({ 
    key: "user.id", 
    value: "12345", 
    metadata: Some("propagation=downstream") 
  }))
  
  // 测试选择性 baggage 传播
  let allowed_keys = ["user.id", "session.id"]
  let filtered_context = propagate_baggage(context_with_tenant, allowed_keys.to_array())
  
  // 验证只有允许的 baggage 项被传播
  assert_eq(filtered_context.baggage.length(), 2)
  assert_true(get_baggage_item(filtered_context, "user.id").is_some())
  assert_true(get_baggage_item(filtered_context, "session.id").is_some())
  assert_eq(get_baggage_item(filtered_context, "tenant.id"), None)
  
  // 测试 baggage 更新
  let updated_user_context = add_baggage_item(
    context_with_tenant, 
    "user.id", 
    "67890", 
    Some("propagation=downstream;updated=true")
  )
  
  // 验证 baggage 更新
  let updated_user_entry = get_baggage_item(updated_user_context, "user.id")
  match updated_user_entry {
    Some(entry) => {
      assert_eq(entry.value, "67890")
      assert_eq(entry.metadata, Some("propagation=downstream;updated=true"))
    }
    None => assert_true(false)
  }
  
  // 验证 baggage 总数不变（更新而非添加）
  assert_eq(updated_user_context.baggage.length(), 3)
}