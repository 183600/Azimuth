// Azimuth 分布式追踪上下文传播测试
// 测试分布式追踪中上下文在不同服务和进程间的传播机制

// 测试1: 基本追踪上下文传播
test "基本追踪上下文传播" {
  // 定义追踪上下文结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int,
    trace_state: String
  }
  
  // 定义传播头结构
  type PropagationHeaders = {
    traceparent: String,
    tracestate: String,
    baggage: Array[(String, String)]
  }
  
  // 创建初始追踪上下文
  let create_root_context = fn() {
    // 生成随机trace_id和span_id
    let trace_id = "trace-" + Time::now().to_string()
    let span_id = "span-" + (Time::now() + 1000).to_string()
    
    {
      trace_id: trace_id,
      span_id: span_id,
      trace_flags: 1,  // 采样标志
      trace_state: ""
    }
  }
  
  // 从上下文创建传播头
  let extract_headers = fn(context: TraceContext, baggage_items: Array[(String, String)]) {
    let traceparent = "00-" + context.trace_id + "-" + context.span_id + "-" + 
                     (context.trace_flags % 16).to_string(16).to_uppercase()
    
    let tracestate = if context.trace_state.length() > 0 {
      context.trace_state
    } else {
      ""
    }
    
    let baggage_header = baggage_items.map(fn(item) {
      let (key, value) = item
      key + "=" + value
    }).join(",")
    
    {
      traceparent: traceparent,
      tracestate: tracestate,
      baggage: baggage_items
    }
  }
  
  // 从传播头提取上下文
  let inject_context = fn(headers: PropagationHeaders) {
    if headers.traceparent.length() >= 55 {
      let parts = headers.traceparent.split("-")
      if parts.length() >= 4 {
        let trace_id = parts[1]
        let span_id = parts[2]
        let trace_flags = Int::from_str_radix(parts[3], 16).get_or(0)
        
        Some({
          trace_id: trace_id,
          span_id: span_id,
          trace_flags: trace_flags,
          trace_state: headers.tracestate
        })
      } else {
        None
      }
    } else {
      None
    }
  }
  
  // 测试上下文创建
  let root_context = create_root_context()
  assert_true(root_context.trace_id.starts_with("trace-"))
  assert_true(root_context.span_id.starts_with("span-"))
  assert_eq(root_context.trace_flags, 1)
  
  // 测试头提取
  let baggage_items = [
    ("user.id", "user-123"),
    ("service.name", "payment-service"),
    ("region", "us-west-2")
  ]
  
  let headers = extract_headers(root_context, baggage_items)
  assert_true(headers.traceparent.starts_with("00-"))
  assert_true(headers.traceparent.contains(root_context.trace_id))
  assert_true(headers.traceparent.contains(root_context.span_id))
  assert_eq(headers.tracestate, "")
  assert_eq(headers.baggage.length(), 3)
  
  // 测试上下文注入
  let injected_context = inject_context(headers)
  assert_true(injected_context.is_some())
  
  match injected_context {
    Some(context) => {
      assert_eq(context.trace_id, root_context.trace_id)
      assert_eq(context.span_id, root_context.span_id)
      assert_eq(context.trace_flags, root_context.trace_flags)
    }
    None => assert_true(false)
  }
  
  // 测试无效traceparent格式
  let invalid_headers = {
    traceparent: "invalid-format",
    tracestate: "",
    baggage: []
  }
  
  let invalid_context = inject_context(invalid_headers)
  assert_eq(invalid_context, None)
}

// 测试2: 跨服务上下文传播
test "跨服务上下文传播" {
  // 定义服务间调用结构
  type ServiceCall = {
    service_name: String,
    operation: String,
    incoming_context: Option[TraceContext],
    outgoing_context: Option[TraceContext],
    headers: Array[(String, String)]
  }
  
  // 定义追踪上下文结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: String
  }
  
  // 创建服务调用链
  let create_service_chain = fn(services: Array[String]) {
    let mut calls = []
    let mut current_context = None
    
    for i in 0..services.length() {
      let service_name = services[i]
      let operation = "process_request"
      
      // 生成新的span_id
      let new_span_id = "span-" + (Time::now() + i * 1000).to_string()
      
      // 创建或更新上下文
      let context = match current_context {
        Some(ctx) => {
          {
            trace_id: ctx.trace_id,
            span_id: new_span_id,
            parent_span_id: Some(ctx.span_id),
            trace_flags: ctx.trace_flags,
            trace_state: ctx.trace_state
          }
        }
        None => {
          // 根服务，创建新的trace_id
          let trace_id = "trace-" + Time::now().to_string()
          {
            trace_id: trace_id,
            span_id: new_span_id,
            parent_span_id: None,
            trace_flags: 1,
            trace_state: ""
          }
        }
      }
      
      // 创建传播头
      let headers = [
        ("traceparent", "00-" + context.trace_id + "-" + context.span_id + "-01"),
        ("x-service-name", service_name),
        ("x-operation-name", operation)
      ]
      
      // 添加父span信息
      match context.parent_span_id {
        Some(parent_id) => {
          headers = headers + [("x-parent-span", parent_id)]
        }
        None => {}
      }
      
      let call = {
        service_name: service_name,
        operation: operation,
        incoming_context: current_context,
        outgoing_context: Some(context),
        headers: headers
      }
      
      calls = calls.push(call)
      current_context = Some(context)
    }
    
    calls
  }
  
  // 验证服务调用链
  let validate_chain = fn(calls: Array[ServiceCall]) {
    if calls.length() == 0 {
      return true
    }
    
    // 验证第一个调用是根调用
    let first_call = calls[0]
    match first_call.incoming_context {
      Some(_) => return false  // 根调用不应该有传入上下文
      None => {}
    }
    
    match first_call.outgoing_context {
      Some(ctx) => {
        if ctx.parent_span_id.is_some() {
          return false  // 根调用不应该有父span
        }
      }
      None => return false  // 应该有传出上下文
    }
    
    // 验证后续调用
    for i in 1..calls.length() {
      let current_call = calls[i]
      let previous_call = calls[i - 1]
      
      // 验证传入上下文
      match current_call.incoming_context {
        Some(incoming_ctx) => {
          match previous_call.outgoing_context {
            Some(outgoing_ctx) => {
              if incoming_ctx.trace_id != outgoing_ctx.trace_id {
                return false  // trace_id应该一致
              }
              
              if incoming_ctx.parent_span_id != Some(outgoing_ctx.span_id) {
                return false  // 父span_id应该匹配
              }
            }
            None => return false
          }
        }
        None => return false  // 非根调用应该有传入上下文
      }
    }
    
    true
  }
  
  // 创建服务链
  let services = ["api-gateway", "auth-service", "payment-service", "notification-service"]
  let service_calls = create_service_chain(services)
  
  // 验证服务调用链
  assert_eq(service_calls.length(), 4)
  assert_true(validate_chain(service_calls))
  
  // 验证第一个调用（根调用）
  let root_call = service_calls[0]
  assert_eq(root_call.service_name, "api-gateway")
  assert_eq(root_call.incoming_context, None)
  assert_true(root_call.outgoing_context.is_some())
  
  match root_call.outgoing_context {
    Some(ctx) => {
      assert_eq(ctx.parent_span_id, None)
      assert_true(ctx.trace_flags == 1)
    }
    None => assert_true(false)
  }
  
  // 验证最后一个调用
  let last_call = service_calls[3]
  assert_eq(last_call.service_name, "notification-service")
  
  match last_call.outgoing_context {
    Some(ctx) => {
      match ctx.parent_span_id {
        Some(parent_id) => {
          // 验证父span是前一个调用的span
          match service_calls[2].outgoing_context {
            Some(prev_ctx) => {
              assert_eq(parent_id, prev_ctx.span_id)
            }
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证所有调用具有相同的trace_id
  let trace_ids = service_calls.map(fn(call) {
    match call.outgoing_context {
      Some(ctx) => ctx.trace_id
      None => ""
    }
  })
  
  let first_trace_id = trace_ids[0]
  for trace_id in trace_ids {
    assert_eq(trace_id, first_trace_id)
  }
}

// 测试3: 异步上下文传播
test "异步上下文传播" {
  // 定义异步任务结构
  type AsyncTask = {
    task_id: String,
    parent_context: TraceContext,
    task_type: String,
    created_at: Int,
    completed_at: Option[Int]
  }
  
  // 定义追踪上下文结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: String
  }
  
  // 创建异步任务
  let create_async_task = fn(parent_context: TraceContext, task_type: String) {
    let task_id = "task-" + Time::now().to_string()
    let span_id = "async-" + (Time::now() + 1000).to_string()
    
    {
      task_id: task_id,
      parent_context: {
        trace_id: parent_context.trace_id,
        span_id: span_id,
        parent_span_id: Some(parent_context.span_id),
        trace_flags: parent_context.trace_flags,
        trace_state: parent_context.trace_state
      },
      task_type: task_type,
      created_at: Time::now(),
      completed_at: None
    }
  }
  
  // 完成异步任务
  let complete_async_task = fn(task: AsyncTask) {
    {
      task_id: task.task_id,
      parent_context: task.parent_context,
      task_type: task.task_type,
      created_at: task.created_at,
      completed_at: Some(Time::now())
    }
  }
  
  // 创建任务链
  let create_task_chain = fn(root_context: TraceContext, task_types: Array[String]) {
    let mut tasks = []
    let mut current_context = root_context
    
    for task_type in task_types {
      let task = create_async_task(current_context, task_type)
      tasks = tasks.push(task)
      current_context = task.parent_context
    }
    
    tasks
  }
  
  // 验证任务链
  let validate_task_chain = fn(tasks: Array[AsyncTask], root_context: TraceContext) {
    if tasks.length() == 0 {
      return true
    }
    
    // 验证所有任务具有相同的trace_id
    for task in tasks {
      if task.parent_context.trace_id != root_context.trace_id {
        return false
      }
    }
    
    // 验证父子关系
    for i in 0..tasks.length() {
      let task = tasks[i]
      
      if i == 0 {
        // 第一个任务的父span应该是根span
        match task.parent_context.parent_span_id {
          Some(parent_id) => {
            if parent_id != root_context.span_id {
              return false
            }
          }
          None => return false
        }
      } else {
        // 其他任务的父span应该是前一个任务的span
        let prev_task = tasks[i - 1]
        match task.parent_context.parent_span_id {
          Some(parent_id) => {
            if parent_id != prev_task.parent_context.span_id {
              return false
            }
          }
          None => return false
        }
      }
    }
    
    true
  }
  
  // 创建根上下文
  let root_context = {
    trace_id: "trace-async-12345",
    span_id: "span-async-67890",
    parent_span_id: None,
    trace_flags: 1,
    trace_state: ""
  }
  
  // 创建任务类型
  let task_types = ["data_fetch", "data_transform", "data_store", "notification"]
  
  // 创建任务链
  let task_chain = create_task_chain(root_context, task_types)
  
  // 验证任务链
  assert_eq(task_chain.length(), 4)
  assert_true(validate_task_chain(task_chain, root_context))
  
  // 完成所有任务
  let mut completed_tasks = []
  for task in task_chain {
    let completed_task = complete_async_task(task)
    completed_tasks = completed_tasks.push(completed_task)
  }
  
  // 验证任务完成
  assert_eq(completed_tasks.length(), 4)
  for task in completed_tasks {
    assert_true(task.completed_at.is_some())
    assert_true(task.completed_at.get_or(0) >= task.created_at)
  }
  
  // 验证上下文传播
  for i in 0..completed_tasks.length() {
    let task = completed_tasks[i]
    assert_eq(task.parent_context.trace_id, root_context.trace_id)
    
    if i == 0 {
      match task.parent_context.parent_span_id {
        Some(parent_id) => assert_eq(parent_id, root_context.span_id)
        None => assert_true(false)
      }
    } else {
      let prev_task = completed_tasks[i - 1]
      match task.parent_context.parent_span_id {
        Some(parent_id) => assert_eq(parent_id, prev_task.parent_context.span_id)
        None => assert_true(false)
      }
    }
  }
}

// 测试4: 跨线程上下文传播
test "跨线程上下文传播" {
  // 定义线程上下文结构
  type ThreadContext = {
    thread_id: String,
    parent_context: TraceContext,
    created_at: Int
  }
  
  // 定义追踪上下文结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: String
  }
  
  // 创建线程上下文
  let create_thread_context = fn(parent_context: TraceContext, thread_id: String) {
    let span_id = "thread-" + thread_id
    
    {
      thread_id: thread_id,
      parent_context: {
        trace_id: parent_context.trace_id,
        span_id: span_id,
        parent_span_id: Some(parent_context.span_id),
        trace_flags: parent_context.trace_flags,
        trace_state: parent_context.trace_state
      },
      created_at: Time::now()
    }
  }
  
  // 创建线程池
  let create_thread_pool = fn(parent_context: TraceContext, thread_count: Int) {
    let mut threads = []
    
    for i in 0..thread_count {
      let thread_id = "worker-" + i.to_string()
      let thread_context = create_thread_context(parent_context, thread_id)
      threads = threads.push(thread_context)
    }
    
    threads
  }
  
  // 验证线程池上下文
  let validate_thread_pool = fn(threads: Array[ThreadContext], parent_context: TraceContext) {
    if threads.length() == 0 {
      return true
    }
    
    // 验证所有线程具有相同的trace_id
    for thread in threads {
      if thread.parent_context.trace_id != parent_context.trace_id {
        return false
      }
    }
    
    // 验证所有线程的父span是同一个
    for thread in threads {
      match thread.parent_context.parent_span_id {
        Some(parent_id) => {
          if parent_id != parent_context.span_id {
            return false
          }
        }
        None => return false
      }
    }
    
    true
  }
  
  // 创建根上下文
  let root_context = {
    trace_id: "trace-thread-12345",
    span_id: "span-thread-67890",
    parent_span_id: None,
    trace_flags: 1,
    trace_state: ""
  }
  
  // 创建线程池
  let thread_pool = create_thread_pool(root_context, 5)
  
  // 验证线程池
  assert_eq(thread_pool.length(), 5)
  assert_true(validate_thread_pool(thread_pool, root_context))
  
  // 验证每个线程的上下文
  for i in 0..thread_pool.length() {
    let thread = thread_pool[i]
    assert_eq(thread.thread_id, "worker-" + i.to_string())
    assert_eq(thread.parent_context.trace_id, root_context.trace_id)
    assert_eq(thread.parent_context.span_id, "thread-" + thread.thread_id)
    
    match thread.parent_context.parent_span_id {
      Some(parent_id) => assert_eq(parent_id, root_context.span_id)
      None => assert_true(false)
    }
  }
  
  // 模拟线程间通信
  let simulate_communication = fn(source_thread: ThreadContext, target_thread: ThreadContext) {
    // 创建从源线程到目标线程的通信上下文
    let comm_span_id = "comm-" + source_thread.thread_id + "-to-" + target_thread.thread_id
    
    {
      source_thread_id: source_thread.thread_id,
      target_thread_id: target_thread.thread_id,
      communication_context: {
        trace_id: source_thread.parent_context.trace_id,
        span_id: comm_span_id,
        parent_span_id: Some(source_thread.parent_context.span_id),
        trace_flags: source_thread.parent_context.trace_flags,
        trace_state: source_thread.parent_context.trace_state
      },
      created_at: Time::now()
    }
  }
  
  // 测试线程间通信
  let comm_0_to_1 = simulate_communication(thread_pool[0], thread_pool[1])
  assert_eq(comm_0_to_1.source_thread_id, "worker-0")
  assert_eq(comm_0_to_1.target_thread_id, "worker-1")
  assert_eq(comm_0_to_1.communication_context.trace_id, root_context.trace_id)
  
  match comm_0_to_1.communication_context.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, thread_pool[0].parent_context.span_id)
    None => assert_true(false)
  }
}

// 测试5: 跨进程上下文传播
test "跨进程上下文传播" {
  // 定义进程上下文结构
  type ProcessContext = {
    process_id: String,
    process_name: String,
    parent_context: TraceContext,
    environment: Array[(String, String)]
  }
  
  // 定义追踪上下文结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: String
  }
  
  // 创建进程上下文
  let create_process_context = fn(parent_context: TraceContext, process_id: String, process_name: String) {
    let span_id = "process-" + process_id
    
    {
      process_id: process_id,
      process_name: process_name,
      parent_context: {
        trace_id: parent_context.trace_id,
        span_id: span_id,
        parent_span_id: Some(parent_context.span_id),
        trace_flags: parent_context.trace_flags,
        trace_state: parent_context.trace_state
      },
      environment: [
        ("TRACE_ID", parent_context.trace_id),
        ("PARENT_SPAN_ID", parent_context.span_id),
        ("PROCESS_ID", process_id)
      ]
    }
  }
  
  // 从环境变量提取上下文
  let extract_from_environment = fn(environment: Array[(String, String)]) {
    let trace_id = environment.find(fn(env) { env.0 == "TRACE_ID" }).map(fn(env) { env.1 })
    let parent_span_id = environment.find(fn(env) { env.0 == "PARENT_SPAN_ID" }).map(fn(env) { env.1 })
    let process_id = environment.find(fn(env) { env.0 == "PROCESS_ID" }).map(fn(env) { env.1 })
    
    match (trace_id, parent_span_id, process_id) {
      (Some(tid), Some(psid), Some(pid)) => {
        Some({
          trace_id: tid,
          span_id: "process-" + pid,
          parent_span_id: Some(psid),
          trace_flags: 1,
          trace_state: ""
        })
      }
      _ => None
    }
  }
  
  // 创建进程链
  let create_process_chain = fn(root_context: TraceContext, processes: Array[(String, String)]) {
    let mut process_contexts = []
    let mut current_context = root_context
    
    for process in processes {
      let (process_id, process_name) = process
      let process_context = create_process_context(current_context, process_id, process_name)
      process_contexts = process_contexts.push(process_context)
      current_context = process_context.parent_context
    }
    
    process_contexts
  }
  
  // 验证进程链
  let validate_process_chain = fn(processes: Array[ProcessContext], root_context: TraceContext) {
    if processes.length() == 0 {
      return true
    }
    
    // 验证所有进程具有相同的trace_id
    for process in processes {
      if process.parent_context.trace_id != root_context.trace_id {
        return false
      }
    }
    
    // 验证父子关系
    for i in 0..processes.length() {
      let process = processes[i]
      
      if i == 0 {
        // 第一个进程的父span应该是根span
        match process.parent_context.parent_span_id {
          Some(parent_id) => {
            if parent_id != root_context.span_id {
              return false
            }
          }
          None => return false
        }
      } else {
        // 其他进程的父span应该是前一个进程的span
        let prev_process = processes[i - 1]
        match process.parent_context.parent_span_id {
          Some(parent_id) => {
            if parent_id != prev_process.parent_context.span_id {
              return false
            }
          }
          None => return false
        }
      }
    }
    
    true
  }
  
  // 创建根上下文
  let root_context = {
    trace_id: "trace-process-12345",
    span_id: "span-process-67890",
    parent_span_id: None,
    trace_flags: 1,
    trace_state: ""
  }
  
  // 创建进程列表
  let processes = [
    ("proc-001", "web-server"),
    ("proc-002", "auth-service"),
    ("proc-003", "database"),
    ("proc-004", "cache-service")
  ]
  
  // 创建进程链
  let process_chain = create_process_chain(root_context, processes)
  
  // 验证进程链
  assert_eq(process_chain.length(), 4)
  assert_true(validate_process_chain(process_chain, root_context))
  
  // 验证每个进程的上下文
  for i in 0..process_chain.length() {
    let process = process_chain[i]
    let (process_id, process_name) = processes[i]
    
    assert_eq(process.process_id, process_id)
    assert_eq(process.process_name, process_name)
    assert_eq(process.parent_context.trace_id, root_context.trace_id)
    assert_eq(process.parent_context.span_id, "process-" + process_id)
    
    // 验证环境变量
    let extracted_context = extract_from_environment(process.environment)
    assert_true(extracted_context.is_some())
    
    match extracted_context {
      Some(ctx) => {
        assert_eq(ctx.trace_id, root_context.trace_id)
        assert_eq(ctx.span_id, "process-" + process_id)
        
        if i == 0 {
          match ctx.parent_span_id {
            Some(parent_id) => assert_eq(parent_id, root_context.span_id)
            None => assert_true(false)
          }
        } else {
          let prev_process = process_chain[i - 1]
          match ctx.parent_span_id {
            Some(parent_id) => assert_eq(parent_id, prev_process.parent_context.span_id)
            None => assert_true(false)
          }
        }
      }
      None => assert_true(false)
    }
  }
}

// 测试6: 消息队列上下文传播
test "消息队列上下文传播" {
  // 定义消息结构
  type Message = {
    message_id: String,
    topic: String,
    payload: String,
    headers: Array[(String, String)],
    trace_context: Option[TraceContext]
  }
  
  // 定义追踪上下文结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: String
  }
  
  // 创建带有追踪上下文的消息
  let create_message_with_context = fn(topic: String, payload: String, context: TraceContext) {
    let message_id = "msg-" + Time::now().to_string()
    
    let headers = [
      ("traceparent", "00-" + context.trace_id + "-" + context.span_id + "-01"),
      ("message-id", message_id),
      ("topic", topic),
      ("timestamp", Time::now().to_string())
    ]
    
    {
      message_id: message_id,
      topic: topic,
      payload: payload,
      headers: headers,
      trace_context: Some(context)
    }
  }
  
  // 从消息头提取上下文
  let extract_context_from_message = fn(message: Message) {
    match message.trace_context {
      Some(ctx) => Some(ctx)
      None => {
        // 尝试从headers提取
        let traceparent = message.headers.find(fn(header) { header.0 == "traceparent" })
        match traceparent {
          Some((_, value)) => {
            let parts = value.split("-")
            if parts.length() >= 4 {
              let trace_id = parts[1]
              let span_id = parts[2]
              let trace_flags = Int::from_str_radix(parts[3], 16).get_or(0)
              
              Some({
                trace_id: trace_id,
                span_id: span_id,
                parent_span_id: None,
                trace_flags: trace_flags,
                trace_state: ""
              })
            } else {
              None
            }
          }
          None => None
        }
      }
    }
  }
  
  // 创建消息生产者
  let create_producer = fn(producer_id: String, initial_context: TraceContext) {
    let producer_span_id = "producer-" + producer_id
    
    {
      producer_id: producer_id,
      context: {
        trace_id: initial_context.trace_id,
        span_id: producer_span_id,
        parent_span_id: Some(initial_context.span_id),
        trace_flags: initial_context.trace_flags,
        trace_state: initial_context.trace_state
      },
      produce_message: fn(topic: String, payload: String) {
        let message_id = "msg-" + producer_id + "-" + Time::now().to_string()
        let message_span_id = "message-" + message_id
        
        let message_context = {
          trace_id: initial_context.trace_id,
          span_id: message_span_id,
          parent_span_id: Some(producer_span_id),
          trace_flags: initial_context.trace_flags,
          trace_state: initial_context.trace_state
        }
        
        create_message_with_context(topic, payload, message_context)
      }
    }
  }
  
  // 创建消息消费者
  let create_consumer = fn(consumer_id: String) {
    let consumer_span_id = "consumer-" + consumer_id
    
    {
      consumer_id: consumer_id,
      consume_message: fn(message: Message) {
        let message_context = extract_context_from_message(message)
        
        match message_context {
          Some(ctx) => {
            let consumer_context = {
              trace_id: ctx.trace_id,
              span_id: consumer_span_id + "-" + Time::now().to_string(),
              parent_span_id: Some(ctx.span_id),
              trace_flags: ctx.trace_flags,
              trace_state: ctx.trace_state
            }
            
            {
              message: message,
              consumer_context: Some(consumer_context),
              processed: true
            }
          }
          None => {
            {
              message: message,
              consumer_context: None,
              processed: false
            }
          }
        }
      }
    }
  }
  
  // 创建根上下文
  let root_context = {
    trace_id: "trace-msg-12345",
    span_id: "span-msg-67890",
    parent_span_id: None,
    trace_flags: 1,
    trace_state: ""
  }
  
  // 创建生产者
  let producer = create_producer("prod-001", root_context)
  
  // 生产消息
  let message1 = producer.produce_message("payment.requests", '{"user_id": "user-123", "amount": 100.50}')
  let message2 = producer.produce_message("notification.emails", '{"to": "user@example.com", "template": "receipt"}')
  
  // 验证消息
  assert_eq(message1.topic, "payment.requests")
  assert_eq(message2.topic, "notification.emails")
  assert_true(message1.trace_context.is_some())
  assert_true(message2.trace_context.is_some())
  
  match message1.trace_context {
    Some(ctx) => {
      assert_eq(ctx.trace_id, root_context.trace_id)
      match ctx.parent_span_id {
        Some(parent_id) => assert_eq(parent_id, producer.context.span_id)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 创建消费者
  let consumer = create_consumer("cons-001")
  
  // 消费消息
  let processed1 = consumer.consume_message(message1)
  let processed2 = consumer.consume_message(message2)
  
  // 验证消费结果
  assert_true(processed1.processed)
  assert_true(processed2.processed)
  assert_true(processed1.consumer_context.is_some())
  assert_true(processed2.consumer_context.is_some())
  
  match processed1.consumer_context {
    Some(ctx) => {
      assert_eq(ctx.trace_id, root_context.trace_id)
      match ctx.parent_span_id {
        Some(parent_id) => {
          match message1.trace_context {
            Some(msg_ctx) => assert_eq(parent_id, msg_ctx.span_id)
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试7: 跨协议上下文传播
test "跨协议上下文传播" {
  // 定义协议适配器
  type ProtocolAdapter = {
    protocol_name: String,
    extract_context: fn(String) -> Option[TraceContext],
    inject_context: fn(TraceContext, String) -> String
  }
  
  // 定义追踪上下文结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: String
  }
  
  // HTTP协议适配器
  let http_adapter = {
    protocol_name: "HTTP",
    extract_context: fn(headers: String) {
      let lines = headers.split("\n")
      let traceparent_line = lines.find(fn(line) { line.starts_with("traceparent:") })
      
      match traceparent_line {
        Some(line) => {
          let parts = line.split(":")
          if parts.length() >= 2 {
            let traceparent = parts[1].trim()
            let tp_parts = traceparent.split("-")
            if tp_parts.length() >= 4 {
              let trace_id = tp_parts[1]
              let span_id = tp_parts[2]
              let trace_flags = Int::from_str_radix(tp_parts[3], 16).get_or(0)
              
              Some({
                trace_id: trace_id,
                span_id: span_id,
                parent_span_id: None,
                trace_flags: trace_flags,
                trace_state: ""
              })
            } else {
              None
            }
          } else {
            None
          }
        }
        None => None
      }
    },
    inject_context: fn(context: TraceContext, headers: String) {
      let traceparent = "traceparent: 00-" + context.trace_id + "-" + context.span_id + "-01"
      headers + "\n" + traceparent
    }
  }
  
  // gRPC协议适配器
  let grpc_adapter = {
    protocol_name: "gRPC",
    extract_context: fn(metadata: String) {
      let entries = metadata.split(",")
      let traceparent_entry = entries.find(fn(entry) { entry.starts_with("traceparent=") })
      
      match traceparent_entry {
        Some(entry) => {
          let parts = entry.split("=")
          if parts.length() >= 2 {
            let traceparent = parts[1]
            let tp_parts = traceparent.split("-")
            if tp_parts.length() >= 4 {
              let trace_id = tp_parts[1]
              let span_id = tp_parts[2]
              let trace_flags = Int::from_str_radix(tp_parts[3], 16).get_or(0)
              
              Some({
                trace_id: trace_id,
                span_id: span_id,
                parent_span_id: None,
                trace_flags: trace_flags,
                trace_state: ""
              })
            } else {
              None
            }
          } else {
            None
          }
        }
        None => None
      }
    },
    inject_context: fn(context: TraceContext, metadata: String) {
      let traceparent = "traceparent=00-" + context.trace_id + "-" + context.span_id + "-01"
      if metadata.length() > 0 {
        metadata + "," + traceparent
      } else {
        traceparent
      }
    }
  }
  
  // AMQP协议适配器
  let amqp_adapter = {
    protocol_name: "AMQP",
    extract_context: fn(properties: String) {
      let entries = properties.split(";")
      let traceparent_entry = entries.find(fn(entry) { entry.starts_with("traceparent=") })
      
      match traceparent_entry {
        Some(entry) => {
          let parts = entry.split("=")
          if parts.length() >= 2 {
            let traceparent = parts[1]
            let tp_parts = traceparent.split("-")
            if tp_parts.length() >= 4 {
              let trace_id = tp_parts[1]
              let span_id = tp_parts[2]
              let trace_flags = Int::from_str_radix(tp_parts[3], 16).get_or(0)
              
              Some({
                trace_id: trace_id,
                span_id: span_id,
                parent_span_id: None,
                trace_flags: trace_flags,
                trace_state: ""
              })
            } else {
              None
            }
          } else {
            None
          }
        }
        None => None
      }
    },
    inject_context: fn(context: TraceContext, properties: String) {
      let traceparent = "traceparent=00-" + context.trace_id + "-" + context.span_id + "-01"
      if properties.length() > 0 {
        properties + ";" + traceparent
      } else {
        traceparent
      }
    }
  }
  
  // 创建适配器列表
  let adapters = [http_adapter, grpc_adapter, amqp_adapter]
  
  // 创建测试上下文
  let test_context = {
    trace_id: "trace-cross-12345",
    span_id: "span-cross-67890",
    parent_span_id: None,
    trace_flags: 1,
    trace_state: ""
  }
  
  // 测试HTTP协议
  let http_headers = "GET /api/payments HTTP/1.1\nHost: api.example.com\nContent-Type: application/json"
  let http_with_context = http_adapter.inject_context(test_context, http_headers)
  
  assert_true(http_with_context.contains("traceparent: 00-trace-cross-12345-span-cross-67890-01"))
  
  let extracted_http_context = http_adapter.extract_context(http_with_context)
  assert_true(extracted_http_context.is_some())
  
  match extracted_http_context {
    Some(ctx) => {
      assert_eq(ctx.trace_id, test_context.trace_id)
      assert_eq(ctx.span_id, test_context.span_id)
    }
    None => assert_true(false)
  }
  
  // 测试gRPC协议
  let grpc_metadata = "authorization=Bearer token123,content-type=application/grpc"
  let grpc_with_context = grpc_adapter.inject_context(test_context, grpc_metadata)
  
  assert_true(grpc_with_context.contains("traceparent=00-trace-cross-12345-span-cross-67890-01"))
  
  let extracted_grpc_context = grpc_adapter.extract_context(grpc_with_context)
  assert_true(extracted_grpc_context.is_some())
  
  match extracted_grpc_context {
    Some(ctx) => {
      assert_eq(ctx.trace_id, test_context.trace_id)
      assert_eq(ctx.span_id, test_context.span_id)
    }
    None => assert_true(false)
  }
  
  // 测试AMQP协议
  let amqp_properties = "content-type=application/json;delivery-mode=2"
  let amqp_with_context = amqp_adapter.inject_context(test_context, amqp_properties)
  
  assert_true(amqp_with_context.contains("traceparent=00-trace-cross-12345-span-cross-67890-01"))
  
  let extracted_amqp_context = amqp_adapter.extract_context(amqp_with_context)
  assert_true(extracted_amqp_context.is_some())
  
  match extracted_amqp_context {
    Some(ctx) => {
      assert_eq(ctx.trace_id, test_context.trace_id)
      assert_eq(ctx.span_id, test_context.span_id)
    }
    None => assert_true(false)
  }
  
  // 测试跨协议转换
  let cross_protocol_convert = fn(source_adapter: ProtocolAdapter, target_adapter: ProtocolAdapter, data: String) {
    match source_adapter.extract_context(data) {
      Some(context) => {
        let empty_target_data = ""
        target_adapter.inject_context(context, empty_target_data)
      }
      None => "conversion_failed"
    }
  }
  
  // HTTP到gRPC转换
  let http_to_grpc = cross_protocol_convert(http_adapter, grpc_adapter, http_with_context)
  assert_false(http_to_grpc == "conversion_failed")
  assert_true(http_to_grpc.contains("traceparent=00-trace-cross-12345-span-cross-67890-01"))
  
  // gRPC到AMQP转换
  let grpc_to_amqp = cross_protocol_convert(grpc_adapter, amqp_adapter, grpc_with_context)
  assert_false(grpc_to_amqp == "conversion_failed")
  assert_true(grpc_to_amqp.contains("traceparent=00-trace-cross-12345-span-cross-67890-01"))
  
  // AMQP到HTTP转换
  let amqp_to_http = cross_protocol_convert(amqp_adapter, http_adapter, amqp_with_context)
  assert_false(amqp_to_http == "conversion_failed")
  assert_true(amqp_to_http.contains("traceparent: 00-trace-cross-12345-span-cross-67890-01"))
}

// 测试8: 上下文传播边界情况
test "上下文传播边界情况" {
  // 定义追踪上下文结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: String
  }
  
  // 创建空上下文
  let empty_context = {
    trace_id: "",
    span_id: "",
    parent_span_id: None,
    trace_flags: 0,
    trace_state: ""
  }
  
  // 创建最大长度上下文
  let max_trace_id = "a".repeat(128)
  let max_span_id = "b".repeat(128)
  let max_trace_state = "c".repeat(512)
  
  let max_context = {
    trace_id: max_trace_id,
    span_id: max_span_id,
    parent_span_id: None,
    trace_flags: 255,
    trace_state: max_trace_state
  }
  
  // 上下文验证函数
  let validate_context = fn(context: TraceContext) {
    let errors = []
    
    if context.trace_id.length() == 0 {
      errors = errors.push("trace_id不能为空")
    } else if context.trace_id.length() > 128 {
      errors = errors.push("trace_id长度不能超过128")
    }
    
    if context.span_id.length() == 0 {
      errors = errors.push("span_id不能为空")
    } else if context.span_id.length() > 128 {
      errors = errors.push("span_id长度不能超过128")
    }
    
    if context.trace_state.length() > 512 {
      errors = errors.push("trace_state长度不能超过512")
    }
    
    if context.trace_flags < 0 || context.trace_flags > 255 {
      errors = errors.push("trace_flags必须在0-255范围内")
    }
    
    errors
  }
  
  // 测试空上下文
  let empty_errors = validate_context(empty_context)
  assert_eq(empty_errors.length(), 2)
  assert_true(empty_errors.contains("trace_id不能为空"))
  assert_true(empty_errors.contains("span_id不能为空"))
  
  // 测试最大长度上下文
  let max_errors = validate_context(max_context)
  assert_eq(max_errors.length(), 0)
  
  // 测试超长trace_id
  let too_long_trace_id = {
    trace_id: "a".repeat(129),
    span_id: "valid-span",
    parent_span_id: None,
    trace_flags: 1,
    trace_state: ""
  }
  
  let too_long_errors = validate_context(too_long_trace_id)
  assert_eq(too_long_errors.length(), 1)
  assert_true(too_long_errors.contains("trace_id长度不能超过128"))
  
  // 测试无效trace_flags
  let invalid_flags = {
    trace_id: "valid-trace",
    span_id: "valid-span",
    parent_span_id: None,
    trace_flags: 256,
    trace_state: ""
  }
  
  let invalid_flags_errors = validate_context(invalid_flags)
  assert_eq(invalid_flags_errors.length(), 1)
  assert_true(invalid_flags_errors.contains("trace_flags必须在0-255范围内"))
  
  // 测试上下文传播中的错误处理
  let safe_extract_context = fn(data: String) {
    if data.length() == 0 {
      None
    } else if data.contains("invalid") {
      None
    } else if data.contains("malformed") {
      None
    } else {
      Some({
        trace_id: "extracted-trace",
        span_id: "extracted-span",
        parent_span_id: None,
        trace_flags: 1,
        trace_state: ""
      })
    }
  }
  
  // 测试安全提取
  assert_eq(safe_extract_context(""), None)
  assert_eq(safe_extract_context("invalid-data"), None)
  assert_eq(safe_extract_context("malformed-data"), None)
  assert_true(safe_extract_context("valid-data").is_some())
  
  // 测试上下文传播中的降级处理
  let fallback_context = {
    trace_id: "fallback-trace",
    span_id: "fallback-span",
    parent_span_id: None,
    trace_flags: 1,
    trace_state: ""
  }
  
  let extract_with_fallback = fn(data: String) {
    match safe_extract_context(data) {
      Some(context) => context
      None => fallback_context
    }
  }
  
  // 测试降级处理
  let extracted1 = extract_with_fallback("")
  assert_eq(extracted1.trace_id, fallback_context.trace_id)
  
  let extracted2 = extract_with_fallback("valid-data")
  assert_eq(extracted2.trace_id, "extracted-trace")
  
  // 测试上下文传播中的循环检测
  let detect_cycles = fn(contexts: Array<TraceContext>) {
    let seen = Map::empty()
    let mut has_cycle = false
    
    for context in contexts {
      let key = context.trace_id + ":" + context.span_id
      if Map::contains_key(seen, key) {
        has_cycle = true
        break
      } else {
        let _ = Map::insert(seen, key, true)
      }
    }
    
    has_cycle
  }
  
  // 测试循环检测
  let no_cycle_contexts = [
    {trace_id: "trace1", span_id: "span1", parent_span_id: None, trace_flags: 1, trace_state: ""},
    {trace_id: "trace2", span_id: "span2", parent_span_id: None, trace_flags: 1, trace_state: ""},
    {trace_id: "trace3", span_id: "span3", parent_span_id: None, trace_flags: 1, trace_state: ""}
  ]
  
  let has_cycle_contexts = [
    {trace_id: "trace1", span_id: "span1", parent_span_id: None, trace_flags: 1, trace_state: ""},
    {trace_id: "trace2", span_id: "span2", parent_span_id: None, trace_flags: 1, trace_state: ""},
    {trace_id: "trace1", span_id: "span1", parent_span_id: None, trace_flags: 1, trace_state: ""}
  ]
  
  assert_false(detect_cycles(no_cycle_contexts))
  assert_true(detect_cycles(has_cycle_contexts))
}

// 测试9: 上下文传播性能测试
test "上下文传播性能测试" {
  // 定义追踪上下文结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: String
  }
  
  // 创建大量上下文
  let create_contexts = fn(count: Int) {
    let mut contexts = []
    
    for i in 0..count {
      let trace_id = "trace-perf-" + i.to_string()
      let span_id = "span-perf-" + i.to_string()
      let parent_span_id = if i > 0 { Some("span-perf-" + (i - 1).to_string()) } else { None }
      
      contexts = contexts.push({
        trace_id: trace_id,
        span_id: span_id,
        parent_span_id: parent_span_id,
        trace_flags: i % 2,
        trace_state: "state-" + (i % 10).to_string()
      })
    }
    
    contexts
  }
  
  // 序列化上下文
  let serialize_context = fn(context: TraceContext) {
    let parent_span_str = match context.parent_span_id {
      Some(id) => id
      None => ""
    }
    
    context.trace_id + "|" + context.span_id + "|" + parent_span_str + "|" + 
    context.trace_flags.to_string() + "|" + context.trace_state
  }
  
  // 反序列化上下文
  let deserialize_context = fn(data: String) {
    let parts = data.split("|")
    if parts.length() >= 5 {
      let trace_id = parts[0]
      let span_id = parts[1]
      let parent_span_str = parts[2]
      let trace_flags = parts[3].to_int()
      let trace_state = parts[4]
      
      let parent_span_id = if parent_span_str.length() > 0 {
        Some(parent_span_str)
      } else {
        None
      }
      
      Some({
        trace_id: trace_id,
        span_id: span_id,
        parent_span_id: parent_span_id,
        trace_flags: trace_flags,
        trace_state: trace_state
      })
    } else {
      None
    }
  }
  
  // 批量序列化
  let batch_serialize = fn(contexts: Array[TraceContext>) {
    contexts.map(serialize_context)
  }
  
  // 批量反序列化
  let batch_deserialize = fn(serialized_data: Array<String>) {
    let mut results = []
    
    for data in serialized_data {
      match deserialize_context(data) {
        Some(context) => results = results.push(context)
        None => {}
      }
    }
    
    results
  }
  
  // 创建不同规模的测试数据
  let small_contexts = create_contexts(100)
  let medium_contexts = create_contexts(1000)
  let large_contexts = create_contexts(10000)
  
  // 测试序列化性能
  let start_time = Time::now()
  let small_serialized = batch_serialize(small_contexts)
  let small_serialize_time = Time::now() - start_time
  
  let start_time = Time::now()
  let medium_serialized = batch_serialize(medium_contexts)
  let medium_serialize_time = Time::now() - start_time
  
  let start_time = Time::now()
  let large_serialized = batch_serialize(large_contexts)
  let large_serialize_time = Time::now() - start_time
  
  // 验证序列化结果
  assert_eq(small_serialized.length(), 100)
  assert_eq(medium_serialized.length(), 1000)
  assert_eq(large_serialized.length(), 10000)
  
  // 验证时间复杂度（应该接近线性）
  assert_true(medium_serialize_time >= small_serialize_time)
  assert_true(large_serialize_time >= medium_serialize_time)
  
  // 测试反序列化性能
  let start_time = Time::now()
  let small_deserialized = batch_deserialize(small_serialized)
  let small_deserialize_time = Time::now() - start_time
  
  let start_time = Time::now()
  let medium_deserialized = batch_deserialize(medium_serialized)
  let medium_deserialize_time = Time::now() - start_time
  
  let start_time = Time::now()
  let large_deserialized = batch_deserialize(large_serialized)
  let large_deserialize_time = Time::now() - start_time
  
  // 验证反序列化结果
  assert_eq(small_deserialized.length(), 100)
  assert_eq(medium_deserialized.length(), 1000)
  assert_eq(large_deserialized.length(), 10000)
  
  // 验证时间复杂度（应该接近线性）
  assert_true(medium_deserialize_time >= small_deserialize_time)
  assert_true(large_deserialize_time >= medium_deserialize_time)
  
  // 验证数据完整性
  assert_eq(small_deserialized[0].trace_id, "trace-perf-0")
  assert_eq(small_deserialized[99].trace_id, "trace-perf-99")
  
  assert_eq(medium_deserialized[0].trace_id, "trace-perf-0")
  assert_eq(medium_deserialized[999].trace_id, "trace-perf-999")
  
  assert_eq(large_deserialized[0].trace_id, "trace-perf-0")
  assert_eq(large_deserialized[9999].trace_id, "trace-perf-9999")
  
  // 验证父子关系
  assert_eq(small_deserialized[50].parent_span_id, Some("span-perf-49"))
  assert_eq(medium_deserialized[500].parent_span_id, Some("span-perf-499"))
  assert_eq(large_deserialized[5000].parent_span_id, Some("span-perf-4999"))
  
  // 验证根节点
  assert_eq(small_deserialized[0].parent_span_id, None)
  assert_eq(medium_deserialized[0].parent_span_id, None)
  assert_eq(large_deserialized[0].parent_span_id, None)
}

// 测试10: 上下文传播安全性测试
test "上下文传播安全性测试" {
  // 定义追踪上下文结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: String
  }
  
  // 创建包含敏感信息的上下文
  let sensitive_context = {
    trace_id: "trace-secure-12345",
    span_id: "span-secure-67890",
    parent_span_id: None,
    trace_flags: 1,
    trace_state: "user.id=12345;user.email=user@example.com;api.key=secret123"
  }
  
  // 上下文清理函数
  let sanitize_context = fn(context: TraceContext, sensitive_keys: Array[String>) {
    let sanitize_trace_state = fn(trace_state: String) {
      let attributes = trace_state.split(";")
      let sanitized = attributes.map(fn(attr) {
        let parts = attr.split("=")
        if parts.length() >= 2 {
          let key = parts[0]
          let value = parts[1]
          
          if sensitive_keys.any(fn(sensitive_key) { key == sensitive_key }) {
            key + "=***REDACTED***"
          } else {
            attr
          }
        } else {
          attr
        }
      })
      
      sanitized.join(";")
    }
    
    {
      trace_id: context.trace_id,
      span_id: context.span_id,
      parent_span_id: context.parent_span_id,
      trace_flags: context.trace_flags,
      trace_state: sanitize_trace_state(context.trace_state)
    }
  }
  
  // 测试敏感信息清理
  let sensitive_keys = ["user.email", "api.key"]
  let sanitized_context = sanitize_context(sensitive_context, sensitive_keys)
  
  assert_eq(sanitized_context.trace_id, sensitive_context.trace_id)
  assert_eq(sanitized_context.span_id, sensitive_context.span_id)
  assert_eq(sanitized_context.parent_span_id, sensitive_context.parent_span_id)
  assert_eq(sanitized_context.trace_flags, sensitive_context.trace_flags)
  
  // 验证敏感信息被清理
  assert_true(sanitized_context.trace_state.contains("user.id=12345"))
  assert_true(sanitized_context.trace_state.contains("user.email=***REDACTED***"))
  assert_true(sanitized_context.trace_state.contains("api.key=***REDACTED***"))
  
  // 测试上下文加密
  let encrypt_context = fn(context: TraceContext, encryption_key: String) {
    // 简化的加密实现（实际应用中应使用真正的加密算法）
    let simple_encrypt = fn(data: String, key: String) {
      let mut result = ""
      let key_bytes = key.to_bytes()
      let data_bytes = data.to_bytes()
      
      for i in 0..data_bytes.length() {
        let key_byte = key_bytes[i % key_bytes.length()]
        let encrypted_byte = data_bytes[i] ^ key_byte
        result = result + encrypted_byte.to_string(16)
      }
      
      result
    }
    
    let context_str = context.trace_id + "|" + context.span_id + "|" + 
                     match context.parent_span_id {
                       Some(id) => id
                       None => ""
                     } + "|" + context.trace_flags.to_string() + "|" + 
                     context.trace_state
    
    {
      encrypted_data: simple_encrypt(context_str, encryption_key),
      encryption_key: encryption_key
    }
  }
  
  // 测试上下文解密
  let decrypt_context = fn(encrypted: {encrypted_data: String, encryption_key: String}) {
    // 简化的解密实现
    let simple_decrypt = fn(data: String, key: String) {
      let mut result = ""
      let key_bytes = key.to_bytes()
      
      // 将十六进制字符串转换为字节数组
      let mut data_bytes = []
      for i in 0..data.length() / 2 {
        let hex_byte = data.substring(i * 2, 2)
        let byte = Int::from_str_radix(hex_byte, 16).get_or(0)
        data_bytes = data_bytes + [byte]
      }
      
      for i in 0..data_bytes.length() {
        let key_byte = key_bytes[i % key_bytes.length()]
        let decrypted_byte = data_bytes[i] ^ key_byte
        result = result + (decrypted_byte as Char).to_string()
      }
      
      result
    }
    
    let context_str = simple_decrypt(encrypted.encrypted_data, encrypted.encryption_key)
    let parts = context_str.split("|")
    
    if parts.length() >= 5 {
      let trace_id = parts[0]
      let span_id = parts[1]
      let parent_span_str = parts[2]
      let trace_flags = parts[3].to_int()
      let trace_state = parts[4]
      
      let parent_span_id = if parent_span_str.length() > 0 {
        Some(parent_span_str)
      } else {
        None
      }
      
      Some({
        trace_id: trace_id,
        span_id: span_id,
        parent_span_id: parent_span_id,
        trace_flags: trace_flags,
        trace_state: trace_state
      })
    } else {
      None
    }
  }
  
  // 测试加密和解密
  let encryption_key = "encryption-key-12345"
  let encrypted_context = encrypt_context(sensitive_context, encryption_key)
  
  assert_true(encrypted_context.encrypted_data.length() > 0)
  assert_eq(encrypted_context.encryption_key, encryption_key)
  
  // 验证加密后的数据不包含原始敏感信息
  assert_false(encrypted_context.encrypted_data.contains("user@example.com"))
  assert_false(encrypted_context.encrypted_data.contains("secret123"))
  
  // 测试解密
  let decrypted_context = decrypt_context(encrypted_context)
  assert_true(decrypted_context.is_some())
  
  match decrypted_context {
    Some(context) => {
      assert_eq(context.trace_id, sensitive_context.trace_id)
      assert_eq(context.span_id, sensitive_context.span_id)
      assert_eq(context.parent_span_id, sensitive_context.parent_span_id)
      assert_eq(context.trace_flags, sensitive_context.trace_flags)
      assert_eq(context.trace_state, sensitive_context.trace_state)
    }
    None => assert_true(false)
  }
  
  // 测试使用错误密钥解密
  let wrong_key_context = {
    encrypted_data: encrypted_context.encrypted_data,
    encryption_key: "wrong-key"
  }
  
  let wrong_decrypted = decrypt_context(wrong_key_context)
  assert_eq(wrong_decrypted, None)
  
  // 测试上下文签名
  let sign_context = fn(context: TraceContext, signing_key: String) {
    // 简化的签名实现（实际应用中应使用真正的签名算法）
    let simple_sign = fn(data: String, key: String) {
      let mut hash = 0
      let data_bytes = data.to_bytes()
      let key_bytes = key.to_bytes()
      
      for i in 0..data_bytes.length() {
        let data_byte = data_bytes[i]
        let key_byte = key_bytes[i % key_bytes.length()]
        hash = (hash + data_byte + key_byte) % 2147483647
      }
      
      hash.to_string(16)
    }
    
    let context_str = context.trace_id + "|" + context.span_id + "|" + 
                     match context.parent_span_id {
                       Some(id) => id
                       None => ""
                     } + "|" + context.trace_flags.to_string() + "|" + 
                     context.trace_state
    
    {
      context: context,
      signature: simple_sign(context_str, signing_key),
      signing_key: signing_key
    }
  }
  
  // 测试上下文验证
  let verify_context = fn(signed: {context: TraceContext, signature: String, signing_key: String>) {
    let recompute_signature = fn(context: TraceContext, key: String) {
      let simple_sign = fn(data: String, key: String) {
        let mut hash = 0
        let data_bytes = data.to_bytes()
        let key_bytes = key.to_bytes()
        
        for i in 0..data_bytes.length() {
          let data_byte = data_bytes[i]
          let key_byte = key_bytes[i % key_bytes.length()]
          hash = (hash + data_byte + key_byte) % 2147483647
        }
        
        hash.to_string(16)
      }
      
      let context_str = context.trace_id + "|" + context.span_id + "|" + 
                       match context.parent_span_id {
                         Some(id) => id
                         None => ""
                       } + "|" + context.trace_flags.to_string() + "|" + 
                       context.trace_state
      
      simple_sign(context_str, key)
    }
    
    let expected_signature = recompute_signature(signed.context, signed.signing_key)
    signed.signature == expected_signature
  }
  
  // 测试签名和验证
  let signing_key = "signing-key-12345"
  let signed_context = sign_context(sensitive_context, signing_key)
  
  assert_true(signed_context.signature.length() > 0)
  assert_eq(signed_context.signing_key, signing_key)
  
  // 测试验证
  assert_true(verify_context(signed_context))
  
  // 测试篡改检测
  let tampered_context = {
    context: {
      trace_id: "tampered-trace",
      span_id: signed_context.context.span_id,
      parent_span_id: signed_context.context.parent_span_id,
      trace_flags: signed_context.context.trace_flags,
      trace_state: signed_context.context.trace_state
    },
    signature: signed_context.signature,
    signing_key: signed_context.signing_key
  }
  
  assert_false(verify_context(tampered_context))
}