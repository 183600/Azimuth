// Azimuth Enhanced Telemetry Test Suite
// 高级遥测系统测试用例，专注于分布式追踪和度量的深度功能

// 测试1: 追踪上下文传播机制
test "分布式追踪上下文传播" {
  // 定义追踪上下文结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: Array[(String, String)]
  }
  
  // 创建根上下文
  let root_context = {
    trace_id: "trace-1234567890abcdef",
    span_id: "span-1234567890",
    parent_span_id: None,
    trace_flags: 1,
    trace_state: [("sampling.decision", "true"), ("service.name", "api-gateway")]
  }
  
  // 验证根上下文
  assert_eq(root_context.trace_id.length(), 16)
  assert_eq(root_context.span_id.length(), 13)
  assert_eq(root_context.parent_span_id, None)
  assert_eq(root_context.trace_flags, 1)
  assert_eq(root_context.trace_state.length(), 2)
  
  // 创建子上下文
  let child_context = {
    trace_id: root_context.trace_id,
    span_id: "span-0987654321",
    parent_span_id: Some(root_context.span_id),
    trace_flags: root_context.trace_flags,
    trace_state: root_context.trace_state.push(("service.name", "payment-service"))
  }
  
  // 验证上下文传播
  assert_eq(child_context.trace_id, root_context.trace_id)
  assert_eq(child_context.parent_span_id, Some(root_context.span_id))
  assert_eq(child_context.trace_flags, root_context.trace_flags)
  assert_eq(child_context.trace_state.length(), 3)
  
  // 测试上下文序列化
  let serialize_context = fn(ctx: TraceContext) {
    let base = "00-" + ctx.trace_id + "-" + ctx.span_id + "-" + ctx.trace_flags.to_string(16)
    if ctx.trace_state.length() > 0 {
      let state_str = ctx.trace_state.map(fn(pair) { pair.0 + "=" + pair.1 }).join(",")
      base + "-" + state_str
    } else {
      base
    }
  }
  
  let serialized = serialize_context(root_context)
  assert_true(serialized.starts_with("00-"))
  assert_true(serialized.contains(root_context.trace_id))
  assert_true(serialized.contains(root_context.span_id))
  
  // 测试上下文反序列化
  let deserialize_context = fn(traceparent: String) {
    let parts = traceparent.split("-")
    if parts.length() >= 4 {
      Some({
        trace_id: parts[1],
        span_id: parts[2],
        parent_span_id: None,
        trace_flags: parts[3].to_int(16),
        trace_state: []
      })
    } else {
      None
    }
  }
  
  let deserialized = deserialize_context(serialized)
  assert_true(deserialized.is_some())
  
  match deserialized {
    Some(ctx) => {
      assert_eq(ctx.trace_id, root_context.trace_id)
      assert_eq(ctx.span_id, root_context.span_id)
      assert_eq(ctx.trace_flags, root_context.trace_flags)
    }
    None => assert_true(false)
  }
}

// 测试2: 度量桶分布算法
test "度量直方图桶分布" {
  // 定义桶边界
  type Bucket = {
    upper_bound: Float,
    count: Int
  }
  
  type Histogram = {
    buckets: Array[Bucket],
    count: Int,
    sum: Float
  }
  
  // 创建指数桶边界
  let create_exponential_buckets = fn(start: Float, factor: Float, count: Int) {
    let mut buckets = []
    let mut current = start
    
    for i in 0..count {
      buckets = buckets.push({
        upper_bound: current,
        count: 0
      })
      current = current * factor
    }
    
    // 添加无穷大桶
    buckets = buckets.push({
      upper_bound: Float::infinity(),
      count: 0
    })
    
    buckets
  }
  
  // 创建直方图
  let buckets = create_exponential_buckets(1.0, 2.0, 5)
  let histogram = {
    buckets: buckets,
    count: 0,
    sum: 0.0
  }
  
  // 验证桶边界
  assert_eq(buckets.length(), 6)
  assert_eq(buckets[0].upper_bound, 1.0)
  assert_eq(buckets[1].upper_bound, 2.0)
  assert_eq(buckets[2].upper_bound, 4.0)
  assert_eq(buckets[3].upper_bound, 8.0)
  assert_eq(buckets[4].upper_bound, 16.0)
  assert_true(buckets[5].upper_bound.is_infinite())
  
  // 观测值并更新桶计数
  let observe = fn(hist: Histogram, value: Float) {
    let mut updated_buckets = hist.buckets
    
    for i in 0..updated_buckets.length() {
      if value <= updated_buckets[i].upper_bound {
        updated_buckets[i] = {
          upper_bound: updated_buckets[i].upper_bound,
          count: updated_buckets[i].count + 1
        }
        break
      }
    }
    
    {
      buckets: updated_buckets,
      count: hist.count + 1,
      sum: hist.sum + value
    }
  }
  
  // 添加观测值
  let observations = [0.5, 1.2, 3.8, 7.5, 15.0, 30.0]
  let mut updated_histogram = histogram
  
  for obs in observations {
    updated_histogram = observe(updated_histogram, obs)
  }
  
  // 验证桶计数
  assert_eq(updated_histogram.count, 6)
  assert_eq(updated_histogram.sum, 58.0)
  assert_eq(updated_histogram.buckets[0].count, 1)  // 0.5 <= 1.0
  assert_eq(updated_histogram.buckets[1].count, 2)  // 0.5, 1.2 <= 2.0
  assert_eq(updated_histogram.buckets[2].count, 3)  // 0.5, 1.2, 3.8 <= 4.0
  assert_eq(updated_histogram.buckets[3].count, 4)  // 0.5, 1.2, 3.8, 7.5 <= 8.0
  assert_eq(updated_histogram.buckets[4].count, 5)  // 0.5, 1.2, 3.8, 7.5, 15.0 <= 16.0
  assert_eq(updated_histogram.buckets[5].count, 6)  // 所有值 <= infinity
  
  // 计算百分位数
  let percentile = fn(hist: Histogram, p: Float) {
    if hist.count == 0 {
      0.0
    } else {
      let target_count = (hist.count as Float * p / 100.0) as Int
      let mut found_count = 0
      
      for bucket in hist.buckets {
        found_count = found_count + bucket.count
        if found_count >= target_count {
          return bucket.upper_bound
        }
      }
      
      Float::infinity()
    }
  }
  
  let p50 = percentile(updated_histogram, 50.0)
  let p95 = percentile(updated_histogram, 95.0)
  let p99 = percentile(updated_histogram, 99.0)
  
  assert_true(p50 >= 4.0)  // 50%的值应该小于等于4.0
  assert_true(p95 >= 15.0)  // 95%的值应该小于等于15.0
  assert_true(p99.is_infinite())  // 99%的值应该小于等于infinity
}

// 测试3: 采样策略决策逻辑
test "多策略采样决策算法" {
  // 定义采样决策
  type SamplingDecision = {
    sampled: Bool,
    attributes: Array[(String, String)]
  }
  
  // 定义采样策略
  enum SamplingStrategy {
    AlwaysOn
    AlwaysOff
    TraceIdRatio(Float)
    ParentBased(Bool)  // Bool表示是否遵循父级决策
    AttributeBased(Array<(String, String)>)  // 属性匹配列表
  }
  
  // 创建采样决策函数
  let make_sampling_decision = fn(trace_id: String, parent_sampled: Option[Bool], 
                                  attributes: Array[(String, String)>, strategy: SamplingStrategy) {
    match strategy {
      SamplingStrategy::AlwaysOn => {
        { sampled: true, attributes: [("sampler.type", "always_on")] }
      }
      SamplingStrategy::AlwaysOff => {
        { sampled: false, attributes: [("sampler.type", "always_off")] }
      }
      SamplingStrategy::TraceIdRatio(ratio) => {
        // 简化的基于trace_id的采样算法
        let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
        let normalized = (hash % 1000) as Float / 1000.0
        let sampled = normalized <= ratio
        
        {
          sampled,
          attributes: [
            ("sampler.type", "trace_id_ratio"),
            ("sampler.ratio", ratio.to_string()),
            ("sampler.decision", sampled.to_string())
          ]
        }
      }
      SamplingStrategy::ParentBased(follow_parent) => {
        match parent_sampled {
          Some(parent_decision) => {
            if follow_parent {
              {
                sampled: parent_decision,
                attributes: [
                  ("sampler.type", "parent_based"),
                  ("sampler.parent_sampled", parent_decision.to_string())
                ]
              }
            } else {
              { sampled: true, attributes: [("sampler.type", "parent_based_override")] }
            }
          }
          None => {
            { sampled: true, attributes: [("sampler.type", "parent_based_root")] }
          }
        }
      }
      SamplingStrategy::AttributeBased(required_attrs) => {
        let mut all_matched = true
        
        for required in required_attrs {
          let (key, expected_value) = required
          let match_found = attributes.any(fn(attr) {
            let (attr_key, attr_value) = attr
            attr_key == key && attr_value == expected_value
          })
          
          if not match_found {
            all_matched = false
            break
          }
        }
        
        {
          sampled: all_matched,
          attributes: [
            ("sampler.type", "attribute_based"),
            ("sampler.matched", all_matched.to_string())
          ]
        }
      }
    }
  }
  
  // 测试AlwaysOn策略
  let always_on_result = make_sampling_decision(
    "trace-12345", 
    None, 
    [], 
    SamplingStrategy::AlwaysOn
  )
  assert_true(always_on_result.sampled)
  assert_eq(always_on_result.attributes[0], ("sampler.type", "always_on"))
  
  // 测试AlwaysOff策略
  let always_off_result = make_sampling_decision(
    "trace-67890", 
    None, 
    [], 
    SamplingStrategy::AlwaysOff
  )
  assert_false(always_off_result.sampled)
  assert_eq(always_off_result.attributes[0], ("sampler.type", "always_off"))
  
  // 测试TraceIdRatio策略
  let ratio_result = make_sampling_decision(
    "trace-abcde", 
    None, 
    [], 
    SamplingStrategy::TraceIdRatio(0.5)
  )
  // 结果取决于trace_id的哈希值，我们只验证结构
  assert_true(ratio_result.attributes.contains(("sampler.type", "trace_id_ratio")))
  assert_true(ratio_result.attributes.contains(("sampler.ratio", "0.5")))
  
  // 测试ParentBased策略 - 有父级
  let parent_based_result = make_sampling_decision(
    "trace-parent", 
    Some(true), 
    [], 
    SamplingStrategy::ParentBased(true)
  )
  assert_true(parent_based_result.sampled)
  assert_eq(parent_based_result.attributes[0], ("sampler.type", "parent_based"))
  assert_eq(parent_based_result.attributes[1], ("sampler.parent_sampled", "true"))
  
  // 测试ParentBased策略 - 无父级
  let parent_based_root_result = make_sampling_decision(
    "trace-root", 
    None, 
    [], 
    SamplingStrategy::ParentBased(true)
  )
  assert_true(parent_based_root_result.sampled)
  assert_eq(parent_based_root_result.attributes[0], ("sampler.type", "parent_based_root"))
  
  // 测试AttributeBased策略 - 匹配
  let attrs = [
    ("service.name", "critical-service"),
    ("environment", "production"),
    ("user.type", "premium")
  ]
  
  let attr_based_match_result = make_sampling_decision(
    "trace-attrs", 
    None, 
    attrs, 
    SamplingStrategy::AttributeBased([
      ("service.name", "critical-service"),
      ("environment", "production")
    ])
  )
  assert_true(attr_based_match_result.sampled)
  assert_eq(attr_based_match_result.attributes[1], ("sampler.matched", "true"))
  
  // 测试AttributeBased策略 - 不匹配
  let attr_based_no_match_result = make_sampling_decision(
    "trace-no-match", 
    None, 
    attrs, 
    SamplingStrategy::AttributeBased([
      ("service.name", "non-existent-service"),
      ("environment", "staging")
    ])
  )
  assert_false(attr_based_no_match_result.sampled)
  assert_eq(attr_based_no_match_result.attributes[1], ("sampler.matched", "false"))
}

// 测试4: 遥测管道流处理
test "遥测数据管道流处理" {
  // 定义遥测数据点
  type TelemetryData = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    operation_name: String,
    duration_ms: Int,
    status: String,
    attributes: Array[(String, String)>
  }
  
  // 定义管道处理阶段
  enum ProcessingStage {
    Ingestion
    Validation
    Transformation
    Enrichment
    Filtering
    Aggregation
    Export
  }
  
  // 定义管道处理器
  type PipelineProcessor = (TelemetryData) -> Option[TelemetryData>
  
  // 创建验证处理器
  let validation_processor: PipelineProcessor = fn(data) {
    let valid_trace_id = data.trace_id.length() == 16
    let valid_span_id = data.span_id.length() == 13
    let valid_duration = data.duration_ms >= 0
    let valid_status = ["ok", "error", "timeout"].contains(data.status)
    
    if valid_trace_id && valid_span_id && valid_duration && valid_status {
      Some(data)
    } else {
      None
    }
  }
  
  // 创建转换处理器
  let transformation_processor: PipelineProcessor = fn(data) {
    let transformed_attrs = data.attributes.map(fn(attr) {
      let (key, value) = attr
      if key == "service.name" {
        (key, value.to_uppercase())
      } else {
        attr
      }
    })
    
    Some({
      timestamp: data.timestamp,
      trace_id: data.trace_id,
      span_id: data.span_id,
      operation_name: data.operation_name.to_lowercase(),
      duration_ms: data.duration_ms,
      status: data.status.to_uppercase(),
      attributes: transformed_attrs
    })
  }
  
  // 创建丰富化处理器
  let enrichment_processor: PipelineProcessor = fn(data) {
    let enriched_attrs = data.attributes.concat([
      ("processed.timestamp", Time::now().to_string()),
      ("pipeline.version", "1.0.0"),
      ("processing.node", "node-123")
    ])
    
    Some({
      timestamp: data.timestamp,
      trace_id: data.trace_id,
      span_id: data.span_id,
      operation_name: data.operation_name,
      duration_ms: data.duration_ms,
      status: data.status,
      attributes: enriched_attrs
    })
  }
  
  // 创建过滤处理器
  let filtering_processor: PipelineProcessor = fn(data) {
    let is_debug = data.attributes.any(fn(attr) {
      let (key, value) = attr
      key == "log.level" && value == "debug"
    })
    
    if is_debug {
      None  // 过滤掉debug级别的数据
    } else {
      Some(data)
    }
  }
  
  // 创建管道执行函数
  let execute_pipeline = fn(data: TelemetryData, processors: Array[PipelineProcessor>) {
    let mut current_data = Some(data)
    let mut stage_results = []
    
    for processor in processors {
      match current_data {
        Some(d) => {
          current_data = processor(d)
          stage_results = stage_results.push(current_data.is_some())
        }
        None => {
          stage_results = stage_results.push(false)
          break
        }
      }
    }
    
    {
      result: current_data,
      stages: stage_results
    }
  }
  
  // 创建测试数据
  let test_data = {
    timestamp: 1640995200,
    trace_id: "trace1234567890ab",
    span_id: "span1234567890",
    operation_name: "HTTP_POST:/api/payments",
    duration_ms: 250,
    status: "ok",
    attributes: [
      ("service.name", "payment-service"),
      ("http.method", "POST"),
      ("http.status_code", "200"),
      ("user.id", "user-12345")
    ]
  }
  
  // 设置管道处理器
  let processors = [
    validation_processor,
    transformation_processor,
    enrichment_processor,
    filtering_processor
  ]
  
  // 执行管道
  let pipeline_result = execute_pipeline(test_data, processors)
  
  // 验证管道执行结果
  assert_true(pipeline_result.result.is_some())
  assert_eq(pipeline_result.stages.length(), 4)
  assert_true(pipeline_result.stages.all(fn(stage) { stage }))
  
  // 验证最终数据
  match pipeline_result.result {
    Some(final_data) => {
      // 验证转换效果
      assert_eq(final_data.operation_name, "http_post:/api/payments")
      assert_eq(final_data.status, "OK")
      
      // 验证服务名称转换
      let service_attr = final_data.attributes.find(fn(attr) {
        attr.0 == "service.name"
      })
      assert_eq(service_attr, Some(("service.name", "PAYMENT-SERVICE")))
      
      // 验证丰富化属性
      assert_true(final_data.attributes.contains(("processed.timestamp", Time::now().to_string())))
      assert_true(final_data.attributes.contains(("pipeline.version", "1.0.0")))
      assert_true(final_data.attributes.contains(("processing.node", "node-123")))
    }
    None => assert_true(false)
  }
  
  // 测试无效数据
  let invalid_data = {
    timestamp: 1640995200,
    trace_id: "short",  // 无效：太短
    span_id: "span1234567890",
    operation_name: "TestOperation",
    duration_ms: -100,  // 无效：负数
    status: "invalid",  // 无效：不在允许的状态中
    attributes: []
  }
  
  let invalid_result = execute_pipeline(invalid_data, processors)
  assert_true(invalid_result.result.is_none())
  assert_false(invalid_result.stages[0])  // 验证阶段失败
}

// 测试5: 行李物品管理
test "分布式追踪行李物品管理" {
  // 定义行李物品
  type BaggageItem = {
    key: String,
    value: String,
    metadata: Option[(String, String)>  // (property, value)
  }
  
  // 定义行李容器
  type Baggage = {
    items: Array[BaggageItem]
  }
  
  // 创建空行李容器
  let empty_baggage = { items: [] }
  assert_eq(empty_baggage.items.length(), 0)
  
  // 添加行李物品
  let add_baggage_item = fn(baggage: Baggage, key: String, value: String, metadata: Option<(String, String)>) {
    let new_item = { key, value, metadata }
    let updated_items = baggage.items.push(new_item)
    { items: updated_items }
  }
  
  // 测试添加物品
  let baggage1 = add_baggage_item(empty_baggage, "user.id", "12345", None)
  assert_eq(baggage1.items.length(), 1)
  assert_eq(baggage1.items[0].key, "user.id")
  assert_eq(baggage1.items[0].value, "12345")
  assert_eq(baggage1.items[0].metadata, None)
  
  // 添加带元数据的物品
  let baggage2 = add_baggage_item(baggage1, "transaction.id", "txn-67890", Some(("propagation", "cross-service")))
  assert_eq(baggage2.items.length(), 2)
  assert_eq(baggage2.items[1].key, "transaction.id")
  assert_eq(baggage2.items[1].value, "txn-67890")
  assert_eq(baggage2.items[1].metadata, Some(("propagation", "cross-service")))
  
  // 获取行李物品
  let get_baggage_value = fn(baggage: Baggage, key: String) {
    baggage.items.find(fn(item) { item.key == key }).map(fn(item) { item.value })
  }
  
  let user_id = get_baggage_value(baggage2, "user.id")
  assert_eq(user_id, Some("12345"))
  
  let transaction_id = get_baggage_value(baggage2, "transaction.id")
  assert_eq(transaction_id, Some("txn-67890"))
  
  let missing_key = get_baggage_value(baggage2, "non.existent")
  assert_eq(missing_key, None)
  
  // 更新行李物品
  let update_baggage_item = fn(baggage: Baggage, key: String, new_value: String) {
    let updated_items = baggage.items.map(fn(item) {
      if item.key == key {
        { key: item.key, value: new_value, metadata: item.metadata }
      } else {
        item
      }
    })
    { items: updated_items }
  }
  
  let baggage3 = update_baggage_item(baggage2, "user.id", "54321")
  let updated_user_id = get_baggage_value(baggage3, "user.id")
  assert_eq(updated_user_id, Some("54321"))
  
  // 验证其他物品未受影响
  let unchanged_transaction_id = get_baggage_value(baggage3, "transaction.id")
  assert_eq(unchanged_transaction_id, Some("txn-67890"))
  
  // 删除行李物品
  let remove_baggage_item = fn(baggage: Baggage, key: String) {
    let filtered_items = baggage.items.filter(fn(item) { item.key != key })
    { items: filtered_items }
  }
  
  let baggage4 = remove_baggage_item(baggage3, "user.id")
  assert_eq(baggage4.items.length(), 1)
  assert_eq(baggage4.items[0].key, "transaction.id")
  
  let removed_user_id = get_baggage_value(baggage4, "user.id")
  assert_eq(removed_user_id, None)
  
  // 行李序列化
  let serialize_baggage = fn(baggage: Baggage) {
    baggage.items.map(fn(item) {
      let base = item.key + "=" + item.value
      match item.metadata {
        Some((prop, val)) => base + ";" + prop + "=" + val
        None => base
      }
    }).join(",")
  }
  
  let serialized = serialize_baggage(baggage2)
  assert_true(serialized.contains("user.id=12345"))
  assert_true(serialized.contains("transaction.id=txn-67890"))
  assert_true(serialized.contains("propagation=cross-service"))
  
  // 行李反序列化
  let deserialize_baggage = fn(serialized: String) {
    let item_strs = serialized.split(",")
    let items = item_strs.map(fn(item_str) {
      let parts = item_str.split(";")
      let key_value = parts[0].split("=")
      let key = key_value[0]
      let value = if key_value.length() > 1 { key_value[1] } else { "" }
      
      let metadata = if parts.length() > 1 {
        let prop_value = parts[1].split("=")
        if prop_value.length() > 1 {
          Some((prop_value[0], prop_value[1]))
        } else {
          None
        }
      } else {
        None
      }
      
      { key, value, metadata }
    })
    
    { items }
  }
  
  let deserialized = deserialize_baggage(serialized)
  assert_eq(deserialized.items.length(), 2)
  
  let deserialized_user_id = get_baggage_value(deserialized, "user.id")
  assert_eq(deserialized_user_id, Some("12345"))
  
  let deserialized_transaction_id = get_baggage_value(deserialized, "transaction.id")
  assert_eq(deserialized_transaction_id, Some("txn-67890"))
}

// 测试6: 资源属性合并
test "多源资源属性合并策略" {
  // 定义资源属性
  type ResourceAttributes = Array<(String, String)>
  
  // 定义属性合并策略
  enum MergeStrategy {
    KeepExisting  // 保留现有值
    Overwrite     // 覆盖现有值
    MergeArray    // 合并数组值
    Concatenate   // 连接字符串值
  }
  
  // 属性合并器
  let merge_attributes = fn(base: ResourceAttributes, incoming: ResourceAttributes, 
                            strategies: Array<(String, MergeStrategy)>) {
    let mut result = base.copy()
    
    for (key, value) in incoming {
      let strategy = strategies.find(fn(s) { s.0 == key })
        .map(fn(s) { s.1 })
        .unwrap_or(MergeStrategy::Overwrite)
      
      // 查找现有键
      let existing_index = result.index_of(fn(attr) { attr.0 == key })
      
      match existing_index {
        Some(idx) => {
          // 键已存在，根据策略合并
          match strategy {
            MergeStrategy::KeepExisting => {
              // 保留现有值，不做任何操作
            }
            MergeStrategy::Overwrite => {
              result[idx] = (key, value)
            }
            MergeStrategy::MergeArray => {
              let existing_value = result[idx].1
              let combined = existing_value + "," + value
              result[idx] = (key, combined)
            }
            MergeStrategy::Concatenate => {
              let existing_value = result[idx].1
              let combined = existing_value + ";" + value
              result[idx] = (key, combined)
            }
          }
        }
        None => {
          // 键不存在，直接添加
          result = result.push((key, value))
        }
      }
    }
    
    result
  }
  
  // 创建基础资源属性
  let base_attributes = [
    ("service.name", "payment-service"),
    ("service.version", "1.2.3"),
    ("deployment.environment", "production"),
    ("host.name", "payment-01"),
    ("tags", "critical,financial")
  ]
  
  // 创建传入资源属性
  let incoming_attributes = [
    ("service.version", "1.2.4"),  // 更新版本
    ("host.name", "payment-02"),    // 更新主机名
    ("process.pid", "12345"),       // 新属性
    ("tags", "pci-compliant"),      // 合并标签
    ("description", "Payment processing service")  // 新属性
  ]
  
  // 定义合并策略
  let merge_strategies = [
    ("service.name", MergeStrategy::KeepExisting),
    ("service.version", MergeStrategy::Overwrite),
    ("deployment.environment", MergeStrategy::KeepExisting),
    ("host.name", MergeStrategy::Overwrite),
    ("tags", MergeStrategy::MergeArray),
    ("description", MergeStrategy::Overwrite)
  ]
  
  // 执行合并
  let merged_attributes = merge_attributes(base_attributes, incoming_attributes, merge_strategies)
  
  // 验证合并结果
  assert_eq(merged_attributes.length(), 6)
  
  // 验证保留现有值的属性
  let service_name = merged_attributes.find(fn(attr) { attr.0 == "service.name" })
  assert_eq(service_name, Some(("service.name", "payment-service")))
  
  let deployment_env = merged_attributes.find(fn(attr) { attr.0 == "deployment.environment" })
  assert_eq(deployment_env, Some(("deployment.environment", "production")))
  
  // 验证覆盖的属性
  let service_version = merged_attributes.find(fn(attr) { attr.0 == "service.version" })
  assert_eq(service_version, Some(("service.version", "1.2.4")))
  
  let host_name = merged_attributes.find(fn(attr) { attr.0 == "host.name" })
  assert_eq(host_name, Some(("host.name", "payment-02")))
  
  // 验证合并的属性
  let tags = merged_attributes.find(fn(attr) { attr.0 == "tags" })
  assert_eq(tags, Some(("tags", "critical,financial,pci-compliant")))
  
  // 验证新增的属性
  let process_pid = merged_attributes.find(fn(attr) { attr.0 == "process.pid" })
  assert_eq(process_pid, Some(("process.pid", "12345")))
  
  let description = merged_attributes.find(fn(attr) { attr.0 == "description" })
  assert_eq(description, Some(("description", "Payment processing service")))
  
  // 测试多级合并
  let level2_attributes = [
    ("service.version", "1.3.0"),    // 再次更新版本
    ("region", "us-west-2"),         // 新属性
    ("availability.zone", "us-west-2a"),  // 新属性
    ("tags", "high-availability")    // 再次合并标签
  ]
  
  let level2_merged = merge_attributes(merged_attributes, level2_attributes, merge_strategies)
  
  // 验证二级合并结果
  assert_eq(level2_merged.length(), 8)
  
  let final_service_version = level2_merged.find(fn(attr) { attr.0 == "service.version" })
  assert_eq(final_service_version, Some(("service.version", "1.3.0")))
  
  let final_tags = level2_merged.find(fn(attr) { attr.0 == "tags" })
  assert_eq(final_tags, Some(("tags", "critical,financial,pci-compliant,high-availability")))
  
  let region = level2_merged.find(fn(attr) { attr.0 == "region" })
  assert_eq(region, Some(("region", "us-west-2")))
  
  let availability_zone = level2_merged.find(fn(attr) { attr.0 == "availability.zone" })
  assert_eq(availability_zone, Some(("availability.zone", "us-west-2a")))
}

// 测试7: Span事件处理
test "Span事件处理和时间线分析" {
  // 定义Span事件
  type SpanEvent = {
    timestamp: Int,
    name: String,
    attributes: Array[(String, String)>
  }
  
  // 定义Span
  type Span = {
    name: String,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    start_time: Int,
    end_time: Option[Int>,
    status: String,
    events: Array[SpanEvent]
  }
  
  // 创建Span
  let create_span = fn(name: String, trace_id: String, span_id: String, 
                      parent_span_id: Option[String>, start_time: Int) {
    {
      name,
      trace_id,
      span_id,
      parent_span_id,
      start_time,
      end_time: None,
      status: "running",
      events: []
    }
  }
  
  // 添加事件
  let add_event = fn(span: Span, timestamp: Int, name: String, 
                    attributes: Array<(String, String)>) {
    let event = { timestamp, name, attributes }
    let updated_events = span.events.push(event)
    { span | events: updated_events }
  }
  
  // 结束Span
  let end_span = fn(span: Span, end_time: Int, status: String) {
    { span | end_time: Some(end_time), status: status }
  }
  
  // 创建测试Span
  let span = create_span(
    "database.query",
    "trace-1234567890abcdef",
    "span-1234567890",
    Some("span-0987654321"),
    1640995200000  // 毫秒时间戳
  )
  
  // 验证初始状态
  assert_eq(span.name, "database.query")
  assert_eq(span.trace_id, "trace-1234567890abcdef")
  assert_eq(span.span_id, "span-1234567890")
  assert_eq(span.parent_span_id, Some("span-0987654321"))
  assert_eq(span.start_time, 1640995200000)
  assert_eq(span.end_time, None)
  assert_eq(span.status, "running")
  assert_eq(span.events.length(), 0)
  
  // 添加事件
  let span_with_events = span
    |> add_event(1640995200100, "query.start", [
      ("db.statement", "SELECT * FROM users WHERE id = ?"),
      ("db.type", "postgresql")
    ])
    |> add_event(1640995200200, "cache.miss", [
      ("cache.key", "user:12345"),
      ("cache.region", "us-west-2")
    ])
    |> add_event(1640995200300, "db.connection.acquired", [
      ("db.pool.name", "primary"),
      ("db.connection.id", "conn-789")
    ])
    |> add_event(1640995200400, "query.executed", [
      ("db.rows_affected", "1"),
      ("db.execution_time", "50ms")
    ])
    |> add_event(1640995200500, "cache.write", [
      ("cache.key", "user:12345"),
      ("cache.ttl", "300s")
    ])
  
  // 验证事件
  assert_eq(span_with_events.events.length(), 5)
  
  // 验证第一个事件
  let first_event = span_with_events.events[0]
  assert_eq(first_event.timestamp, 1640995200100)
  assert_eq(first_event.name, "query.start")
  assert_true(first_event.attributes.contains(("db.statement", "SELECT * FROM users WHERE id = ?")))
  assert_true(first_event.attributes.contains(("db.type", "postgresql")))
  
  // 验证最后一个事件
  let last_event = span_with_events.events[4]
  assert_eq(last_event.timestamp, 1640995200500)
  assert_eq(last_event.name, "cache.write")
  assert_true(last_event.attributes.contains(("cache.key", "user:12345")))
  assert_true(last_event.attributes.contains(("cache.ttl", "300s")))
  
  // 结束Span
  let completed_span = end_span(span_with_events, 1640995200600, "ok")
  
  // 验证完成状态
  assert_eq(completed_span.end_time, Some(1640995200600))
  assert_eq(completed_span.status, "ok")
  
  // 计算Span持续时间
  let span_duration = match completed_span.end_time {
    Some(end_time) => end_time - completed_span.start_time
    None => 0
  }
  
  assert_eq(span_duration, 600)  // 600ms
  
  // 分析事件时间线
  let analyze_event_timeline = fn(span: Span) {
    if span.events.length() == 0 {
      return {
        total_events: 0,
        event_names: [],
        time_between_events: [],
        critical_path: []
      }
    }
    
    let event_names = span.events.map(fn(e) { e.name })
    let mut time_between_events = []
    
    for i in 1..span.events.length() {
      let prev_time = span.events[i - 1].timestamp
      let curr_time = span.events[i].timestamp
      time_between_events = time_between_events.push(curr_time - prev_time)
    }
    
    // 识别关键路径（耗时最长的事件序列）
    let mut critical_path = []
    let mut max_duration = 0
    
    for i in 0..time_between_events.length() {
      if time_between_events[i] > max_duration {
        max_duration = time_between_events[i]
        critical_path = [
          span.events[i].name,
          span.events[i + 1].name
        ]
      }
    }
    
    {
      total_events: span.events.length(),
      event_names,
      time_between_events,
      critical_path
    }
  }
  
  // 分析事件时间线
  let timeline_analysis = analyze_event_timeline(completed_span)
  
  // 验证分析结果
  assert_eq(timeline_analysis.total_events, 5)
  assert_eq(timeline_analysis.event_names.length(), 5)
  assert_true(timeline_analysis.event_names.contains("query.start"))
  assert_true(timeline_analysis.event_names.contains("cache.write"))
  
  assert_eq(timeline_analysis.time_between_events.length(), 4)
  assert_eq(timeline_analysis.time_between_events[0], 100)  // query.start到cache.miss
  assert_eq(timeline_analysis.time_between_events[1], 100)  // cache.miss到db.connection.acquired
  assert_eq(timeline_analysis.time_between_events[2], 100)  // db.connection.acquired到query.executed
  assert_eq(timeline_analysis.time_between_events[3], 100)  // query.executed到cache.write
  
  // 在这个测试中，所有间隔相等，所以关键路径应该是第一对
  assert_eq(timeline_analysis.critical_path.length(), 2)
  assert_eq(timeline_analysis.critical_path[0], "query.start")
  assert_eq(timeline_analysis.critical_path[1], "cache.miss")
  
  // 按名称搜索事件
  let find_events_by_name = fn(span: Span, name: String) {
    span.events.filter(fn(e) { e.name == name })
  }
  
  let cache_events = find_events_by_name(completed_span, "cache")
  assert_eq(cache_events.length(), 2)  // cache.miss和cache.write
  
  // 按属性搜索事件
  let find_events_by_attribute = fn(span: Span, key: String, value: String) {
    span.events.filter(fn(e) { 
      e.attributes.any(fn(attr) { attr.0 == key && attr.1 == value })
    })
  }
  
  let db_events = find_events_by_attribute(completed_span, "db.type", "postgresql")
  assert_eq(db_events.length(), 2)  // query.start和query.executed
  
  // 计算事件属性统计
  let attribute_stats = fn(span: Span) {
    let mut stats = []
    
    for event in span.events {
      for attr in event.attributes {
        let (key, _) = attr
        let existing = stats.find(fn(stat) { stat.0 == key })
        
        match existing {
          Some((_, count)) => {
            // 更新计数
            let index = stats.index_of(fn(stat) { stat.0 == key }).unwrap()
            stats[index] = (key, count + 1)
          }
          None => {
            stats = stats.push((key, 1))
          }
        }
      }
    }
    
    stats.sort(fn(a, b) { b.1 - a.1 })  // 按计数降序排序
  }
  
  let stats = attribute_stats(completed_span)
  assert_true(stats.length() > 0)
  
  // 验证最常见的属性
  let most_common = stats[0]
  assert_eq(most_common.0, "cache.key")  // cache.key出现了两次
  assert_eq(most_common.1, 2)
}

// 测试8: Span链接关系
test "Span间链接关系和因果追踪" {
  // 定义Span链接
  type SpanLink = {
    trace_id: String,
    span_id: String,
    trace_state: Array[(String, String)],
    attributes: Array[(String, String)>
  }
  
  // 定义Span（简化版，专注于链接）
  type LinkedSpan = {
    span_id: String,
    trace_id: String,
    links: Array[SpanLink>
  }
  
  // 创建Span
  let create_linked_span = fn(span_id: String, trace_id: String) {
    {
      span_id,
      trace_id,
      links: []
    }
  }
  
  // 添加链接
  let add_link = fn(span: LinkedSpan, linked_trace_id: String, linked_span_id: String,
                    trace_state: Array[(String, String)>, attributes: Array<(String, String)>) {
    let link = {
      trace_id: linked_trace_id,
      span_id: linked_span_id,
      trace_state,
      attributes
    }
    let updated_links = span.links.push(link)
    { span | links: updated_links }
  }
  
  // 创建主Span
  let main_span = create_linked_span("span-main-123", "trace-main-456")
  
  // 验证初始状态
  assert_eq(main_span.span_id, "span-main-123")
  assert_eq(main_span.trace_id, "trace-main-456")
  assert_eq(main_span.links.length(), 0)
  
  // 添加来自其他追踪的链接
  let linked_span = main_span
    |> add_link(
      "trace-parent-789",
      "span-parent-012",
      [("sampling.decision", "true"), ("service.name", "api-gateway")],
      [("link.type", "parent"), ("causality", "direct")]
    )
    |> add_link(
      "trace-async-345",
      "span-async-678",
      [("sampling.decision", "false"), ("service.name", "worker-service")],
      [("link.type", "async"), ("causality", "indirect")]
    )
    |> add_link(
      "trace-batch-901",
      "span-batch-234",
      [("sampling.decision", "true"), ("service.name", "batch-processor")],
      [("link.type", "batch"), ("causality", "correlated")]
    )
  
  // 验证链接
  assert_eq(linked_span.links.length(), 3)
  
  // 验证第一个链接（父链接）
  let parent_link = linked_span.links[0]
  assert_eq(parent_link.trace_id, "trace-parent-789")
  assert_eq(parent_link.span_id, "span-parent-012")
  assert_eq(parent_link.trace_state.length(), 2)
  assert_eq(parent_link.attributes.length(), 2)
  
  // 验证链接属性
  assert_true(parent_link.attributes.contains(("link.type", "parent")))
  assert_true(parent_link.attributes.contains(("causality", "direct")))
  
  // 验证第二个链接（异步链接）
  let async_link = linked_span.links[1]
  assert_eq(async_link.trace_id, "trace-async-345")
  assert_eq(async_link.span_id, "span-async-678")
  assert_true(async_link.attributes.contains(("link.type", "async")))
  assert_true(async_link.attributes.contains(("causality", "indirect")))
  
  // 验证第三个链接（批处理链接）
  let batch_link = linked_span.links[2]
  assert_eq(batch_link.trace_id, "trace-batch-901")
  assert_eq(batch_link.span_id, "span-batch-234")
  assert_true(batch_link.attributes.contains(("link.type", "batch")))
  assert_true(batch_link.attributes.contains(("causality", "correlated")))
  
  // 按链接类型过滤
  let filter_links_by_type = fn(span: LinkedSpan, link_type: String) {
    span.links.filter(fn(link) {
      link.attributes.any(fn(attr) {
        attr.0 == "link.type" && attr.1 == link_type
      })
    })
  }
  
  let parent_links = filter_links_by_type(linked_span, "parent")
  assert_eq(parent_links.length(), 1)
  assert_eq(parent_links[0].span_id, "span-parent-012")
  
  let async_links = filter_links_by_type(linked_span, "async")
  assert_eq(async_links.length(), 1)
  assert_eq(async_links[0].span_id, "span-async-678")
  
  let batch_links = filter_links_by_type(linked_span, "batch")
  assert_eq(batch_links.length(), 1)
  assert_eq(batch_links[0].span_id, "span-batch-234")
  
  // 按因果性过滤
  let filter_links_by_causality = fn(span: LinkedSpan, causality: String) {
    span.links.filter(fn(link) {
      link.attributes.any(fn(attr) {
        attr.0 == "causality" && attr.1 == causality
      })
    })
  }
  
  let direct_links = filter_links_by_causality(linked_span, "direct")
  assert_eq(direct_links.length(), 1)
  
  let indirect_links = filter_links_by_causality(linked_span, "indirect")
  assert_eq(indirect_links.length(), 1)
  
  let correlated_links = filter_links_by_causality(linked_span, "correlated")
  assert_eq(correlated_links.length(), 1)
  
  // 分析链接关系
  let analyze_link_relationships = fn(span: LinkedSpan) {
    let mut link_types = []
    let mut causalities = []
    let mut trace_ids = []
    
    for link in span.links {
      // 收集链接类型
      for attr in link.attributes {
        if attr.0 == "link.type" && not(link_types.contains(attr.1)) {
          link_types = link_types.push(attr.1)
        }
        if attr.0 == "causality" && not(causalities.contains(attr.1)) {
          causalities = causalities.push(attr.1)
        }
      }
      
      // 收集追踪ID
      if not(trace_ids.contains(link.trace_id)) {
        trace_ids = trace_ids.push(link.trace_id)
      }
    }
    
    {
      total_links: span.links.length(),
      link_types,
      causalities,
      trace_ids,
      has_parent: link_types.contains("parent"),
      has_async: link_types.contains("async"),
      has_batch: link_types.contains("batch")
    }
  }
  
  // 分析链接关系
  let analysis = analyze_link_relationships(linked_span)
  
  // 验证分析结果
  assert_eq(analysis.total_links, 3)
  assert_eq(analysis.link_types.length(), 3)
  assert_eq(analysis.causalities.length(), 3)
  assert_eq(analysis.trace_ids.length(), 3)
  
  assert_true(analysis.link_types.contains("parent"))
  assert_true(analysis.link_types.contains("async"))
  assert_true(analysis.link_types.contains("batch"))
  
  assert_true(analysis.causalities.contains("direct"))
  assert_true(analysis.causalities.contains("indirect"))
  assert_true(analysis.causalities.contains("correlated"))
  
  assert_true(analysis.has_parent)
  assert_true(analysis.has_async)
  assert_true(analysis.has_batch)
  
  // 检查循环依赖
  let check_circular_dependencies = fn(span: LinkedSpan, all_spans: Array<LinkedSpan>) {
    let mut visited = []
    let mut stack = [(span.span_id, [])]
    let mut circular_paths = []
    
    while stack.length() > 0 {
      let (current_id, path) = stack.pop()
      
      if visited.contains(current_id) {
        continue
      }
      
      visited = visited.push(current_id)
      let new_path = path.push(current_id)
      
      // 查找当前Span
      match all_spans.find(fn(s) { s.span_id == current_id }) {
        Some(current_span) => {
          // 检查每个链接
          for link in current_span.links {
            let linked_span_id = link.span_id
            
            // 检查是否形成循环
            if new_path.contains(linked_span_id) {
              let cycle_start = new_path.index_of(fn(id) { id == linked_span_id }).unwrap()
              let circular_path = new_path.slice(cycle_start).push(linked_span_id)
              circular_paths = circular_paths.push(circular_path)
            } else {
              stack = stack.push((linked_span_id, new_path))
            }
          }
        }
        None => {}  // Span不存在
      }
    }
    
    circular_paths
  }
  
  // 创建Span集合用于循环检测
  let span_collection = [
    linked_span,
    create_linked_span("span-parent-012", "trace-parent-789"),
    create_linked_span("span-async-678", "trace-async-345"),
    create_linked_span("span-batch-234", "trace-batch-901")
  ]
  
  // 检查循环依赖
  let circular_deps = check_circular_dependencies(linked_span, span_collection)
  assert_eq(circular_deps.length(), 0)  // 没有循环依赖
  
  // 创建一个有循环依赖的场景
  let circular_span1 = create_linked_span("span-circular-1", "trace-circular")
    |> add_link("trace-circular", "span-circular-2", [], [("link.type", "parent")])
  
  let circular_span2 = create_linked_span("span-circular-2", "trace-circular")
    |> add_link("trace-circular", "span-circular-3", [], [("link.type", "parent")])
  
  let circular_span3 = create_linked_span("span-circular-3", "trace-circular")
    |> add_link("trace-circular", "span-circular-1", [], [("link.type", "parent")])
  
  let circular_collection = [circular_span1, circular_span2, circular_span3]
  let circular_paths = check_circular_dependencies(circular_span1, circular_collection)
  
  // 应该检测到循环
  assert_true(circular_paths.length() > 0)
  assert_true(circular_paths[0].length() >= 3)  // 至少包含3个Span形成循环
}

// 测试9: 配置热重载
test "遥测系统配置热重载机制" {
  // 定义配置项
  type ConfigItem = {
    key: String,
    value: String,
    type: String,  // "string", "int", "float", "bool"
    description: String,
    required: Bool,
    reloadable: Bool
  }
  
  // 定义配置版本
  type ConfigVersion = {
    version: String,
    timestamp: Int,
    items: Array[ConfigItem>
  }
  
  // 定义配置变更
  enum ConfigChange {
    Added(ConfigItem)
    Updated(String, String)  // key, new_value
    Removed(String)          // key
    TypeChanged(String, String)  // key, new_type
  }
  
  // 创建初始配置
  let initial_config = {
    version: "1.0.0",
    timestamp: 1640995200,
    items: [
      {
        key: "sampling.probability",
        value: "0.1",
        type: "float",
        description: "Probability of sampling a trace",
        required: false,
        reloadable: true
      },
      {
        key: "batch.size",
        value: "100",
        type: "int",
        description: "Number of items in a batch",
        required: true,
        reloadable: true
      },
      {
        key: "export.timeout",
        value: "5000",
        type: "int",
        description: "Export timeout in milliseconds",
        required: true,
        reloadable: false
      },
      {
        key: "debug.enabled",
        value: "false",
        type: "bool",
        description: "Enable debug mode",
        required: false,
        reloadable: true
      }
    ]
  }
  
  // 验证初始配置
  assert_eq(initial_config.version, "1.0.0")
  assert_eq(initial_config.items.length(), 4)
  
  // 查找配置项
  let find_config_item = fn(config: ConfigVersion, key: String) {
    config.items.find(fn(item) { item.key == key })
  }
  
  let sampling_config = find_config_item(initial_config, "sampling.probability")
  assert_eq(sampling_config, Some({
    key: "sampling.probability",
    value: "0.1",
    type: "float",
    description: "Probability of sampling a trace",
    required: false,
    reloadable: true
  }))
  
  // 应用配置变更
  let apply_config_changes = fn(config: ConfigVersion, changes: Array[ConfigChange>, new_version: String) {
    let mut updated_items = config.items.copy()
    
    for change in changes {
      match change {
        ConfigChange::Added(item) => {
          // 检查是否已存在
          if not(updated_items.any(fn(i) { i.key == item.key })) {
            updated_items = updated_items.push(item)
          }
        }
        ConfigChange::Updated(key, new_value) => {
          let index = updated_items.index_of(fn(item) { item.key == key })
          match index {
            Some(idx) => {
              let existing_item = updated_items[idx]
              updated_items[idx] = {
                key: existing_item.key,
                value: new_value,
                type: existing_item.type,
                description: existing_item.description,
                required: existing_item.required,
                reloadable: existing_item.reloadable
              }
            }
            None => {}  // 项目不存在，忽略
          }
        }
        ConfigChange::Removed(key) => {
          updated_items = updated_items.filter(fn(item) { item.key != key })
        }
        ConfigChange::TypeChanged(key, new_type) => {
          let index = updated_items.index_of(fn(item) { item.key == key })
          match index {
            Some(idx) => {
              let existing_item = updated_items[idx]
              updated_items[idx] = {
                key: existing_item.key,
                value: existing_item.value,
                type: new_type,
                description: existing_item.description,
                required: existing_item.required,
                reloadable: existing_item.reloadable
              }
            }
            None => {}  // 项目不存在，忽略
          }
        }
      }
    }
    
    {
      version: new_version,
      timestamp: Time::now(),
      items: updated_items
    }
  }
  
  // 创建配置变更
  let config_changes = [
    ConfigChange::Updated("sampling.probability", "0.2"),  // 更新采样概率
    ConfigChange::Updated("batch.size", "200"),            // 更新批处理大小
    ConfigChange::Added({
      key: "max.retries",
      value: "3",
      type: "int",
      description: "Maximum number of retries",
      required: false,
      reloadable: true
    }),                                                  // 添加新配置
    ConfigChange::TypeChanged("debug.enabled", "string")  // 更改类型
  ]
  
  // 应用变更
  let updated_config = apply_config_changes(initial_config, config_changes, "1.1.0")
  
  // 验证变更结果
  assert_eq(updated_config.version, "1.1.0")
  assert_eq(updated_config.items.length(), 5)  // 添加了一个新项
  
  // 验证更新的值
  let updated_sampling = find_config_item(updated_config, "sampling.probability")
  assert_eq(updated_sampling.unwrap().value, "0.2")
  
  let updated_batch_size = find_config_item(updated_config, "batch.size")
  assert_eq(updated_batch_size.unwrap().value, "200")
  
  // 验证新添加的项
  let new_retries = find_config_item(updated_config, "max.retries")
  assert_eq(new_retries.unwrap().value, "3")
  
  // 验证类型更改
  let updated_debug = find_config_item(updated_config, "debug.enabled")
  assert_eq(updated_debug.unwrap().type, "string")
  
  // 验证未更改的项
  let unchanged_timeout = find_config_item(updated_config, "export.timeout")
  assert_eq(unchanged_timeout.unwrap().value, "5000")
  
  // 检查可重载配置
  let get_reloadable_items = fn(config: ConfigVersion) {
    config.items.filter(fn(item) { item.reloadable })
  }
  
  let reloadable_items = get_reloadable_items(updated_config)
  assert_eq(reloadable_items.length(), 4)  // 除export.timeout外的所有项
  
  // 检查必需配置
  let get_required_items = fn(config: ConfigVersion) {
    config.items.filter(fn(item) { item.required })
  }
  
  let required_items = get_required_items(updated_config)
  assert_eq(required_items.length(), 2)  // batch.size和export.timeout
  
  // 验证必需配置都有值
  for item in required_items {
    assert_true(item.value.length() > 0)
  }
  
  // 配置验证
  let validate_config = fn(config: ConfigVersion) {
    let mut errors = []
    
    for item in config.items {
      // 验证必需项
      if item.required && item.value.length() == 0 {
        errors = errors.push("Required item '" + item.key + "' has no value")
      }
      
      // 验证类型和值
      match item.type {
        "int" => {
          match item.value.to_int() {
            Some(_) => {}  // 有效整数
            None => errors = errors.push("Item '" + item.key + "' value is not a valid integer")
          }
        }
        "float" => {
          match item.value.to_float() {
            Some(_) => {}  // 有效浮点数
            None => errors = errors.push("Item '" + item.key + "' value is not a valid float")
          }
        }
        "bool" => {
          if item.value != "true" && item.value != "false" {
            errors = errors.push("Item '" + item.key + "' value must be 'true' or 'false'")
          }
        }
        _ => {}  // 字符串类型或其他类型
      }
      
      // 验证范围
      match item.key {
        "sampling.probability" => {
          match item.value.to_float() {
            Some(prob) => {
              if prob < 0.0 || prob > 1.0 {
                errors = errors.push("Sampling probability must be between 0.0 and 1.0")
              }
            }
            None => {}  // 已经在上面验证过类型
          }
        }
        "batch.size" => {
          match item.value.to_int() {
            Some(size) => {
              if size <= 0 {
                errors = errors.push("Batch size must be positive")
              }
            }
            None => {}  // 已经在上面验证过类型
          }
        }
        _ => {}  // 其他项不需要特殊验证
      }
    }
    
    errors
  }
  
  // 验证更新后的配置
  let validation_errors = validate_config(updated_config)
  assert_eq(validation_errors.length(), 0)  // 配置应该有效
  
  // 创建无效配置进行测试
  let invalid_changes = [
    ConfigChange::Updated("sampling.probability", "1.5"),  // 超出范围
    ConfigChange::Updated("batch.size", "0"),              // 无效值
    ConfigChange::Updated("debug.enabled", "maybe")        // 无效布尔值
  ]
  
  let invalid_config = apply_config_changes(updated_config, invalid_changes, "1.2.0")
  let invalid_errors = validate_config(invalid_config)
  
  // 应该有验证错误
  assert_true(invalid_errors.length() > 0)
  assert_true(invalid_errors.any(fn(err) { err.contains("Sampling probability") }))
  assert_true(invalid_errors.any(fn(err) { err.contains("Batch size") }))
  assert_true(invalid_errors.any(fn(err) { err.contains("true or false") }))
}

// 测试10: 遥测数据保留策略
test "遥测数据保留策略和清理机制" {
  // 定义数据保留策略
  type RetentionPolicy = {
    max_age_days: Int,
    max_count: Int,
    max_size_mb: Int,
    priority_tags: Array[String>  // 高优先级标签，这些数据保留更久
  }
  
  // 定义遥测数据记录
  type TelemetryRecord = {
    id: String,
    timestamp: Int,
    size_bytes: Int,
    tags: Array[String>,
    data: String  // 简化的数据内容
  }
  
  // 定义保留决策
  enum RetentionDecision {
    Keep(String)      // 保留，附带原因
    Delete(String)    // 删除，附带原因
    Archive(String)   // 归档，附带原因
  }
  
  // 创建保留策略
  let policy = {
    max_age_days: 30,
    max_count: 10000,
    max_size_mb: 500,
    priority_tags: ["critical", "security", "compliance"]
  }
  
  // 验证策略
  assert_eq(policy.max_age_days, 30)
  assert_eq(policy.max_count, 10000)
  assert_eq(policy.max_size_mb, 500)
  assert_eq(policy.priority_tags.length(), 3)
  
  // 创建测试数据
  let current_time = 1640995200  // 2022-01-01的时间戳
  let day_in_seconds = 86400
  
  let create_record = fn(id: String, days_ago: Int, size_kb: Int, tags: Array[String>) {
    {
      id,
      timestamp: current_time - (days_ago * day_in_seconds),
      size_bytes: size_kb * 1024,
      tags,
      data: "Sample telemetry data for " + id
    }
  }
  
  // 创建不同年龄和大小的记录
  let records = [
    create_record("record-1", 5, 100, ["critical", "performance"]),     // 5天前，100KB，高优先级
    create_record("record-2", 15, 200, ["debug", "verbose"]),           // 15天前，200KB，低优先级
    create_record("record-3", 35, 150, ["security", "audit"]),          // 35天前，150KB，高优先级
    create_record("record-4", 45, 50, ["performance", "metrics"]),      // 45天前，50KB，低优先级
    create_record("record-5", 60, 300, ["compliance", "regulatory"]),   // 60天前，300KB，高优先级
    create_record("record-6", 10, 75, ["user", "activity"]),            // 10天前，75KB，低优先级
    create_record("record-7", 25, 120, ["error", "exception"]),         // 25天前，120KB，中优先级
    create_record("record-8", 40, 80, ["info", "general"]),             // 40天前，80KB，低优先级
    create_record("record-9", 20, 250, ["critical", "security"]),       // 20天前，250KB，高优先级
    create_record("record-10", 50, 180, ["debug", "trace"])             // 50天前，180KB，低优先级
  ]
  
  // 验证记录
  assert_eq(records.length(), 10)
  
  // 计算记录年龄
  let calculate_age_days = fn(record: TelemetryRecord) {
    (current_time - record.timestamp) / day_in_seconds
  }
  
  // 检查记录年龄
  assert_eq(calculate_age_days(records[0]), 5)   // 5天前
  assert_eq(calculate_age_days(records[2]), 35)  // 35天前
  assert_eq(calculate_age_days(records[4]), 60)  // 60天前
  
  // 检查记录大小
  assert_eq(records[0].size_bytes, 100 * 1024)   // 100KB
  assert_eq(records[4].size_bytes, 300 * 1024)   // 300KB
  
  // 检查记录标签
  assert_true(records[0].tags.contains("critical"))
  assert_true(records[2].tags.contains("security"))
  assert_true(records[4].tags.contains("compliance"))
  
  // 保留决策函数
  let make_retention_decision = fn(record: TelemetryRecord, policy: RetentionPolicy, 
                                  current_count: Int, current_size_mb: Int) {
    let age_days = calculate_age_days(record)
    let has_priority_tag = record.tags.any(fn(tag) { 
      policy.priority_tags.contains(tag) 
    })
    
    // 检查年龄
    if age_days > policy.max_age_days {
      if has_priority_tag {
        // 高优先级标签，延长保留期
        if age_days > policy.max_age_days * 2 {
          RetentionDecision::Archive("Old but high priority")
        } else {
          RetentionDecision::Keep("High priority tag")
        }
      } else {
        RetentionDecision::Delete("Exceeded maximum age")
      }
    } else if current_count > policy.max_count {
      if has_priority_tag {
        RetentionDecision::Keep("High priority, despite count limit")
      } else {
        RetentionDecision::Delete("Exceeded maximum count")
      }
    } else if current_size_mb > policy.max_size_mb {
      if has_priority_tag {
        RetentionDecision::Keep("High priority, despite size limit")
      } else {
        RetentionDecision::Archive("Size limit exceeded")
      }
    } else {
      RetentionDecision::Keep("Within all limits")
    }
  }
  
  // 计算当前总大小
  let total_size_mb = records.reduce(fn(acc, record) { 
    acc + (record.size_bytes / (1024 * 1024)) 
  }, 0)
  
  assert_eq(total_size_mb, 1)  // 所有记录总大小约1MB
  
  // 测试保留决策
  let decisions = records.map(fn(record) {
    make_retention_decision(record, policy, records.length(), total_size_mb)
  })
  
  // 验证决策
  assert_eq(decisions.length(), 10)
  
  // 5天前的记录应该保留
  match decisions[0] {
    RetentionDecision::Keep(reason) => assert_eq(reason, "Within all limits")
    _ => assert_true(false)
  }
  
  // 35天前的记录，但有security标签，应该保留
  match decisions[2] {
    RetentionDecision::Keep(reason) => assert_eq(reason, "High priority tag")
    _ => assert_true(false)
  }
  
  // 60天前的记录，但有compliance标签，应该归档
  match decisions[4] {
    RetentionDecision::Archive(reason) => assert_eq(reason, "Old but high priority")
    _ => assert_true(false)
  }
  
  // 45天前的记录，没有高优先级标签，应该删除
  match decisions[3] {
    RetentionDecision::Delete(reason) => assert_eq(reason, "Exceeded maximum age")
    _ => assert_true(false)
  }
  
  // 50天前的记录，没有高优先级标签，应该删除
  match decisions[9] {
    RetentionDecision::Delete(reason) => assert_eq(reason, "Exceeded maximum age")
    _ => assert_true(false)
  }
  
  // 应用保留策略
  let apply_retention_policy = fn(records: Array[TelemetryRecord], policy: RetentionPolicy) {
    let total_size_mb = records.reduce(fn(acc, record) { 
      acc + (record.size_bytes / (1024 * 1024)) 
    }, 0)
    
    let mut kept_records = []
    let mut archived_records = []
    let mut deleted_records = []
    
    // 第一轮：应用基本保留规则
    for record in records {
      match make_retention_decision(record, policy, records.length(), total_size_mb) {
        RetentionDecision::Keep(_) => {
          kept_records = kept_records.push(record)
        }
        RetentionDecision::Archive(_) => {
          archived_records = archived_records.push(record)
        }
        RetentionDecision::Delete(_) => {
          deleted_records = deleted_records.push(record)
        }
      }
    }
    
    // 第二轮：如果仍然超过限制，进一步处理
    if kept_records.length() > policy.max_count {
      // 按年龄排序，删除最旧的低优先级记录
      let sorted_by_age = kept_records.sort(fn(a, b) {
        calculate_age_days(b) - calculate_age_days(a)
      })
      
      let mut final_kept = []
      let mut to_delete = []
      let mut count = 0
      
      for record in sorted_by_age {
        let has_priority = record.tags.any(fn(tag) { 
          policy.priority_tags.contains(tag) 
        })
        
        if count < policy.max_count || has_priority {
          final_kept = final_kept.push(record)
          count = count + 1
        } else {
          to_delete = to_delete.push(record)
        }
      }
      
      kept_records = final_kept
      deleted_records = deleted_records.concat(to_delete)
    }
    
    {
      kept: kept_records,
      archived: archived_records,
      deleted: deleted_records
    }
  }
  
  // 应用保留策略
  let retention_result = apply_retention_policy(records, policy)
  
  // 验证结果
  assert_eq(retention_result.kept.length(), 6)   // 保留6条记录
  assert_eq(retention_result.archived.length(), 2)  // 归档2条记录
  assert_eq(retention_result.deleted.length(), 2)   // 删除2条记录
  
  // 验证保留的记录ID
  let kept_ids = retention_result.kept.map(fn(r) { r.id })
  assert_true(kept_ids.contains("record-1"))   // 5天前，高优先级
  assert_true(kept_ids.contains("record-2"))   // 15天前，低优先级但在限制内
  assert_true(kept_ids.contains("record-3"))   // 35天前，高优先级
  assert_true(kept_ids.contains("record-6"))   // 10天前，低优先级但在限制内
  assert_true(kept_ids.contains("record-7"))   // 25天前，中优先级
  assert_true(kept_ids.contains("record-9"))   // 20天前，高优先级
  
  // 验证归档的记录ID
  let archived_ids = retention_result.archived.map(fn(r) { r.id })
  assert_true(archived_ids.contains("record-5"))   // 60天前，高优先级但太旧
  
  // 验证删除的记录ID
  let deleted_ids = retention_result.deleted.map(fn(r) { r.id })
  assert_true(deleted_ids.contains("record-4"))   // 45天前，低优先级
  assert_true(deleted_ids.contains("record-8"))   // 40天前，低优先级
  
  // 计算保留统计
  let calculate_retention_stats = fn(result: {kept: Array[TelemetryRecord], 
                                            archived: Array[TelemetryRecord>, 
                                            deleted: Array[TelemetryRecord>}) {
    let total_kept_size = result.kept.reduce(fn(acc, r) { acc + r.size_bytes }, 0)
    let total_archived_size = result.archived.reduce(fn(acc, r) { acc + r.size_bytes }, 0)
    let total_deleted_size = result.deleted.reduce(fn(acc, r) { acc + r.size_bytes }, 0)
    
    {
      kept_count: result.kept.length(),
      archived_count: result.archived.length(),
      deleted_count: result.deleted.length(),
      kept_size_mb: total_kept_size / (1024 * 1024),
      archived_size_mb: total_archived_size / (1024 * 1024),
      deleted_size_mb: total_deleted_size / (1024 * 1024),
      space_reclaimed_mb: total_deleted_size / (1024 * 1024)
    }
  }
  
  // 计算统计
  let stats = calculate_retention_stats(retention_result)
  
  // 验证统计
  assert_eq(stats.kept_count, 6)
  assert_eq(stats.archived_count, 2)
  assert_eq(stats.deleted_count, 2)
  
  // 验证空间回收
  assert_true(stats.space_reclaimed_mb > 0)
  assert_eq(stats.space_reclaimed_mb, stats.deleted_size_mb)
  
  // 测试极端情况：空记录集
  let empty_result = apply_retention_policy([], policy)
  assert_eq(empty_result.kept.length(), 0)
  assert_eq(empty_result.archived.length(), 0)
  assert_eq(empty_result.deleted.length(), 0)
  
  // 测试极端情况：所有记录都很旧且低优先级
  let old_records = [
    create_record("old-1", 60, 100, ["debug"]),
    create_record("old-2", 90, 150, ["trace"]),
    create_record("old-3", 120, 200, ["verbose"])
  ]
  
  let old_result = apply_retention_policy(old_records, policy)
  assert_eq(old_result.kept.length(), 0)      // 没有记录保留
  assert_eq(old_result.archived.length(), 0)  // 没有记录归档
  assert_eq(old_result.deleted.length(), 3)   // 所有记录删除
}