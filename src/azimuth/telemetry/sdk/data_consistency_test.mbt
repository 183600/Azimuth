// 数据一致性测试 - 测试并发场景下的数据完整性

test "concurrent_span_creation_consistency" {
  // 测试并发span创建的数据一致性
  
  struct SpanData {
    span_id : String
    trace_id : String
    parent_span_id : Option[String]
    operation_name : String
    start_time_ns : Int
    end_time_ns : Option[Int]
    status : String
    attributes : Array[(String, String)]
  }
  
  struct ConsistencyCheck {
    total_spans_created : Int
    unique_span_ids : Int
    duplicate_span_ids : Int
    orphaned_spans : Int  // 没有有效父span的span
    malformed_spans : Int
  }
  
  func generate_span_id() -> String {
    // 模拟生成span ID（8字节十六进制）
    let mut id = ""
    for i = 0; i < 8; i = i + 1 {
      let digit = (i * 3) % 16
      id = id + match digit {
        0 => "0", 1 => "1", 2 => "2", 3 => "3",
        4 => "4", 5 => "5", 6 => "6", 7 => "7",
        8 => "8", 9 => "9", 10 => "a", 11 => "b",
        12 => "c", 13 => "d", 14 => "e", 15 => "f",
        _ => "0"
      }
    }
    id
  }
  
  func simulate_concurrent_span_creation(thread_count : Int, spans_per_thread : Int) -> (Array[SpanData], ConsistencyCheck) {
    let mut all_spans = []
    let base_trace_id = "0123456789abcdef0123456789abcdef"
    
    // 模拟并发创建span
    for thread_id = 0; thread_id < thread_count; thread_id = thread_id + 1 {
      for span_index = 0; span_index < spans_per_thread; span_index = span_index + 1 {
        let span_id = generate_span_id() + thread_id.to_string() + span_index.to_string()
        let parent_span_id = if span_index > 0 {
          Some(generate_span_id() + thread_id.to_string() + (span_index - 1).to_string())
        } else {
          None
        }
        
        let span = SpanData{
          span_id: span_id,
          trace_id: base_trace_id,
          parent_span_id: parent_span_id,
          operation_name: "operation.thread_" + thread_id.to_string() + "." + span_index.to_string(),
          start_time_ns: 1640995200000000000 + thread_id * 1000000 + span_index * 1000,
          end_time_ns: Some(1640995200000000000 + thread_id * 1000000 + span_index * 1000 + 500),
          status: "ok",
          attributes: [
            ("thread_id", thread_id.to_string()),
            ("span_index", span_index.to_string())
          ]
        }
        
        all_spans.push(span)
      }
    }
    
    // 数据一致性检查
    let check = perform_consistency_check(all_spans)
    (all_spans, check)
  }
  
  func perform_consistency_check(spans : Array[SpanData]) -> ConsistencyCheck {
    let total_spans = spans.length()
    
    // 检查span ID唯一性
    let mut span_id_counts = @{}  // 简化：使用字符串映射
    let mut duplicate_count = 0
    
    for span in spans {
      match span_id_counts.get(span.span_id) {
        Some(count) => {
          span_id_counts.set(span.span_id, count + 1)
          duplicate_count = duplicate_count + 1
        }
        None => span_id_counts.set(span.span_id, 1)
      }
    }
    
    let unique_span_ids = span_id_counts.length()
    
    // 检查孤立span
    let mut orphaned_count = 0
    let mut valid_parent_ids = @{}
    
    // 首先收集所有有效的span ID
    for span in spans {
      valid_parent_ids.set(span.span_id, true)
    }
    
    // 然后检查每个span的父span是否有效
    for span in spans {
      match span.parent_span_id {
        Some(parent_id) => {
          match valid_parent_ids.get(parent_id) {
            None => orphaned_count = orphaned_count + 1
            Some(_) => {}  // 父span存在
          }
        }
        None => {}  // 根span，没有父span
      }
    }
    
    // 检查格式错误的span
    let mut malformed_count = 0
    for span in spans {
      if span.span_id.length() == 0 || span.trace_id.length() == 0 {
        malformed_count = malformed_count + 1
      } else if span.start_time_ns <= 0 {
        malformed_count = malformed_count + 1
      } else if span.end_time_ns.map_or(false, fn(end_time) { end_time < span.start_time_ns }) {
        malformed_count = malformed_count + 1
      }
    }
    
    ConsistencyCheck{
      total_spans_created: total_spans,
      unique_span_ids: unique_span_ids,
      duplicate_span_ids: duplicate_count,
      orphaned_spans: orphaned_count,
      malformed_spans: malformed_count
    }
  }
  
  // 测试不同并发级别的span创建
  let test_scenarios = [
    (2, 10),   // 2个线程，每个10个span
    (4, 25),   // 4个线程，每个25个span
    (8, 50),   // 8个线程，每个50个span
    (16, 100)  // 16个线程，每个100个span
  ]
  
  for (thread_count, spans_per_thread) in test_scenarios {
    let (spans, consistency_check) = simulate_concurrent_span_creation(thread_count, spans_per_thread)
    let expected_total = thread_count * spans_per_thread
    
    // 验证数据一致性
    assert_eq(consistency_check.total_spans_created, expected_total,
      "Should create expected number of spans for {thread_count} threads")
    assert_eq(consistency_check.unique_span_ids, expected_total,
      "All span IDs should be unique for {thread_count} threads")
    assert_eq(consistency_check.duplicate_span_ids, 0,
      "Should have no duplicate span IDs for {thread_count} threads")
    assert_eq(consistency_check.malformed_spans, 0,
      "Should have no malformed spans for {thread_count} threads")
    
    // 验证父子关系（只有根span没有父span）
    let expected_orphaned = thread_count  // 每个线程的第一个span是根span
    assert_eq(consistency_check.orphaned_spans, expected_orphaned,
      "Should have expected number of root spans for {thread_count} threads")
  }
}

test "concurrent_attribute_modification" {
  // 测试并发属性修改的数据一致性
  
  struct AttributeOperation {
    operation_type : String  // "add", "update", "delete"
    key : String
    value : String
    timestamp : Int
    thread_id : Int
  }
  
  struct AttributeState {
    attributes : Array[(String, String)]
    operation_history : Array[AttributeOperation]
    conflict_count : Int
  }
  
  func simulate_concurrent_attribute_operations(initial_attrs : Array[(String, String)], 
                                               thread_count : Int, 
                                               operations_per_thread : Int) -> AttributeState {
    let mut all_operations = []
    let mut current_attributes = initial_attrs.to_array()
    let mut conflict_count = 0
    
    // 模拟并发属性操作
    for thread_id = 0; thread_id < thread_count; thread_id = thread_id + 1 {
      for op_index = 0; op_index < operations_per_thread; op_index = op_index + 1 {
        let timestamp = thread_id * 1000000 + op_index * 1000
        let base_key = "attr_" + (op_index % 5).to_string()  // 5个不同的属性键
        
        let operation = match op_index % 3 {
          0 => AttributeOperation{
            operation_type: "add",
            key: base_key + "_" + thread_id.to_string(),
            value: "value_" + thread_id.to_string() + "_" + op_index.to_string(),
            timestamp: timestamp,
            thread_id: thread_id
          }
          1 => AttributeOperation{
            operation_type: "update",
            key: base_key,
            value: "updated_" + thread_id.to_string() + "_" + op_index.to_string(),
            timestamp: timestamp,
            thread_id: thread_id
          }
          2 => AttributeOperation{
            operation_type: "delete",
            key: base_key,
            value: "",
            timestamp: timestamp,
            thread_id: thread_id
          }
          _ => AttributeOperation{
            operation_type: "unknown",
            key: "",
            value: "",
            timestamp: timestamp,
            thread_id: thread_id
          }
        }
        
        all_operations.push(operation)
      }
    }
    
    // 按时间戳排序操作（模拟串行化）
    let sorted_operations = all_operations.sort_by(fn(op1, op2) { op1.timestamp - op2.timestamp })
    
    // 应用操作并检查冲突
    for operation in sorted_operations {
      match operation.operation_type {
        "add" => {
          // 检查属性是否已存在
          let mut exists = false
          for attr in current_attributes {
            if attr.0 == operation.key {
              exists = true
              conflict_count = conflict_count + 1  // 冲突：尝试添加已存在的属性
              break
            }
          }
          if !exists {
            current_attributes.push((operation.key, operation.value))
          }
        }
        
        "update" => {
          let mut found = false
          let mut updated_attrs = []
          for attr in current_attributes {
            if attr.0 == operation.key {
              updated_attrs.push((operation.key, operation.value))
              found = true
            } else {
              updated_attrs.push(attr)
            }
          }
          if found {
            current_attributes = updated_attrs
          } else {
            conflict_count = conflict_count + 1  // 冲突：尝试更新不存在的属性
          }
        }
        
        "delete" => {
          let mut found = false
          let mut remaining_attrs = []
          for attr in current_attributes {
            if attr.0 == operation.key {
              found = true
            } else {
              remaining_attrs.push(attr)
            }
          }
          if found {
            current_attributes = remaining_attrs
          } else {
            conflict_count = conflict_count + 1  // 冲突：尝试删除不存在的属性
          }
        }
        
        _ => {}  // 忽略未知操作
      }
    }
    
    AttributeState{
      attributes: current_attributes,
      operation_history: sorted_operations,
      conflict_count: conflict_count
    }
  }
  
  // 测试并发属性操作
  let initial_attributes = [
    ("base_attr_1", "initial_value_1"),
    ("base_attr_2", "initial_value_2"),
    ("base_attr_3", "initial_value_3")
  ]
  
  let test_scenarios = [
    (2, 10),   // 2个线程，每个10个操作
    (4, 15),   // 4个线程，每个15个操作
    (8, 20)    // 8个线程，每个20个操作
  ]
  
  for (thread_count, operations_per_thread) in test_scenarios {
    let final_state = simulate_concurrent_attribute_operations(initial_attributes, thread_count, operations_per_thread)
    let total_operations = thread_count * operations_per_thread
    
    // 验证最终状态
    assert_eq(final_state.operation_history.length(), total_operations,
      "Should record all operations for {thread_count} threads")
    
    // 验证属性的一致性
    let mut attribute_keys = @{}
    for attr in final_state.attributes {
      match attribute_keys.get(attr.0) {
        Some(_) => @test.fail("Duplicate attribute key found: " + attr.0)
        None => attribute_keys.set(attr.0, true)
      }
    }
    
    // 验证冲突在合理范围内
    let conflict_ratio = final_state.conflict_count.to_double() / total_operations.to_double()
    assert_eq(conflict_ratio <= 0.3, true,  // 允许最多30%的冲突率
      "Conflict ratio should be reasonable for {thread_count} threads")
    
    // 验证最终属性数量在合理范围内
    let min_expected_attrs = initial_attributes.length() / 2  // 至少保留一半初始属性
    let max_expected_attrs = initial_attributes.length() + total_operations / 3  // 最多增加1/3的操作数
    assert_eq(final_state.attributes.length() >= min_expected_attrs, true,
      "Should retain minimum expected attributes")
    assert_eq(final_state.attributes.length() <= max_expected_attrs, true,
      "Should not exceed maximum expected attributes")
  }
}

test "concurrent_batch_processing_integrity" {
  // 测试并发批处理的数据完整性
  
  struct BatchItem {
    item_id : String
    data : String
    batch_id : String
    processing_order : Int
    thread_id : Int
  }
  
  struct BatchIntegrity {
    total_items : Int
    complete_batches : Int
    incomplete_batches : Int
    missing_items : Int
    duplicate_items : Int
    order_violations : Int
  }
  
  func simulate_concurrent_batch_processing(thread_count : Int, items_per_thread : Int, batch_size : Int) -> (Array[BatchItem], BatchIntegrity) {
    let mut all_items = []
    let mut batch_counter = 0
    
    // 模拟并发批处理
    for thread_id = 0; thread_id < thread_count; thread_id = thread_id + 1 {
      for item_index = 0; item_index < items_per_thread; item_index = item_index + 1 {
        let batch_id = "batch_" + (batch_counter / batch_size).to_string()
        batch_counter = batch_counter + 1
        
        let item = BatchItem{
          item_id: "item_" + thread_id.to_string() + "_" + item_index.to_string(),
          data: "data_" + thread_id.to_string() + "_" + item_index.to_string(),
          batch_id: batch_id,
          processing_order: item_index,
          thread_id: thread_id
        }
        
        all_items.push(item)
      }
    }
    
    // 数据完整性检查
    let integrity = check_batch_integrity(all_items, batch_size)
    (all_items, integrity)
  }
  
  func check_batch_integrity(items : Array[BatchItem], batch_size : Int) -> BatchIntegrity {
    let total_items = items.length()
    
    // 按批次分组
    let mut batch_groups = @{}
    for item in items {
      match batch_groups.get(item.batch_id) {
        Some(batch_items) => {
          let mut new_batch_items = batch_items.to_array()
          new_batch_items.push(item)
          batch_groups.set(item.batch_id, new_batch_items)
        }
        None => batch_groups.set(item.batch_id, [item])
      }
    }
    
    // 检查每个批次的完整性
    let mut complete_batches = 0
    let mut incomplete_batches = 0
    let mut missing_items = 0
    let mut duplicate_items = 0
    let mut order_violations = 0
    
    for (batch_id, batch_items) in batch_groups.to_array() {
      let expected_size = batch_size
      let actual_size = batch_items.length()
      
      if actual_size == expected_size {
        complete_batches = complete_batches + 1
      } else {
        incomplete_batches = incomplete_batches + 1
        missing_items = missing_items + (expected_size - actual_size)
      }
      
      // 检查重复项
      let mut item_ids = @{}
      for item in batch_items {
        match item_ids.get(item.item_id) {
          Some(_) => duplicate_items = duplicate_items + 1
          None => item_ids.set(item.item_id, true)
        }
      }
      
      // 检查处理顺序
      let sorted_items = batch_items.sort_by(fn(item1, item2) { item1.processing_order - item2.processing_order })
      for i = 1; i < sorted_items.length(); i = i + 1 {
        if sorted_items[i].processing_order < sorted_items[i-1].processing_order {
          order_violations = order_violations + 1
        }
      }
    }
    
    BatchIntegrity{
      total_items: total_items,
      complete_batches: complete_batches,
      incomplete_batches: incomplete_batches,
      missing_items: missing_items,
      duplicate_items: duplicate_items,
      order_violations: order_violations
    }
  }
  
  // 测试不同批处理配置
  let test_scenarios = [
    (2, 50, 10),   // 2个线程，每个50项，批大小10
    (4, 100, 20),  // 4个线程，每个100项，批大小20
    (8, 200, 25),  // 8个线程，每个200项，批大小25
    (16, 400, 50)  // 16个线程，每个400项，批大小50
  ]
  
  for (thread_count, items_per_thread, batch_size) in test_scenarios {
    let (items, integrity) = simulate_concurrent_batch_processing(thread_count, items_per_thread, batch_size)
    let expected_total = thread_count * items_per_thread
    
    // 验证数据完整性
    assert_eq(integrity.total_items, expected_total,
      "Should process expected total items for {thread_count} threads")
    assert_eq(integrity.duplicate_items, 0,
      "Should have no duplicate items for {thread_count} threads")
    assert_eq(integrity.order_violations, 0,
      "Should maintain processing order for {thread_count} threads")
    
    // 验证批次完整性（允许最后一批可能不完整）
    let expected_batches = expected_total / batch_size
    let expected_complete = expected_batches
    let expected_incomplete = if expected_total % batch_size > 0 { 1 } else { 0 }
    
    assert_eq(integrity.complete_batches >= expected_complete - 1, true,  // 允许一个批次的误差
      "Should have expected complete batches for {thread_count} threads")
    assert_eq(integrity.incomplete_batches <= expected_incomplete + 1, true,
      "Should have expected incomplete batches for {thread_count} threads")
    
    // 验证缺失项在合理范围内
    let max_acceptable_missing = batch_size  // 最多允许一个批次的大小
    assert_eq(integrity.missing_items <= max_acceptable_missing, true,
      "Missing items should be within acceptable range for {thread_count} threads")
  }
}

test "concurrent_context_propagation_consistency" {
  // 测试并发生成上下文传播的一致性
  
  struct ContextData {
    context_id : String
    trace_id : String
    span_id : String
    baggage_items : Array[(String, String)]
    parent_context : Option[String]
    propagation_path : Array[String]
  }
  
  struct ContextConsistency {
    total_contexts : Int
    valid_trace_chains : Int
    broken_trace_chains : Int
    orphaned_contexts : Int
    baggage_inconsistencies : Int
  }
  
  func simulate_concurrent_context_propagation(thread_count : Int, depth_per_thread : Int) -> (Array[ContextData], ContextConsistency) {
    let mut all_contexts = []
    let mut context_counter = 0
    
    // 模拟并发生成上下文传播链
    for thread_id = 0; thread_id < thread_count; thread_id = thread_id + 1 {
      let trace_id = "trace_" + thread_id.to_string() + "_" + "0123456789abcdef"
      let mut parent_context_id = None
      let mut propagation_path = []
      
      for depth = 0; depth < depth_per_thread; depth = depth + 1 {
        let context_id = "ctx_" + thread_id.to_string() + "_" + depth.to_string()
        let span_id = "span_" + thread_id.to_string() + "_" + depth.to_string()
        
        // 创建baggage项
        let baggage_items = [
          ("thread_id", thread_id.to_string()),
          ("depth", depth.to_string()),
          ("operation", "op_" + depth.to_string())
        ]
        
        propagation_path.push(context_id)
        
        let context = ContextData{
          context_id: context_id,
          trace_id: trace_id,
          span_id: span_id,
          baggage_items: baggage_items,
          parent_context: parent_context_id,
          propagation_path: propagation_path.to_array()
        }
        
        all_contexts.push(context)
        parent_context_id = Some(context_id)
        context_counter = context_counter + 1
      }
    }
    
    // 上下文一致性检查
    let consistency = check_context_consistency(all_contexts)
    (all_contexts, consistency)
  }
  
  func check_context_consistency(contexts : Array[ContextData]) -> ContextConsistency {
    let total_contexts = contexts.length()
    
    // 按trace_id分组
    let mut trace_groups = @{}
    for context in contexts {
      match trace_groups.get(context.trace_id) {
        Some(group_contexts) => {
          let mut new_contexts = group_contexts.to_array()
          new_contexts.push(context)
          trace_groups.set(context.trace_id, new_contexts)
        }
        None => trace_groups.set(context.trace_id, [context])
      }
    }
    
    // 检查每个trace链的一致性
    let mut valid_trace_chains = 0
    let mut broken_trace_chains = 0
    let mut orphaned_contexts = 0
    let mut baggage_inconsistencies = 0
    
    for (trace_id, trace_contexts) in trace_groups.to_array() {
      let mut is_valid_chain = true
      let mut context_ids = @{}
      
      for context in trace_contexts {
        // 检查上下文ID唯一性
        match context_ids.get(context.context_id) {
          Some(_) => {
            is_valid_chain = false
            broken_trace_chains = broken_trace_chains + 1
          }
          None => context_ids.set(context.context_id, true)
        }
        
        // 检查父子关系
        match context.parent_context {
          Some(parent_id) => {
            let mut parent_found = false
            for other_context in trace_contexts {
              if other_context.context_id == parent_id {
                parent_found = true
                break
              }
            }
            if !parent_found {
              orphaned_contexts = orphaned_contexts + 1
              is_valid_chain = false
            }
          }
          None => {}  // 根上下文
        }
        
        // 检查baggage一致性
        if context.baggage_items.length() < 3 {
          baggage_inconsistencies = baggage_inconsistencies + 1
        }
        
        // 检查传播路径
        if context.propagation_path.length() == 0 {
          baggage_inconsistencies = baggage_inconsistencies + 1
        }
      }
      
      if is_valid_chain {
        valid_trace_chains = valid_trace_chains + 1
      }
    }
    
    ContextConsistency{
      total_contexts: total_contexts,
      valid_trace_chains: valid_trace_chains,
      broken_trace_chains: broken_trace_chains,
      orphaned_contexts: orphaned_contexts,
      baggage_inconsistencies: baggage_inconsistencies
    }
  }
  
  // 测试不同并发级别的上下文传播
  let test_scenarios = [
    (2, 5),    // 2个线程，每个深度5
    (4, 8),    // 4个线程，每个深度8
    (8, 10),   // 8个线程，每个深度10
    (16, 15)   // 16个线程，每个深度15
  ]
  
  for (thread_count, depth_per_thread) in test_scenarios {
    let (contexts, consistency) = simulate_concurrent_context_propagation(thread_count, depth_per_thread)
    let expected_total = thread_count * depth_per_thread
    
    // 验证上下文一致性
    assert_eq(consistency.total_contexts, expected_total,
      "Should create expected total contexts for {thread_count} threads")
    assert_eq(consistency.valid_trace_chains, thread_count,
      "All trace chains should be valid for {thread_count} threads")
    assert_eq(consistency.broken_trace_chains, 0,
      "Should have no broken trace chains for {thread_count} threads")
    assert_eq(consistency.orphaned_contexts, thread_count,  // 每个trace的根上下文
      "Should have expected orphaned contexts (roots) for {thread_count} threads")
    assert_eq(consistency.baggage_inconsistencies, 0,
      "Should have no baggage inconsistencies for {thread_count} threads")
    
    // 验证每个上下文的结构
    for context in contexts {
      assert_eq(context.context_id.length() > 0, true)
      assert_eq(context.trace_id.length() > 0, true)
      assert_eq(context.span_id.length() > 0, true)
      assert_eq(context.baggage_items.length() >= 3, true)
      assert_eq(context.propagation_path.length() > 0, true)
    }
  }
}