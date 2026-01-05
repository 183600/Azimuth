// Azimuth 遥测数据生命周期管理测试
// 测试遥测数据的生命周期管理功能

// 测试1: 数据创建和初始化
test "数据创建和初始化测试" {
  // 创建生命周期管理器
  let lifecycle_manager = LifecycleManager::new()
  
  // 配置数据创建策略
  LifecycleManager::set_creation_strategy(lifecycle_manager, {
    auto_generate_ids: true,
    validate_on_create: true,
    assign_default_attributes: true,
    timestamp_on_creation: true
  })
  
  // 创建遥测Span
  let span_creation_request = {
    operation_name: "database.query",
    service_name: "payment-service",
    parent_span_id: None,
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM orders")
    ]
  }
  
  let created_span = LifecycleManager::create_span(lifecycle_manager, span_creation_request)
  
  // 验证自动生成的ID
  assert_true(created_span.trace_id.length() > 0)
  assert_true(created_span.span_id.length() > 0)
  
  // 验证默认属性
  assert_true(created_span.attributes.any(fn(a) { a[0] == "service.name" and a[1] == "payment-service" }))
  assert_true(created_span.attributes.any(fn(a) { a[0] == "telemetry.version" }))
  
  // 验证时间戳
  assert_true(created_span.start_time > 0)
  assert_eq(created_span.end_time, 0)  // 未结束，结束时间为0
  
  // 验证初始状态
  assert_eq(created_span.status, "created")
  
  // 创建遥测Metric
  let metric_creation_request = {
    name: "http.request.duration",
    description: "HTTP请求持续时间",
    unit: "ms",
    metric_type: "histogram",
    service_name: "api.gateway"
  }
  
  let created_metric = LifecycleManager::create_metric(lifecycle_manager, metric_creation_request)
  
  // 验证Metric创建
  assert_eq(created_metric.name, "http.request.duration")
  assert_eq(created_metric.description, "HTTP请求持续时间")
  assert_eq(created_metric.unit, "ms")
  assert_eq(created_metric.metric_type, "histogram")
  
  // 验证自动生成的属性
  assert_true(created_metric.attributes.any(fn(a) { a[0] == "service.name" and a[1] == "api.gateway" }))
  assert_true(created_metric.attributes.any(fn(a) { a[0] == "metric.created_at" }))
  
  // 创建遥测Log
  let log_creation_request = {
    level: "info",
    message: "Processing payment request",
    service_name: "payment-service",
    trace_id: created_span.trace_id,
    span_id: created_span.span_id
  }
  
  let created_log = LifecycleManager::create_log(lifecycle_manager, log_creation_request)
  
  // 验证Log创建
  assert_eq(created_log.level, "info")
  assert_eq(created_log.message, "Processing payment request")
  assert_eq(created_log.trace_id, created_span.trace_id)
  assert_eq(created_log.span_id, created_span.span_id)
  
  // 验证时间戳
  assert_true(created_log.timestamp > 0)
}

// 测试2: 数据状态转换
test "数据状态转换测试" {
  // 创建生命周期管理器
  let lifecycle_manager = LifecycleManager::new()
  
  // 配置状态转换规则
  LifecycleManager::add_state_transition(lifecycle_manager, {
    from_state: "created",
    to_state: "active",
    condition: fn(data) { data.start_time > 0 and data.end_time == 0 },
    action: fn(data) { { data | status: "active" } }
  })
  
  LifecycleManager::add_state_transition(lifecycle_manager, {
    from_state: "active",
    to_state: "completed",
    condition: fn(data) { data.end_time > 0 },
    action: fn(data) { { data | status: "completed" } }
  })
  
  LifecycleManager::add_state_transition(lifecycle_manager, {
    from_state: "active",
    to_state: "error",
    condition: fn(data) { data.attributes.any(fn(a) { a[0] == "error.type" }) },
    action: fn(data) { { data | status: "error" } }
  })
  
  // 创建Span
  let span_request = {
    operation_name: "http.request",
    service_name: "api.gateway"
  }
  
  let span = LifecycleManager::create_span(lifecycle_manager, span_request)
  assert_eq(span.status, "created")
  
  // 激活Span
  let activated_span = LifecycleManager::activate_span(lifecycle_manager, span.span_id)
  assert_eq(activated_span.status, "active")
  
  // 完成Span
  let completed_span = LifecycleManager::complete_span(lifecycle_manager, span.span_id, {
    end_time: Time::now(),
    attributes: [("http.status_code", "200")]
  })
  assert_eq(completed_span.status, "completed")
  assert_true(completed_span.end_time > 0)
  
  // 创建另一个Span用于错误测试
  let error_span_request = {
    operation_name: "database.query",
    service_name: "payment-service"
  }
  
  let error_span = LifecycleManager::create_span(lifecycle_manager, error_span_request)
  let activated_error_span = LifecycleManager::activate_span(lifecycle_manager, error_span.span_id)
  
  // 设置错误状态
  let error_span_with_error = LifecycleManager::set_error(lifecycle_manager, error_span.span_id, {
    error_type: "connection_timeout",
    error_message: "Database connection timeout"
  })
  
  assert_eq(error_span_with_error.status, "error")
  assert_true(error_span_with_error.attributes.any(fn(a) { a[0] == "error.type" and a[1] == "connection_timeout" }))
  
  // 测试无效状态转换
  let invalid_transition_result = LifecycleManager::force_state_transition(lifecycle_manager, span.span_id, "created", "invalid_state")
  
  match invalid_transition_result {
    Ok(_) => assert_true(false),  // 应该失败
    Err(error) => {
      assert_true(error.contains("invalid") or error.contains("transition"))
    }
  }
  
  // 测试状态历史记录
  let state_history = LifecycleManager::get_state_history(lifecycle_manager, error_span.span_id)
  assert_eq(state_history.length(), 3)  // created -> active -> error
  
  assert_eq(state_history[0].from_state, "none")
  assert_eq(state_history[0].to_state, "created")
  
  assert_eq(state_history[1].from_state, "created")
  assert_eq(state_history[1].to_state, "active")
  
  assert_eq(state_history[2].from_state, "active")
  assert_eq(state_history[2].to_state, "error")
}

// 测试3: 数据过期和清理
test "数据过期和清理测试" {
  // 创建生命周期管理器
  let lifecycle_manager = LifecycleManager::new()
  
  // 配置过期策略
  LifecycleManager::set_retention_policy(lifecycle_manager, {
    span_retention_days: 7,      // Span保留7天
    metric_retention_days: 30,   // Metric保留30天
    log_retention_days: 14,      // Log保留14天
    archive_before_delete: true
  })
  
  // 创建不同时间的测试数据
  let base_time = Time::now()
  
  // 创建7天前的Span（应该被清理）
  let old_span_request = {
    operation_name: "old.operation",
    service_name: "old-service"
  }
  
  let old_span = LifecycleManager::create_span_with_timestamp(lifecycle_manager, old_span_request, base_time - 7 * 24 * 3600 * 1000)
  LifecycleManager::complete_span(lifecycle_manager, old_span.span_id, {
    end_time: base_time - 7 * 24 * 3600 * 1000 + 1000
  })
  
  // 创建3天前的Span（不应该被清理）
  let recent_span_request = {
    operation_name: "recent.operation",
    service_name: "recent-service"
  }
  
  let recent_span = LifecycleManager::create_span_with_timestamp(lifecycle_manager, recent_span_request, base_time - 3 * 24 * 3600 * 1000)
  LifecycleManager::complete_span(lifecycle_manager, recent_span.span_id, {
    end_time: base_time - 3 * 24 * 3600 * 1000 + 1000
  })
  
  // 创建35天前的Metric（应该被清理）
  let old_metric_request = {
    name: "old.metric",
    service_name: "old-service"
  }
  
  let old_metric = LifecycleManager::create_metric_with_timestamp(lifecycle_manager, old_metric_request, base_time - 35 * 24 * 3600 * 1000)
  
  // 创建15天前的Log（应该被清理）
  let old_log_request = {
    level: "info",
    message: "Old log message",
    service_name: "old-service"
  }
  
  let old_log = LifecycleManager::create_log_with_timestamp(lifecycle_manager, old_log_request, base_time - 15 * 24 * 3600 * 1000)
  
  // 执行清理
  let cleanup_result = LifecycleManager::cleanup_expired_data(lifecycle_manager)
  
  // 验证清理结果
  assert_eq(cleanup_result.expired_spans.length(), 1)
  assert_eq(cleanup_result.expired_metrics.length(), 1)
  assert_eq(cleanup_result.expired_logs.length(), 1)
  
  assert_eq(cleanup_result.expired_spans[0].span_id, old_span.span_id)
  assert_eq(cleanup_result.expired_metrics[0].id, old_metric.id)
  assert_eq(cleanup_result.expired_logs[0].id, old_log.id)
  
  // 验证数据已被归档
  assert_true(cleanup_result.archived_data.length() > 0)
  
  // 验证近期数据未被清理
  let remaining_span = LifecycleManager::get_span(lifecycle_manager, recent_span.span_id)
  assert_true(remaining_span != None)
  
  // 测试自定义清理策略
  let custom_cleanup_result = LifecycleManager::cleanup_with_custom_policy(lifecycle_manager, {
    data_type: "span",
    condition: fn(data) { data.service_name == "old-service" },
    archive: false
  })
  
  // 验证自定义清理结果
  assert_eq(custom_cleanup_result.cleaned_items.length(), 1)
  
  // 测试清理统计
  let cleanup_stats = LifecycleManager::get_cleanup_statistics(lifecycle_manager)
  
  assert_true(cleanup_stats.total_cleaned > 0)
  assert_true(cleanup_stats.total_archived > 0)
  assert_true(cleanup_stats.space_freed > 0)
}

// 测试4: 数据归档和恢复
test "数据归档和恢复测试" {
  // 创建生命周期管理器
  let lifecycle_manager = LifecycleManager::new()
  
  // 配置归档策略
  LifecycleManager::set_archive_strategy(lifecycle_manager, {
    archive_format: "compressed_json",
    compression_algorithm: "gzip",
    archive_location: "s3://telemetry-archive/",
    partition_by: "date",
    verify_after_archive: true
  })
  
  // 创建测试数据
  let test_data = []
  
  for i in 0..100 {
    let span_request = {
      operation_name: "operation." + i.to_string(),
      service_name: "service-" + (i % 5).to_string()
    }
    
    let span = LifecycleManager::create_span(lifecycle_manager, span_request)
    LifecycleManager::complete_span(lifecycle_manager, span.span_id, {
      end_time: Time::now() + i * 1000,
      attributes: [("iteration", i.to_string())]
    })
    
    test_data = test_data.push(span)
  }
  
  // 执行归档
  let archive_result = LifecycleManager::archive_data(lifecycle_manager, test_data)
  
  // 验证归档结果
  assert_true(archive_result.success)
  assert_eq(archive_result.archived_items, 100)
  assert_true(archive_result.archive_id.length() > 0)
  assert_true(archive_result.archive_size > 0)
  
  // 验证归档元数据
  assert_eq(archive_result.metadata.item_count, 100)
  assert_eq(archive_result.metadata.data_types, ["span"])
  assert_true(archive_result.metadata.archive_timestamp > 0)
  
  // 测试归档数据恢复
  let restore_result = LifecycleManager::restore_from_archive(lifecycle_manager, archive_result.archive_id)
  
  // 验证恢复结果
  assert_true(restore_result.success)
  assert_eq(restore_result.restored_items, 100)
  assert_eq(restore_result.data.length(), 100)
  
  // 验证恢复的数据完整性
  for i in 0..test_data.length() {
    let original = test_data[i]
    let restored = restore_result.data.find(fn(d) { d.span_id == original.span_id })
    
    assert_true(restored != None)
    match restored {
      Some(item) => {
        assert_eq(item.operation_name, original.operation_name)
        assert_eq(item.service_name, original.service_name)
      }
      None => assert_true(false)
    }
  }
  
  // 测试部分恢复
  let partial_restore_result = LifecycleManager::restore_from_archive_with_filter(lifecycle_manager, archive_result.archive_id, {
    service_name: "service-0"
  })
  
  // 验证部分恢复结果
  assert_true(partial_restore_result.success)
  assert_eq(partial_restore_result.restored_items, 20)  // 100个项目中20个属于service-0
  
  // 测试归档压缩比
  let original_size = test_data.reduce(fn(acc, item) { 
    acc + item.to_string().length() 
  }, 0)
  
  let compression_ratio = archive_result.archive_size.to_float() / original_size.to_float()
  assert_true(compression_ratio < 0.5)  // 压缩后应小于原始大小的50%
  
  // 测试归档验证
  let verification_result = LifecycleManager::verify_archive(lifecycle_manager, archive_result.archive_id)
  
  assert_true(verification_result.is_valid)
  assert_eq(verification_result.verified_items, 100)
  assert_eq(verification_result.corrupted_items, 0)
}

// 测试5: 数据版本管理
test "数据版本管理测试" {
  // 创建生命周期管理器
  let lifecycle_manager = LifecycleManager::new()
  
  // 配置版本管理策略
  LifecycleManager::set_versioning_strategy(lifecycle_manager, {
    auto_version_on_update: true,
    max_versions_per_item: 5,
    version_metadata: true,
    version_retention_days: 90
  })
  
  // 创建初始数据
  let initial_span_request = {
    operation_name: "versioned.operation",
    service_name: "versioned-service",
    attributes: [("version", "1.0")]
  }
  
  let initial_span = LifecycleManager::create_span(lifecycle_manager, initial_span_request)
  
  // 验证初始版本
  assert_eq(initial_span.version, 1)
  
  // 更新数据
  let updated_span_v1 = LifecycleManager::update_span(lifecycle_manager, initial_span.span_id, {
    attributes: [("version", "1.1"), ("feature", "new_feature")]
  })
  
  assert_eq(updated_span_v1.version, 2)
  assert_true(updated_span_v1.attributes.any(fn(a) { a[0] == "version" and a[1] == "1.1" }))
  assert_true(updated_span_v1.attributes.any(fn(a) { a[0] == "feature" and a[1] == "new_feature" }))
  
  // 再次更新
  let updated_span_v2 = LifecycleManager::update_span(lifecycle_manager, initial_span.span_id, {
    attributes: [("version", "1.2"), ("performance", "optimized")]
  })
  
  assert_eq(updated_span_v2.version, 3)
  assert_true(updated_span_v2.attributes.any(fn(a) { a[0] == "version" and a[1] == "1.2" }))
  assert_true(updated_span_v2.attributes.any(fn(a) { a[0] == "performance" and a[1] == "optimized" }))
  
  // 获取版本历史
  let version_history = LifecycleManager::get_version_history(lifecycle_manager, initial_span.span_id)
  
  assert_eq(version_history.length(), 3)
  assert_eq(version_history[0].version, 1)
  assert_eq(version_history[1].version, 2)
  assert_eq(version_history[2].version, 3)
  
  // 验证版本时间戳
  assert_true(version_history[0].timestamp <= version_history[1].timestamp)
  assert_true(version_history[1].timestamp <= version_history[2].timestamp)
  
  // 恢复到特定版本
  let restored_span_v1 = LifecycleManager::restore_to_version(lifecycle_manager, initial_span.span_id, 2)
  
  assert_eq(restored_span_v1.version, 4)  // 恢复操作创建新版本
  assert_true(restored_span_v1.attributes.any(fn(a) { a[0] == "version" and a[1] == "1.1" }))
  assert_true(restored_span_v1.attributes.any(fn(a) { a[0] == "feature" and a[1] == "new_feature" }))
  assert_false(restored_span_v1.attributes.any(fn(a) { a[0] == "performance" }))  // v1不应有此属性
  
  // 比较版本差异
  let diff_v2_v1 = LifecycleManager::compare_versions(lifecycle_manager, initial_span.span_id, 3, 2)
  
  assert_eq(diff_v2_v1.added.length(), 1)  // 添加了performance属性
  assert_eq(diff_v2_v1.removed.length(), 1)  // 移除了feature属性
  assert_eq(diff_v2_v1.modified.length(), 1)  // 修改了version属性
  
  // 测试版本清理
  LifecycleManager::update_span(lifecycle_manager, initial_span.span_id, {
    attributes: [("version", "1.3")]
  })
  
  LifecycleManager::update_span(lifecycle_manager, initial_span.span_id, {
    attributes: [("version", "1.4")]
  })
  
  LifecycleManager::update_span(lifecycle_manager, initial_span.span_id, {
    attributes: [("version", "1.5")]
  })
  
  // 现在有6个版本，超过最大版本数限制
  let cleanup_versions_result = LifecycleManager::cleanup_old_versions(lifecycle_manager, initial_span.span_id)
  
  assert_eq(cleanup_versions_result.removed_versions, 1)  // 移除了最旧的版本
  assert_eq(cleanup_versions_result.remaining_versions, 5)  // 保留5个版本
  
  // 验证版本清理后的历史
  let cleaned_version_history = LifecycleManager::get_version_history(lifecycle_manager, initial_span.span_id)
  assert_eq(cleaned_version_history.length(), 5)
  assert_eq(cleaned_version_history[0].version, 2)  // 最早的版本现在是v2
}

// 测试6: 数据生命周期事件
test "数据生命周期事件测试" {
  // 创建生命周期管理器
  let lifecycle_manager = LifecycleManager::new()
  
  // 创建事件监听器
  let event_log = Ref::new([])
  
  LifecycleManager::add_event_listener(lifecycle_manager, {
    name: "test_listener",
    events: ["created", "updated", "deleted", "archived", "restored"],
    handler: fn(event) {
      let current_log = Ref::get(event_log)
      Ref::set(event_log, ArrayUtil::append(current_log, [event]))
    }
  })
  
  // 创建数据并验证事件
  let span_request = {
    operation_name: "event.test.operation",
    service_name: "event-test-service"
  }
  
  let span = LifecycleManager::create_span(lifecycle_manager, span_request)
  
  // 验证创建事件
  let current_log = Ref::get(event_log)
  assert_eq(current_log.length(), 1)
  assert_eq(current_log[0].event_type, "created")
  assert_eq(current_log[0].data_type, "span")
  assert_eq(current_log[0].item_id, span.span_id)
  
  // 更新数据并验证事件
  LifecycleManager::update_span(lifecycle_manager, span.span_id, {
    attributes: [("updated", "true")]
  })
  
  let updated_log = Ref::get(event_log)
  assert_eq(updated_log.length(), 2)
  assert_eq(updated_log[1].event_type, "updated")
  assert_eq(updated_log[1].data_type, "span")
  assert_eq(updated_log[1].item_id, span.span_id)
  
  // 归档数据并验证事件
  let archive_result = LifecycleManager::archive_data(lifecycle_manager, [span])
  
  let archived_log = Ref::get(event_log)
  assert_eq(archived_log.length(), 3)
  assert_eq(archived_log[2].event_type, "archived")
  assert_eq(archived_log[2].data_type, "span")
  assert_eq(archived_log[2].archive_id, archive_result.archive_id)
  
  // 恢复数据并验证事件
  LifecycleManager::restore_from_archive(lifecycle_manager, archive_result.archive_id)
  
  let restored_log = Ref::get(event_log)
  assert_eq(restored_log.length(), 4)
  assert_eq(restored_log[3].event_type, "restored")
  assert_eq(restored_log[3].data_type, "span")
  
  // 删除数据并验证事件
  LifecycleManager::delete_span(lifecycle_manager, span.span_id)
  
  let deleted_log = Ref::get(event_log)
  assert_eq(deleted_log.length(), 5)
  assert_eq(deleted_log[4].event_type, "deleted")
  assert_eq(deleted_log[4].data_type, "span")
  assert_eq(deleted_log[4].item_id, span.span_id)
  
  // 测试事件过滤
  let filtered_events = LifecycleManager::get_events(lifecycle_manager, {
    event_type: Some("created"),
    data_type: Some("span"),
    start_time: Some(Time::now() - 3600000),  // 最近1小时
    end_time: Some(Time::now())
  })
  
  assert_eq(filtered_events.length(), 1)
  assert_eq(filtered_events[0].event_type, "created")
  
  // 测试事件统计
  let event_stats = LifecycleManager::get_event_statistics(lifecycle_manager)
  
  assert_eq(event_stats.total_events, 5)
  assert_eq(event_stats.event_counts.get("created"), Some(1))
  assert_eq(event_stats.event_counts.get("updated"), Some(1))
  assert_eq(event_stats.event_counts.get("archived"), Some(1))
  assert_eq(event_stats.event_counts.get("restored"), Some(1))
  assert_eq(event_stats.event_counts.get("deleted"), Some(1))
  
  // 测试条件事件监听器
  LifecycleManager::add_conditional_event_listener(lifecycle_manager, {
    name: "conditional_listener",
    condition: fn(event) { 
      event.event_type == "created" and event.data_type == "span" 
    },
    handler: fn(event) {
      // 只有满足条件的Span创建事件才会触发
      assert_eq(event.event_type, "created")
      assert_eq(event.data_type, "span")
    }
  })
  
  // 创建新的Span以测试条件监听器
  let conditional_span_request = {
    operation_name: "conditional.test.operation",
    service_name: "conditional-test-service"
  }
  
  LifecycleManager::create_span(lifecycle_manager, conditional_span_request)
  
  // 创建Metric以测试条件监听器（不应触发）
  let metric_request = {
    name: "conditional.test.metric",
    service_name: "conditional-test-service"
  }
  
  LifecycleManager::create_metric(lifecycle_manager, metric_request)
}

// 测试7: 数据生命周期策略应用
test "数据生命周期策略应用测试" {
  // 创建生命周期管理器
  let lifecycle_manager = LifecycleManager::new()
  
  // 创建服务级别的生命周期策略
  LifecycleManager::add_service_policy(lifecycle_manager, {
    service_name: "payment-service",
    span_retention_days: 14,      // 比默认更长的保留期
    metric_retention_days: 60,    // 比默认更长的保留期
    archive_immediately: true,    // 立即归档
    priority: "high"              // 高优先级
  })
  
  // 创建操作级别的生命周期策略
  LifecycleManager::add_operation_policy(lifecycle_manager, {
    service_name: "payment-service",
    operation_name: "critical.operation",
    span_retention_days: 30,      // 关键操作保留更长时间
    archive_immediately: true,
    encrypt_before_archive: true, // 归档前加密
    priority: "critical"
  })
  
  // 创建测试数据
  let payment_span_request = {
    operation_name: "payment.process",
    service_name: "payment-service"
  }
  
  let payment_span = LifecycleManager::create_span(lifecycle_manager, payment_span_request)
  LifecycleManager::complete_span(lifecycle_manager, payment_span.span_id, {
    end_time: Time::now() + 1000
  })
  
  let critical_operation_request = {
    operation_name: "critical.operation",
    service_name: "payment-service"
  }
  
  let critical_operation_span = LifecycleManager::create_span(lifecycle_manager, critical_operation_request)
  LifecycleManager::complete_span(lifecycle_manager, critical_operation_span.span_id, {
    end_time: Time::now() + 1000
  })
  
  // 应用生命周期策略
  let policy_application_result = LifecycleManager::apply_lifecycle_policies(lifecycle_manager)
  
  // 验证策略应用结果
  assert_eq(policy_application_result.processed_items, 2)
  assert_eq(policy_application_result.applied_policies, 2)
  
  // 验证支付Span的策略应用
  let payment_span_policy = LifecycleManager::get_applied_policy(lifecycle_manager, payment_span.span_id)
  assert_true(payment_span_policy != None)
  
  match payment_span_policy {
    Some(policy) => {
      assert_eq(policy.service_name, "payment-service")
      assert_eq(policy.span_retention_days, 14)
      assert_eq(policy.archive_immediately, true)
      assert_eq(policy.priority, "high")
    }
    None => assert_true(false)
  }
  
  // 验证关键操作Span的策略应用
  let critical_operation_span_policy = LifecycleManager::get_applied_policy(lifecycle_manager, critical_operation_span.span_id)
  assert_true(critical_operation_span_policy != None)
  
  match critical_operation_span_policy {
    Some(policy) => {
      assert_eq(policy.service_name, "payment-service")
      assert_eq(policy.operation_name, "critical.operation")
      assert_eq(policy.span_retention_days, 30)
      assert_eq(policy.archive_immediately, true)
      assert_eq(policy.encrypt_before_archive, true)
      assert_eq(policy.priority, "critical")
    }
    None => assert_true(false)
  }
  
  // 测试策略优先级
  let default_service_span_request = {
    operation_name: "default.operation",
    service_name: "default-service"
  }
  
  let default_service_span = LifecycleManager::create_span(lifecycle_manager, default_service_span_request)
  LifecycleManager::complete_span(lifecycle_manager, default_service_span.span_id, {
    end_time: Time::now() + 1000
  })
  
  let default_service_span_policy = LifecycleManager::get_applied_policy(lifecycle_manager, default_service_span.span_id)
  
  match default_service_span_policy {
    Some(policy) => {
      assert_eq(policy.service_name, "default-service")
      assert_eq(policy.priority, "normal")  // 默认优先级
    }
    None => {}  // 可能没有应用特定策略
  }
  
  // 测试策略冲突解决
  LifecycleManager::add_global_policy(lifecycle_manager, {
    span_retention_days: 7,       // 全局策略，比服务策略更短
    archive_immediately: false,   // 全局策略，与服务策略冲突
    priority: "normal"
  })
  
  let conflict_resolution_result = LifecycleManager::resolve_policy_conflicts(lifecycle_manager)
  
  // 验证冲突解决结果（服务级别策略应优先于全局策略）
  assert_eq(conflict_resolution_result.resolved_conflicts, 1)
  
  let resolved_payment_span_policy = LifecycleManager::get_applied_policy(lifecycle_manager, payment_span.span_id)
  
  match resolved_payment_span_policy {
    Some(policy) => {
      assert_eq(policy.span_retention_days, 14)  // 服务策略优先
      assert_eq(policy.archive_immediately, true)  // 服务策略优先
    }
    None => assert_true(false)
  }
  
  // 测试策略效果统计
  let policy_stats = LifecycleManager::get_policy_statistics(lifecycle_manager)
  
  assert_eq(policy_stats.total_policies, 4)  // 1个全局 + 1个服务 + 1个操作 + 1个默认
  assert_eq(policy_stats.applied_policies, 3)  // 应用于3个数据项
  assert_eq(policy_stats.policy_conflicts, 1)   // 1个已解决的冲突
}