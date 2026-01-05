// Azimuth 高级功能测试用例
// 专注于核心遥测系统的高级功能和特性

// 测试1: 遥测数据批处理
test "遥测数据批处理测试" {
  // 创建批处理器
  let batch_processor = BatchProcessor::new({
    max_batch_size: 100,
    max_wait_time: 5000,  // 5秒
    flush_threshold: 80   // 80%容量时触发刷新
  })
  
  // 添加遥测数据到批处理器
  for i in 0..=150 {
    let telemetry_data = {
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      service_name: "service-" + (i % 5).to_string(),
      operation_name: "operation-" + (i % 10).to_string(),
      timestamp: 1640995200 + i,
      duration: 50 + (i % 100),
      status: if i % 20 == 0 { "error" } else { "ok" },
      attributes: [
        ("http.method", "GET"),
        ("http.status_code", (200 + (i % 3) * 100).to_string())
      ]
    }
    
    BatchProcessor::add_data(batch_processor, telemetry_data)
  }
  
  // 等待批处理完成
  BatchProcessor::flush(batch_processor)
  
  // 验证批处理结果
  let batch_results = BatchProcessor::get_batch_results(batch_processor)
  
  // 应该有两个批次：第一个100个，第二个50个
  assert_eq(batch_results.length(), 2)
  assert_eq(batch_results[0].items.length(), 100)
  assert_eq(batch_results[1].items.length(), 50)
  
  // 验证批处理时间
  assert_true(batch_results[0].processing_time > 0)
  assert_true(batch_results[1].processing_time > 0)
  
  // 验证批处理效率
  let total_items = batch_results.map(fn(b) { b.items.length() }).reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(total_items, 150)
  
  // 测试批处理吞吐量
  let throughput = total_items.to_float() / (batch_results.map(fn(b) { b.processing_time }).reduce(fn(acc, x) { acc + x }, 0) / 1000.0)
  assert_true(throughput > 10.0)  // 至少每秒处理10个项目
}

// 测试2: 遥测数据持久化
test "遥测数据持久化测试" {
  // 创建持久化管理器
  let persistence_manager = PersistenceManager::new({
    storage_backend: "file",
    connection_string: "/tmp/azimuth_telemetry.db",
    retention_period: 86400 * 7  // 7天保留期
  })
  
  // 初始化存储
  let init_result = PersistenceManager::initialize(persistence_manager)
  assert_true(init_result.success)
  
  // 创建测试遥测数据
  let test_data = []
  for i in 0..=50 {
    test_data = test_data.push({
      id: "telemetry-" + i.to_string(),
      trace_id: "trace-" + i.to_string(),
      timestamp: 1640995200 + i * 60,
      service_name: "service-" + (i % 3).to_string(),
      metrics: {
        request_count: 10 + i * 2,
        error_count: if i % 10 == 0 { 1 } else { 0 },
        latency_avg: 50.0 + (i % 20) * 2.5
      },
      tags: [
        ("environment", "production"),
        ("version", "1.2.3")
      ]
    })
  }
  
  // 存储遥测数据
  let store_result = PersistenceManager::store_batch(persistence_manager, test_data)
  assert_true(store_result.success)
  assert_eq(store_result.stored_count, 51)
  
  // 查询特定时间范围的数据
  let query_result = PersistenceManager::query_by_time_range(
    persistence_manager, 
    1640995200, 
    1640995200 + 30 * 60
  )
  
  assert_true(query_result.success)
  assert_eq(query_result.data.length(), 31)  // 0到30 inclusive
  
  // 按服务名称查询
  let service_query = PersistenceManager::query_by_attribute(
    persistence_manager,
    "service_name",
    "service-1"
  )
  
  assert_true(service_query.success)
  assert_true(service_query.data.length() > 0)
  
  // 验证查询结果的数据完整性
  let first_item = service_query.data[0]
  assert_eq(first_item.service_name, "service-1")
  assert_true(first_item.metrics.request_count > 0)
  
  // 测试数据更新
  let update_data = {
    id: "telemetry-5",
    metrics: {
      request_count: 100,
      error_count: 2,
      latency_avg: 75.5
    }
  }
  
  let update_result = PersistenceManager::update(persistence_manager, update_data)
  assert_true(update_result.success)
  
  // 验证更新结果
  let verify_result = PersistenceManager::get_by_id(persistence_manager, "telemetry-5")
  assert_true(verify_result.success)
  assert_eq(verify_result.data.metrics.request_count, 100)
  
  // 测试数据删除
  let delete_result = PersistenceManager::delete_by_id(persistence_manager, "telemetry-10")
  assert_true(delete_result.success)
  
  // 验证删除结果
  let deleted_check = PersistenceManager::get_by_id(persistence_manager, "telemetry-10")
  assert_false(deleted_check.success)
  
  // 测试过期数据清理
  let old_timestamp = 1640995200 - (86400 * 8)  // 8天前
  let old_data = {
    id: "old-telemetry",
    trace_id: "old-trace",
    timestamp: old_timestamp,
    service_name: "old-service",
    metrics: { request_count: 5, error_count: 0, latency_avg: 100.0 },
    tags: []
  }
  
  PersistenceManager::store(persistence_manager, old_data)
  let cleanup_result = PersistenceManager::cleanup_expired(persistence_manager)
  assert_true(cleanup_result.success)
  assert_true(cleanup_result.deleted_count > 0)
}

// 测试3: 配置热更新
test "配置热更新测试" {
  // 创建配置管理器
  let config_manager = ConfigManager::new({
    config_file: "/tmp/azimuth_config.json",
    auto_reload: true,
    reload_interval: 1000  // 1秒检查间隔
  })
  
  // 初始化配置
  let initial_config = {
    telemetry: {
      sampling_rate: 0.1,
      batch_size: 100,
      flush_interval: 5000
    },
    storage: {
      backend: "file",
      path: "/tmp/telemetry.db",
      retention_days: 7
    },
    network: {
      timeout: 30000,
      retry_attempts: 3,
      backoff_multiplier: 2.0
    }
  }
  
  ConfigManager::load_config(config_manager, initial_config)
  
  // 验证初始配置
  let current_config = ConfigManager::get_config(config_manager)
  assert_eq(current_config.telemetry.sampling_rate, 0.1)
  assert_eq(current_config.telemetry.batch_size, 100)
  assert_eq(current_config.storage.retention_days, 7)
  
  // 注册配置变更监听器
  let config_changes = { mut changes: [] }
  
  ConfigManager::add_change_listener(config_manager, fn(path, old_value, new_value) {
    config_changes.changes = config_changes.changes.push({
      path,
      old_value,
      new_value,
      timestamp: Time::now()
    })
  })
  
  // 模拟配置文件更新
  let updated_config = {
    telemetry: {
      sampling_rate: 0.2,  // 更新
      batch_size: 200,     // 更新
      flush_interval: 5000 // 不变
    },
    storage: {
      backend: "file",
      path: "/tmp/telemetry.db",
      retention_days: 14   // 更新
    },
    network: {
      timeout: 30000,
      retry_attempts: 3,
      backoff_multiplier: 2.0
    }
  }
  
  // 触发配置更新
  ConfigManager::update_config(config_manager, updated_config)
  
  // 等待配置重载
  Thread::sleep(2000)  // 等待2秒确保配置重载
  
  // 验证配置已更新
  let reloaded_config = ConfigManager::get_config(config_manager)
  assert_eq(reloaded_config.telemetry.sampling_rate, 0.2)
  assert_eq(reloaded_config.telemetry.batch_size, 200)
  assert_eq(reloaded_config.storage.retention_days, 14)
  
  // 验证配置变更监听器被触发
  assert_true(config_changes.changes.length() > 0)
  
  let sampling_change = config_changes.changes.find(fn(c) { c.path == "telemetry.sampling_rate" })
  assert_true(sampling_change != None)
  
  match sampling_change {
    Some(change) => {
      assert_eq(change.old_value, 0.1)
      assert_eq(change.new_value, 0.2)
    }
    None => assert_true(false)
  }
  
  // 测试配置验证
  let invalid_config = {
    telemetry: {
      sampling_rate: 1.5,  // 无效：大于1.0
      batch_size: -10,     // 无效：负数
      flush_interval: 5000
    },
    storage: {
      backend: "file",
      path: "/tmp/telemetry.db",
      retention_days: 14
    },
    network: {
      timeout: 30000,
      retry_attempts: 3,
      backoff_multiplier: 2.0
    }
  }
  
  // 尝试应用无效配置
  let validation_result = ConfigManager::validate_and_update(config_manager, invalid_config)
  assert_false(validation_result.success)
  assert_true(validation_result.errors.length() > 0)
  
  // 验证配置未被应用
  let unchanged_config = ConfigManager::get_config(config_manager)
  assert_eq(unchanged_config.telemetry.sampling_rate, 0.2)  // 保持之前的值
  assert_eq(unchanged_config.telemetry.batch_size, 200)    // 保持之前的值
  
  // 测试配置回滚
  let rollback_result = ConfigManager::rollback_to_previous(config_manager)
  assert_true(rollback_result.success)
  
  let rolled_back_config = ConfigManager::get_config(config_manager)
  assert_eq(rolled_back_config.telemetry.sampling_rate, 0.1)
  assert_eq(rolled_back_config.telemetry.batch_size, 100)
  
  // 测试配置导出
  let exported_config = ConfigManager::export_config(config_manager, "yaml")
  assert_true(exported_config.length() > 0)
  assert_true(exported_config.contains("sampling_rate"))
  assert_true(exported_config.contains("batch_size"))
}

// 测试4: 遥测数据生命周期管理
test "遥测数据生命周期管理测试" {
  // 创建生命周期管理器
  let lifecycle_manager = LifecycleManager::new({
    retention_policies: [
      {
        name: "default",
        criteria: { service_name: "*", environment: "*" },
        retention_period: 86400 * 7,  // 7天
        aggregation_period: 3600       // 1小时聚合
      },
      {
        name: "critical",
        criteria: { service_name: "critical.*", environment: "production" },
        retention_period: 86400 * 30, // 30天
        aggregation_period: 3600       // 1小时聚合
      },
      {
        name: "debug",
        criteria: { service_name: "*", environment: "debug" },
        retention_period: 86400,       // 1天
        aggregation_period: 1800       // 30分钟聚合
      }
    ]
  })
  
  let base_time = 1640995200  // 2022-01-01 00:00:00 UTC
  
  // 创建不同类型和年龄的测试数据
  let test_data = []
  
  // 默认数据 - 5天前
  for i in 0..=10 {
    test_data = test_data.push({
      id: "default-" + i.to_string(),
      trace_id: "trace-default-" + i.to_string(),
      timestamp: base_time - (5 * 86400) + i * 3600,
      service_name: "service-" + (i % 3).to_string(),
      environment: "production",
      data_size: 1024 + i * 100
    })
  }
  
  // 关键数据 - 20天前
  for i in 0..=10 {
    test_data = test_data.push({
      id: "critical-" + i.to_string(),
      trace_id: "trace-critical-" + i.to_string(),
      timestamp: base_time - (20 * 86400) + i * 3600,
      service_name: "critical-service-" + (i % 2).to_string(),
      environment: "production",
      data_size: 2048 + i * 200
    })
  }
  
  // 调试数据 - 2天前
  for i in 0..=10 {
    test_data = test_data.push({
      id: "debug-" + i.to_string(),
      trace_id: "trace-debug-" + i.to_string(),
      timestamp: base_time - (2 * 86400) + i * 3600,
      service_name: "debug-service-" + (i % 3).to_string(),
      environment: "debug",
      data_size: 512 + i * 50
    })
  }
  
  // 过期数据 - 10天前
  for i in 0..=10 {
    test_data = test_data.push({
      id: "expired-" + i.to_string(),
      trace_id: "trace-expired-" + i.to_string(),
      timestamp: base_time - (10 * 86400) + i * 3600,
      service_name: "expired-service-" + (i % 3).to_string(),
      environment: "production",
      data_size: 1024 + i * 100
    })
  }
  
  // 添加数据到生命周期管理器
  for data in test_data {
    LifecycleManager::add_data(lifecycle_manager, data)
  }
  
  // 执行生命周期检查
  let lifecycle_result = LifecycleManager::execute_lifecycle_check(lifecycle_manager)
  
  // 验证生命周期检查结果
  assert_true(lifecycle_result.success)
  assert_true(lifecycle_result.expired_data_count > 0)
  assert_true(lifecycle_result.aggregated_data_count > 0)
  
  // 验证过期数据被识别
  let expired_data = lifecycle_result.expired_data
  assert_true(expired_data.length() > 0)
  
  // 检查默认策略过期数据（7天保留期，10天前的数据应该过期）
  let default_expired = expired_data.filter(fn(d) { d.id.starts_with("expired-") })
  assert_true(default_expired.length() > 0)
  
  // 检查关键策略数据（30天保留期，20天前的数据不应过期）
  let critical_expired = expired_data.filter(fn(d) { d.id.starts_with("critical-") })
  assert_eq(critical_expired.length(), 0)  // 不应过期
  
  // 检查调试策略数据（1天保留期，2天前的数据应该过期）
  let debug_expired = expired_data.filter(fn(d) { d.id.starts_with("debug-") })
  assert_true(debug_expired.length() > 0)
  
  // 验证数据聚合
  let aggregated_data = lifecycle_result.aggregated_data
  assert_true(aggregated_data.length() > 0)
  
  // 检查聚合数据的正确性
  let service_aggregation = aggregated_data.find(fn(a) { a.service_name == "service-1" })
  assert_true(service_aggregation != None)
  
  match service_aggregation {
    Some(agg) => {
      assert_true(agg.aggregated_metrics.request_count > 0)
      assert_true(agg.sample_count > 0)
      assert_eq(agg.aggregation_period, 3600)  // 1小时
    }
    None => assert_true(false)
  }
  
  // 测试数据清理
  let cleanup_result = LifecycleManager::cleanup_expired_data(lifecycle_manager)
  assert_true(cleanup_result.success)
  assert_eq(cleanup_result.deleted_count, expired_data.length())
  
  // 验证清理后数据状态
  let remaining_data = LifecycleManager::get_all_data(lifecycle_manager)
  
  // 过期数据应被清理
  let remaining_expired = remaining_data.filter(fn(d) { d.id.starts_with("expired-") })
  assert_eq(remaining_expired.length(), 0)
  
  // 关键数据应保留
  let remaining_critical = remaining_data.filter(fn(d) { d.id.starts_with("critical-") })
  assert_true(remaining_critical.length() > 0)
  
  // 调试数据应被清理
  let remaining_debug = remaining_data.filter(fn(d) { d.id.starts_with("debug-") })
  assert_eq(remaining_debug.length(), 0)
  
  // 默认数据应保留（5天前，7天保留期）
  let remaining_default = remaining_data.filter(fn(d) { d.id.starts_with("default-") })
  assert_true(remaining_default.length() > 0)
  
  // 测试存储空间回收
  let space_reclaimed = LifecycleManager::reclaim_storage_space(lifecycle_manager)
  assert_true(space_reclaimed > 0)
}

// 测试5: 多租户隔离
test "多租户隔离测试" {
  // 创建多租户管理器
  let tenant_manager = TenantManager::new({
    isolation_level: "strict",  // 严格隔离
    default_quota: {
      max_spans_per_day: 100000,
      max_storage_mb: 1024,
      max_retention_days: 7
    }
  })
  
  // 创建租户
  let tenant_a = TenantManager::create_tenant(tenant_manager, {
    id: "tenant-a",
    name: "Customer A",
    quota: {
      max_spans_per_day: 50000,
      max_storage_mb: 512,
      max_retention_days: 7
    },
    access_policies: [
      "read_own_data",
      "write_own_data"
    ]
  })
  
  let tenant_b = TenantManager::create_tenant(tenant_manager, {
    id: "tenant-b",
    name: "Customer B",
    quota: {
      max_spans_per_day: 200000,
      max_storage_mb: 2048,
      max_retention_days: 14
    },
    access_policies: [
      "read_own_data",
      "write_own_data",
      "share_data_with_tenant-c"
    ]
  })
  
  let tenant_c = TenantManager::create_tenant(tenant_manager, {
    id: "tenant-c",
    name: "Customer C",
    quota: {
      max_spans_per_day: 75000,
      max_storage_mb: 768,
      max_retention_days: 10
    },
    access_policies: [
      "read_own_data",
      "write_own_data",
      "read_shared_data"
    ]
  })
  
  // 验证租户创建
  assert_true(tenant_a.success)
  assert_true(tenant_b.success)
  assert_true(tenant_c.success)
  
  // 为租户A添加数据
  for i in 0..=100 {
    let span_data = {
      trace_id: "trace-a-" + i.to_string(),
      span_id: "span-a-" + i.to_string(),
      tenant_id: "tenant-a",
      service_name: "service-a",
      timestamp: 1640995200 + i * 60,
      attributes: [
        ("tenant.data", "sensitive-a-" + i.to_string()),
        ("customer.id", "customer-a")
      ]
    }
    
    TenantManager::add_span_data(tenant_manager, span_data)
  }
  
  // 为租户B添加数据
  for i in 0..=200 {
    let span_data = {
      trace_id: "trace-b-" + i.to_string(),
      span_id: "span-b-" + i.to_string(),
      tenant_id: "tenant-b",
      service_name: "service-b",
      timestamp: 1640995200 + i * 60,
      attributes: [
        ("tenant.data", "sensitive-b-" + i.to_string()),
        ("customer.id", "customer-b")
      ]
    }
    
    TenantManager::add_span_data(tenant_manager, span_data)
  }
  
  // 测试租户数据隔离
  let tenant_a_data = TenantManager::get_tenant_data(tenant_manager, "tenant-a")
  assert_eq(tenant_a_data.length(), 101)
  
  let tenant_b_data = TenantManager::get_tenant_data(tenant_manager, "tenant-b")
  assert_eq(tenant_b_data.length(), 201)
  
  // 验证数据不交叉
  let a_data_in_b = tenant_b_data.filter(fn(d) { d.tenant_id == "tenant-a" })
  assert_eq(a_data_in_b.length(), 0)
  
  let b_data_in_a = tenant_a_data.filter(fn(d) { d.tenant_id == "tenant-b" })
  assert_eq(b_data_in_a.length(), 0)
  
  // 测试租户配额
  let tenant_a_quota = TenantManager::get_quota_usage(tenant_manager, "tenant-a")
  assert_eq(tenant_a_quota.span_count, 101)
  assert_eq(tenant_a_quota.storage_used_mb, 101 * 0.001)  // 假设每个span 1KB
  assert_true(tenant_a_quota.quota_exceeded == false)
  
  // 测试跨租户访问控制
  let cross_tenant_access = TenantManager::check_access_permission(
    tenant_manager,
    "tenant-a",
    "tenant-b",
    "read"
  )
  
  assert_false(cross_tenant_access)  // 租户A不应能访问租户B的数据
  
  // 测试共享数据访问
  let shared_access = TenantManager::check_access_permission(
    tenant_manager,
    tenant_b.id,
    tenant_c.id,
    "read"
  )
  
  assert_true(shared_access)  // 租户B共享数据给租户C
  
  // 创建共享数据
  let shared_data = {
    trace_id: "trace-shared",
    span_id: "span-shared",
    tenant_id: "tenant-b",
    service_name: "shared-service",
    timestamp: 1640995200,
    attributes: [
      ("data.type", "shared"),
      ("shared.with", "tenant-c")
    ],
    shared_with: ["tenant-c"]
  }
  
  TenantManager::add_span_data(tenant_manager, shared_data)
  
  // 测试共享数据访问
  let shared_data_for_c = TenantManager::get_shared_data(tenant_manager, "tenant-c")
  assert_true(shared_data_for_c.length() > 0)
  
  let shared_trace = shared_data_for_c.find(fn(d) { d.trace_id == "trace-shared" })
  assert_true(shared_trace != None)
  
  // 测试租户数据聚合隔离
  let tenant_a_metrics = TenantManager::get_aggregated_metrics(tenant_manager, "tenant-a")
  let tenant_b_metrics = TenantManager::get_aggregated_metrics(tenant_manager, "tenant-b")
  
  assert_true(tenant_a_metrics.total_spans > 0)
  assert_true(tenant_b_metrics.total_spans > 0)
  assert_true(tenant_a_metrics.total_spans != tenant_b_metrics.total_spans)
  
  // 测试租户数据删除
  let delete_result = TenantManager::delete_tenant_data(tenant_manager, "tenant-a", "trace-a-50")
  assert_true(delete_result.success)
  
  let updated_tenant_a_data = TenantManager::get_tenant_data(tenant_manager, "tenant-a")
  assert_eq(updated_tenant_a_data.length(), 100)  // 减少了一个
  
  // 验证其他租户数据不受影响
  let unchanged_tenant_b_data = TenantManager::get_tenant_data(tenant_manager, "tenant-b")
  assert_eq(unchanged_tenant_b_data.length(), 201)  // 不变
}

// 测试6: 遥测数据导出
test "遥测数据导出测试" {
  // 创建数据导出管理器
  let export_manager = ExportManager::new({
    max_export_size: 100 * 1024 * 1024,  // 100MB
    supported_formats: ["json", "csv", "parquet", "prometheus"],
    compression: true
  })
  
  // 创建测试数据
  let test_data = []
  for i in 0..=100 {
    test_data = test_data.push({
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      parent_span_id: if i > 0 { Some("span-" + (i - 1).to_string()) } else { None },
      service_name: "service-" + (i % 5).to_string(),
      operation_name: "operation-" + (i % 10).to_string(),
      start_time: 1640995200 + i * 10,
      end_time: 1640995200 + i * 10 + 50 + (i % 20),
      status: if i % 15 == 0 { "error" } else { "ok" },
      attributes: [
        ("http.method", "GET"),
        ("http.status_code", (200 + (i % 4) * 100).to_string()),
        ("user.id", "user-" + (i % 50).to_string())
      ],
      events: if i % 20 == 0 {
        [{
          name: "error",
          timestamp: 1640995200 + i * 10 + 25,
          attributes: [("error.message", "Timeout error")]
        }]
      } else {
        []
      },
      links: if i % 25 == 0 {
        [{
          trace_id: "linked-trace-" + i.to_string(),
          span_id: "linked-span-" + i.to_string()
        }]
      } else {
        []
      }
    })
  }
  
  // 测试JSON格式导出
  let json_export_result = ExportManager::export(
    export_manager,
    test_data,
    {
      format: "json",
      include_attributes: true,
      include_events: true,
      include_links: true,
      compression: false
    }
  )
  
  assert_true(json_export_result.success)
  assert_true(json_export_result.data_size > 0)
  assert_true(json_export_result.data.contains("\"trace_id\""))
  assert_true(json_export_result.data.contains("\"service_name\""))
  
  // 验证JSON导出数据完整性
  let json_parsed = JSON::parse(json_export_result.data)
  assert_true(json_parsed.is_array)
  assert_eq(json_parsed.length(), 101)
  
  // 测试CSV格式导出
  let csv_export_result = ExportManager::export(
    export_manager,
    test_data,
    {
      format: "csv",
      include_attributes: false,  // CSV不支持复杂属性
      include_events: false,
      include_links: false,
      compression: false
    }
  )
  
  assert_true(csv_export_result.success)
  assert_true(csv_export_result.data_size > 0)
  assert_true(csv_export_result.data.contains("trace_id,span_id"))
  
  // 验证CSV导出格式
  let csv_lines = csv_export_result.data.split("\n")
  assert_eq(csv_lines.length(), 102)  // 标题行 + 101数据行
  
  let headers = csv_lines[0].split(",")
  assert_true(headers.contains("trace_id"))
  assert_true(headers.contains("span_id"))
  assert_true(headers.contains("service_name"))
  assert_true(headers.contains("operation_name"))
  
  // 测试Parquet格式导出
  let parquet_export_result = ExportManager::export(
    export_manager,
    test_data,
    {
      format: "parquet",
      include_attributes: true,
      include_events: true,
      include_links: true,
      compression: true  // Parquet默认使用压缩
    }
  )
  
  assert_true(parquet_export_result.success)
  assert_true(parquet_export_result.data_size > 0)
  
  // 验证Parquet导出压缩效果
  let json_size = json_export_result.data_size
  let parquet_size = parquet_export_result.data_size
  assert_true(parquet_size < json_size)  // Parquet应该更小
  
  // 测试Prometheus格式导出（仅指标）
  let prometheus_export_result = ExportManager::export_metrics(
    export_manager,
    test_data,
    {
      format: "prometheus",
      metric_types: ["counter", "histogram", "gauge"],
      include_labels: true
    }
  )
  
  assert_true(prometheus_export_result.success)
  assert_true(prometheus_export_result.data.contains("# HELP"))
  assert_true(prometheus_export_result.data.contains("# TYPE"))
  assert_true(prometheus_export_result.data.contains("azimuth_spans_total"))
  
  // 测试导出过滤
  let filtered_export_result = ExportManager::export_with_filter(
    export_manager,
    test_data,
    {
      format: "json",
      time_range: {
        start: 1640995200,
        end: 1640995200 + 500
      },
      service_filter: ["service-1", "service-2"],
      status_filter: ["ok"],
      attribute_filter: [
        { key: "http.status_code", value: "200" }
      ]
    }
  )
  
  assert_true(filtered_export_result.success)
  
  // 验证过滤结果
  let filtered_data = JSON::parse(filtered_export_result.data)
  assert_true(filtered_data.length() < test_data.length())  // 过滤后数据应该更少
  
  // 检查过滤条件
  for item in filtered_data {
    assert_eq(item.status, "ok")  // 状态应为ok
    assert_true(item.service_name == "service-1" or item.service_name == "service-2")  // 服务名称应匹配
  }
  
  // 测试导出任务调度
  let scheduled_export = ExportManager::schedule_export(
    export_manager,
    {
      name: "daily_export",
      schedule: "0 2 * * *",  // 每天凌晨2点
      format: "parquet",
      compression: true,
      destination: {
        type: "s3",
        bucket: "telemetry-exports",
        prefix: "daily/"
      },
      filters: {
        time_range: "last_24_hours"
      }
    }
  )
  
  assert_true(scheduled_export.success)
  
  // 验证导出任务已创建
  let export_jobs = ExportManager::get_scheduled_exports(export_manager)
  assert_true(export_jobs.length() > 0)
  
  let daily_export_job = export_jobs.find(fn(j) { j.name == "daily_export" })
  assert_true(daily_export_job != None)
  
  match daily_export_job {
    Some(job) => {
      assert_eq(job.format, "parquet")
      assert_eq(job.schedule, "0 2 * * *")
      assert_eq(job.destination.type, "s3")
    }
    None => assert_true(false)
  }
  
  // 测试导出性能
  let start_time = Time::now()
  let performance_export = ExportManager::export(
    export_manager,
    test_data,
    {
      format: "parquet",
      include_attributes: true,
      include_events: true,
      include_links: true,
      compression: true
    }
  )
  let end_time = Time::now()
  
  let export_duration = end_time - start_time
  let throughput = test_data.length().to_float() / (export_duration / 1000.0)
  
  assert_true(performance_export.success)
  assert_true(throughput > 100.0)  // 每秒至少处理100条记录
}

// 测试7: 自定义指标聚合
test "自定义指标聚合测试" {
  // 创建自定义指标管理器
  let custom_metrics_manager = CustomMetricsManager::new({
    max_custom_metrics: 1000,
    aggregation_window: 300,  // 5分钟窗口
    default_aggregations: ["sum", "avg", "min", "max", "count"]
  })
  
  // 注册自定义指标
  let business_metric = CustomMetricsManager::register_metric(custom_metrics_manager, {
    name: "business.revenue",
    description: "Total revenue generated",
    unit: "currency",
    value_type: "float",
    tags: ["business", "revenue"],
    aggregations: ["sum", "avg", "min", "max"]
  })
  
  let user_engagement_metric = CustomMetricsManager::register_metric(custom_metrics_manager, {
    name: "user.engagement",
    description: "User engagement score",
    unit: "score",
    value_type: "float",
    tags: ["user", "engagement"],
    aggregations: ["avg", "p50", "p95", "p99"]
  })
  
  let error_severity_metric = CustomMetricsManager::register_metric(custom_metrics_manager, {
    name: "error.severity",
    description: "Error severity distribution",
    unit: "count",
    value_type: "int",
    tags: ["error", "severity"],
    aggregations: ["count", "sum"]
  })
  
  // 验证指标注册
  assert_true(business_metric.success)
  assert_true(user_engagement_metric.success)
  assert_true(error_severity_metric.success)
  
  // 添加自定义指标数据点
  let base_time = 1640995200
  
  // 业务收入数据
  for i in 0..=100 {
    CustomMetricsManager::add_data_point(custom_metrics_manager, {
      metric_name: "business.revenue",
      timestamp: base_time + i * 60,
      value: 100.0 + (i % 20) * 10.5 + (i % 5) * 2.25,
      attributes: [
        ("product.category", "electronics"),
        ("region", "us-west-" + (i % 3).to_string()),
        ("customer.segment", "premium")
      ]
    })
  }
  
  // 用户参与度数据
  for i in 0..=80 {
    CustomMetricsManager::add_data_point(custom_metrics_manager, {
      metric_name: "user.engagement",
      timestamp: base_time + i * 75,
      value: 0.5 + (i % 10) * 0.1 + (i % 3) * 0.05,
      attributes: [
        ("user.type", "premium"),
        ("feature", "dashboard"),
        ("session.duration", (300 + i * 10).to_string())
      ]
    })
  }
  
  // 错误严重性数据
  for i in 0..=50 {
    CustomMetricsManager::add_data_point(custom_metrics_manager, {
      metric_name: "error.severity",
      timestamp: base_time + i * 120,
      value: i % 5,  // 0-4的严重性级别
      attributes: [
        ("error.type", "timeout"),
        ("service", "api-service"),
        ("environment", "production")
      ]
    })
  }
  
  // 执行指标聚合
  let aggregation_result = CustomMetricsManager::aggregate_metrics(
    custom_metrics_manager,
    base_time,
    base_time + 6000,  // 100分钟范围
    {
      group_by: ["product.category", "region"],
      aggregations: ["sum", "avg", "min", "max", "p95"]
    }
  )
  
  // 验证聚合结果
  assert_true(aggregation_result.success)
  assert_true(aggregation_result.aggregated_metrics.length() > 0)
  
  // 检查业务收入聚合
  let revenue_aggregations = aggregation_result.aggregated_metrics.filter(fn(m) { 
    m.metric_name == "business.revenue" 
  })
  assert_true(revenue_aggregations.length() > 0)
  
  let revenue_by_region = revenue_aggregations.group_by(fn(m) { 
    m.attributes.find(fn(a) { a.0 == "region" }).unwrap().1 
  })
  
  // 验证每个区域都有聚合数据
  assert_true(revenue_by_region.contains_key("us-west-0"))
  assert_true(revenue_by_region.contains_key("us-west-1"))
  assert_true(revenue_by_region.contains_key("us-west-2"))
  
  // 检查聚合值的正确性
  let us_west_0_revenue = revenue_by_region.get("us-west-0").unwrap()
  let revenue_sum = us_west_0_revenue.aggregations.find(fn(a) { a.type == "sum" }).unwrap()
  let revenue_avg = us_west_0_revenue.aggregations.find(fn(a) { a.type == "avg" }).unwrap()
  let revenue_min = us_west_0_revenue.aggregations.find(fn(a) { a.type == "min" }).unwrap()
  let revenue_max = us_west_0_revenue.aggregations.find(fn(a) { a.type == "max" }).unwrap()
  
  assert_true(revenue_sum.value > 0)
  assert_true(revenue_avg.value > 0)
  assert_true(revenue_min.value <= revenue_avg.value)
  assert_true(revenue_max.value >= revenue_avg.value)
  
  // 测试自定义聚合函数
  CustomMetricsManager::register_custom_aggregation(custom_metrics_manager, {
    name: "revenue_growth_rate",
    description: "Calculate revenue growth rate",
    function: fn(values) {
      if values.length() < 2 {
        0.0
      } else {
        let first = values[0]
        let last = values[values.length() - 1]
        (last - first) / first * 100.0
      }
    }
  })
  
  // 应用自定义聚合
  let custom_aggregation_result = CustomMetricsManager::apply_custom_aggregation(
    custom_metrics_manager,
    "business.revenue",
    "revenue_growth_rate",
    base_time,
    base_time + 6000
  )
  
  assert_true(custom_aggregation_result.success)
  assert_true(custom_aggregation_result.value >= 0.0)  // 增长率应为正数或零
  
  // 测试指标降采样
  let downsampling_result = CustomMetricsManager::downsample_metrics(
    custom_metrics_manager,
    {
      input_interval: 60,    // 1分钟原始数据
      output_interval: 300,  // 5分钟降采样数据
      aggregation: "avg"     // 使用平均值
    }
  )
  
  assert_true(downsampling_result.success)
  assert_true(downsampling_result.downsampled_points.length() > 0)
  
  // 验证降采样数据点数量约为原始数据的1/5
  let original_points = CustomMetricsManager::get_data_points_count(
    custom_metrics_manager, 
    base_time, 
    base_time + 6000
  )
  let expected_downsampled = original_points / 5
  assert_true(
    downsampling_result.downsampled_points.length() >= expected_downsampled - 2 and 
    downsampling_result.downsampled_points.length() <= expected_downsampled + 2
  )
  
  // 测试指标异常检测
  let anomaly_detection_result = CustomMetricsManager::detect_anomalies(
    custom_metrics_manager,
    "business.revenue",
    {
      algorithm: "statistical",
      parameters: {
        threshold: 2.0,  // 2个标准差
        window_size: 20
      }
    }
  )
  
  assert_true(anomaly_detection_result.success)
  
  // 如果有异常，验证异常数据点
  if anomaly_detection_result.anomalies.length() > 0 {
    let first_anomaly = anomaly_detection_result.anomalies[0]
    assert_true(first_anomaly.deviation > 2.0)
    assert_true(first_anomaly.timestamp >= base_time)
  }
  
  // 测试指标预测
  let prediction_result = CustomMetricsManager::predict_metrics(
    custom_metrics_manager,
    "business.revenue",
    {
      algorithm: "linear_regression",
      parameters: {
        prediction_window: 300,  // 预测未来5分钟
        training_window: 1800    // 使用过去30分钟数据训练
      }
    }
  )
  
  assert_true(prediction_result.success)
  assert_true(prediction_result.predictions.length() > 0)
  
  // 验证预测时间戳在未来
  let first_prediction = prediction_result.predictions[0]
  assert_true(first_prediction.timestamp > base_time + 6000)
  assert_true(first_prediction.value >= 0.0)
}

// 测试8: 遥测数据安全加密
test "遥测数据安全加密测试" {
  // 创建安全管理器
  let security_manager = SecurityManager::new({
    encryption_algorithm: "AES-256-GCM",
    key_rotation_interval: 86400 * 7,  // 7天轮换一次密钥
    key_derivation: "PBKDF2",
    key_derivation_iterations: 100000
  })
  
  // 初始化安全管理器
  let init_result = SecurityManager::initialize(security_manager, "master-key-password")
  assert_true(init_result.success)
  
  // 创建加密策略
  let encryption_policies = [
    {
      name: "pii_encryption",
      description: "Encrypt personally identifiable information",
      field_patterns: [
        "user.id",
        "user.email",
        "user.name",
        "*.ssn",
        "*.credit_card"
      ],
      key_id: "pii-key-1",
      encryption_level: "high"
    },
    {
      name: "sensitive_business_data",
      description: "Encrypt sensitive business metrics",
      field_patterns: [
        "revenue.*",
        "profit.*",
        "*.secret",
        "*.token"
      ],
      key_id: "business-key-1",
      encryption_level: "medium"
    }
  ]
  
  // 注册加密策略
  for policy in encryption_policies {
    let policy_result = SecurityManager::add_encryption_policy(security_manager, policy)
    assert_true(policy_result.success)
  }
  
  // 创建测试数据（包含敏感信息）
  let sensitive_telemetry_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    service_name: "payment-service",
    operation_name: "process_payment",
    timestamp: 1640995200,
    attributes: [
      ("user.id", "user-98765"),
      ("user.email", "customer@example.com"),
      ("payment.amount", "99.99"),
      ("payment.currency", "USD"),
      ("payment.method", "credit_card"),
      ("payment.last_four", "1234"),
      ("payment.token", "tok_1A2B3C4D5E6F7G8H"),
      ("revenue.total", "9999.99"),
      ("business.secret", "confidential-business-data")
    ],
    events: [
      {
        name: "payment_processed",
        timestamp: 1640995205,
        attributes: [
          ("user.id", "user-98765"),
          ("transaction.id", "txn-11111")
        ]
      }
    ]
  }
  
  // 加密敏感数据
  let encryption_result = SecurityManager::encrypt_sensitive_data(
    security_manager,
    sensitive_telemetry_data
  )
  
  assert_true(encryption_result.success)
  
  // 验证敏感字段已加密
  let encrypted_data = encryption_result.encrypted_data
  
  // 检查用户ID是否已加密
  let user_id_attr = encrypted_data.attributes.find(fn(a) { a.0 == "user.id" })
  assert_true(user_id_attr != None)
  
  match user_id_attr {
    Some(attr) => {
      assert_false(attr.1.contains("user-98765"))  // 原始值不应存在
      assert_true(attr.1.length() > 20)           // 加密后值应该更长
    }
    None => assert_true(false)
  }
  
  // 检查用户邮箱是否已加密
  let user_email_attr = encrypted_data.attributes.find(fn(a) { a.0 == "user.email" })
  assert_true(user_email_attr != None)
  
  match user_email_attr {
    Some(attr) => {
      assert_false(attr.1.contains("customer@example.com"))
      assert_true(attr.1.length() > 20)
    }
    None => assert_true(false)
  }
  
  // 检查非敏感字段未加密
  let payment_amount_attr = encrypted_data.attributes.find(fn(a) { a.0 == "payment.amount" })
  assert_true(payment_amount_attr != None)
  
  match payment_amount_attr {
    Some(attr) => {
      assert_eq(attr.1, "99.99")  // 应保持原始值
    }
    None => assert_true(false)
  }
  
  // 检查事件中的敏感数据是否已加密
  let event = encrypted_data.events[0]
  let event_user_id_attr = event.attributes.find(fn(a) { a.0 == "user.id" })
  assert_true(event_user_id_attr != None)
  
  match event_user_id_attr {
    Some(attr) => {
      assert_false(attr.1.contains("user-98765"))
    }
    None => assert_true(false)
  }
  
  // 解密数据
  let decryption_result = SecurityManager::decrypt_sensitive_data(
    security_manager,
    encrypted_data
  )
  
  assert_true(decryption_result.success)
  
  // 验证解密后的数据与原始数据一致
  let decrypted_data = decryption_result.decrypted_data
  assert_eq(decrypted_data.trace_id, sensitive_telemetry_data.trace_id)
  assert_eq(decrypted_data.span_id, sensitive_telemetry_data.span_id)
  
  // 验证敏感字段已正确解密
  let decrypted_user_id = decrypted_data.attributes.find(fn(a) { a.0 == "user.id" })
  assert_true(decrypted_user_id != None)
  
  match decrypted_user_id {
    Some(attr) => {
      assert_eq(attr.1, "user-98765")
    }
    None => assert_true(false)
  }
  
  // 测试密钥轮换
  let key_rotation_result = SecurityManager::rotate_encryption_keys(security_manager)
  assert_true(key_rotation_result.success)
  
  // 验证新密钥已生成
  let key_status = SecurityManager::get_key_status(security_manager)
  assert_true(key_status.active_keys.length() > 0)
  assert_true(key_status.rotated_keys.length() > 0)
  
  // 使用新密钥加密数据
  let new_encryption_result = SecurityManager::encrypt_sensitive_data(
    security_manager,
    sensitive_telemetry_data
  )
  
  assert_true(new_encryption_result.success)
  
  // 验证旧密钥仍能解密之前的数据
  let old_decryption_result = SecurityManager::decrypt_sensitive_data(
    security_manager,
    encrypted_data
  )
  
  assert_true(old_decryption_result.success)
  
  // 测试数据脱敏
  let masking_result = SecurityManager::mask_sensitive_data(
    security_manager,
    sensitive_telemetry_data,
    {
      masking_rules: [
        {
          field_pattern: "user.email",
          masking_type: "partial",
          masking_params: { visible_chars: 2, mask_char: "*" }
        },
        {
          field_pattern: "payment.last_four",
          masking_type: "hash",
          masking_params: { algorithm: "sha256" }
        }
      ]
    }
  )
  
  assert_true(masking_result.success)
  
  // 验证脱敏结果
  let masked_data = masking_result.masked_data
  
  let masked_email = masked_data.attributes.find(fn(a) { a.0 == "user.email" })
  assert_true(masked_email != None)
  
  match masked_email {
    Some(attr) => {
      assert_eq(attr.1, "cu***************")  // 只显示前2个字符
    }
    None => assert_true(false)
  }
  
  let masked_last_four = masked_data.attributes.find(fn(a) { a.0 == "payment.last_four" })
  assert_true(masked_last_four != None)
  
  match masked_last_four {
    Some(attr) => {
      assert_false(attr.1.contains("1234"))  // 原始值不应存在
      assert_true(attr.1.length() == 64)     // SHA256哈希长度
    }
    None => assert_true(false)
  }
  
  // 测试访问控制
  let access_policies = [
    {
      name: "pii_access_policy",
      description: "Control access to PII data",
      permissions: ["read_pii", "write_pii"],
      roles: ["customer_service", "analyst"],
      conditions: [
        { field: "data_classification", operator: "equals", value: "pii" },
        { field: "user.role", operator: "in", value: ["customer_service", "analyst"] }
      ]
    }
  ]
  
  // 注册访问策略
  for policy in access_policies {
    let policy_result = SecurityManager::add_access_policy(security_manager, policy)
    assert_true(policy_result.success)
  }
  
  // 测试访问权限检查
  let access_check = SecurityManager::check_access_permission(
    security_manager,
    {
      user_id: "user-123",
      role: "customer_service",
      permissions: ["read_pii"]
    },
    {
      data_type: "user.email",
      classification: "pii"
    }
  )
  
  assert_true(access_check.granted)
  
  // 测试无权限访问
  let denied_access = SecurityManager::check_access_permission(
    security_manager,
    {
      user_id: "user-456",
      role: "intern",
      permissions: ["read_basic"]
    },
    {
      data_type: "user.email",
      classification: "pii"
    }
  )
  
  assert_false(denied_access.granted)
  
  // 测试审计日志
  let audit_log = SecurityManager::get_access_audit_log(security_manager)
  assert_true(audit_log.length() >= 2)  // 至少记录两次访问检查
  
  // 验证审计日志内容
  let granted_log = audit_log.find(fn(entry) { entry.user_id == "user-123" })
  assert_true(granted_log != None)
  
  match granted_log {
    Some(log) => {
      assert_eq(log.data_type, "user.email")
      assert_eq(log.result, "granted")
      assert_true(log.timestamp > 0)
    }
    None => assert_true(false)
  }
}

// 测试9: 遥测系统健康检查
test "遥测系统健康检查测试" {
  // 创建健康检查管理器
  let health_check_manager = HealthCheckManager::new({
    check_interval: 30000,      // 30秒检查间隔
    timeout: 5000,              // 5秒超时
    failure_threshold: 3,       // 连续3次失败标记为不健康
    recovery_threshold: 2       // 连续2次成功标记为恢复
  })
  
  // 注册健康检查项
  let database_check = HealthCheckManager::register_check(health_check_manager, {
    name: "database_connectivity",
    description: "Check database connectivity and performance",
    check_type: "connectivity",
    target: "postgresql://telemetry-db:5432/azimuth",
    parameters: {
      query_timeout: 2000,
      expected_response_time: 100
    },
    severity: "critical"
  })
  
  let storage_check = HealthCheckManager::register_check(health_check_manager, {
    name: "storage_availability",
    description: "Check storage system availability and space",
    check_type: "resource",
    target: "file:///var/lib/azimuth/telemetry",
    parameters: {
      min_free_space_gb: 10,
      max_io_latency_ms: 100
    },
    severity: "warning"
  })
  
  let ingestion_check = HealthCheckManager::register_check(health_check_manager, {
    name: "telemetry_ingestion_rate",
    description: "Check telemetry data ingestion rate",
    check_type: "metric",
    target: "internal",
    parameters: {
      min_rate_per_second: 100,
      max_latency_ms: 500
    },
    severity: "warning"
  })
  
  let memory_check = HealthCheckManager::register_check(health_check_manager, {
    name: "memory_usage",
    description: "Check system memory usage",
    check_type: "resource",
    target: "system",
    parameters: {
      max_usage_percentage: 85,
      check_gc_pressure: true
    },
    severity: "warning"
  })
  
  // 验证健康检查注册
  assert_true(database_check.success)
  assert_true(storage_check.success)
  assert_true(ingestion_check.success)
  assert_true(memory_check.success)
  
  // 模拟健康检查执行
  let database_result = HealthCheckManager::execute_check(health_check_manager, "database_connectivity")
  assert_true(database_result.success)
  assert_true(database_result.response_time < 5000)  // 应在超时内完成
  
  let storage_result = HealthCheckManager::execute_check(health_check_manager, "storage_availability")
  assert_true(storage_result.success)
  assert_true(storage_result.details.free_space_gb > 0)
  
  let ingestion_result = HealthCheckManager::execute_check(health_check_manager, "telemetry_ingestion_rate")
  assert_true(ingestion_result.success)
  assert_true(ingestion_result.metrics.current_rate >= 0)
  
  let memory_result = HealthCheckManager::execute_check(health_check_manager, "memory_usage")
  assert_true(memory_result.success)
  assert_true(memory_result.metrics.usage_percentage >= 0 and memory_result.metrics.usage_percentage <= 100)
  
  // 执行所有健康检查
  let all_checks_result = HealthCheckManager::execute_all_checks(health_check_manager)
  assert_true(all_checks_result.success)
  assert_eq(all_checks_result.checked_count, 4)
  assert_eq(all_checks_result.passed_count, 4)
  assert_eq(all_checks_result.failed_count, 0)
  
  // 模拟一个检查失败
  HealthCheckManager::simulate_check_failure(health_check_manager, "database_connectivity", {
    error_type: "connection_timeout",
    error_message: "Connection timeout after 5000ms",
    retry_after: 30000
  })
  
  // 再次执行检查
  let failed_checks_result = HealthCheckManager::execute_all_checks(health_check_manager)
  assert_true(failed_checks_result.success)
  assert_eq(failed_checks_result.checked_count, 4)
  assert_eq(failed_checks_result.passed_count, 3)
  assert_eq(failed_checks_result.failed_count, 1)
  
  // 验证失败检查的详细信息
  let failed_check = failed_checks_result.results.find(fn(r) { r.check_name == "database_connectivity" })
  assert_true(failed_check != None)
  
  match failed_check {
    Some(check) => {
      assert_false(check.healthy)
      assert_eq(check.error_type, "connection_timeout")
      assert_true(check.error_message.contains("timeout"))
    }
    None => assert_true(false)
  }
  
  // 测试连续失败阈值
  for i in 0..=2 {
    HealthCheckManager::simulate_check_failure(health_check_manager, "storage_availability", {
      error_type: "insufficient_space",
      error_message: "Only 5GB free space available",
      retry_after: 60000
    })
    
    let check_result = HealthCheckManager::execute_check(health_check_manager, "storage_availability")
    assert_false(check_result.healthy)
  }
  
  // 验证系统整体健康状态
  let system_health = HealthCheckManager::get_system_health(health_check_manager)
  assert_eq(system_health.status, "degraded")  // 有检查失败，系统状态降级
  
  // 验证健康状态详情
  assert_true(system_health.healthy_checks.length() > 0)
  assert_true(system_health.unhealthy_checks.length() > 0)
  assert_true(system_health.overall_score < 100.0)
  assert_true(system_health.overall_score > 0.0)
  
  // 测试健康检查恢复
  HealthCheckManager::simulate_check_recovery(health_check_manager, "database_connectivity")
  
  let recovery_result = HealthCheckManager::execute_check(health_check_manager, "database_connectivity")
  assert_true(recovery_result.healthy)
  
  // 再次检查系统健康状态
  let recovered_system_health = HealthCheckManager::get_system_health(health_check_manager)
  assert_true(recovered_system_health.overall_score > system_health.overall_score)
  
  // 测试健康检查通知
  let notification_manager = HealthCheckNotificationManager::new()
  
  HealthCheckNotificationManager::add_notification_channel(notification_manager, {
    name: "email_alerts",
    type: "email",
    config: {
      smtp_server: "smtp.example.com",
      from: "alerts@azimuth.com",
      to: ["ops-team@example.com"],
      template: "health_check_alert"
    },
    filters: {
      severity: ["critical", "warning"],
      status_change: ["healthy_to_unhealthy", "unhealthy_to_healthy"]
    }
  })
  
  HealthCheckNotificationManager::add_notification_channel(notification_manager, {
    name: "slack_webhook",
    type: "webhook",
    config: {
      url: "https://hooks.slack.com/services/XXXXX/YYYYY/ZZZZZ",
      channel: "#azimuth-alerts",
      username: "AzimuthBot"
    },
    filters: {
      severity: ["critical"],
      status_change: ["healthy_to_unhealthy"]
    }
  })
  
  // 模拟关键检查失败触发通知
  HealthCheckManager::simulate_check_failure(health_check_manager, "memory_usage", {
    error_type: "high_memory_usage",
    error_message: "Memory usage at 92%",
    retry_after: 30000
  })
  
  let critical_check_result = HealthCheckManager::execute_check(health_check_manager, "memory_usage")
  assert_false(critical_check_result.healthy)
  
  // 检查通知是否已发送
  let notification_history = HealthCheckNotificationManager::get_notification_history(notification_manager)
  assert_true(notification_history.length() > 0)
  
  let memory_failure_notification = notification_history.find(fn(n) { 
    n.check_name == "memory_usage" and n.event_type == "healthy_to_unhealthy" 
  })
  assert_true(memory_failure_notification != None)
  
  match memory_failure_notification {
    Some(notification) => {
      assert_true(notification.channels.contains("email_alerts"))
      assert_true(notification.channels.contains("slack_webhook"))
      assert_true(notification.sent)
    }
    None => assert_true(false)
  }
  
  // 测试健康检查报告生成
  let health_report = HealthCheckManager::generate_health_report(health_check_manager, {
    format: "json",
    include_history: true,
    include_metrics: true,
    time_range: 86400  // 过去24小时
  })
  
  assert_true(health_report.success)
  assert_true(health_report.report_data.length() > 0)
  
  // 验证报告内容
  let report_data = JSON::parse(health_report.report_data)
  assert_true(report_data.contains_key("system_health"))
  assert_true(report_data.contains_key("check_results"))
  assert_true(report_data.contains_key("historical_data"))
  
  // 测试健康检查自动化响应
  let automation_manager = HealthCheckAutomationManager::new()
  
  HealthCheckAutomationManager::add_automation_rule(automation_manager, {
    name: "auto_restart_on_failure",
    description: "Automatically restart service on critical failure",
    trigger: {
      check_name: "database_connectivity",
      status: "unhealthy",
      consecutive_failures: 2
    },
    actions: [
      {
        type: "restart_service",
        parameters: {
          service_name: "telemetry-ingestion",
          graceful_shutdown_timeout: 30000
        }
      },
      {
        type: "send_notification",
        parameters: {
          channel: "ops_team",
          message: "Service telemetry-ingestion restarted due to database connectivity issues"
        }
      }
    ]
  })
  
  // 触发自动化规则
  HealthCheckManager::simulate_check_failure(health_check_manager, "database_connectivity", {
    error_type: "connection_refused",
    error_message: "Connection refused by database server",
    retry_after: 30000
  })
  
  let automation_result = HealthCheckAutomationManager::evaluate_and_execute_rules(
    automation_manager,
    health_check_manager
  )
  
  assert_true(automation_result.success)
  assert_true(automation_result.executed_rules.length() > 0)
  
  // 验证自动化执行结果
  let restart_rule = automation_result.executed_rules.find(fn(r) { r.rule_name == "auto_restart_on_failure" })
  assert_true(restart_rule != None)
  
  match restart_rule {
    Some(rule) => {
      assert_true(rule.success)
      assert_true(rule.executed_actions.length() > 0)
      
      let restart_action = rule.executed_actions.find(fn(a) { a.type == "restart_service" })
      assert_true(restart_action != None)
    }
    None => assert_true(false)
  }
}

// 测试10: 遥测数据版本兼容性
test "遥测数据版本兼容性测试" {
  // 创建版本兼容性管理器
  let compatibility_manager = VersionCompatibilityManager::new({
    current_version: "2.1.0",
    supported_versions: ["1.0.0", "1.1.0", "1.2.0", "2.0.0", "2.1.0"],
    default_compatibility_mode: "backward"
  })
  
  // 注册版本兼容性规则
  let compatibility_rules = [
    {
      from_version: "1.0.0",
      to_version: "2.1.0",
      transformations: [
        {
          field: "trace_id",
          operation: "rename",
          target: "traceId"
        },
        {
          field: "span_id",
          operation: "rename",
          target: "spanId"
        },
        {
          field: "parent_span_id",
          operation: "rename",
          target: "parentSpanId"
        },
        {
          field: "start_time",
          operation: "rename",
          target: "startTimeUnixNano"
        },
        {
          field: "end_time",
          operation: "rename",
          target: "endTimeUnixNano"
        },
        {
          field: "attributes",
          operation: "transform",
          transformer: "convert_kv_list_to_map"
        }
      ]
    },
    {
      from_version: "1.1.0",
      to_version: "2.1.0",
      transformations: [
        {
          field: "trace_id",
          operation: "rename",
          target: "traceId"
        },
        {
          field: "span_id",
          operation: "rename",
          target: "spanId"
        },
        {
          field: "status",
          operation: "map_values",
          mapping: {
            "ok": "STATUS_CODE_OK",
            "error": "STATUS_CODE_ERROR"
          }
        }
      ]
    },
    {
      from_version: "2.0.0",
      to_version: "2.1.0",
      transformations: [
        {
          field: "library_resource",
          operation: "add_field",
          default_value: {
            "service.name": "unknown",
            "service.version": "unknown"
          }
        }
      ]
    }
  ]
  
  // 注册兼容性规则
  for rule in compatibility_rules {
    let rule_result = VersionCompatibilityManager::add_compatibility_rule(
      compatibility_manager,
      rule
    )
    assert_true(rule_result.success)
  }
  
  // 创建不同版本的测试数据
  
  // 版本1.0.0的数据格式
  let v1_0_0_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: "span-11111",
    service_name: "payment-service",
    operation_name: "process_payment",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    attributes: [
      ("user.id", "user-98765"),
      ("payment.amount", "99.99")
    ],
    events: [
      {
        name: "payment_processed",
        timestamp: 1640995225,
        attributes: [
          ("transaction.id", "txn-22222")
        ]
      }
    ]
  }
  
  // 版本1.1.0的数据格式
  let v1_1_0_data = {
    trace_id: "trace-23456",
    span_id: "span-78901",
    service_name: "user-service",
    operation_name: "authenticate_user",
    start_time: 1640995300,
    end_time: 1640995320,
    status: "error",
    attributes: [
      ("user.id", "user-54321"),
      ("auth.method", "oauth2")
    ],
    error: {
      message: "Invalid credentials",
      code: 401
    }
  }
  
  // 版本2.0.0的数据格式
  let v2_0_0_data = {
    traceId: "trace-34567",
    spanId: "span-89012",
    parentSpanId: "span-33333",
    name: "database_query",
    kind: "CLIENT",
    startTimeUnixNano: 1640995400000000,
    endTimeUnixNano: 1640995450000000,
    status: {
      code: "STATUS_CODE_OK"
    },
    attributes: {
      "db.system": "postgresql",
      "db.statement": "SELECT * FROM users"
    },
    resource: {
      "service.name": "api-service",
      "service.version": "1.2.3"
    }
  }
  
  // 测试从1.0.0升级到2.1.0
  let v1_to_v2_1_result = VersionCompatibilityManager::transform_data(
    compatibility_manager,
    v1_0_0_data,
    {
      from_version: "1.0.0",
      to_version: "2.1.0"
    }
  )
  
  assert_true(v1_to_v2_1_result.success)
  
  // 验证转换后的数据结构
  let transformed_v1_data = v1_to_v2_1_result.transformed_data
  
  // 检查字段重命名
  assert_true(transformed_v1_data.contains_key("traceId"))
  assert_true(transformed_v1_data.contains_key("spanId"))
  assert_true(transformed_v1_data.contains_key("parentSpanId"))
  assert_true(transformed_v1_data.contains_key("startTimeUnixNano"))
  assert_true(transformed_v1_data.contains_key("endTimeUnixNano"))
  
  // 检查旧字段不存在
  assert_false(transformed_v1_data.contains_key("trace_id"))
  assert_false(transformed_v1_data.contains_key("span_id"))
  
  // 检查属性转换
  assert_true(transformed_v1_data.contains_key("attributes"))
  let attributes = transformed_v1_data.get("attributes")
  assert_true(attributes.contains_key("user.id"))
  assert_true(attributes.contains_key("payment.amount"))
  
  // 检查时间格式转换（秒到纳秒）
  assert_eq(transformed_v1_data.get("startTimeUnixNano"), 1640995200000000)
  assert_eq(transformed_v1_data.get("endTimeUnixNano"), 1640995250000000)
  
  // 测试从1.1.0升级到2.1.0
  let v1_1_to_v2_1_result = VersionCompatibilityManager::transform_data(
    compatibility_manager,
    v1_1_0_data,
    {
      from_version: "1.1.0",
      to_version: "2.1.0"
    }
  )
  
  assert_true(v1_1_to_v2_1_result.success)
  
  // 验证状态码映射
  let transformed_v1_1_data = v1_1_to_v2_1_result.transformed_data
  assert_eq(transformed_v1_1_data.get("status"), "STATUS_CODE_ERROR")
  
  // 测试从2.0.0升级到2.1.0
  let v2_0_to_v2_1_result = VersionCompatibilityManager::transform_data(
    compatibility_manager,
    v2_0_0_data,
    {
      from_version: "2.0.0",
      to_version: "2.1.0"
    }
  )
  
  assert_true(v2_0_to_v2_1_result.success)
  
  // 验证资源字段添加
  let transformed_v2_0_data = v2_0_to_v2_1_result.transformed_data
  assert_true(transformed_v2_0_data.contains_key("library_resource"))
  
  // 测试向下兼容（从2.1.0降级到1.0.0）
  let v2_1_to_v1_0_result = VersionCompatibilityManager::transform_data(
    compatibility_manager,
    transformed_v1_data,
    {
      from_version: "2.1.0",
      to_version: "1.0.0",
      compatibility_mode: "forward"
    }
  )
  
  assert_true(v2_1_to_v1_0_result.success)
  
  // 验证降级后的数据结构
  let downgraded_data = v2_1_to_v1_0_result.transformed_data
  
  // 检查字段名称恢复
  assert_true(downgraded_data.contains_key("trace_id"))
  assert_true(downgraded_data.contains_key("span_id"))
  assert_true(downgraded_data.contains_key("start_time"))
  assert_true(downgraded_data.contains_key("end_time"))
  
  // 检查新字段不存在
  assert_false(downgraded_data.contains_key("traceId"))
  assert_false(downgraded_data.contains_key("spanId"))
  
  // 测试批量数据转换
  let batch_data = [
    { data: v1_0_0_data, version: "1.0.0" },
    { data: v1_1_0_data, version: "1.1.0" },
    { data: v2_0_0_data, version: "2.0.0" }
  ]
  
  let batch_transform_result = VersionCompatibilityManager::transform_batch(
    compatibility_manager,
    batch_data,
    {
      target_version: "2.1.0",
      error_handling: "skip_invalid"
    }
  )
  
  assert_true(batch_transform_result.success)
  assert_eq(batch_transform_result.transformed_count, 3)
  assert_eq(batch_transform_result.failed_count, 0)
  
  // 验证批量转换结果
  let transformed_batch = batch_transform_result.transformed_data
  assert_eq(transformed_batch.length(), 3)
  
  // 检查所有数据都已转换为目标版本
  for item in transformed_batch {
    assert_true(item.contains_key("traceId"))
    assert_true(item.contains_key("spanId"))
  }
  
  // 测试版本检测
  let version_detection = VersionCompatibilityManager::detect_data_version(
    compatibility_manager,
    v1_0_0_data
  )
  
  assert_true(version_detection.success)
  assert_eq(version_detection.detected_version, "1.0.0")
  assert_true(version_detection.confidence > 0.8)
  
  // 测试兼容性检查
  let compatibility_check = VersionCompatibilityManager::check_compatibility(
    compatibility_manager,
    {
      from_version: "1.0.0",
      to_version: "2.1.0"
    }
  )
  
  assert_true(compatibility_check.compatible)
  assert_true(compatibility_check.transformations_required)
  assert_true(compatibility_check.transformation_path.length() > 0)
  
  // 测试不兼容版本
  let incompatible_check = VersionCompatibilityManager::check_compatibility(
    compatibility_manager,
    {
      from_version: "0.9.0",
      to_version: "2.1.0"
    }
  )
  
  assert_false(incompatible_check.compatible)
  assert_true(incompatible_check.reason.contains("not supported"))
  
  // 测试版本迁移计划
  let migration_plan = VersionCompatibilityManager::create_migration_plan(
    compatibility_manager,
    {
      current_version: "1.0.0",
      target_version: "2.1.0",
      data_volume_estimate: 1000000,  // 100万条记录
      acceptable_downtime: 3600       // 1小时
    }
  )
  
  assert_true(migration_plan.feasible)
  assert_true(migration_plan.steps.length() > 0)
  assert_true(migration_plan.estimated_duration <= 3600)
  
  // 验证迁移步骤
  let first_step = migration_plan.steps[0]
  assert_eq(first_step.operation, "backup_data")
  assert_true(first_step.estimated_duration > 0)
  
  let last_step = migration_plan.steps[migration_plan.steps.length() - 1]
  assert_eq(last_step.operation, "validate_migration")
  
  // 测试版本兼容性报告
  let compatibility_report = VersionCompatibilityManager::generate_compatibility_report(
    compatibility_manager,
    {
      include_all_versions: true,
      include_transformation_details: true,
      format: "json"
    }
  )
  
  assert_true(compatibility_report.success)
  assert_true(compatibility_report.report_data.length() > 0)
  
  // 验证报告内容
  let report_data = JSON::parse(compatibility_report.report_data)
  assert_true(report_data.contains_key("current_version"))
  assert_true(report_data.contains_key("supported_versions"))
  assert_true(report_data.contains_key("compatibility_matrix"))
  assert_true(report_data.contains_key("transformation_rules"))
}