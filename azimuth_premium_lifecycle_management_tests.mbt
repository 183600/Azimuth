// Azimuth 遥测数据生命周期管理测试用例
// 专注于遥测数据的生命周期管理功能，包括数据保留、归档和清理

// 测试1: 数据保留策略管理
test "数据保留策略管理测试" {
  // 创建数据生命周期管理器
  let lifecycle_manager = TelemetryDataLifecycleManager::new()
  
  // 配置数据存储层
  LifecycleManager::configure_storage_layers(lifecycle_manager, [
    {
      name: "hot_storage",
      type: "elasticsearch",
      retention_days: 7,
      config: {
        hosts: ["localhost:9200"],
        index_pattern: "telemetry-hot-{yyyy.MM.dd}",
        shards: 3,
        replicas: 1
      }
    },
    {
      name: "warm_storage",
      type: "s3",
      retention_days: 30,
      config: {
        bucket: "telemetry-warm-data",
        prefix: "telemetry/warm/",
        compression: "gzip",
        storage_class: "standard_ia"
      }
    },
    {
      name: "cold_storage",
      type: "glacier",
      retention_days: 365,
      config: {
        vault: "telemetry-cold-vault",
        retrieval_time: "12_hours",
        encryption: "aws_kms"
      }
    }
  ])
  
  // 创建服务级别保留策略
  LifecycleManager::create_retention_policy(lifecycle_manager, {
    name: "critical_services_policy",
    description: "关键服务数据保留策略",
    services: ["payment.service", "auth.service", "user.service"],
    rules: [
      {
        storage_layer: "hot_storage",
        retention_days: 14,
        conditions: [
          { field: "status", operator: "equals", value: "error" },
          { field: "duration", operator: "greater_than", value: 1000 }
        ]
      },
      {
        storage_layer: "warm_storage",
        retention_days: 90,
        conditions: []
      },
      {
        storage_layer: "cold_storage",
        retention_days: 2555,  // 7年
        conditions: []
      }
    ]
  })
  
  // 创建常规服务保留策略
  LifecycleManager::create_retention_policy(lifecycle_manager, {
    name: "standard_services_policy",
    description: "标准服务数据保留策略",
    services: ["logging.service", "notification.service"],
    rules: [
      {
        storage_layer: "hot_storage",
        retention_days: 7,
        conditions: []
      },
      {
        storage_layer: "warm_storage",
        retention_days: 30,
        conditions: []
      },
      {
        storage_layer: "cold_storage",
        retention_days: 365,
        conditions: []
      }
    ]
  })
  
  // 创建测试遥测数据
  let test_data = []
  let base_time = Time::now() - (30 * 24 * 60 * 60 * 1000)  // 30天前
  
  for i in 0..=100 {
    let timestamp = base_time + i * 24 * 60 * 60 * 1000  // 每天一个数据点
    let service_type = if i % 3 == 0 { "payment.service" } else { "logging.service" }
    let status = if i % 10 == 0 { "error" } else { "ok" }
    let duration = if i % 15 == 0 { 1500 } else { 100 }
    
    let telemetry_record = {
      trace_id: "trace-lifecycle-" + i.to_string(),
      span_id: "span-lifecycle-" + i.to_string(),
      timestamp: timestamp,
      service_name: service_type,
      operation_name: "operation-" + (i % 10).to_string(),
      duration: duration,
      status: status,
      storage_location: "hot_storage",
      created_at: timestamp,
      last_accessed: timestamp,
      access_count: if i < 10 { i * 10 } else { 0 },
      data_size: 1024 + (i % 500) * 10
    }
    test_data = test_data.push(telemetry_record)
  }
  
  // 应用保留策略
  let policy_application_result = LifecycleManager::apply_retention_policies(lifecycle_manager, test_data)
  
  // 验证策略应用结果
  assert_true(policy_application_result.total_processed > 0)
  assert_true(policy_application_result.migrated_records > 0)
  
  // 检查关键服务数据的迁移
  let critical_service_records = test_data.filter(fn(r) { r.service_name == "payment.service" })
  let critical_migrations = policy_application_result.migration_details.filter(fn(m) { 
    m.service == "payment.service" 
  })
  
  assert_true(critical_migrations.length() > 0)
  
  // 验证错误数据的延长保留
  let error_records = test_data.filter(fn(r) { r.status == "error" and r.service_name == "payment.service" })
  let error_migrations = policy_application_result.migration_details.filter(fn(m) { 
    m.service == "payment.service" and m.reason.contains("error") 
  })
  
  assert_true(error_migrations.length() > 0)
  
  // 测试策略更新
  let updated_policy = {
    name: "critical_services_policy",
    description: "更新后的关键服务数据保留策略",
    services: ["payment.service", "auth.service", "user.service"],
    rules: [
      {
        storage_layer: "hot_storage",
        retention_days: 21,  // 延长热存储保留期
        conditions: [
          { field: "status", operator: "equals", value: "error" },
          { field: "duration", operator: "greater_than", value: 500 }  // 降低阈值
        ]
      },
      {
        storage_layer: "warm_storage",
        retention_days: 120,  // 延长温存储保留期
        conditions: []
      },
      {
        storage_layer: "cold_storage",
        retention_days: 2555,  // 保持冷存储保留期不变
        conditions: []
      }
    ]
  }
  
  // 更新策略
  let policy_update_result = LifecycleManager::update_retention_policy(lifecycle_manager, updated_policy)
  assert_true(policy_update_result.success)
  
  // 验证策略更新
  let current_policy = LifecycleManager::get_retention_policy(lifecycle_manager, "critical_services_policy")
  assert_true(current_policy != None)
  
  match current_policy {
    Some(policy) => {
      assert_eq(policy.rules[0].retention_days, 21)
      assert_eq(policy.rules[1].retention_days, 120)
    }
    None => assert_true(false)
  }
}

// 测试2: 自动数据迁移
test "自动数据迁移测试" {
  // 创建数据迁移管理器
  let migration_manager = DataMigrationManager::new()
  
  // 配置迁移规则
  MigrationManager::add_migration_rule(migration_manager, {
    name: "hot_to_warm_migration",
    source_layer: "hot_storage",
    target_layer: "warm_storage",
    conditions: [
      { field: "age_days", operator: "greater_than", value: 7 },
      { field: "access_count", operator: "less_than", value: 5 }
    ],
    schedule: {
      type: "cron",
      expression: "0 2 * * *"  // 每天凌晨2点
    },
    batch_size: 1000,
    compression: true
  })
  
  MigrationManager::add_migration_rule(migration_manager, {
    name: "warm_to_cold_migration",
    source_layer: "warm_storage",
    target_layer: "cold_storage",
    conditions: [
      { field: "age_days", operator: "greater_than", value: 30 },
      { field: "last_accessed_days", operator: "greater_than", value: 15 }
    ],
    schedule: {
      type: "cron",
      expression: "0 3 * * 0"  // 每周日凌晨3点
    },
    batch_size: 5000,
    compression: true,
    encryption: true
  })
  
  // 创建测试数据集
  let hot_data = []
  let warm_data = []
  let current_time = Time::now()
  
  // 创建热存储数据（不同年龄）
  for i in 0..=50 {
    let age_days = i
    let access_count = if i < 10 { 10 - i } else { 0 }
    let last_accessed_days = if i < 5 { 1 } else { i - 4 }
    
    let record = {
      id: "hot-record-" + i.to_string(),
      trace_id: "trace-" + i.to_string(),
      timestamp: current_time - (age_days * 24 * 60 * 60 * 1000),
      service_name: "service-" + (i % 5).to_string(),
      storage_layer: "hot_storage",
      age_days: age_days,
      access_count: access_count,
      last_accessed_days: last_accessed_days,
      data_size: 2048 + (i % 1000) * 10,
      created_at: current_time - (age_days * 24 * 60 * 60 * 1000),
      last_accessed: current_time - (last_accessed_days * 24 * 60 * 60 * 1000)
    }
    hot_data = hot_data.push(record)
  }
  
  // 创建温存储数据
  for i in 0..=30 {
    let age_days = 30 + i
    let last_accessed_days = if i < 10 { 5 } else { i - 4 }
    
    let record = {
      id: "warm-record-" + i.to_string(),
      trace_id: "trace-warm-" + i.to_string(),
      timestamp: current_time - (age_days * 24 * 60 * 60 * 1000),
      service_name: "warm-service-" + (i % 3).to_string(),
      storage_layer: "warm_storage",
      age_days: age_days,
      access_count: 0,
      last_accessed_days: last_accessed_days,
      data_size: 4096 + (i % 2000) * 10,
      created_at: current_time - (age_days * 24 * 60 * 60 * 1000),
      last_accessed: current_time - (last_accessed_days * 24 * 60 * 60 * 1000)
    }
    warm_data = warm_data.push(record)
  }
  
  // 执行热到温迁移
  let hot_to_warm_result = MigrationManager::execute_migration(migration_manager, "hot_to_warm_migration", hot_data)
  
  // 验证热到温迁移结果
  assert_true(hot_to_warm_result.total_evaluated > 0)
  assert_true(hot_to_warm_result.migrated_count > 0)
  
  // 检查符合迁移条件的数据
  let eligible_for_hot_to_warm = hot_data.filter(fn(r) { 
    r.age_days > 7 and r.access_count < 5 
  })
  assert_eq(hot_to_warm_result.migrated_count, eligible_for_hot_to_warm.length())
  
  // 验证迁移记录
  for migration in hot_to_warm_result.migration_details {
    assert_eq(migration.source_layer, "hot_storage")
    assert_eq(migration.target_layer, "warm_storage")
    assert_true(migration.compressed)
    assert_true(migration.migration_time > 0)
  }
  
  // 执行温到冷迁移
  let warm_to_cold_result = MigrationManager::execute_migration(migration_manager, "warm_to_cold_migration", warm_data)
  
  // 验证温到冷迁移结果
  assert_true(warm_to_cold_result.total_evaluated > 0)
  assert_true(warm_to_cold_result.migrated_count > 0)
  
  // 检查符合迁移条件的数据
  let eligible_for_warm_to_cold = warm_data.filter(fn(r) { 
    r.age_days > 30 and r.last_accessed_days > 15 
  })
  assert_eq(warm_to_cold_result.migrated_count, eligible_for_warm_to_cold.length())
  
  // 验证冷迁移记录
  for migration in warm_to_cold_result.migration_details {
    assert_eq(migration.source_layer, "warm_storage")
    assert_eq(migration.target_layer, "cold_storage")
    assert_true(migration.compressed)
    assert_true(migration.encrypted)
  }
  
  // 测试迁移统计
  let migration_stats = MigrationManager::get_migration_statistics(migration_manager)
  
  assert_true(migration_stats.total_migrations > 0)
  assert_true(migration_stats.total_data_volume_gb > 0)
  assert_true(migration_stats.compression_savings_gb > 0)
  assert_true(migration_stats.avg_migration_time_ms > 0)
  
  // 测试迁移失败处理
  let failing_migration_rule = {
    name: "failing_migration",
    source_layer: "hot_storage",
    target_layer: "unavailable_storage",
    conditions: [{ field: "age_days", operator: "greater_than", value: 1 }],
    schedule: { type: "immediate" },
    batch_size: 10,
    retry_attempts: 3
  }
  
  MigrationManager::add_migration_rule(migration_manager, failing_migration_rule)
  
  // 执行失败的迁移
  let failing_migration_result = MigrationManager::execute_migration(migration_manager, "failing_migration", hot_data)
  
  // 验证失败处理
  assert_true(failing_migration_result.total_evaluated > 0)
  assert_eq(failing_migration_result.migrated_count, 0)
  assert_true(failing_migration_result.failed_count > 0)
  assert_true(failing_migration_result.retry_attempts > 0)
  
  // 检查失败记录
  for failure in failing_migration_result.failure_details {
    assert_true(failure.error_message.length() > 0)
    assert_true(failure.retry_count <= 3)
    assert_true(failure.timestamp > 0)
  }
}

// 测试3: 数据清理和删除
test "数据清理和删除测试" {
  // 创建数据清理管理器
  let cleanup_manager = DataCleanupManager::new()
  
  // 配置清理规则
  CleanupManager::add_cleanup_rule(cleanup_manager, {
    name: "expired_data_cleanup",
    description: "清理过期数据",
    storage_layers: ["hot_storage", "warm_storage", "cold_storage"],
    conditions: [
      { field: "age_days", operator: "greater_than", value: 365 },
      { field: "service_name", operator: "not_in", value: ["audit.service", "compliance.service"] }
    ],
    action: "delete",
    schedule: {
      type: "cron",
      expression: "0 1 1 * *"  // 每月1日凌晨1点
    },
    dry_run: false,
    backup_before_delete: true
  })
  
  CleanupManager::add_cleanup_rule(cleanup_manager, {
    name: "low_value_data_cleanup",
    description: "清理低价值数据",
    storage_layers: ["hot_storage"],
    conditions: [
      { field: "age_days", operator: "greater_than", value: 14 },
      { field: "access_count", operator: "equals", value: 0 },
      { field: "status", operator: "equals", value: "ok" },
      { field: "duration", operator: "less_than", value: 100 }
    ],
    action: "delete",
    schedule: { type: "daily", time: "02:00" },
    dry_run: false,
    backup_before_delete: false
  })
  
  CleanupManager::add_cleanup_rule(cleanup_manager, {
    name: "test_data_cleanup",
    description: "清理测试数据",
    storage_layers: ["hot_storage", "warm_storage"],
    conditions: [
      { field: "service_name", operator: "contains", value: "test" },
      { field: "environment", operator: "equals", value: "development" }
    ],
    action: "delete",
    schedule: { type: "immediate" },
    dry_run: false,
    backup_before_delete: false
  })
  
  // 创建测试数据集
  let cleanup_test_data = []
  let current_time = Time::now()
  
  // 创建过期数据
  for i in 0..=20 {
    let record = {
      id: "expired-" + i.to_string(),
      trace_id: "trace-expired-" + i.to_string(),
      timestamp: current_time - (400 * 24 * 60 * 60 * 1000),  // 400天前
      service_name: "regular.service-" + (i % 3).to_string(),
      storage_layer: "cold_storage",
      age_days: 400,
      access_count: 0,
      status: "ok",
      duration: 50,
      environment: "production",
      data_size: 1024,
      created_at: current_time - (400 * 24 * 60 * 60 * 1000)
    }
    cleanup_test_data = cleanup_test_data.push(record)
  }
  
  // 创建低价值数据
  for i in 0..=15 {
    let record = {
      id: "low-value-" + i.to_string(),
      trace_id: "trace-low-value-" + i.to_string(),
      timestamp: current_time - (20 * 24 * 60 * 60 * 1000),  // 20天前
      service_name: "logging.service",
      storage_layer: "hot_storage",
      age_days: 20,
      access_count: 0,
      status: "ok",
      duration: 50,
      environment: "production",
      data_size: 512,
      created_at: current_time - (20 * 24 * 60 * 60 * 1000)
    }
    cleanup_test_data = cleanup_test_data.push(record)
  }
  
  // 创建测试数据
  for i in 0..=10 {
    let record = {
      id: "test-data-" + i.to_string(),
      trace_id: "trace-test-" + i.to_string(),
      timestamp: current_time - (5 * 24 * 60 * 60 * 1000),  // 5天前
      service_name: "test.service-" + (i % 2).to_string(),
      storage_layer: "hot_storage",
      age_days: 5,
      access_count: 1,
      status: "ok",
      duration: 100,
      environment: "development",
      data_size: 256,
      created_at: current_time - (5 * 24 * 60 * 60 * 1000)
    }
    cleanup_test_data = cleanup_test_data.push(record)
  }
  
  // 创建需要保留的数据
  for i in 0..=5 {
    let record = {
      id: "keep-" + i.to_string(),
      trace_id: "trace-keep-" + i.to_string(),
      timestamp: current_time - (400 * 24 * 60 * 60 * 1000),  // 400天前
      service_name: "audit.service",  // 审计服务，不应被删除
      storage_layer: "cold_storage",
      age_days: 400,
      access_count: 0,
      status: "ok",
      duration: 100,
      environment: "production",
      data_size: 2048,
      created_at: current_time - (400 * 24 * 60 * 60 * 1000)
    }
    cleanup_test_data = cleanup_test_data.push(record)
  }
  
  // 执行过期数据清理
  let expired_cleanup_result = CleanupManager::execute_cleanup(cleanup_manager, "expired_data_cleanup", cleanup_test_data)
  
  // 验证过期数据清理结果
  assert_true(expired_cleanup_result.total_evaluated > 0)
  assert_true(expired_cleanup_result.deleted_count > 0)
  
  // 检查审计服务数据是否被保留
  let audit_records = expired_cleanup_result.preserved_data.filter(fn(r) { r.service_name == "audit.service" })
  assert_eq(audit_records.length(), 6)  // 所有审计服务数据都应被保留
  
  // 执行低价值数据清理
  let low_value_cleanup_result = CleanupManager::execute_cleanup(cleanup_manager, "low_value_data_cleanup", cleanup_test_data)
  
  // 验证低价值数据清理结果
  assert_true(low_value_cleanup_result.total_evaluated > 0)
  assert_true(low_value_cleanup_result.deleted_count > 0)
  
  // 执行测试数据清理
  let test_cleanup_result = CleanupManager::execute_cleanup(cleanup_manager, "test_data_cleanup", cleanup_test_data)
  
  // 验证测试数据清理结果
  assert_true(test_cleanup_result.total_evaluated > 0)
  assert_eq(test_cleanup_result.deleted_count, 11)  // 所有测试数据都应被删除
  
  // 测试干运行模式
  let dry_run_rule = {
    name: "dry_run_cleanup",
    description: "干运行清理测试",
    storage_layers: ["hot_storage"],
    conditions: [{ field: "age_days", operator: "greater_than", value: 1 }],
    action: "delete",
    schedule: { type: "immediate" },
    dry_run: true,
    backup_before_delete: false
  }
  
  CleanupManager::add_cleanup_rule(cleanup_manager, dry_run_rule)
  
  // 执行干运行清理
  let dry_run_result = CleanupManager::execute_cleanup(cleanup_manager, "dry_run_cleanup", cleanup_test_data)
  
  // 验证干运行结果
  assert_true(dry_run_result.total_evaluated > 0)
  assert_true(dry_run_result.would_be_deleted_count > 0)
  assert_eq(dry_run_result.deleted_count, 0)  // 干运行不应实际删除数据
  
  // 测试清理统计
  let cleanup_stats = CleanupManager::get_cleanup_statistics(cleanup_manager)
  
  assert_true(cleanup_stats.total_cleanups > 0)
  assert_true(cleanup_stats.total_records_deleted > 0)
  assert_true(cleanup_stats.total_space_freed_gb > 0)
  assert_true(cleanup_stats.avg_cleanup_time_ms > 0)
  
  // 测试清理恢复
  let backup_restore_result = CleanupManager::restore_from_backup(cleanup_manager, {
    backup_id: expired_cleanup_result.backup_id,
    target_layer: "hot_storage",
    filters: [
      { field: "service_name", operator: "equals", value: "regular.service-0" }
    ]
  })
  
  // 验证恢复结果
  if expired_cleanup_result.backup_created {
    assert_true(backup_restore_result.success)
    assert_true(backup_restore_result.restored_count > 0)
  }
}

// 测试4: 数据生命周期监控和报告
test "数据生命周期监控和报告测试" {
  // 创建生命周期监控器
  let lifecycle_monitor = LifecycleMonitor::new()
  
  // 配置监控指标
  LifecycleMonitor::configure_metrics(lifecycle_monitor, [
    {
      name: "storage_utilization",
      description: "存储利用率",
      storage_layers: ["hot_storage", "warm_storage", "cold_storage"],
      collection_interval: 300000  // 5分钟
    },
    {
      name: "data_age_distribution",
      description: "数据年龄分布",
      storage_layers: ["hot_storage", "warm_storage", "cold_storage"],
      collection_interval: 3600000  // 1小时
    },
    {
      name: "migration_performance",
      description: "迁移性能指标",
      storage_layers: ["hot_storage", "warm_storage", "cold_storage"],
      collection_interval: 60000  // 1分钟
    },
    {
      name: "cleanup_effectiveness",
      description: "清理效果指标",
      storage_layers: ["hot_storage", "warm_storage", "cold_storage"],
      collection_interval: 3600000  // 1小时
    }
  ])
  
  // 启动监控
  LifecycleMonitor::start(lifecycle_monitor)
  
  // 模拟存储使用情况
  let storage_metrics = [
    {
      layer: "hot_storage",
      total_capacity_gb: 1000.0,
      used_capacity_gb: 750.0,
      record_count: 10000000,
      avg_record_size_kb: 75.0,
      oldest_record_days: 14,
      newest_record_days: 0
    },
    {
      layer: "warm_storage",
      total_capacity_gb: 5000.0,
      used_capacity_gb: 2000.0,
      record_count: 50000000,
      avg_record_size_kb: 40.0,
      oldest_record_days: 120,
      newest_record_days: 8
    },
    {
      layer: "cold_storage",
      total_capacity_gb: 10000.0,
      used_capacity_gb: 1500.0,
      record_count: 100000000,
      avg_record_size_kb: 15.0,
      oldest_record_days: 400,
      newest_record_days: 31
    }
  ]
  
  // 添加存储指标
  for metric in storage_metrics {
    LifecycleMonitor::add_storage_metric(lifecycle_monitor, metric)
  }
  
  // 模拟迁移性能数据
  let migration_metrics = []
  for i in 0..=24 {  // 24小时的迁移数据
    let timestamp = Time::now() - (24 - i) * 60 * 60 * 1000
    
    let metric = {
      timestamp: timestamp,
      hot_to_warm_migrations: 100 + (i % 50) * 2,
      warm_to_cold_migrations: 50 + (i % 30) * 3,
      hot_to_warm_volume_gb: 10.0 + (i % 10) * 2.0,
      warm_to_cold_volume_gb: 20.0 + (i % 15) * 3.0,
      avg_migration_time_ms: 500 + (i % 200) * 5,
      migration_success_rate: 0.95 + (i % 10) * 0.005,
      compression_ratio: 0.3 + (i % 20) * 0.02
    }
    migration_metrics = migration_metrics.push(metric)
  }
  
  // 添加迁移性能指标
  for metric in migration_metrics {
    LifecycleMonitor::add_migration_metric(lifecycle_monitor, metric)
  }
  
  // 模拟清理效果数据
  let cleanup_metrics = []
  for i in 0..=30 {  // 30天的清理数据
    let timestamp = Time::now() - (30 - i) * 24 * 60 * 60 * 1000
    
    let metric = {
      timestamp: timestamp,
      records_deleted: 100000 + (i % 50000) * 2,
      space_freed_gb: 50.0 + (i % 20) * 5.0,
      cleanup_duration_ms: 30000 + (i % 10000) * 2,
      cleanup_success_rate: 0.98 + (i % 5) * 0.004,
      backup_size_gb: if i % 7 == 0 { 10.0 + (i % 5) * 2.0 } else { 0.0 }
    }
    cleanup_metrics = cleanup_metrics.push(metric)
  }
  
  // 添加清理效果指标
  for metric in cleanup_metrics {
    LifecycleMonitor::add_cleanup_metric(lifecycle_monitor, metric)
  }
  
  // 等待指标收集
  Time::sleep(1000)
  
  // 生成存储利用率报告
  let storage_report = LifecycleMonitor::generate_storage_report(lifecycle_monitor)
  
  // 验证存储报告
  assert_true(storage_report.total_layers >= 3)
  assert_true(storage_report.total_capacity_gb > 0)
  assert_true(storage_report.total_used_gb > 0)
  assert_true(storage_report.overall_utilization > 0.0)
  
  // 检查各层利用率
  let hot_storage_util = storage_report.layer_utilization.find(fn(u) { u.layer == "hot_storage" })
  assert_true(hot_storage_util != None)
  
  match hot_storage_util {
    Some(util) => {
      assert_eq(util.utilization, 0.75)  // 750GB/1000GB = 75%
      assert_true(util.record_count > 0)
      assert_true(util.avg_record_size_kb > 0)
    }
    None => assert_true(false)
  }
  
  // 生成数据年龄分布报告
  let age_distribution_report = LifecycleMonitor::generate_age_distribution_report(lifecycle_monitor)
  
  // 验证年龄分布报告
  assert_true(age_distribution_report.total_records > 0)
  assert_true(age_distribution_report.age_buckets.length() > 0)
  
  // 检查年龄分布
  let age_buckets = age_distribution_report.age_buckets
  let recent_records = age_buckets.find(fn(b) { b.age_range == "0-7 days" })
  let old_records = age_buckets.find(fn(b) { b.age_range == ">365 days" })
  
  assert_true(recent_records != None)
  assert_true(old_records != None)
  
  // 生成迁移性能报告
  let migration_report = LifecycleMonitor::generate_migration_performance_report(lifecycle_monitor)
  
  // 验证迁移性能报告
  assert_true(migration_report.total_migrations > 0)
  assert_true(migration_report.total_volume_gb > 0)
  assert_true(migration_report.avg_success_rate > 0.9)
  assert_true(migration_report.avg_compression_ratio > 0.0)
  
  // 检查迁移趋势
  assert_true(migration_report.hourly_stats.length() > 0)
  assert_true(migration_report.peak_migration_hour >= 0 and migration_report.peak_migration_hour <= 23)
  
  // 生成清理效果报告
  let cleanup_report = LifecycleMonitor::generate_cleanup_effectiveness_report(lifecycle_monitor)
  
  // 验证清理效果报告
  assert_true(cleanup_report.total_cleanup_operations > 0)
  assert_true(cleanup_report.total_records_deleted > 0)
  assert_true(cleanup_report.total_space_freed_gb > 0)
  assert_true(cleanup_report.avg_success_rate > 0.95)
  
  // 检查清理趋势
  assert_true(cleanup_report.daily_stats.length() > 0)
  assert_true(cleanup_report.most_effective_day != "")
  
  // 生成综合生命周期报告
  let comprehensive_report = LifecycleMonitor::generate_comprehensive_report(lifecycle_monitor, {
    include_storage_analysis: true,
    include_migration_analysis: true,
    include_cleanup_analysis: true,
    include_recommendations: true,
    time_range_days: 30
  })
  
  // 验证综合报告
  assert_true(comprehensive_report.storage_analysis != None)
  assert_true(comprehensive_report.migration_analysis != None)
  assert_true(comprehensive_report.cleanup_analysis != None)
  assert_true(comprehensive_report.recommendations.length() > 0)
  
  // 检查建议
  let storage_recommendations = comprehensive_report.recommendations.filter(fn(r) { r.category == "storage" })
  assert_true(storage_recommendations.length() > 0)
  
  let migration_recommendations = comprehensive_report.recommendations.filter(fn(r) { r.category == "migration" })
  assert_true(migration_recommendations.length() > 0)
  
  let cleanup_recommendations = comprehensive_report.recommendations.filter(fn(r) { r.category == "cleanup" })
  assert_true(cleanup_recommendations.length() > 0)
  
  // 停止监控
  LifecycleMonitor::stop(lifecycle_monitor)
}

// 测试5: 数据生命周期优化建议
test "数据生命周期优化建议测试" {
  // 创建生命周期优化器
  let lifecycle_optimizer = LifecycleOptimizer::new()
  
  // 配置优化目标
  LifecycleOptimizer::set_optimization_goals(lifecycle_optimizer, {
    cost_reduction_target: 0.2,  // 20%成本削减
    performance_target: 0.9,     // 90%性能目标
    compliance_level: "high",    // 高合规性
    data_availability: 0.99      // 99%数据可用性
  })
  
  // 添加当前配置信息
  LifecycleOptimizer::add_current_configuration(lifecycle_optimizer, {
    storage_layers: [
      {
        name: "hot_storage",
        type: "ssd",
        cost_per_gb_month: 0.23,
        performance_tier: "high",
        retention_days: 7,
        compression_enabled: false
      },
      {
        name: "warm_storage",
        type: "hdd",
        cost_per_gb_month: 0.045,
        performance_tier: "medium",
        retention_days: 30,
        compression_enabled: true
      },
      {
        name: "cold_storage",
        type: "tape",
        cost_per_gb_month: 0.004,
        performance_tier: "low",
        retention_days: 365,
        compression_enabled: true
      }
    ],
    migration_policies: [
      {
        name: "hot_to_warm",
        conditions: [{ field: "age_days", operator: "greater_than", value: 7 }],
        schedule: "daily"
      },
      {
        name: "warm_to_cold",
        conditions: [{ field: "age_days", operator: "greater_than", value: 30 }],
        schedule: "weekly"
      }
    ],
    cleanup_policies: [
      {
        name: "expired_cleanup",
        conditions: [{ field: "age_days", operator: "greater_than", value: 365 }],
        schedule: "monthly"
      }
    ]
  })
  
  // 添加数据使用模式
  let data_usage_patterns = [
    {
      service_name: "payment.service",
      access_pattern: "frequent_recent",
      data_volume_gb: 100.0,
      avg_record_size_kb: 50.0,
      access_frequency: "high",
      retention_requirement: 2555  // 7年合规要求
    },
    {
      service_name: "logging.service",
      access_pattern: "infrequent_old",
      data_volume_gb: 500.0,
      avg_record_size_kb: 25.0,
      access_frequency: "low",
      retention_requirement: 90
    },
    {
      service_name: "analytics.service",
      access_pattern: "periodic_batch",
      data_volume_gb: 200.0,
      avg_record_size_kb: 100.0,
      access_frequency: "medium",
      retention_requirement: 365
    },
    {
      service_name: "user.service",
      access_pattern: "frequent_random",
      data_volume_gb: 150.0,
      avg_record_size_kb: 30.0,
      access_frequency: "high",
      retention_requirement: 1825  // 5年合规要求
    }
  ]
  
  for pattern in data_usage_patterns {
    LifecycleOptimizer::add_usage_pattern(lifecycle_optimizer, pattern)
  }
  
  // 添加成本数据
  let current_costs = {
    hot_storage_monthly: 23000.0,
    warm_storage_monthly: 22500.0,
    cold_storage_monthly: 4000.0,
    migration_monthly: 5000.0,
    cleanup_monthly: 2000.0,
    total_monthly: 56500.0
  }
  
  LifecycleOptimizer::set_current_costs(lifecycle_optimizer, current_costs)
  
  // 执行优化分析
  let optimization_analysis = LifecycleOptimizer::analyze(lifecycle_optimizer)
  
  // 验证优化分析结果
  assert_true(optimization_analysis.potential_cost_reduction > 0.0)
  assert_true(optimization_analysis.optimization_recommendations.length() > 0)
  
  // 检查存储优化建议
  let storage_optimizations = optimization_analysis.optimization_recommendations.filter(fn(r) { r.category == "storage" })
  assert_true(storage_optimizations.length() > 0)
  
  // 检查压缩优化建议
  let compression_optimization = storage_optimizations.find(fn(r) { r.type == "compression" })
  assert_true(compression_optimization != None)
  
  match compression_optimization {
    Some(opt) => {
      assert_true(opt.cost_savings > 0.0)
      assert_true(opt.implementation_effort == "low")
      assert_true(opt.risk_level == "low")
    }
    None => assert_true(false)
  }
  
  // 检查存储层优化建议
  let storage_tier_optimization = storage_optimizations.find(fn(r) { r.type == "storage_tier" })
  assert_true(storage_tier_optimization != None)
  
  // 检查迁移优化建议
  let migration_optimizations = optimization_analysis.optimization_recommendations.filter(fn(r) { r.category == "migration" })
  assert_true(migration_optimizations.length() > 0)
  
  let schedule_optimization = migration_optimizations.find(fn(r) { r.type == "schedule" })
  assert_true(schedule_optimization != None)
  
  match schedule_optimization {
    Some(opt) => {
      assert_true(opt.cost_savings > 0.0)
      assert_true(opt.implementation_effort == "medium")
    }
    None => assert_true(false)
  }
  
  // 检查清理优化建议
  let cleanup_optimizations = optimization_analysis.optimization_recommendations.filter(fn(r) { r.category == "cleanup" })
  assert_true(cleanup_optimizations.length() > 0)
  
  let policy_optimization = cleanup_optimizations.find(fn(r) { r.type == "policy" })
  assert_true(policy_optimization != None)
  
  // 生成优化实施计划
  let implementation_plan = LifecycleOptimizer::generate_implementation_plan(lifecycle_optimizer, optimization_analysis)
  
  // 验证实施计划
  assert_true(implementation_plan.phases.length() > 0)
  assert_true(implementation_plan.total_cost_savings > 0.0)
  assert_true(implementation_plan.implementation_duration_weeks > 0)
  
  // 检查第一阶段实施
  let phase1 = implementation_plan.phases[0]
  assert_true(phase1.tasks.length() > 0)
  assert_true(phase1.estimated_cost_savings > 0.0)
  assert_true(phase1.estimated_effort_person_days > 0)
  
  // 检查高优先级任务
  let high_priority_tasks = phase1.tasks.filter(fn(t) { t.priority == "high" })
  assert_true(high_priority_tasks.length() > 0)
  
  // 模拟优化实施效果
  let implementation_results = LifecycleOptimizer::simulate_implementation(lifecycle_optimizer, implementation_plan)
  
  // 验证实施效果
  assert_true(implementation_results.achieved_cost_reduction >= optimization_analysis.potential_cost_reduction * 0.8)
  assert_true(implementation_results.performance_impact >= -0.1)  // 性能影响不超过10%
  assert_true(implementation_results.compliance_maintained)
  assert_true(implementation_results.data_availability >= 0.98)
  
  // 检查各层成本变化
  let cost_changes = implementation_results.cost_changes_by_layer
  assert_true(cost_changes.length() > 0)
  
  for change in cost_changes {
    assert_true(change.layer != "")
    assert_true(change.old_cost > 0.0)
    assert_true(change.new_cost >= 0.0)
    assert_true(change.savings_percentage >= 0.0)
  }
  
  // 测试优化效果监控
  let optimization_monitoring = LifecycleOptimizer::setup_monitoring(lifecycle_optimizer, {
    metrics: ["cost_savings", "storage_utilization", "migration_efficiency", "cleanup_effectiveness"],
    alert_thresholds: [
      { metric: "cost_savings", operator: "less_than", value: 0.15, severity: "warning" },
      { metric: "storage_utilization", operator: "greater_than", value: 0.85, severity: "critical" }
    ],
    reporting_frequency: "weekly"
  })
  
  // 验证监控设置
  assert_true(optimization_monitoring.metrics.length() > 0)
  assert_true(optimization_monitoring.alert_thresholds.length() > 0)
  assert_eq(optimization_monitoring.reporting_frequency, "weekly")
}