// 遥测数据长期存储和归档测试
// 测试数据生命周期管理、冷热数据分离、压缩存储和检索优化

pub test "遥测数据生命周期管理测试" {
  let storage_manager = azimuth::StorageManager::new()
  
  // 创建生命周期管理器
  let lifecycle_manager = azimuth::StorageManager::create_lifecycle_manager(storage_manager)
  
  // 定义数据生命周期策略
  let lifecycle_policies = [
    {
      data_type: "trace",
      hot_storage_days: 7,
      warm_storage_days: 30,
      cold_storage_days: 365,
      archive_after_days: 365,
      delete_after_days: 2555,  # 7年
      access_frequency_threshold: 10  # 访问次数阈值
    },
    {
      data_type: "metrics",
      hot_storage_days: 14,
      warm_storage_days: 90,
      cold_storage_days: 730,
      archive_after_days: 730,
      delete_after_days: 3650,  # 10年
      access_frequency_threshold: 50
    },
    {
      data_type: "logs",
      hot_storage_days: 3,
      warm_storage_days: 30,
      cold_storage_days: 180,
      archive_after_days: 180,
      delete_after_days: 1825,  # 5年
      access_frequency_threshold: 5
    }
  ]
  
  // 注册生命周期策略
  for policy in lifecycle_policies {
    azimuth::LifecycleManager::register_policy(lifecycle_manager, policy)
  }
  
  // 模拟创建不同时间的遥测数据
  let current_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let day_in_nanos = 24 * 60 * 60 * 1000000000L
  
  let test_data = [
    {
      id: "trace-001",
      type: "trace",
      created_at: current_time,
      last_accessed: current_time,
      access_count: 15,
      size: 1024
    },
    {
      id: "metrics-001",
      type: "metrics", 
      created_at: current_time - (10 * day_in_nanos),  # 10天前
      last_accessed: current_time - (2 * day_in_nanos), # 2天前
      access_count: 60,
      size: 512
    },
    {
      id: "logs-001",
      type: "logs",
      created_at: current_time - (40 * day_in_nanos),  # 40天前
      last_accessed: current_time - (35 * day_in_nanos), # 35天前
      access_count: 3,
      size: 2048
    },
    {
      id: "trace-002",
      type: "trace",
      created_at: current_time - (400 * day_in_nanos),  # 400天前
      last_accessed: current_time - (200 * day_in_nanos), # 200天前
      access_count: 8,
      size: 1536
    }
  ]
  
  // 注册测试数据
  for data in test_data {
    azimuth::LifecycleManager::register_data(lifecycle_manager, data)
  }
  
  // 测试数据生命周期状态评估
  let lifecycle_states = []
  
  for data in test_data {
    let state = azimuth::LifecycleManager::evaluate_lifecycle_state(lifecycle_manager, data.id)
    lifecycle_states.push(state)
  }
  
  // 验证生命周期状态
  # trace-001: 创建时间<7天，应该是hot状态
  assert_eq(lifecycle_states[0].current_storage, "hot")
  
  # metrics-001: 创建时间10-14天，访问次数>50，应该是warm状态
  assert_eq(lifecycle_states[1].current_storage, "warm")
  
  # logs-001: 创建时间30-40天，访问次数<5，应该是cold状态
  assert_eq(lifecycle_states[2].current_storage, "cold")
  
  # trace-002: 创建时间>365天，应该是archived状态
  assert_eq(lifecycle_states[3].current_storage, "archived")
  
  // 测试生命周期转换
  let transition_plan = azimuth::LifecycleManager::create_transition_plan(lifecycle_manager)
  
  assert_true(transition_plan.transitions.length >= 2)  # 至少有warm->cold和cold->archived的转换
  
  // 执行生命周期转换
  let transition_result = azimuth::LifecycleManager::execute_transitions(lifecycle_manager, transition_plan)
  
  assert_true(transition_result.success)
  assert_true(transition_result.processed_count >= 2)
  
  // 测试访问频率统计
  for data in test_data {
    azimuth::LifecycleManager::record_access(lifecycle_manager, data.id, current_time)
  }
  
  let access_stats = azimuth::LifecycleManager::get_access_statistics(lifecycle_manager)
  assert_true(access_stats.total_accesses >= 4)
  
  // 测试数据保留策略
  let retention_policies = [
    {
      data_type: "trace",
      min_retention_days: 30,
      max_retention_days: 2555,
      compliance_requirements: ["SOX", "GDPR"]
    },
    {
      data_type: "metrics",
      min_retention_days: 90,
      max_retention_days: 3650,
      compliance_requirements: ["PCI-DSS"]
    }
  ]
  
  for policy in retention_policies {
    azimuth::LifecycleManager::register_retention_policy(lifecycle_manager, policy)
  }
  
  // 测试合规性检查
  let compliance_report = azimuth::LifecycleManager::check_compliance(lifecycle_manager)
  
  assert_true(compliance_report.overall_compliant)
  assert_true(compliance_report.policy_checks.length >= 2)
  
  // 测试数据清理
  let expired_data = [
    {
      id: "old-trace-001",
      type: "trace",
      created_at: current_time - (3000 * day_in_nanos),  # 3000天前，超过7年
      size: 1024
    }
  ]
  
  for data in expired_data {
    azimuth::LifecycleManager::register_data(lifecycle_manager, data)
  }
  
  let cleanup_plan = azimuth::LifecycleManager::create_cleanup_plan(lifecycle_manager)
  assert_true(cleanup_plan.items_to_delete.length >= 1)
  
  let cleanup_result = azimuth::LifecycleManager::execute_cleanup(lifecycle_manager, cleanup_plan)
  assert_true(cleanup_result.success)
  assert_true(cleanup_result.deleted_count >= 1)
}

pub test "冷热数据分离存储测试" {
  let storage_manager = azimuth::StorageManager::new()
  
  // 创建分层存储管理器
  let tiered_storage = azimuth::StorageManager::create_tiered_storage(storage_manager)
  
  // 定义存储层级
  let storage_tiers = [
    {
      name: "hot",
      type: "ssd",
      capacity_gb: 1000,
      performance_tier: "high",
      cost_per_gb: 0.23,
      compression_enabled: false,
      encryption_enabled: true,
      backup_enabled: true
    },
    {
      name: "warm",
      type: "hdd",
      capacity_gb: 5000,
      performance_tier: "medium",
      cost_per_gb: 0.08,
      compression_enabled: true,
      encryption_enabled: true,
      backup_enabled: true
    },
    {
      name: "cold",
      type: "hdd_archive",
      capacity_gb: 20000,
      performance_tier: "low",
      cost_per_gb: 0.02,
      compression_enabled: true,
      encryption_enabled: true,
      backup_enabled: false
    },
    {
      name: "archive",
      type: "object_storage",
      capacity_gb: 100000,
      performance_tier: "very_low",
      cost_per_gb: 0.005,
      compression_enabled: true,
      encryption_enabled: true,
      backup_enabled: false
    }
  ]
  
  // 注册存储层级
  for tier in storage_tiers {
    azimuth::TieredStorage::register_tier(tiered_storage, tier)
  }
  
  // 模拟数据写入
  let data_samples = []
  let current_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # 生成不同类型和访问模式的数据
  for i in 0..1000 {
    let data = {
      id: "data-" + i.to_string(),
      type: if i % 3 == 0 { "trace" } else if i % 3 == 1 { "metrics" } else { "logs" },
      size: 1024 + (i % 10) * 512,  # 1KB-6KB
      created_at: current_time - ((i % 365) * 24 * 60 * 60 * 1000000000L),  # 过去365天内
      last_accessed: current_time - ((i % 30) * 24 * 60 * 60 * 1000000000L), # 过去30天内
      access_count: i % 100,  # 0-99次访问
      priority: if i % 10 == 0 { "high" } else if i % 10 == 1 { "medium" } else { "low" }
    }
    
    data_samples.push(data)
  }
  
  // 写入数据到适当的存储层级
  for data in data_samples {
    let target_tier = azimuth::TieredStorage::determine_target_tier(tiered_storage, data)
    let write_result = azimuth::TieredStorage::write_data(tiered_storage, data.id, data, target_tier)
    assert_true(write_result.success)
  }
  
  // 验证数据分布
  let tier_distribution = azimuth::TieredStorage::get_data_distribution(tiered_storage)
  
  assert_true(tier_distribution.hot.count > 0)
  assert_true(tier_distribution.warm.count > 0)
  assert_true(tier_distribution.cold.count > 0)
  assert_true(tier_distribution.archive.count >= 0)
  
  # 验证存储使用情况
  assert_true(tier_distribution.hot.used_gb > 0)
  assert_true(tier_distribution.warm.used_gb > 0)
  assert_true(tier_distribution.cold.used_gb > 0)
  
  // 测试数据层级迁移
  let migration_candidates = azimuth::TieredStorage::find_migration_candidates(tiered_storage)
  assert_true(migration_candidates.length >= 1)
  
  // 执行数据迁移
  let migration_plan = azimuth::TieredStorage::create_migration_plan(tiered_storage, migration_candidates)
  let migration_result = azimuth::TieredStorage::execute_migration(tiered_storage, migration_plan)
  
  assert_true(migration_result.success)
  assert_true(migration_result.migrated_count >= 1)
  
  // 测试跨层级查询
  let query_criteria = {
    data_type: "trace",
    time_range: {
      start: current_time - (30 * 24 * 60 * 60 * 1000000000L),
      end: current_time
    },
    access_pattern: "frequent"
  }
  
  let query_result = azimuth::TieredStorage::query_across_tiers(tiered_storage, query_criteria)
  
  assert_true(query_result.total_found >= 0)
  assert_true(query_result.results.length >= 0)
  assert_true(query_result.sources.length >= 1)  # 应该从多个层级查询
  
  // 验证查询结果包含预期的层级
  assert_true(query_result.sources.contains("hot"))
  assert_true(query_result.sources.contains("warm") || query_result.sources.contains("cold"))
  
  // 测试存储成本优化
  let cost_analysis = azimuth::TieredStorage::analyze_storage_costs(tiered_storage)
  
  assert_true(cost_analysis.total_monthly_cost > 0)
  assert_true(cost_analysis.hot_storage_cost > 0)
  assert_true(cost_analysis.warm_storage_cost > 0)
  assert_true(cost_analysis.cold_storage_cost > 0)
  assert_true(cost_analysis.archive_storage_cost >= 0)
  
  # 热存储成本应该最高
  assert_true(cost_analysis.hot_storage_cost_per_gb > cost_analysis.warm_storage_cost_per_gb)
  assert_true(cost_analysis.warm_storage_cost_per_gb > cost_analysis.cold_storage_cost_per_gb)
  assert_true(cost_analysis.cold_storage_cost_per_gb > cost_analysis.archive_storage_cost_per_gb)
  
  // 测试存储性能监控
  let performance_metrics = azimuth::TieredStorage::get_performance_metrics(tiered_storage)
  
  assert_true(performance_metrics.hot.read_latency_ms < performance_metrics.warm.read_latency_ms)
  assert_true(performance_metrics.warm.read_latency_ms < performance_metrics.cold.read_latency_ms)
  assert_true(performance_metrics.cold.read_latency_ms < performance_metrics.archive.read_latency_ms)
  
  assert_true(performance_metrics.hot.write_throughput_mbps > performance_metrics.warm.write_throughput_mbps)
  assert_true(performance_metrics.warm.write_throughput_mbps > performance_metrics.cold.write_throughput_mbps)
  assert_true(performance_metrics.cold.write_throughput_mbps > performance_metrics.archive.write_throughput_mbps)
  
  // 测试自动数据分层策略
  let auto_tiering_config = {
    enabled: true,
    evaluation_interval_hours: 24,
    hot_tier_criteria: {
      max_age_days: 7,
      min_access_count: 10,
      priority_levels: ["high"]
    },
    warm_tier_criteria: {
      max_age_days: 30,
      min_access_count: 5,
      priority_levels: ["high", "medium"]
    },
    cold_tier_criteria: {
      max_age_days: 180,
      min_access_count: 1,
      priority_levels: ["high", "medium", "low"]
    }
  }
  
  azimuth::TieredStorage::configure_auto_tiering(tiered_storage, auto_tiering_config)
  
  // 运行自动分层
  let auto_tier_result = azimuth::TieredStorage::run_auto_tiering(tiered_storage)
  assert_true(auto_tier_result.success)
  assert_true(auto_tier_result.processed_items >= 1)
  
  // 测试存储容量管理
  let capacity_manager = azimuth::TieredStorage::create_capacity_manager(tiered_storage)
  
  // 模拟存储压力
  azimuth::CapacityManager::simulate_pressure(capacity_manager, {
    tier: "hot",
    usage_percentage: 85.0
  })
  
  let pressure_response = azimuth::CapacityManager::handle_capacity_pressure(capacity_manager)
  assert_true(pressure_response.actions_taken.length >= 1)
  
  # 验证采取了适当的行动
  let actions = pressure_response.actions_taken
  assert_true(actions.contains("data_migration") || actions.contains("compression_increase"))
}

pub test "数据压缩和归档测试" {
  let storage_manager = azimuth::StorageManager::new()
  
  // 创建压缩归档管理器
  let archive_manager = azimuth::StorageManager::create_archive_manager(storage_manager)
  
  // 定义压缩算法配置
  let compression_configs = [
    {
      name: "fast_lz4",
      algorithm: "lz4",
      level: 1,
      speed_priority: "high",
      ratio_priority: "low",
      suitable_for: ["real_time", "frequent_access"]
    },
    {
      name: "balanced_zstd",
      algorithm: "zstd",
      level: 5,
      speed_priority: "medium",
      ratio_priority: "medium",
      suitable_for: ["warm_storage", "occasional_access"]
    },
    {
      name: "max_gzip",
      algorithm: "gzip",
      level: 9,
      speed_priority: "low",
      ratio_priority: "high",
      suitable_for: ["cold_storage", "archive"]
    }
  ]
  
  // 注册压缩配置
  for config in compression_configs {
    azimuth::ArchiveManager::register_compression_config(archive_manager, config)
  }
  
  // 生成测试数据
  let test_datasets = []
  
  for i in 0..100 {
    let dataset = {
      id: "dataset-" + i.to_string(),
      type: if i % 3 == 0 { "trace_spans" } else if i % 3 == 1 { "metrics_series" } else { "log_entries" },
      raw_size: 1024 * 1024 + (i * 10240),  # 1MB-2MB
      data: generate_test_data(i),  # 生成测试数据的函数
      access_frequency: if i % 10 == 0 { "high" } else if i % 10 < 5 { "medium" } else { "low" },
      retention_days: if i % 5 == 0 { 3650 } else if i % 5 < 3 { 730 } else { 180 }
    }
    
    test_datasets.push(dataset)
  }
  
  // 测试不同压缩算法的效果
  let compression_results = []
  
  for config in compression_configs {
    let algorithm_results = []
    
    for dataset in test_datasets {
      let compression_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
      
      let compressed_data = azimuth::ArchiveManager::compress_with_config(
        archive_manager, 
        dataset.data, 
        config.name
      )
      
      let compression_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
      
      let decompression_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
      
      let decompressed_data = azimuth::ArchiveManager::decompress_with_config(
        archive_manager, 
        compressed_data, 
        config.name
      )
      
      let decompression_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
      
      # 验证数据完整性
      let integrity_check = azimuth::ArchiveManager::verify_data_integrity(
        archive_manager, 
        dataset.data, 
        decompressed_data
      )
      
      assert_true(integrity_check)
      
      algorithm_results.push({
        dataset_id: dataset.id,
        original_size: dataset.raw_size,
        compressed_size: compressed_data.length(),
        compression_ratio: dataset.raw_size.to_double() / compressed_data.length().to_double(),
        compression_time_ms: (compression_end - compression_start) / 1000000L,
        decompression_time_ms: (decompression_end - decompression_start) / 1000000L
      })
    }
    
    # 计算平均性能指标
    let avg_compression_ratio = algorithm_results.reduce(0.0, fn(acc, result) { 
      acc + result.compression_ratio 
    }) / algorithm_results.length().to_double()
    
    let avg_compression_time = algorithm_results.reduce(0L, fn(acc, result) { 
      acc + result.compression_time_ms 
    }) / algorithm_results.length().to_long()
    
    let avg_decompression_time = algorithm_results.reduce(0L, fn(acc, result) { 
      acc + result.decompression_time_ms 
    }) / algorithm_results.length().to_long()
    
    compression_results.push({
      algorithm: config.name,
      avg_compression_ratio,
      avg_compression_time,
      avg_decompression_time,
      results: algorithm_results
    })
  }
  
  // 验证压缩算法性能特性
  let lz4_results = compression_results.find(fn(r) { r.algorithm == "fast_lz4" })
  let zstd_results = compression_results.find(fn(r) { r.algorithm == "balanced_zstd" })
  let gzip_results = compression_results.find(fn(r) { r.algorithm == "max_gzip" })
  
  # LZ4应该最快但压缩比最低
  assert_true(lz4_results.avg_compression_time < zstd_results.avg_compression_time)
  assert_true(lz4_results.avg_compression_time < gzip_results.avg_compression_time)
  assert_true(lz4_results.avg_compression_ratio < zstd_results.avg_compression_ratio)
  assert_true(lz4_results.avg_compression_ratio < gzip_results.avg_compression_ratio)
  
  # GZIP应该压缩比最高但最慢
  assert_true(gzip_results.avg_compression_ratio > zstd_results.avg_compression_ratio)
  assert_true(gzip_results.avg_compression_ratio > lz4_results.avg_compression_ratio)
  assert_true(gzip_results.avg_compression_time > zstd_results.avg_compression_time)
  assert_true(gzip_results.avg_compression_time > lz4_results.avg_compression_time)
  
  // 测试智能压缩策略选择
  let smart_compression_results = []
  
  for dataset in test_datasets {
    let selected_algorithm = azimuth::ArchiveManager::select_optimal_compression(
      archive_manager, 
      dataset
    )
    
    let compressed_data = azimuth::ArchiveManager::compress_with_config(
      archive_manager, 
      dataset.data, 
      selected_algorithm
    )
    
    smart_compression_results.push({
      dataset_id: dataset.id,
      selected_algorithm,
      original_size: dataset.raw_size,
      compressed_size: compressed_data.length(),
      access_frequency: dataset.access_frequency
    })
  }
  
  # 验证智能选择的合理性
  let high_freq_results = smart_compression_results.filter(fn(r) { r.access_frequency == "high" })
  let low_freq_results = smart_compression_results.filter(fn(r) { r.access_frequency == "low" })
  
  # 高频访问数据应该选择快速压缩算法
  let high_freq_algorithms = high_freq_results.map(fn(r) { r.selected_algorithm })
  assert_true(high_freq_algorithms.filter(fn(alg) { alg == "fast_lz4" }).length >= high_freq_algorithms.length() / 2)
  
  # 低频访问数据应该选择高压缩比算法
  let low_freq_algorithms = low_freq_results.map(fn(r) { r.selected_algorithm })
  assert_true(low_freq_algorithms.filter(fn(alg) { alg == "max_gzip" }).length >= low_freq_algorithms.length() / 2)
  
  // 测试归档策略
  let archive_policies = [
    {
      name: "daily_archive",
      schedule: "0 2 * * *",  # 每天凌晨2点
      data_age_days: 30,
      compression_config: "balanced_zstd",
      target_storage: "cold",
      retention_days: 365
    },
    {
      name: "weekly_archive",
      schedule: "0 3 * * 0",  # 每周日凌晨3点
      data_age_days: 7,
      compression_config: "max_gzip",
      target_storage: "archive",
      retention_days: 2555
    }
  ]
  
  for policy in archive_policies {
    azimuth::ArchiveManager::register_archive_policy(archive_manager, policy)
  }
  
  // 测试归档执行
  let archive_candidates = azimuth::ArchiveManager::find_archive_candidates(archive_manager, "daily_archive")
  assert_true(archive_candidates.length >= 1)
  
  let archive_execution = azimuth::ArchiveManager::execute_archive(archive_manager, "daily_archive", archive_candidates)
  assert_true(archive_execution.success)
  assert_true(archive_execution.archived_count >= 1)
  
  // 验证归档后的数据可访问性
  for candidate in archive_candidates {
    let archived_data = azimuth::ArchiveManager::retrieve_archived_data(archive_manager, candidate.id)
    assert_true(archived_data.is_available)
    assert_true(archived_data.data.length() > 0)
  }
  
  // 测试增量归档
  let incremental_archive = azimuth::ArchiveManager::create_incremental_archive(archive_manager)
  
  # 创建基础归档
  let base_archive_data = test_datasets.slice(0, 50)
  let base_archive_result = azimuth::IncrementalArchive::create_base_archive(
    incremental_archive, 
    base_archive_data, 
    "max_gzip"
  )
  assert_true(base_archive_result.success)
  
  # 创建增量归档
  let incremental_archive_data = test_datasets.slice(50, 100)
  let incremental_archive_result = azimuth::IncrementalArchive::create_incremental_archive(
    incremental_archive, 
    incremental_archive_data, 
    base_archive_result.archive_id,
    "max_gzip"
  )
  assert_true(incremental_archive_result.success)
  
  # 验证增量归档的空间效率
  let full_archive_size = base_archive_result.size + incremental_archive_result.size
  let separate_archive_size = azimuth::ArchiveManager::estimate_separate_archive_size(
    archive_manager, 
    test_datasets, 
    "max_gzip"
  )
  
  assert_true(full_archive_size < separate_archive_size)
  
  // 测试归档数据检索优化
  let retrieval_optimizer = azimuth::ArchiveManager::create_retrieval_optimizer(archive_manager)
  
  # 创建索引
  let index_result = azimuth::RetrievalOptimizer::create_index(retrieval_optimizer, {
    fields: ["dataset_id", "type", "created_at", "access_frequency"],
    index_type: "composite"
  })
  assert_true(index_result.success)
  
  # 测试索引查询
  let indexed_query = {
    type: "metrics_series",
    time_range: {
      start: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - (7 * 24 * 60 * 60 * 1000000000L),
      end: azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    }
  }
  
  let indexed_results = azimuth::RetrievalOptimizer::query_with_index(retrieval_optimizer, indexed_query)
  assert_true(indexed_results.total_found >= 0)
  assert_true(indexed_results.query_time_ms < 1000)  # 应该在1秒内完成
  
  # 测试缓存优化
  let cache_config = {
    max_size_mb: 100,
    ttl_minutes: 30,
    eviction_policy: "lru"
  }
  
  azimuth::RetrievalOptimizer::configure_cache(retrieval_optimizer, cache_config)
  
  # 第一次查询（冷缓存）
  let cold_query_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let cold_results = azimuth::RetrievalOptimizer::query_with_cache(retrieval_optimizer, indexed_query)
  let cold_query_time = (azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - cold_query_start) / 1000000L
  
  # 第二次查询（热缓存）
  let hot_query_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let hot_results = azimuth::RetrievalOptimizer::query_with_cache(retrieval_optimizer, indexed_query)
  let hot_query_time = (azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - hot_query_start) / 1000000L
  
  # 缓存应该提高查询性能
  assert_true(hot_query_time < cold_query_time)
  assert_eq(cold_results.total_found, hot_results.total_found)
}