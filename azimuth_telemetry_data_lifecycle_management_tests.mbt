// Azimuth Telemetry Data Lifecycle Management Tests
// 遥测数据生命周期管理测试用例
// 测试遥测数据的创建、存储、检索、归档和销毁的完整生命周期

import "azimuth/azimuth"

// Test 1: 遥测数据创建和初始存储测试
pub test "遥测数据创建和初始存储测试" {
  // 创建生命周期管理器
  let lifecycle_manager = azimuth::TelemetryDataLifecycleManager::new()
  
  // 配置数据保留策略
  azimuth::TelemetryDataLifecycleManager::set_retention_policy(lifecycle_manager, {
    "spans.hot.ttl": "7d",        // Span热数据保留7天
    "spans.warm.ttl": "30d",      // Span温数据保留30天
    "spans.cold.ttl": "90d",      // Span冷数据保留90天
    "metrics.hot.ttl": "14d",     // 度量热数据保留14天
    "metrics.warm.ttl": "60d",    // 度量温数据保留60天
    "metrics.cold.ttl": "180d",   // 度量冷数据保留180天
    "logs.hot.ttl": "3d",         // 日志热数据保留3天
    "logs.warm.ttl": "21d",       // 日志温数据保留21天
    "logs.cold.ttl": "60d"        // 日志冷数据保留60天
  })
  
  // 创建测试数据
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "lifecycle-test")
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "lifecycle-test")
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "lifecycle-test")
  
  // 创建Span数据
  let spans = []
  for i in 0..100 {
    let span = azimuth::Tracer::start_span(tracer, "lifecycle-span-" + i.to_string())
    azimuth::Span::add_event(span, "lifecycle.test.event", Some([
      ("span.index", azimuth::IntValue(i)),
      ("test.phase", azimuth::StringValue("creation"))
    ]))
    azimuth::Span::set_status(span, azimuth::Ok)
    azimuth::Span::end(span)
    
    spans.push(span)
  }
  
  // 创建度量数据
  let metrics = []
  let counter = azimuth::Meter::create_counter(meter, "lifecycle.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "lifecycle.histogram", Some("Lifecycle histogram"), Some("ms"))
  let gauge = azimuth::Meter::create_gauge(meter, "lifecycle.gauge")
  
  for i in 0..200 {
    azimuth::Counter::add(counter, i.to_double(), Some([
      ("metric.index", azimuth::IntValue(i))
    ]))
    
    azimuth::Histogram::record(histogram, i.to_double(), Some([
      ("metric.index", azimuth::IntValue(i))
    ]))
    
    azimuth::Gauge::record(gauge, 50.0 + azimuth::Math::sin(i.to_double() * 0.1) * 20.0, Some([
      ("metric.index", azimuth::IntValue(i))
    ]))
    
    metrics.push({
      "index": i,
      "counter": i.to_double(),
      "histogram": i.to_double(),
      "gauge": 50.0 + azimuth::Math::sin(i.to_double() * 0.1) * 20.0
    })
  }
  
  // 创建日志数据
  let logs = []
  for i in 0..300 {
    let severity = match i % 5 {
      0 => azimuth::Trace,
      1 => azimuth::Debug,
      2 => azimuth::Info,
      3 => azimuth::Warn,
      _ => azimuth::Error
    }
    
    let log_record = azimuth::LogRecord::new(severity, "Lifecycle test log " + i.to_string())
    azimuth::Logger::emit(logger, log_record)
    
    logs.push({
      "index": i,
      "severity": severity,
      "message": "Lifecycle test log " + i.to_string()
    })
  }
  
  // 初始存储数据
  let storage_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let span_storage_result = azimuth::TelemetryDataLifecycleManager::store_spans(lifecycle_manager, spans)
  let metric_storage_result = azimuth::TelemetryDataLifecycleManager::store_metrics(lifecycle_manager, metrics)
  let log_storage_result = azimuth::TelemetryDataLifecycleManager::store_logs(lifecycle_manager, logs)
  
  let storage_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let storage_duration = storage_end_time - storage_start_time
  
  // 验证存储结果
  assert_true(azimuth::StorageResult::is_success(span_storage_result))
  assert_true(azimuth::StorageResult::is_success(metric_storage_result))
  assert_true(azimuth::StorageResult::is_success(log_storage_result))
  
  assert_eq(azimuth::StorageResult::get_stored_count(span_storage_result), spans.length())
  assert_eq(azimuth::StorageResult::get_stored_count(metric_storage_result), metrics.length())
  assert_eq(azimuth::StorageResult::get_stored_count(log_storage_result), logs.length())
  
  // 验证数据分布在不同存储层
  let span_distribution = azimuth::TelemetryDataLifecycleManager::get_data_distribution(lifecycle_manager, "spans")
  let metric_distribution = azimuth::TelemetryDataLifecycleManager::get_data_distribution(lifecycle_manager, "metrics")
  let log_distribution = azimuth::TelemetryDataLifecycleManager::get_data_distribution(lifecycle_manager, "logs")
  
  // 新数据应该主要在热存储层
  assert_true(azimuth::DataDistribution::get_hot_count(span_distribution) > 0)
  assert_true(azimuth::DataDistribution::get_hot_count(metric_distribution) > 0)
  assert_true(azimuth::DataDistribution::get_hot_count(log_distribution) > 0)
  
  // 创建存储性能度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "lifecycle-storage")
  let storage_rate_gauge = azimuth::Meter::create_gauge(meter, "data.storage.rate")
  let storage_latency_histogram = azimuth::Meter::create_histogram(meter, "storage.latency", Some("Data storage latency"), Some("ms"))
  
  let total_stored = spans.length() + metrics.length() + logs.length()
  let storage_rate = total_stored.to_double() / (storage_duration.to_double() / 1000000000.0)  // 数据点/秒
  
  azimuth::Gauge::record(storage_rate_gauge, storage_rate)
  azimuth::Histogram::record(storage_latency_histogram, storage_duration.to_double() / 1000000.0)
  
  // 验证存储性能
  assert_true(storage_rate > 100.0)  // 至少每秒存储100个数据点
}

// Test 2: 遥测数据分层存储和迁移测试
pub test "遥测数据分层存储和迁移测试" {
  // 创建生命周期管理器
  let lifecycle_manager = azimuth::TelemetryDataLifecycleManager::new()
  
  // 配置数据迁移策略
  azimuth::TelemetryDataLifecycleManager::set_migration_policy(lifecycle_manager, {
    "hot.to.warm.threshold": "7d",   // 7天后从热存储迁移到温存储
    "warm.to.cold.threshold": "30d", // 30天后从温存储迁移到冷存储
    "cold.to.archive.threshold": "90d", // 90天后从冷存储迁移到归档
    "archive.retention": "365d"      // 归档数据保留1年
  })
  
  // 创建不同时间的数据
  let current_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let day_in_nanos = 24 * 60 * 60 * 1000000000L
  
  let data_batches = [
    {
      "name": "recent.data",
      "age_days": 1,
      "expected_tier": "hot"
    },
    {
      "name": "warm.data",
      "age_days": 10,
      "expected_tier": "warm"
    },
    {
      "name": "cold.data",
      "age_days": 35,
      "expected_tier": "cold"
    },
    {
      "name": "archive.data",
      "age_days": 100,
      "expected_tier": "archive"
    }
  ]
  
  for batch in data_batches {
    let batch_timestamp = current_time - (batch["age_days"] * day_in_nanos)
    
    // 创建不同类型的数据
    let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), batch["name"])
    let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), batch["name"])
    let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), batch["name"])
    
    // 创建Span数据
    let spans = []
    for i in 0..50 {
      let span = azimuth::Tracer::start_span(tracer, batch["name"] + "-span-" + i.to_string())
      azimuth::Span::add_event(span, "test.event", Some([
        ("batch.name", azimuth::StringValue(batch["name"])),
        ("data.age", azimuth::IntValue(batch["age_days"]))
      ]))
      azimuth::Span::end(span)
      spans.push(span)
    }
    
    // 创建度量数据
    let metrics = []
    let counter = azimuth::Meter::create_counter(meter, batch["name"] + ".counter")
    for i in 0..50 {
      azimuth::Counter::add(counter, i.to_double())
      metrics.push({
        "index": i,
        "value": i.to_double(),
        "timestamp": batch_timestamp + (i * 1000000L)
      })
    }
    
    // 创建日志数据
    let logs = []
    for i in 0..50 {
      let log_record = azimuth::LogRecord::new(azimuth::Info, batch["name"] + " log " + i.to_string())
      azimuth::Logger::emit(logger, log_record)
      logs.push({
        "index": i,
        "message": batch["name"] + " log " + i.to_string(),
        "timestamp": batch_timestamp + (i * 1000000L)
      })
    }
    
    // 存储数据（使用模拟的时间戳）
    azimuth::TelemetryDataLifecycleManager::store_spans_with_timestamp(lifecycle_manager, spans, batch_timestamp)
    azimuth::TelemetryDataLifecycleManager::store_metrics_with_timestamp(lifecycle_manager, metrics, batch_timestamp)
    azimuth::TelemetryDataLifecycleManager::store_logs_with_timestamp(lifecycle_manager, logs, batch_timestamp)
  }
  
  // 执行数据迁移
  let migration_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let migration_result = azimuth::TelemetryDataLifecycleManager::execute_migration(lifecycle_manager)
  
  let migration_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let migration_duration = migration_end_time - migration_start_time
  
  // 验证迁移结果
  assert_true(azimuth::MigrationResult::is_success(migration_result))
  
  // 验证数据分布
  for batch in data_batches {
    let span_distribution = azimuth::TelemetryDataLifecycleManager::get_data_distribution_by_age(lifecycle_manager, "spans", batch["age_days"])
    let metric_distribution = azimuth::TelemetryDataLifecycleManager::get_data_distribution_by_age(lifecycle_manager, "metrics", batch["age_days"])
    let log_distribution = azimuth::TelemetryDataLifecycleManager::get_data_distribution_by_age(lifecycle_manager, "logs", batch["age_days"])
    
    // 验证数据在正确的存储层
    if batch["expected_tier"] == "hot" {
      assert_true(azimuth::DataDistribution::get_hot_count(span_distribution) > 0)
      assert_true(azimuth::DataDistribution::get_hot_count(metric_distribution) > 0)
      assert_true(azimuth::DataDistribution::get_hot_count(log_distribution) > 0)
    } else if batch["expected_tier"] == "warm" {
      assert_true(azimuth::DataDistribution::get_warm_count(span_distribution) > 0)
      assert_true(azimuth::DataDistribution::get_warm_count(metric_distribution) > 0)
      assert_true(azimuth::DataDistribution::get_warm_count(log_distribution) > 0)
    } else if batch["expected_tier"] == "cold" {
      assert_true(azimuth::DataDistribution::get_cold_count(span_distribution) > 0)
      assert_true(azimuth::DataDistribution::get_cold_count(metric_distribution) > 0)
      assert_true(azimuth::DataDistribution::get_cold_count(log_distribution) > 0)
    } else if batch["expected_tier"] == "archive" {
      assert_true(azimuth::DataDistribution::get_archive_count(span_distribution) > 0)
      assert_true(azimuth::DataDistribution::get_archive_count(metric_distribution) > 0)
      assert_true(azimuth::DataDistribution::get_archive_count(log_distribution) > 0)
    }
  }
  
  // 创建迁移性能度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "lifecycle-migration")
  let migration_rate_gauge = azimuth::Meter::create_gauge(meter, "data.migration.rate")
  let migration_latency_histogram = azimuth::Meter::create_histogram(meter, "migration.latency", Some("Data migration latency"), Some("ms"))
  
  let migrated_items = azimuth::MigrationResult::get_migrated_count(migration_result)
  let migration_rate = migrated_items.to_double() / (migration_duration.to_double() / 1000000000.0)  // 数据点/秒
  
  azimuth::Gauge::record(migration_rate_gauge, migration_rate)
  azimuth::Histogram::record(migration_latency_histogram, migration_duration.to_double() / 1000000.0)
  
  // 验证迁移性能
  assert_true(migration_rate > 10.0)  // 至少每秒迁移10个数据点
}

// Test 3: 遥测数据检索和查询测试
pub test "遥测数据检索和查询测试" {
  // 创建生命周期管理器
  let lifecycle_manager = azimuth::TelemetryDataLifecycleManager::new()
  
  // 创建测试数据
  let current_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let day_in_nanos = 24 * 60 * 60 * 1000000000L
  
  // 创建不同时间的数据
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "retrieval-test")
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "retrieval-test")
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "retrieval-test")
  
  // 创建Span数据
  let spans = []
  for i in 0..200 {
    let span_timestamp = current_time - (i * day_in_nanos / 10)  // 分布在过去20天
    let span = azimuth::Tracer::start_span(tracer, "retrieval-span-" + i.to_string())
    azimuth::Span::add_event(span, "retrieval.test.event", Some([
      ("span.index", azimuth::IntValue(i)),
      ("test.category", azimuth::StringValue(match i % 5 {
        0 => "performance",
        1 => "error",
        2 => "business",
        3 => "security",
        _ => "infrastructure"
      }))
    ]))
    azimuth::Span::end(span)
    spans.push((span, span_timestamp))
  }
  
  // 创建度量数据
  let metrics = []
  let counter = azimuth::Meter::create_counter(meter, "retrieval.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "retrieval.histogram", Some("Retrieval histogram"), Some("ms"))
  
  for i in 0..300 {
    let metric_timestamp = current_time - (i * day_in_nanos / 15)  // 分布在过去20天
    let category = match i % 4 {
      0 => "system",
      1 => "application",
      2 => "network",
      _ => "database"
    }
    
    azimuth::Counter::add(counter, i.to_double(), Some([
      ("metric.index", azimuth::IntValue(i)),
      ("metric.category", azimuth::StringValue(category))
    ]))
    
    azimuth::Histogram::record(histogram, i.to_double(), Some([
      ("metric.index", azimuth::IntValue(i)),
      ("metric.category", azimuth::StringValue(category))
    ]))
    
    metrics.push({
      "index": i,
      "value": i.to_double(),
      "category": category,
      "timestamp": metric_timestamp
    })
  }
  
  // 创建日志数据
  let logs = []
  for i in 0..400 {
    let log_timestamp = current_time - (i * day_in_nanos / 20)  // 分布在过去20天
    let severity = match i % 5 {
      0 => azimuth::Trace,
      1 => azimuth::Debug,
      2 => azimuth::Info,
      3 => azimuth::Warn,
      _ => azimuth::Error
    }
    
    let service = "service-" + (i % 10).to_string()
    
    let log_record = azimuth::LogRecord::new(severity, "Retrieval test log " + i.to_string())
    azimuth::Logger::emit(logger, log_record)
    
    logs.push({
      "index": i,
      "severity": severity,
      "service": service,
      "message": "Retrieval test log " + i.to_string(),
      "timestamp": log_timestamp
    })
  }
  
  // 存储数据
  for (span, timestamp) in spans {
    azimuth::TelemetryDataLifecycleManager::store_spans_with_timestamp(lifecycle_manager, [span], timestamp)
  }
  
  azimuth::TelemetryDataLifecycleManager::store_metrics_with_timestamp(lifecycle_manager, metrics, current_time)
  azimuth::TelemetryDataLifecycleManager::store_logs_with_timestamp(lifecycle_manager, logs, current_time)
  
  // 执行数据迁移以确保数据分布在不同的存储层
  azimuth::TelemetryDataLifecycleManager::execute_migration(lifecycle_manager)
  
  // 测试跨存储层查询
  let query_tests = [
    {
      "name": "recent.spans.query",
      "type": "spans",
      "time_range": "1d",
      "expected_min_results": 10
    },
    {
      "name": "old.spans.query",
      "type": "spans",
      "time_range": "15d",
      "expected_min_results": 100
    },
    {
      "name": "all.spans.query",
      "type": "spans",
      "time_range": "20d",
      "expected_min_results": 150
    },
    {
      "name": "recent.metrics.query",
      "type": "metrics",
      "time_range": "1d",
      "expected_min_results": 15
    },
    {
      "name": "old.metrics.query",
      "type": "metrics",
      "time_range": "15d",
      "expected_min_results": 200
    },
    {
      "name": "all.metrics.query",
      "type": "metrics",
      "time_range": "20d",
      "expected_min_results": 250
    },
    {
      "name": "recent.logs.query",
      "type": "logs",
      "time_range": "1d",
      "expected_min_results": 20
    },
    {
      "name": "old.logs.query",
      "type": "logs",
      "time_range": "15d",
      "expected_min_results": 250
    },
    {
      "name": "all.logs.query",
      "type": "logs",
      "time_range": "20d",
      "expected_min_results": 300
    }
  ]
  
  for query_test in query_tests {
    let query_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    let query_result = match query_test["type"] {
      "spans" => azimuth::TelemetryDataLifecycleManager::query_spans(lifecycle_manager, query_test["time_range"]),
      "metrics" => azimuth::TelemetryDataLifecycleManager::query_metrics(lifecycle_manager, query_test["time_range"]),
      "logs" => azimuth::TelemetryDataLifecycleManager::query_logs(lifecycle_manager, query_test["time_range"]),
      _ => azimuth::QueryResult::empty()
    }
    
    let query_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let query_duration = query_end_time - query_start_time
    
    // 验证查询结果
    assert_true(azimuth::QueryResult::is_success(query_result))
    assert_true(azimuth::QueryResult::get_result_count(query_result) >= query_test["expected_min_results"])
    
    // 验证查询时间戳
    let results = azimuth::QueryResult::get_results(query_result)
    for result in results {
      assert_true(result["timestamp"] >= current_time - parse_time_range(query_test["time_range"]))
      assert_true(result["timestamp"] <= current_time)
    }
    
    // 记录查询性能
    let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "lifecycle-query")
    let query_latency_histogram = azimuth::Meter::create_histogram(meter, "query.latency", Some("Query latency"), Some("ms"))
    let query_result_count_histogram = azimuth::Meter::create_histogram(meter, "query.result.count", Some("Query result count"), None)
    
    azimuth::Histogram::record(query_latency_histogram, query_duration.to_double() / 1000000.0, Some([
      ("query.type", azimuth::StringValue(query_test["type"])),
      ("time.range", azimuth::StringValue(query_test["time_range"]))
    ]))
    
    azimuth::Histogram::record(query_result_count_histogram, azimuth::QueryResult::get_result_count(query_result).to_double(), Some([
      ("query.type", azimuth::StringValue(query_test["type"])),
      ("time.range", azimuth::StringValue(query_test["time_range"]))
    ]))
  }
}

// Test 4: 遥测数据归档和销毁测试
pub test "遥测数据归档和销毁测试" {
  // 创建生命周期管理器
  let lifecycle_manager = azimuth::TelemetryDataLifecycleManager::new()
  
  // 配置归档和销毁策略
  azimuth::TelemetryDataLifecycleManager::set_archive_policy(lifecycle_manager, {
    "archive.compression": "gzip",
    "archive.format": "json",
    "archive.encryption": "aes256"
  })
  
  azimuth::TelemetryDataLifecycleManager::set_destruction_policy(lifecycle_manager, {
    "destruction.method": "secure",
    "destruction.confirmation": "required",
    "destruction.audit": "enabled"
  })
  
  // 创建过期数据
  let current_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let day_in_nanos = 24 * 60 * 60 * 1000000000L
  
  // 创建超过归档期限的数据（100天前）
  let archive_data_timestamp = current_time - (100 * day_in_nanos)
  
  // 创建超过销毁期限的数据（400天前）
  let destruction_data_timestamp = current_time - (400 * day_in_nanos)
  
  // 创建应归档的数据
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "archive-test")
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "archive-test")
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "archive-test")
  
  let archive_spans = []
  let archive_metrics = []
  let archive_logs = []
  
  for i in 0..50 {
    let span = azimuth::Tracer::start_span(tracer, "archive-span-" + i.to_string())
    azimuth::Span::add_event(span, "archive.test.event", Some([
      ("span.index", azimuth::IntValue(i))
    ]))
    azimuth::Span::end(span)
    archive_spans.push(span)
    
    let counter = azimuth::Meter::create_counter(meter, "archive.counter")
    azimuth::Counter::add(counter, i.to_double())
    archive_metrics.push({
      "index": i,
      "value": i.to_double()
    })
    
    let log_record = azimuth::LogRecord::new(azimuth::Info, "Archive test log " + i.to_string())
    azimuth::Logger::emit(logger, log_record)
    archive_logs.push({
      "index": i,
      "message": "Archive test log " + i.to_string()
    })
  }
  
  // 创建应销毁的数据
  let destruction_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "destruction-test")
  let destruction_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "destruction-test")
  let destruction_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "destruction-test")
  
  let destruction_spans = []
  let destruction_metrics = []
  let destruction_logs = []
  
  for i in 0..30 {
    let span = azimuth::Tracer::start_span(destruction_tracer, "destruction-span-" + i.to_string())
    azimuth::Span::add_event(span, "destruction.test.event", Some([
      ("span.index", azimuth::IntValue(i))
    ]))
    azimuth::Span::end(span)
    destruction_spans.push(span)
    
    let counter = azimuth::Meter::create_counter(destruction_meter, "destruction.counter")
    azimuth::Counter::add(counter, i.to_double())
    destruction_metrics.push({
      "index": i,
      "value": i.to_double()
    })
    
    let log_record = azimuth::LogRecord::new(azimuth::Info, "Destruction test log " + i.to_string())
    azimuth::Logger::emit(destruction_logger, log_record)
    destruction_logs.push({
      "index": i,
      "message": "Destruction test log " + i.to_string()
    })
  }
  
  // 存储数据
  azimuth::TelemetryDataLifecycleManager::store_spans_with_timestamp(lifecycle_manager, archive_spans, archive_data_timestamp)
  azimuth::TelemetryDataLifecycleManager::store_metrics_with_timestamp(lifecycle_manager, archive_metrics, archive_data_timestamp)
  azimuth::TelemetryDataLifecycleManager::store_logs_with_timestamp(lifecycle_manager, archive_logs, archive_data_timestamp)
  
  azimuth::TelemetryDataLifecycleManager::store_spans_with_timestamp(lifecycle_manager, destruction_spans, destruction_data_timestamp)
  azimuth::TelemetryDataLifecycleManager::store_metrics_with_timestamp(lifecycle_manager, destruction_metrics, destruction_data_timestamp)
  azimuth::TelemetryDataLifecycleManager::store_logs_with_timestamp(lifecycle_manager, destruction_logs, destruction_data_timestamp)
  
  // 执行数据迁移
  azimuth::TelemetryDataLifecycleManager::execute_migration(lifecycle_manager)
  
  // 验证数据在正确的存储层
  let archive_span_distribution = azimuth::TelemetryDataLifecycleManager::get_data_distribution_by_age(lifecycle_manager, "spans", 100)
  let destruction_span_distribution = azimuth::TelemetryDataLifecycleManager::get_data_distribution_by_age(lifecycle_manager, "spans", 400)
  
  // 归档数据应该在归档存储层
  assert_true(azimuth::DataDistribution::get_archive_count(archive_span_distribution) > 0)
  
  // 销毁数据应该已经不存在
  assert_true(azimuth::DataDistribution::get_total_count(destruction_span_distribution) == 0)
  
  // 执行归档操作
  let archive_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let archive_result = azimuth::TelemetryDataLifecycleManager::execute_archive(lifecycle_manager)
  
  let archive_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let archive_duration = archive_end_time - archive_start_time
  
  // 验证归档结果
  assert_true(azimuth::ArchiveResult::is_success(archive_result))
  assert_true(azimuth::ArchiveResult::get_archived_count(archive_result) > 0)
  
  // 验证归档文件存在
  let archive_files = azimuth::TelemetryDataLifecycleManager::get_archive_files(lifecycle_manager)
  assert_true(archive_files.length() > 0)
  
  // 执行销毁操作
  let destruction_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let destruction_result = azimuth::TelemetryDataLifecycleManager::execute_destruction(lifecycle_manager)
  
  let destruction_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let destruction_duration = destruction_end_time - destruction_start_time
  
  // 验证销毁结果
  assert_true(azimuth::DestructionResult::is_success(destruction_result))
  assert_true(azimuth::DestructionResult::get_destroyed_count(destruction_result) > 0)
  
  // 验证销毁审计日志
  let audit_logs = azimuth::TelemetryDataLifecycleManager::get_destruction_audit_logs(lifecycle_manager)
  assert_true(audit_logs.length() > 0)
  
  // 创建归档和销毁性能度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "lifecycle-cleanup")
  let archive_rate_gauge = azimuth::Meter::create_gauge(meter, "data.archive.rate")
  let destruction_rate_gauge = azimuth::Meter::create_gauge(meter, "data.destruction.rate")
  let archive_latency_histogram = azimuth::Meter::create_histogram(meter, "archive.latency", Some("Archive latency"), Some("ms"))
  let destruction_latency_histogram = azimuth::Meter::create_histogram(meter, "destruction.latency", Some("Destruction latency"), Some("ms"))
  
  let archived_count = azimuth::ArchiveResult::get_archived_count(archive_result)
  let destroyed_count = azimuth::DestructionResult::get_destroyed_count(destruction_result)
  
  let archive_rate = archived_count.to_double() / (archive_duration.to_double() / 1000000000.0)
  let destruction_rate = destroyed_count.to_double() / (destruction_duration.to_double() / 1000000000.0)
  
  azimuth::Gauge::record(archive_rate_gauge, archive_rate)
  azimuth::Gauge::record(destruction_rate_gauge, destruction_rate)
  azimuth::Histogram::record(archive_latency_histogram, archive_duration.to_double() / 1000000.0)
  azimuth::Histogram::record(destruction_latency_histogram, destruction_duration.to_double() / 1000000.0)
  
  // 验证归档和销毁性能
  assert_true(archive_rate > 5.0)  // 至少每秒归档5个数据点
  assert_true(destruction_rate > 10.0)  // 至少每秒销毁10个数据点
}

// 辅助函数：解析时间范围字符串
fn parse_time_range(time_range : String) -> Int {
  match time_range.substring(time_range.length() - 1, time_range.length()) {
    "d" => {
      let days = Int::from_string(time_range.substring(0, time_range.length() - 1))
      days * 24 * 60 * 60 * 1000000000L
    }
    "h" => {
      let hours = Int::from_string(time_range.substring(0, time_range.length() - 1))
      hours * 60 * 60 * 1000000000L
    }
    "m" => {
      let minutes = Int::from_string(time_range.substring(0, time_range.length() - 1))
      minutes * 60 * 1000000000L
    }
    _ => 0L
  }
}