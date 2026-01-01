// 遥测数据生命周期管理测试
import "azimuth/azimuth"

pub test "遥测数据生命周期管理测试" {
  // 测试数据创建阶段
  let creation_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建Span
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "lifecycle-tracer")
  let span = azimuth::Tracer::start_span(tracer, "lifecycle-operation")
  
  // 创建度量
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "lifecycle-meter")
  let counter = azimuth::Meter::create_counter(meter, "lifecycle.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "lifecycle.histogram")
  
  // 创建日志记录
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "lifecycle-logger")
  let log_record = azimuth::LogRecord::new(azimuth::Info, "Lifecycle test log")
  
  let creation_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let creation_duration = creation_end_time - creation_start_time
  
  // 验证创建阶段
  assert_true(creation_duration > 0L)
  assert_eq(azimuth::Span::name(span), "lifecycle-operation")
  assert_eq(counter.name, "lifecycle.counter")
  assert_eq(histogram.name, "lifecycle.histogram")
  assert_eq(logger.scope.name, "lifecycle-logger")
  
  // 测试数据更新阶段
  let update_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 更新Span
  azimuth::Span::add_event(span, "lifecycle.event", Some([
    ("phase", azimuth::StringValue("update")),
    ("timestamp", azimuth::StringValue("2025-01-01T12:00:00Z"))
  ]))
  azimuth::Span::set_status(span, azimuth::Ok)
  
  // 更新度量
  azimuth::Counter::add(counter, 1.0)
  azimuth::Histogram::record(histogram, 100.0)
  
  // 更新日志记录
  let updated_log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Updated lifecycle log"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(span))),
    Some(azimuth::Context::root())
  )
  
  let update_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let update_duration = update_end_time - update_start_time
  
  // 验证更新阶段
  assert_true(update_duration > 0L)
  
  // 测试数据持久化阶段
  let persistence_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 模拟数据持久化操作
  azimuth::Logger::emit(logger, updated_log_record)
  
  // 结束Span以触发持久化
  azimuth::Span::end(span)
  
  let persistence_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let persistence_duration = persistence_end_time - persistence_start_time
  
  // 验证持久化阶段
  assert_true(persistence_duration > 0L)
  
  // 测试数据归档阶段
  let archive_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建归档相关的资源
  let archive_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("archive.policy", azimuth::StringValue("30-days")),
    ("archive.storage", azimuth::StringValue("cold-storage")),
    ("archive.compressed", azimuth::BoolValue(true))
  ])
  
  let archive_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let archive_duration = archive_end_time - archive_start_time
  
  // 验证归档阶段
  assert_true(archive_duration > 0L)
  assert_eq(azimuth::Resource::get_attribute(archive_resource, "archive.policy"), Some(azimuth::StringValue("30-days")))
  
  // 测试数据清理阶段
  let cleanup_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 模拟数据清理操作
  let cleanup_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("cleanup.policy", azimuth::StringValue("90-days")),
    ("cleanup.method", azimuth::StringValue("secure-delete")),
    ("cleanup.compliant", azimuth::BoolValue(true))
  ])
  
  let cleanup_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let cleanup_duration = cleanup_end_time - cleanup_start_time
  
  // 验证清理阶段
  assert_true(cleanup_duration > 0L)
  assert_eq(azimuth::Resource::get_attribute(cleanup_resource, "cleanup.policy"), Some(azimuth::StringValue("90-days")))
  
  // 测试完整生命周期时间
  let full_lifecycle_duration = cleanup_end_time - creation_start_time
  
  // 验证完整生命周期时间
  assert_true(full_lifecycle_duration > 0L)
  
  // 测试数据版本控制
  let version_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let version_1_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("data.version", azimuth::StringValue("1.0")),
    ("data.created", azimuth::StringValue("2025-01-01T12:00:00Z"))
  ])
  
  let version_2_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("data.version", azimuth::StringValue("2.0")),
    ("data.created", azimuth::StringValue("2025-01-01T12:30:00Z")),
    ("data.previous.version", azimuth::StringValue("1.0"))
  ])
  
  let version_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let version_duration = version_end_time - version_start_time
  
  // 验证版本控制
  assert_true(version_duration > 0L)
  assert_eq(azimuth::Resource::get_attribute(version_1_resource, "data.version"), Some(azimuth::StringValue("1.0")))
  assert_eq(azimuth::Resource::get_attribute(version_2_resource, "data.version"), Some(azimuth::StringValue("2.0")))
  
  // 测试数据保留策略
  let retention_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let retention_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("retention.period.days", azimuth::IntValue(365)),
    ("retention.policy", azimuth::StringValue("compliance-driven")),
    ("retention.auto.delete", azimuth::BoolValue(true))
  ])
  
  let retention_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let retention_duration = retention_end_time - retention_start_time
  
  // 验证保留策略
  assert_true(retention_duration > 0L)
  assert_eq(azimuth::Resource::get_attribute(retention_resource, "retention.period.days"), Some(azimuth::IntValue(365)))
  
  // 测试数据迁移
  let migration_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let source_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("storage.system", azimuth::StringValue("legacy")),
    ("data.format", azimuth::StringValue("old-format"))
  ])
  
  let target_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("storage.system", azimuth::StringValue("modern")),
    ("data.format", azimuth::StringValue("new-format")),
    ("migration.source", azimuth::StringValue("legacy"))
  ])
  
  let migration_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let migration_duration = migration_end_time - migration_start_time
  
  // 验证数据迁移
  assert_true(migration_duration > 0L)
  assert_eq(azimuth::Resource::get_attribute(target_resource, "migration.source"), Some(azimuth::StringValue("legacy")))
  
  // 测试数据备份
  let backup_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let backup_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("backup.frequency", azimuth::StringValue("daily")),
    ("backup.location", azimuth::StringValue("secure-backup-storage")),
    ("backup.encrypted", azimuth::BoolValue(true)),
    ("backup.retention", azimuth::StringValue("30-days"))
  ])
  
  let backup_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let backup_duration = backup_end_time - backup_start_time
  
  // 验证数据备份
  assert_true(backup_duration > 0L)
  assert_eq(azimuth::Resource::get_attribute(backup_resource, "backup.frequency"), Some(azimuth::StringValue("daily")))
  
  // 测试数据恢复
  let recovery_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let recovery_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("recovery.point", azimuth::StringValue("2025-01-01T12:00:00Z")),
    ("recovery.source", azimuth::StringValue("backup")),
    ("recovery.status", azimuth::StringValue("success"))
  ])
  
  let recovery_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let recovery_duration = recovery_end_time - recovery_start_time
  
  // 验证数据恢复
  assert_true(recovery_duration > 0L)
  assert_eq(azimuth::Resource::get_attribute(recovery_resource, "recovery.status"), Some(azimuth::StringValue("success")))
  
  // 测试生命周期度量
  let lifecycle_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "lifecycle-metrics")
  
  let creation_time_histogram = azimuth::Meter::create_histogram(lifecycle_meter, "lifecycle.creation.time")
  let update_time_histogram = azimuth::Meter::create_histogram(lifecycle_meter, "lifecycle.update.time")
  let persistence_time_histogram = azimuth::Meter::create_histogram(lifecycle_meter, "lifecycle.persistence.time")
  let archive_time_histogram = azimuth::Meter::create_histogram(lifecycle_meter, "lifecycle.archive.time")
  let cleanup_time_histogram = azimuth::Meter::create_histogram(lifecycle_meter, "lifecycle.cleanup.time")
  
  // 记录生命周期各阶段的时间
  azimuth::Histogram::record(creation_time_histogram, creation_duration.to_double())
  azimuth::Histogram::record(update_time_histogram, update_duration.to_double())
  azimuth::Histogram::record(persistence_time_histogram, persistence_duration.to_double())
  azimuth::Histogram::record(archive_time_histogram, archive_duration.to_double())
  azimuth::Histogram::record(cleanup_time_histogram, cleanup_duration.to_double())
  
  // 验证生命周期度量
  assert_eq(creation_time_histogram.name, "lifecycle.creation.time")
  assert_eq(update_time_histogram.name, "lifecycle.update.time")
  assert_eq(persistence_time_histogram.name, "lifecycle.persistence.time")
  assert_eq(archive_time_histogram.name, "lifecycle.archive.time")
  assert_eq(cleanup_time_histogram.name, "lifecycle.cleanup.time")
}