// Azimuth Premium Multi-Environment Configuration Management Test Suite
// 高级多环境配置管理和同步测试套件 - 专注于多环境配置管理和同步机制

// Test 1: 多环境配置管理
test "multi-environment configuration management" {
  // 创建多环境配置管理器
  let config_manager = @azimuth.MultiEnvironmentConfigManager::new()
  
  // 配置环境
  let environments = [
    @azimuth.Environment::new(
      "development",
      "开发环境",
      @azimuth.EnvironmentType::Development,
      @azimuth.EnvironmentTier::Basic,
      ["dev-team@example.com"],
      true // 启用
    ),
    @azimuth.Environment::new(
      "staging",
      "预发布环境",
      @azimuth.EnvironmentType::Staging,
      @azimuth.EnvironmentTier::Standard,
      ["qa-team@example.com", "dev-team@example.com"],
      true
    ),
    @azimuth.Environment::new(
      "production",
      "生产环境",
      @azimuth.EnvironmentType::Production,
      @azimuth.EnvironmentTier::Premium,
      ["ops-team@example.com", "management@example.com"],
      true
    ),
    @azimuth.Environment::new(
      "disaster_recovery",
      "灾难恢复环境",
      @azimuth.EnvironmentType::DisasterRecovery,
      @azimuth.EnvironmentTier::Standard,
      ["ops-team@example.com", "dr-team@example.com"],
      false // 按需启用
    )
  ]
  
  @azimuth.MultiEnvironmentConfigManager::configure_environments(config_manager, environments)
  
  // 配置配置模板
  let config_templates = [
    @azimuth.ConfigTemplate::new(
      "database_config",
      "数据库配置",
      [
        @azimuth.ConfigField::new("host", "主机地址", @azimuth.FieldType::String, true),
        @azimuth.ConfigField::new("port", "端口", @azimuth.FieldType::Integer, true),
        @azimuth.ConfigField::new("username", "用户名", @azimuth.FieldType::String, true),
        @azimuth.ConfigField::new("password", "密码", @azimuth.FieldType::Secret, true),
        @azimuth.ConfigField::new("max_connections", "最大连接数", @azimuth.FieldType::Integer, false),
        @azimuth.ConfigField::new("connection_timeout", "连接超时", @azimuth.FieldType::Integer, false),
        @azimuth.ConfigField::new("ssl_enabled", "启用SSL", @azimuth.FieldType::Boolean, false)
      ]
    ),
    @azimuth.ConfigTemplate::new(
      "service_config",
      "服务配置",
      [
        @azimuth.ConfigField::new("service_name", "服务名称", @azimuth.FieldType::String, true),
        @azimuth.ConfigField::new("version", "版本", @azimuth.FieldType::String, true),
        @azimuth.ConfigField::new("port", "端口", @azimuth.FieldType::Integer, true),
        @azimuth.ConfigField::new("log_level", "日志级别", @azimuth.FieldType::String, false),
        @azimuth.ConfigField::new("max_workers", "最大工作线程", @azimuth.FieldType::Integer, false),
        @azimuth.ConfigField::new("health_check_interval", "健康检查间隔", @azimuth.FieldType::Integer, false),
        @azimuth.ConfigField::new("metrics_enabled", "启用指标", @azimuth.FieldType::Boolean, false)
      ]
    ),
    @azimuth.ConfigTemplate::new(
      "feature_flags",
      "功能开关",
      [
        @azimuth.ConfigField::new("new_ui_enabled", "启用新UI", @azimuth.FieldType::Boolean, false),
        @azimuth.ConfigField::new("advanced_analytics", "高级分析", @azimuth.FieldType::Boolean, false),
        @azimuth.ConfigField::new("beta_features", "Beta功能", @azimuth.FieldType::Boolean, false),
        @azimuth.ConfigField::new("debug_mode", "调试模式", @azimuth.FieldType::Boolean, false)
      ]
    )
  ]
  
  @azimuth.MultiEnvironmentConfigManager::configure_templates(config_manager, config_templates)
  
  // 创建环境特定配置
  let dev_database_config = @azimuth.EnvironmentConfig::new("development", "database_config", [
    ("host", @azimuth.StringValue("dev-db.example.com")),
    ("port", @azimuth.IntValue(5432)),
    ("username", @azimuth.StringValue("dev_user")),
    ("password", @azimuth.SecretValue("dev_password_123")),
    ("max_connections", @azimuth.IntValue(10)),
    ("connection_timeout", @azimuth.IntValue(30)),
    ("ssl_enabled", @azimuth.BoolValue(false))
  ])
  
  let staging_database_config = @azimuth.EnvironmentConfig::new("staging", "database_config", [
    ("host", @azimuth.StringValue("staging-db.example.com")),
    ("port", @azimuth.IntValue(5432)),
    ("username", @azimuth.StringValue("staging_user")),
    ("password", @azimuth.SecretValue("staging_password_456")),
    ("max_connections", @azimuth.IntValue(50)),
    ("connection_timeout", @azimuth.IntValue(20)),
    ("ssl_enabled", @azimuth.BoolValue(true))
  ])
  
  let prod_database_config = @azimuth.EnvironmentConfig::new("production", "database_config", [
    ("host", @azimuth.StringValue("prod-db.example.com")),
    ("port", @azimuth.IntValue(5432)),
    ("username", @azimuth.StringValue("prod_user")),
    ("password", @azimuth.SecretValue("prod_password_789")),
    ("max_connections", @azimuth.IntValue(200)),
    ("connection_timeout", @azimuth.IntValue(15)),
    ("ssl_enabled", @azimuth.BoolValue(true))
  ])
  
  let dr_database_config = @azimuth.EnvironmentConfig::new("disaster_recovery", "database_config", [
    ("host", @azimuth.StringValue("dr-db.example.com")),
    ("port", @azimuth.IntValue(5432)),
    ("username", @azimuth.StringValue("dr_user")),
    ("password", @azimuth.SecretValue("dr_password_012")),
    ("max_connections", @azimuth.IntValue(100)),
    ("connection_timeout", @azimuth.IntValue(20)),
    ("ssl_enabled", @azimuth.BoolValue(true))
  ])
  
  // 创建服务配置
  let dev_service_config = @azimuth.EnvironmentConfig::new("development", "service_config", [
    ("service_name", @azimuth.StringValue("azimuth-dev")),
    ("version", @azimuth.StringValue("1.0.0-dev")),
    ("port", @azimuth.IntValue(8080)),
    ("log_level", @azimuth.StringValue("DEBUG")),
    ("max_workers", @azimuth.IntValue(2)),
    ("health_check_interval", @azimuth.IntValue(60)),
    ("metrics_enabled", @azimuth.BoolValue(true))
  ])
  
  let prod_service_config = @azimuth.EnvironmentConfig::new("production", "service_config", [
    ("service_name", @azimuth.StringValue("azimuth-prod")),
    ("version", @azimuth.StringValue("1.0.0")),
    ("port", @azimuth.IntValue(8443)),
    ("log_level", @azimuth.StringValue("INFO")),
    ("max_workers", @azimuth.IntValue(8)),
    ("health_check_interval", @azimuth.IntValue(30)),
    ("metrics_enabled", @azimuth.BoolValue(true))
  ])
  
  // 创建功能开关配置
  let dev_feature_flags = @azimuth.EnvironmentConfig::new("development", "feature_flags", [
    ("new_ui_enabled", @azimuth.BoolValue(true)),
    ("advanced_analytics", @azimuth.BoolValue(true)),
    ("beta_features", @azimuth.BoolValue(true)),
    ("debug_mode", @azimuth.BoolValue(true))
  ])
  
  let prod_feature_flags = @azimuth.EnvironmentConfig::new("production", "feature_flags", [
    ("new_ui_enabled", @azimuth.BoolValue(false)),
    ("advanced_analytics", @azimuth.BoolValue(true)),
    ("beta_features", @azimuth.BoolValue(false)),
    ("debug_mode", @azimuth.BoolValue(false))
  ])
  
  // 注册所有配置
  let configs = [
    dev_database_config, staging_database_config, prod_database_config, dr_database_config,
    dev_service_config, prod_service_config,
    dev_feature_flags, prod_feature_flags
  ]
  
  for config in configs {
    @azimuth.MultiEnvironmentConfigManager::register_config(config_manager, config)
  }
  
  // 验证配置注册
  let all_configs = @azimuth.MultiEnvironmentConfigManager::get_all_configs(config_manager)
  assert_eq(all_configs.length(), 8)
  
  // 验证环境特定配置
  let dev_configs = @azimuth.MultiEnvironmentConfigManager::get_configs_by_environment(config_manager, "development")
  assert_eq(dev_configs.length(), 3)
  
  let prod_configs = @azimuth.MultiEnvironmentConfigManager::get_configs_by_environment(config_manager, "production")
  assert_eq(prod_configs.length(), 3)
  
  // 测试配置验证
  let validation_request = @azimuth.ConfigValidationRequest::new()
    .with_environments(["development", "staging", "production"])
    .with_templates(["database_config", "service_config", "feature_flags"])
    .with_validation_rules([
      @azimuth.ValidationRule::new("required_fields", "必填字段检查", @azimuth.RuleType::RequiredFields),
      @azimuth.ValidationRule::new("data_types", "数据类型检查", @azimuth.RuleType::DataTypeCheck),
      @azimuth.ValidationRule::new("value_ranges", "值范围检查", @azimuth.RuleType::ValueRange),
      @azimuth.ValidationRule::new("security", "安全检查", @azimuth.RuleType::Security)
    ])
  
  let validation_result = @azimuth.MultiEnvironmentConfigManager::validate_configs(config_manager, validation_request)
  
  // 验证配置验证结果
  assert_true(validation_result.success)
  assert_eq(validation_result.validated_configs, 8)
  assert_eq(validation_result.validation_errors.length(), 0)
  
  // 测试配置差异分析
  let diff_request = @azimuth.ConfigDiffRequest::new()
    .with_source_environment("staging")
    .with_target_environment("production")
    .with_config_types(["database_config", "service_config"])
    .with_diff_options([
      @azimuth.DiffOption::ShowValues,
      @azimuth.DiffOption::ShowTypes,
      @azimuth.DiffOption::IgnoreSecrets
    ])
  
  let diff_result = @azimuth.MultiEnvironmentConfigManager::compare_configs(config_manager, diff_request)
  
  // 验证配置差异结果
  assert_true(diff_result.success)
  assert_true(diff_result.config_differences.length() >= 2)
  
  // 验证数据库配置差异
  let db_diff = diff_result.config_differences.find(fn(diff) {
    diff.config_type == "database_config"
  })
  match db_diff {
    Some(diff) => {
      assert_true(diff.field_differences.length() >= 3) // host, username, max_connections等
      
      // 验证主机差异
      let host_diff = diff.field_differences.find_fn(field_diff) {
        field_diff.field_name == "host"
      }
      match host_diff {
        Some(field_diff) => {
          assert_eq(field_diff.source_value, @azimuth.StringValue("staging-db.example.com"))
          assert_eq(field_diff.target_value, @azimuth.StringValue("prod-db.example.com"))
          assert_eq(field_diff.difference_type, @azimuth.DifferenceType::ValueChange)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试配置继承
  let inheritance_request = @azimuth.ConfigInheritanceRequest::new()
    .with_base_environment("staging")
    .with_target_environments(["production"])
    .with_inheritance_rules([
      @azimuth.InheritanceRule::new("service_config", "port", @azimuth.InheritanceType::Override),
      @azimuth.InheritanceRule::new("service_config", "log_level", @azimuth.InheritanceType::Inherit),
      @azimuth.InheritanceRule::new("database_config", "connection_timeout", @azimuth.InheritanceType::Inherit)
    ])
  
  let inheritance_result = @azimuth.MultiEnvironmentConfigManager::apply_inheritance(config_manager, inheritance_request)
  
  // 验证配置继承结果
  assert_true(inheritance_result.success)
  assert_true(inheritance_result.applied_inheritances.length() >= 1)
  
  for inheritance in inheritance_result.applied_inheritances {
    assert_true(inheritance.config_type.length() > 0)
    assert_true(inheritance.field_name.length() > 0)
    assert_true(inheritance.source_environment.length() > 0)
    assert_true(inheritance.target_environment.length() > 0)
    assert_true(inheritance.inheritance_type != @azimuth.InheritanceType::None)
  }
  
  // 测试配置版本控制
  let versioning_request = @azimuth.ConfigVersioningRequest::new()
    .with_environment("production")
    .with_config_type("service_config")
    .with_version("1.0.0")
    .with_change_description("初始生产配置")
    .with_changed_by("ops-team@example.com")
  
  let versioning_result = @azimuth.MultiEnvironmentConfigManager::create_config_version(config_manager, versioning_request)
  
  // 验证配置版本控制结果
  assert_true(versioning_result.success)
  assert_true(versioning_result.version_id.length() > 0)
  assert_true(versioning_result.created_at > 0)
  
  // 测试配置回滚
  let rollback_request = @azimuth.ConfigRollbackRequest::new()
    .with_environment("production")
    .with_config_type("service_config")
    .with_target_version("1.0.0")
    .with_rollback_reason("配置错误导致服务异常")
    .with_rollback_by("ops-team@example.com")
  
  let rollback_result = @azimuth.MultiEnvironmentConfigManager::rollback_config(config_manager, rollback_request)
  
  // 验证配置回滚结果
  assert_true(rollback_result.success)
  assert_true(rollback_result.rollback_id.length() > 0)
  assert_true(rollback_result.rollback_at > 0)
  
  // 测试配置搜索
  let search_request = @azimuth.ConfigSearchRequest::new()
    .with_search_term("database")
    .with_environments(["development", "staging", "production"])
    .with_search_fields(["host", "username"])
    .with_case_sensitive(false)
  
  let search_result = @azimuth.MultiEnvironmentConfigManager::search_configs(config_manager, search_request)
  
  // 验证配置搜索结果
  assert_true(search_result.success)
  assert_true(search_result.matches.length() >= 3)
  
  for match in search_result.matches {
    assert_true(match.environment.length() > 0)
    assert_true(match.config_type.length() > 0)
    assert_true(match.field_name.length() > 0)
    assert_true(match.matched_value.length() > 0)
  }
}

// Test 2: 配置同步和传播
test "configuration synchronization and propagation" {
  // 创建配置同步管理器
  let sync_manager = @azimuth.ConfigSyncManager::new()
  
  // 配置同步策略
  let sync_strategies = [
    @azimuth.SyncStrategy::new(
      "bidirectional",
      "双向同步",
      @azimuth.SyncDirection::Bidirectional,
      @azimuth.SyncMode::RealTime,
      @azimuth.ConflictResolution::LastWriterWins
    ),
    @azimuth.SyncStrategy::new(
      "unidirectional",
      "单向同步",
      @azimuth.SyncDirection::Unidirectional,
      @azimuth.SyncMode::Scheduled,
      @azimuth.ConflictResolution::SourceWins
    ),
    @azimuth.SyncStrategy::new(
      "manual",
      "手动同步",
      @azimuth.SyncDirection::Unidirectional,
      @azimuth.SyncMode::OnDemand,
      @azimuth.ConflictResolution::Manual
    )
  ]
  
  @azimuth.ConfigSyncManager::configure_strategies(sync_manager, sync_strategies)
  
  // 配置同步规则
  let sync_rules = [
    @azimuth.SyncRule::new(
      "feature_flags_sync",
      "功能开关同步",
      ["development", "staging"],
      ["feature_flags"],
      @azimuth.SyncStrategy::new(
        "feature_flag_strategy",
        "功能开关同步策略",
        @azimuth.SyncDirection::Unidirectional,
        @azimuth.SyncMode::RealTime,
        @azimuth.ConflictResolution::SourceWins
      ),
      @azimuth.FieldFilter::Include(["new_ui_enabled", "advanced_analytics"])
    ),
    @azimuth.SyncRule::new(
      "service_config_sync",
      "服务配置同步",
      ["staging", "production"],
      ["service_config"],
      @azimuth.SyncStrategy::new(
        "service_config_strategy",
        "服务配置同步策略",
        @azimuth.SyncDirection::Unidirectional,
        @azimuth.SyncMode::Scheduled,
        @azimuth.ConflictResolution::Manual
      ),
      @azimuth.FieldFilter::Exclude(["port", "max_workers"])
    ),
    @azimuth.SyncRule::new(
      "database_config_sync",
      "数据库配置同步",
      ["production", "disaster_recovery"],
      ["database_config"],
      @azimuth.SyncStrategy::new(
        "database_config_strategy",
        "数据库配置同步策略",
        @azimuth.SyncDirection::Unidirectional,
        @azimuth.SyncMode::RealTime,
        @azimuth.ConflictResolution::SourceWins
      ),
      @azimuth.FieldFilter::Include(["max_connections", "connection_timeout"])
    )
  ]
  
  @azimuth.ConfigSyncManager::configure_rules(sync_manager, sync_rules)
  
  // 创建配置变更事件
  let current_time = @azimuth.Time::now_unix_nanos()
  let mut config_changes = []
  
  // 功能开关变更
  let feature_flag_change = @azimuth.ConfigChangeEvent::new(
    "development",
    "feature_flags",
    "new_ui_enabled",
    @azimuth.BoolValue(false), // 旧值
    @azimuth.BoolValue(true),  // 新值
    current_time,
    "dev-team@example.com",
    "启用新UI进行测试"
  )
  config_changes = config_changes.push(feature_flag_change)
  
  // 服务配置变更
  let service_config_change = @azimuth.ConfigChangeEvent::new(
    "staging",
    "service_config",
    "log_level",
    @azimuth.StringValue("INFO"), // 旧值
    @azimuth.StringValue("DEBUG"), // 新值
    current_time + 1000000000,
    "qa-team@example.com",
    "启用调试日志进行问题排查"
  )
  config_changes = config_changes.push(service_config_change)
  
  // 数据库配置变更
  let database_config_change = @azimuth.ConfigChangeEvent::new(
    "production",
    "database_config",
    "max_connections",
    @azimuth.IntValue(200), // 旧值
    @azimuth.IntValue(250), // 新值
    current_time + 2000000000,
    "ops-team@example.com",
    "增加最大连接数以应对流量增长"
  )
  config_changes = config_changes.push(database_config_change)
  
  // 执行配置同步
  let sync_request = @azimuth.ConfigSyncRequest::new()
    .with_config_changes(config_changes)
    .with_sync_rules(["feature_flags_sync", "service_config_sync", "database_config_sync"])
    .with_sync_options([
      @azimuth.SyncOption::ValidateBeforeSync,
      @azimuth.SyncOption::CreateBackup,
      @azimuth.SyncOption::NotifyOnSuccess,
      @azimuth.SyncOption::NotifyOnFailure
    ])
  
  let sync_result = @azimuth.ConfigSyncManager::sync_configs(sync_manager, sync_request)
  
  // 验证配置同步结果
  assert_true(sync_result.success)
  assert_eq(sync_result.processed_changes, 3)
  assert_eq(sync_result.successful_syncs, 3)
  assert_eq(sync_result.failed_syncs, 0)
  assert_true(sync_result.sync_duration_ms > 0)
  
  // 验证同步详情
  for sync_detail in sync_result.sync_details {
    assert_true(sync_detail.source_environment.length() > 0)
    assert_true(sync_detail.target_environments.length() >= 1)
    assert_true(sync_detail.config_type.length() > 0)
    assert_true(sync_detail.field_name.length() > 0)
    assert_true(sync_detail.sync_status == @azimuth.SyncStatus::Success)
    assert_true(sync_detail.synced_at > 0)
  }
  
  // 验证功能开关同步
  let feature_flag_sync = sync_result.sync_details.find_fn(detail) {
    detail.config_type == "feature_flags" && detail.field_name == "new_ui_enabled"
  }
  match feature_flag_sync {
    Some(detail) => {
      assert_eq(detail.source_environment, "development")
      assert_true(detail.target_environments.contains("staging"))
      assert_eq(detail.old_value, @azimuth.BoolValue(false))
      assert_eq(detail.new_value, @azimuth.BoolValue(true))
    }
    None => assert_true(false)
  }
  
  // 测试同步冲突检测
  let conflict_changes = [
    @azimuth.ConfigChangeEvent::new(
      "staging",
      "service_config",
      "log_level",
      @azimuth.StringValue("INFO"),
      @azimuth.StringValue("WARN"),
      current_time + 3000000000,
      "qa-team@example.com",
      "降低日志级别"
    ),
    @azimuth.ConfigChangeEvent::new(
      "production",
      "service_config",
      "log_level",
      @azimuth.StringValue("INFO"),
      @azimuth.StringValue("ERROR"),
      current_time + 3100000000,
      "ops-team@example.com",
      "进一步降低日志级别"
    )
  ]
  
  let conflict_sync_request = @azimuth.ConfigSyncRequest::new()
    .with_config_changes(conflict_changes)
    .with_sync_rules(["service_config_sync"])
    .with_conflict_resolution(@azimuth.ConflictResolution::Manual)
  
  let conflict_sync_result = @azimuth.ConfigSyncManager::sync_configs(sync_manager, conflict_sync_request)
  
  // 验证冲突检测结果
  assert_true(conflict_sync_result.success)
  assert_true(conflict_sync_result.detected_conflicts.length() >= 1)
  
  for conflict in conflict_sync_result.detected_conflicts {
    assert_true(conflict.config_type.length() > 0)
    assert_true(conflict.field_name.length() > 0)
    assert_true(conflict.conflicting_environments.length() >= 2)
    assert_true(conflict.conflict_type != @azimuth.ConflictType::None)
    assert_true(conflict.resolution_options.length() >= 2)
  }
  
  // 测试同步状态监控
  let status_request = @azimuth.SyncStatusRequest::new()
    .with_environments(["development", "staging", "production", "disaster_recovery"])
    .with_config_types(["feature_flags", "service_config", "database_config"])
    .with_time_range(@azimuth.TimeRange::LastHours(24))
  
  let status_result = @azimuth.ConfigSyncManager::get_sync_status(sync_manager, status_request)
  
  // 验证同步状态结果
  assert_true(status_result.success)
  assert_true(status_result.sync_status_summary.total_syncs >= 3)
  assert_true(status_result.sync_status_summary.successful_syncs >= 3)
  assert_true(status_result.environment_sync_status.length() >= 4)
  
  // 验证环境同步状态
  for env_status in status_result.environment_sync_status {
    assert_true(env_status.environment.length() > 0)
    assert_true(env_status.last_sync_time > 0)
    assert_true(env_status.sync_health_score >= 0.0 && env_status.sync_health_score <= 1.0)
    assert_true(env_status.pending_changes >= 0)
  }
  
  // 测试同步回滚
  let rollback_request = @azimuth.SyncRollbackRequest::new()
    .with_sync_id(sync_result.sync_id)
    .with_rollback_reason("同步导致配置错误")
    .with_rollback_by("ops-team@example.com")
    .with_rollback_options([
      @azimuth.RollbackOption::ValidateAfterRollback,
      @azimuth.RollbackOption::NotifyOnRollback
    ])
  
  let rollback_result = @azimuth.ConfigSyncManager::rollback_sync(sync_manager, rollback_request)
  
  // 验证同步回滚结果
  assert_true(rollback_result.success)
  assert_true(rollback_result.rollback_id.length() > 0)
  assert_true(rollback_result.rolled_back_changes >= 1)
  assert_true(rollback_result.rollback_at > 0)
  
  // 测试同步历史记录
  let history_request = @azimuth.SyncHistoryRequest::new()
    .with_environments(["development", "staging", "production"])
    .with_time_range(@azimuth.TimeRange::LastDays(7))
    .with_pagination(@azimuth.Pagination::new(1, 50))
  
  let history_result = @azimuth.ConfigSyncManager::get_sync_history(sync_manager, history_request)
  
  // 验证同步历史结果
  assert_true(history_result.success)
  assert_true(history_result.sync_history.length() >= 1)
  assert_true(history_result.total_records >= 1)
  
  for history_item in history_result.sync_history {
    assert_true(history_item.sync_id.length() > 0)
    assert_true(history_item.source_environment.length() > 0)
    assert_true(history_item.config_type.length() > 0)
    assert_true(history_item.sync_time > 0)
    assert_true(history_item.sync_status != @azimuth.SyncStatus::Unknown)
  }
  
  // 测试同步统计
  let stats_request = @azimuth.SyncStatsRequest::new()
    .with_time_range(@azimuth.TimeRange::LastDays(30))
    .with_group_by(@azimuth.GroupBy::Environment)
  
  let stats_result = @azimuth.ConfigSyncManager::get_sync_statistics(sync_manager, stats_request)
  
  // 验证同步统计结果
  assert_true(stats_result.success)
  assert_true(stats_result.overall_statistics.total_syncs >= 1)
  assert_true(stats_result.overall_statistics.success_rate >= 0.0)
  assert_true(stats_result.overall_statistics.average_sync_time_ms > 0)
  assert_true(stats_result.grouped_statistics.length() >= 1)
  
  // 验证分组统计
  for group_stats in stats_result.grouped_statistics {
    assert_true(group_stats.group_key.length() > 0)
    assert_true(group_stats.total_syncs >= 0)
    assert_true(group_stats.successful_syncs >= 0)
    assert_true(group_stats.failed_syncs >= 0)
    assert_true(group_stats.success_rate >= 0.0 && group_stats.success_rate <= 1.0)
  }
}

// Test 3: 配置安全和访问控制
test "configuration security and access control" {
  // 创建配置安全管理器
  let security_manager = @azimuth.ConfigSecurityManager::new()
  
  // 配置访问控制策略
  let access_policies = [
    @azimuth.AccessPolicy::new(
      "developer_policy",
      "开发者策略",
      ["dev-team@example.com"],
      ["development"],
      ["database_config", "service_config", "feature_flags"],
      [@azimuth.Permission::Read, @azimuth.Permission::Write],
      @azimuth.AccessLevel::Environment
    ),
    @azimuth.AccessPolicy::new(
      "qa_policy",
      "QA策略",
      ["qa-team@example.com"],
      ["staging"],
      ["database_config", "service_config", "feature_flags"],
      [@azimuth.Permission::Read, @azimuth.Permission::Write],
      @azimuth.AccessLevel::Environment
    ),
    @azimuth.AccessPolicy::new(
      "ops_policy",
      "运维策略",
      ["ops-team@example.com"],
      ["staging", "production", "disaster_recovery"],
      ["database_config", "service_config"],
      [@azimuth.Permission::Read, @azimuth.Permission::Write, @azimuth.Permission::Admin],
      @azimuth.AccessLevel::Environment
    ),
    @azimuth.AccessPolicy::new(
      "management_policy",
      "管理策略",
      ["management@example.com"],
      ["production"],
      ["feature_flags"],
      [@azimuth.Permission::Read, @azimuth.Permission::Write],
      @azimuth.AccessLevel::Field
    )
  ]
  
  @azimuth.ConfigSecurityManager::configure_access_policies(security_manager, access_policies)
  
  // 配置加密策略
  let encryption_policies = [
    @azimuth.EncryptionPolicy::new(
      "secret_fields",
      "敏感字段加密",
      [@azimuth.FieldType::Secret],
      @azimuth.EncryptionAlgorithm::AES256,
      @azimuth.KeyRotation::Days(90)
    ),
    @azimuth.EncryptionPolicy::new(
      "connection_strings",
      "连接字符串加密",
      [@azimuth.FieldType::ConnectionString],
      @azimuth.EncryptionAlgorithm::AES256,
      @azimuth.KeyRotation::Days(30)
    )
  ]
  
  @azimuth.ConfigSecurityManager::configure_encryption_policies(security_manager, encryption_policies)
  
  // 配置审计策略
  let audit_policies = [
    @azimuth.AuditPolicy::new(
      "config_access_audit",
      "配置访问审计",
      [@azimuth.AuditEvent::ConfigRead, @azimuth.AuditEvent::ConfigWrite, @azimuth.AuditEvent::ConfigDelete],
      @azimuth.Retention::Days(365)
    ),
    @azimuth.AuditPolicy::new(
      "security_events_audit",
      "安全事件审计",
      [@azimuth.AuditEvent::AccessDenied, @azimuth.AuditEvent::PrivilegeEscalation, @azimuth.AuditEvent::DataExfiltration],
      @azimuth.Retention::Years(7)
    )
  ]
  
  @azimuth.ConfigSecurityManager::configure_audit_policies(security_manager, audit_policies)
  
  // 测试访问控制
  let access_requests = [
    @azimuth.AccessRequest::new(
      "dev-team@example.com",
      "development",
      "database_config",
      @azimuth.Permission::Read
    ),
    @azimuth.AccessRequest::new(
      "dev-team@example.com",
      "production",
      "database_config",
      @azimuth.Permission::Write
    ),
    @azimuth.AccessRequest::new(
      "ops-team@example.com",
      "production",
      "database_config",
      @azimuth.Permission::Admin
    ),
    @azimuth.AccessRequest::new(
      "qa-team@example.com",
      "staging",
      "feature_flags",
      @azimuth.Permission::Write
    ),
    @azimuth.AccessRequest::new(
      "unauthorized@example.com",
      "production",
      "database_config",
      @azimuth.Permission::Read
    )
  ]
  
  for request in access_requests {
    let access_result = @azimuth.ConfigSecurityManager::check_access(security_manager, request)
    
    // 验证访问控制结果
    match (request.user, request.environment, request.permission) {
      ("dev-team@example.com", "development", @azimuth.Permission::Read) => {
        assert_true(access_result.granted)
      }
      ("dev-team@example.com", "production", @azimuth.Permission::Write) => {
        assert_false(access_result.granted)
        assert_eq(access_result.reason, "Insufficient privileges")
      }
      ("ops-team@example.com", "production", @azimuth.Permission::Admin) => {
        assert_true(access_result.granted)
      }
      ("qa-team@example.com", "staging", @azimuth.Permission::Write) => {
        assert_true(access_result.granted)
      }
      ("unauthorized@example.com", "production", @azimuth.Permission::Read) => {
        assert_false(access_result.granted)
        assert_eq(access_result.reason, "User not found in access policies")
      }
      _ => {}
    }
    
    // 验证审计记录
    assert_true(access_result.audit_id.length() > 0)
    assert_true(access_result.audit_timestamp > 0)
  }
  
  // 测试敏感数据加密
  let sensitive_data = @azimuth.ConfigData::new("production", "database_config", [
    ("host", @azimuth.StringValue("prod-db.example.com")),
    ("port", @azimuth.IntValue(5432)),
    ("username", @azimuth.StringValue("prod_user")),
    ("password", @azimuth.SecretValue("prod_password_789")),
    ("connection_string", @azimuth.ConnectionStringValue("postgresql://prod_user:prod_password_789@prod-db.example.com:5432/azimuth"))
  ])
  
  let encryption_request = @azimuth.EncryptionRequest::new()
    .with_config_data(sensitive_data)
    .with_encryption_policies(["secret_fields", "connection_strings"])
    .with_key_id("config-encryption-key-001")
  
  let encryption_result = @azimuth.ConfigSecurityManager::encrypt_sensitive_data(security_manager, encryption_request)
  
  // 验证加密结果
  assert_true(encryption_result.success)
  assert_true(encryption_result.encrypted_data.fields.length() >= 2) // password和connection_string应该被加密
  
  for field in encryption_result.encrypted_data.fields {
    match field.1 {
      @azimuth.SecretValue(encrypted_value) => {
        assert_true(encrypted_value.length() > 0)
        assert_ne(encrypted_value, "prod_password_789") // 应该不是原始值
      }
      @azimuth.ConnectionStringValue(encrypted_value) => {
        assert_true(encrypted_value.length() > 0)
        assert_ne(encrypted_value, "postgresql://prod_user:prod_password_789@prod-db.example.com:5432/azimuth") // 应该不是原始值
      }
      _ => {}
    }
  }
  
  // 测试敏感数据解密
  let decryption_request = @azimuth.DecryptionRequest::new()
    .with_encrypted_data(encryption_result.encrypted_data)
    .with_key_id("config-encryption-key-001")
    .with_access_context(@azimuth.AccessContext::new("ops-team@example.com", "production", @azimuth.Permission::Read))
  
  let decryption_result = @azimuth.ConfigSecurityManager::decrypt_sensitive_data(security_manager, decryption_request)
  
  // 验证解密结果
  assert_true(decryption_result.success)
  assert_true(decryption_result.decrypted_data.fields.length() >= 2)
  
  // 验证解密后的值是否正确
  let password_field = decryption_result.decrypted_data.fields.find_fn(field) {
    field.0 == "password"
  }
  match password_field {
    Some(field) => {
      match field.1 {
        @azimuth.SecretValue(value) => assert_eq(value, "prod_password_789")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试安全审计日志
  let audit_request = @azimuth.AuditLogRequest::new()
    .with_time_range(@azimuth.TimeRange::LastDays(7))
    .with_event_types([@azimuth.AuditEvent::ConfigRead, @azimuth.AuditEvent::ConfigWrite, @azimuth.AuditEvent::AccessDenied])
    .with_users(["dev-team@example.com", "ops-team@example.com", "unauthorized@example.com"])
    .with_pagination(@azimuth.Pagination::new(1, 100))
  
  let audit_result = @azimuth.ConfigSecurityManager::get_audit_logs(security_manager, audit_request)
  
  // 验证审计日志结果
  assert_true(audit_result.success)
  assert_true(audit_result.audit_logs.length() >= 5)
  assert_true(audit_result.total_records >= 5)
  
  // 验证审计日志内容
  for audit_log in audit_result.audit_logs {
    assert_true(audit_log.event_id.length() > 0)
    assert_true(audit_log.user.length() > 0)
    assert_true(audit_log.event_type != @azimuth.AuditEvent::Unknown)
    assert_true(audit_log.timestamp > 0)
    assert_true(audit_log.resource_type.length() > 0)
    assert_true(audit_log.resource_id.length() > 0)
    assert_true(audit_log.outcome != @azimuth.AuditOutcome::Unknown)
  }
  
  // 测试安全事件检测
  let security_events_request = @azimuth.SecurityEventDetectionRequest::new()
    .with_time_range(@azimuth.TimeRange::LastHours(24))
    .with_detection_rules([
      @azimuth.DetectionRule::new("brute_force", "暴力破解检测", @azimuth.RuleType::Frequency, 5),
      @azimuth.DetectionRule::new("privilege_escalation", "权限提升检测", @azimuth.RuleType::Anomaly),
      @azimuth.DetectionRule::new("data_exfiltration", "数据泄露检测", @azimuth.RuleType::Pattern)
    ])
  
  let security_events_result = @azimuth.ConfigSecurityManager::detect_security_events(security_manager, security_events_request)
  
  // 验证安全事件检测结果
  assert_true(security_events_result.success)
  assert_true(security_events_result.detected_events.length() >= 0)
  
  for event in security_events_result.detected_events {
    assert_true(event.event_id.length() > 0)
    assert_true(event.event_type != @azimuth.SecurityEventType::Unknown)
    assert_true(event.severity >= 1 && event.severity <= 5)
    assert_true(event.timestamp > 0)
    assert_true(event.description.length() > 0)
    assert_true(event.affected_resources.length() >= 1)
  }
  
  // 测试密钥轮换
  let key_rotation_request = @azimuth.KeyRotationRequest::new()
    .with_key_id("config-encryption-key-001")
    .with_rotation_reason("定期密钥轮换")
    .with_new_key_algorithm(@azimuth.EncryptionAlgorithm::AES256)
    .with_rotation_by("security-admin@example.com")
  
  let key_rotation_result = @azimuth.ConfigSecurityManager::rotate_encryption_key(security_manager, key_rotation_request)
  
  // 验证密钥轮换结果
  assert_true(key_rotation_result.success)
  assert_true(key_rotation_result.new_key_id.length() > 0)
  assert_true(key_rotation_result.rotation_time > 0)
  assert_true(key_rotation_result.affected_configs >= 1)
  
  // 测试合规性检查
  let compliance_request = @azimuth.ComplianceCheckRequest::new()
    .with_compliance_standards([@azimuth.ComplianceStandard::SOC2, @azimuth.ComplianceStandard::GDPR, @azimuth.ComplianceStandard::HIPAA])
    .with_environments(["production"])
    .with_check_types([
      @azimuth.ComplianceCheckType::AccessControl,
      @azimuth.ComplianceCheckType::DataEncryption,
      @azimuth.ComplianceCheckType::AuditLogging,
      @azimuth.ComplianceCheckType::DataRetention
    ])
  
  let compliance_result = @azimuth.ConfigSecurityManager::check_compliance(security_manager, compliance_request)
  
  // 验证合规性检查结果
  assert_true(compliance_result.success)
  assert_eq(compliance_result.checked_standards.length(), 3)
  assert_true(compliance_result.overall_compliance_score >= 0.0 && compliance_result.overall_compliance_score <= 1.0)
  
  for standard_result in compliance_result.standard_results {
    assert_true(standard_result.standard != @azimuth.ComplianceStandard::Unknown)
    assert_true(standard_result.compliance_score >= 0.0 && standard_result.compliance_score <= 1.0)
    assert_true(standard_result.check_results.length() >= 1)
    
    for check_result in standard_result.check_results {
      assert_true(check_result.check_type != @azimuth.ComplianceCheckType::Unknown)
      assert_true(check_result.status != @azimuth.ComplianceStatus::Unknown)
      assert_true(check_result.description.length() > 0)
      
      if check_result.status == @azimuth.ComplianceStatus::NonCompliant {
        assert_true(check_result.remediation_steps.length() >= 1)
      }
    }
  }
}

// Test 4: 配置部署和发布管理
test "configuration deployment and release management" {
  // 创建配置部署管理器
  let deployment_manager = @azimuth.ConfigDeploymentManager::new()
  
  // 配置部署流水线
  let deployment_pipeline = @azimuth.DeploymentPipeline::new([
    @azimuth.PipelineStage::new(
      "validation",
      "验证阶段",
      [
        @azimuth.ValidationTask::new("schema_validation", "模式验证", @azimuth.TaskType::Automated),
        @azimuth.ValidationTask::new("security_scan", "安全扫描", @azimuth.TaskType::Automated),
        @azimuth.ValidationTask::new("compliance_check", "合规检查", @azimuth.TaskType::Automated)
      ],
      @azimuth.StageType::Validation,
      @azimuth.ExecutionOrder::Sequential
    ),
    @azimuth.PipelineStage::new(
      "testing",
      "测试阶段",
      [
        @azimuth.TestingTask::new("unit_tests", "单元测试", @azimuth.TaskType::Automated),
        @azimuth.TestingTask::new("integration_tests", "集成测试", @azimuth.TaskType::Automated),
        @azimuth.TestingTask::new("performance_tests", "性能测试", @azimuth.TaskType::Automated),
        @azimuth.TestingTask::new("manual_verification", "手动验证", @azimuth.TaskType::Manual)
      ],
      @azimuth.StageType::Testing,
      @azimuth.ExecutionOrder::Parallel
    ),
    @azimuth.PipelineStage::new(
      "staging_deployment",
      "预发布部署",
      [
        @azimuth.DeploymentTask::new("backup_config", "备份配置", @azimuth.TaskType::Automated),
        @azimuth.DeploymentTask::new("deploy_to_staging", "部署到预发布", @azimuth.TaskType::Automated),
        @azimuth.DeploymentTask::new("health_check", "健康检查", @azimuth.TaskType::Automated),
        @azimuth.DeploymentTask::new("smoke_tests",冒烟测试", @azimuth.TaskType::Automated)
      ],
      @azimuth.StageType::Deployment,
      @azimuth.ExecutionOrder::Sequential
    ),
    @azimuth.PipelineStage::new(
      "production_deployment",
      "生产部署",
      [
        @azimuth.DeploymentTask::new("approval_gate", "审批门禁", @azimuth.TaskType::Manual),
        @azimuth.DeploymentTask::new("backup_production", "备份生产环境", @azimuth.TaskType::Automated),
        @azimuth.DeploymentTask::new("blue_green_deployment", "蓝绿部署", @azimuth.TaskType::Automated),
        @azimuth.DeploymentTask::new("post_deployment_tests", "部署后测试", @azimuth.TaskType::Automated)
      ],
      @azimuth.StageType::Deployment,
      @azimuth.ExecutionOrder::Sequential
    )
  ])
  
  @azimuth.ConfigDeploymentManager::configure_pipeline(deployment_manager, deployment_pipeline)
  
  // 配置部署策略
  let deployment_strategies = [
    @azimuth.DeploymentStrategy::new(
      "rolling_update",
      "滚动更新",
      @azimuth.StrategyType::Rolling,
      @azimuth.RollbackCapability::Automatic,
      @azimuth.DowntimeTolerance::Zero
    ),
    @azimuth.DeploymentStrategy::new(
      "blue_green",
      "蓝绿部署",
      @azimuth.StrategyType::BlueGreen,
      @azimuth.RollbackCapability::Instant,
      @azimuth.DowntimeTolerance::Zero
    ),
    @azimuth.DeploymentStrategy::new(
      "canary",
      "金丝雀部署",
      @azimuth.StrategyType::Canary,
      @azimuth.RollbackCapability::Automatic,
      @azimuth.DowntimeTolerance::Zero
    )
  ]
  
  @azimuth.ConfigDeploymentManager::configure_strategies(deployment_manager, deployment_strategies)
  
  // 创建配置变更包
  let current_time = @azimuth.Time::now_unix_nanos()
  let config_changes = [
    @azimuth.ConfigChange::new(
      "service_config",
      "log_level",
      @azimuth.StringValue("INFO"),
      @azimuth.StringValue("DEBUG"),
      "增加日志详细度以便问题排查"
    ),
    @azimuth.ConfigChange::new(
      "service_config",
      "max_workers",
      @azimuth.IntValue(8),
      @azimuth.IntValue(10),
      "增加工作线程以提高并发处理能力"
    ),
    @azimuth.ConfigChange::new(
      "database_config",
      "max_connections",
      @azimuth.IntValue(200),
      @azimuth.IntValue(250),
      "增加数据库连接池大小以应对流量增长"
    ),
    @azimuth.ConfigChange::new(
      "feature_flags",
      "new_ui_enabled",
      @azimuth.BoolValue(false),
      @azimuth.BoolValue(true),
      "启用新UI界面"
    )
  ]
  
  let change_package = @azimuth.ConfigChangePackage::new(
    "config-change-001",
    "性能优化和UI改进",
    config_changes,
    current_time,
    "ops-team@example.com",
    @azimuth.ChangePriority::High,
    ["performance", "ui", "optimization"]
  )
  
  // 执行配置部署
  let deployment_request = @azimuth.ConfigDeploymentRequest::new()
    .with_change_package(change_package)
    .with_target_environments(["staging", "production"])
    .with_deployment_strategy("blue_green")
    .with_deployment_options([
      @azimuth.DeploymentOption::RunValidation,
      @azimuth.DeploymentOption::RunTests,
      @azimuth.DeploymentOption::CreateBackup,
      @azimuth.DeploymentOption::EnableRollback,
      @azimuth.DeploymentOption::NotifyStakeholders
    ])
    .with_approval_requirements([
      @azimuth.ApprovalRequirement::new("production_deployment", "生产部署审批", ["manager@example.com"])
    ])
  
  let deployment_result = @azimuth.ConfigDeploymentManager::deploy_config(deployment_manager, deployment_request)
  
  // 验证配置部署结果
  assert_true(deployment_result.success)
  assert_true(deployment_result.deployment_id.length() > 0)
  assert_eq(deployment_result.deployed_environments.length(), 2)
  assert_true(deployment_result.deployment_duration_ms > 0)
  
  // 验证部署阶段结果
  for stage_result in deployment_result.stage_results {
    assert_true(stage_result.stage_name.length() > 0)
    assert_true(stage_result.status != @azimuth.StageStatus::Unknown)
    assert_true(stage_result.start_time > 0)
    assert_true(stage_result.end_time >= stage_result.start_time)
    assert_true(stage_result.task_results.length() >= 1)
    
    // 验证任务结果
    for task_result in stage_result.task_results {
      assert_true(task_result.task_name.length() > 0)
      assert_true(task_result.status != @azimuth.TaskStatus::Unknown)
      assert_true(task_result.start_time > 0)
      assert_true(task_result.end_time >= task_result.start_time)
      
      if task_result.status == @azimuth.TaskStatus::Failed {
        assert_true(task_result.error_message.length() > 0)
      }
    }
  }
  
  // 测试部署回滚
  let rollback_request = @azimuth.DeploymentRollbackRequest::new()
    .with_deployment_id(deployment_result.deployment_id)
    .with_rollback_reason("配置变更导致服务性能下降")
    .with_rollback_by("ops-team@example.com")
    .with_rollback_options([
      @azimuth.RollbackOption::ValidateAfterRollback,
      @azimuth.RollbackOption::NotifyStakeholders,
      @azimuth.RollbackOption::CreateIncident
    ])
  
  let rollback_result = @azimuth.ConfigDeploymentManager::rollback_deployment(deployment_manager, rollback_request)
  
  // 验证部署回滚结果
  assert_true(rollback_result.success)
  assert_true(rollback_result.rollback_id.length() > 0)
  assert_true(rollback_result.rolled_back_environments.length() >= 1)
  assert_true(rollback_result.rollback_duration_ms > 0)
  
  // 测试部署历史记录
  let history_request = @azimuth.DeploymentHistoryRequest::new()
    .with_environments(["staging", "production"])
    .with_time_range(@azimuth.TimeRange::LastDays(30))
    .with_status_filter([@azimuth.DeploymentStatus::Success, @azimuth.DeploymentStatus::Failed, @azimuth.DeploymentStatus::RolledBack])
    .with_pagination(@azimuth.Pagination::new(1, 50))
  
  let history_result = @azimuth.ConfigDeploymentManager::get_deployment_history(deployment_manager, history_request)
  
  // 验证部署历史结果
  assert_true(history_result.success)
  assert_true(history_result.deployments.length() >= 2)
  assert_true(history_result.total_records >= 2)
  
  for deployment in history_result.deployments {
    assert_true(deployment.deployment_id.length() > 0)
    assert_true(deployment.change_package_id.length() > 0)
    assert_true(deployment.target_environments.length() >= 1)
    assert_true(deployment.deployment_status != @azimuth.DeploymentStatus::Unknown)
    assert_true(deployment.deployment_time > 0)
    assert_true(deployment.deployment_duration_ms > 0)
  }
  
  // 测试部署统计
  let stats_request = @azimuth.DeploymentStatsRequest::new()
    .with_time_range(@azimuth.TimeRange::LastDays(90))
    .with_group_by(@azimuth.GroupBy::Environment)
    .with_include_metrics([
      @azimuth.DeploymentMetric::SuccessRate,
      @azimuth.DeploymentMetric::FailureRate,
      @azimuth.DeploymentMetric::RollbackRate,
      @azimuth.DeploymentMetric::AverageDeploymentTime,
      @azimuth.DeploymentMetric::AverageRollbackTime
    ])
  
  let stats_result = @azimuth.ConfigDeploymentManager::get_deployment_statistics(deployment_manager, stats_request)
  
  // 验证部署统计结果
  assert_true(stats_result.success)
  assert_true(stats_result.overall_statistics.total_deployments >= 2)
  assert_true(stats_result.overall_statistics.success_rate >= 0.0 && stats_result.overall_statistics.success_rate <= 1.0)
  assert_true(stats_result.overall_statistics.failure_rate >= 0.0 && stats_result.overall_statistics.failure_rate <= 1.0)
  assert_true(stats_result.overall_statistics.rollback_rate >= 0.0 && stats_result.overall_statistics.rollback_rate <= 1.0)
  assert_true(stats_result.overall_statistics.average_deployment_time_ms > 0)
  assert_true(stats_result.grouped_statistics.length() >= 1)
  
  // 测试部署模板
  let template_request = @azimuth.DeploymentTemplateRequest::new()
    .with_template_name("standard_production_deployment")
    .with_template_description("标准生产环境部署模板")
    .with_deployment_strategy("blue_green")
    .with_pipeline_stages(["validation", "testing", "staging_deployment", "production_deployment"])
    .with_deployment_options([
      @azimuth.DeploymentOption::RunValidation,
      @azimuth.DeploymentOption::RunTests,
      @azimuth.DeploymentOption::CreateBackup,
      @azimuth.DeploymentOption::EnableRollback
    ])
    .with_approval_requirements([
      @azimuth.ApprovalRequirement::new("production_deployment", "生产部署审批", ["manager@example.com"])
    ])
    .with_notification_settings([
      @azimuth.NotificationSetting::new("deployment_started", "部署开始通知", ["ops-team@example.com"]),
      @azimuth.NotificationSetting::new("deployment_completed", "部署完成通知", ["ops-team@example.com", "stakeholders@example.com"]),
      @azimuth.NotificationSetting::new("deployment_failed", "部署失败通知", ["ops-team@example.com", "manager@example.com"])
    ])
  
  let template_result = @azimuth.ConfigDeploymentManager::create_deployment_template(deployment_manager, template_request)
  
  // 验证部署模板结果
  assert_true(template_result.success)
  assert_true(template_result.template_id.length() > 0)
  assert_true(template_result.created_at > 0)
  
  // 测试基于模板的部署
  let template_deployment_request = @azimuth.TemplateBasedDeploymentRequest::new()
    .with_template_id(template_result.template_id)
    .with_change_package(change_package)
    .with_target_environments(["production"])
    .with_template_overrides([
      @azimuth.TemplateOverride::new("approval_requirements", "审批要求", [
        @azimuth.ApprovalRequirement::new("emergency_deployment", "紧急部署审批", ["cto@example.com"])
      ])
    ])
  
  let template_deployment_result = @azimuth.ConfigDeploymentManager::deploy_from_template(deployment_manager, template_deployment_request)
  
  // 验证基于模板的部署结果
  assert_true(template_deployment_result.success)
  assert_true(template_deployment_result.deployment_id.length() > 0)
  assert_true(template_deployment_result.template_id == template_result.template_id)
  
  // 测试部署通知
  let notification_request = @azimuth.DeploymentNotificationRequest::new()
    .with_deployment_id(deployment_result.deployment_id)
    .with_notification_types([@azimuth.NotificationType::Email, @azimuth.NotificationType::Slack])
    .with_recipients(["ops-team@example.com", "stakeholders@example.com"])
    .with_message_template("部署通知：{{deployment_status}} - {{deployment_id}}")
  
  let notification_result = @azimuth.ConfigDeploymentManager::send_deployment_notification(deployment_manager, notification_request)
  
  // 验证部署通知结果
  assert_true(notification_result.success)
  assert_true(notification_result.sent_notifications.length() >= 1)
  
  for notification in notification_result.sent_notifications {
    assert_true(notification.notification_id.length() > 0)
    assert_true(notification.recipient.length() > 0)
    assert_true(notification.notification_type != @azimuth.NotificationType::Unknown)
    assert_true(notification.sent_at > 0)
    assert_true(notification.message.length() > 0)
  }
}