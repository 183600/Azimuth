// Azimuth 遥测系统配置管理测试
// 专注于遥测系统的配置加载、更新和管理功能

// 测试1: 遥测配置加载与验证
test "遥测配置加载与验证" {
  // 模拟默认遥测配置
  let default_config = {
    sampling_rate: 1.0,
    batch_size: 100,
    flush_interval: 5000,
    retry_attempts: 3,
    timeout: 30000,
    compression: true,
    encryption: false,
    storage_type: "file",
    retention_days: 30,
    metrics_enabled: true,
    logs_enabled: true,
    traces_enabled: true
  }
  
  // 模拟用户自定义配置
  let user_config = {
    sampling_rate: 0.5,
    batch_size: 200,
    flush_interval: 10000,
    encryption: true,
    storage_type: "database",
    retention_days: 60
  }
  
  // 配置合并函数
  let merge_configs = fn(default, user) {
    {
      sampling_rate: user.sampling_rate,
      batch_size: user.batch_size,
      flush_interval: user.flush_interval,
      retry_attempts: default.retry_attempts,
      timeout: default.timeout,
      compression: default.compression,
      encryption: user.encryption,
      storage_type: user.storage_type,
      retention_days: user.retention_days,
      metrics_enabled: default.metrics_enabled,
      logs_enabled: default.logs_enabled,
      traces_enabled: default.traces_enabled
    }
  }
  
  // 配置验证函数
  let validate_config = fn(config) {
    let mut errors = []
    
    // 验证采样率
    if config.sampling_rate < 0.0 or config.sampling_rate > 1.0 {
      errors = errors.push("采样率必须在0.0到1.0之间")
    }
    
    // 验证批量大小
    if config.batch_size <= 0 {
      errors = errors.push("批量大小必须大于0")
    }
    
    // 验证刷新间隔
    if config.flush_interval <= 0 {
      errors = errors.push("刷新间隔必须大于0")
    }
    
    // 验证重试次数
    if config.retry_attempts < 0 {
      errors = errors.push("重试次数不能为负数")
    }
    
    // 验证超时时间
    if config.timeout <= 0 {
      errors = errors.push("超时时间必须大于0")
    }
    
    // 验证存储类型
    if config.storage_type != "file" and config.storage_type != "database" and config.storage_type != "cloud" {
      errors = errors.push("存储类型必须是file、database或cloud")
    }
    
    // 验证保留天数
    if config.retention_days <= 0 {
      errors = errors.push("保留天数必须大于0")
    }
    
    errors
  }
  
  // 执行配置合并
  let merged_config = merge_configs(default_config, user_config)
  
  // 验证配置合并结果
  assert_eq(merged_config.sampling_rate, 0.5) // 用户配置
  assert_eq(merged_config.batch_size, 200) // 用户配置
  assert_eq(merged_config.flush_interval, 10000) // 用户配置
  assert_eq(merged_config.retry_attempts, 3) // 默认配置
  assert_eq(merged_config.timeout, 30000) // 默认配置
  assert_eq(merged_config.compression, true) // 默认配置
  assert_eq(merged_config.encryption, true) // 用户配置
  assert_eq(merged_config.storage_type, "database") // 用户配置
  assert_eq(merged_config.retention_days, 60) // 用户配置
  assert_eq(merged_config.metrics_enabled, true) // 默认配置
  assert_eq(merged_config.logs_enabled, true) // 默认配置
  assert_eq(merged_config.traces_enabled, true) // 默认配置
  
  // 执行配置验证
  let validation_errors = validate_config(merged_config)
  
  // 验证配置验证结果
  assert_eq(validation_errors.length(), 0) // 没有错误
  
  // 测试无效配置
  let invalid_config = {
    sampling_rate: 1.5, // 无效：大于1.0
    batch_size: -10, // 无效：小于0
    flush_interval: 10000,
    retry_attempts: 3,
    timeout: 30000,
    compression: true,
    encryption: false,
    storage_type: "invalid_type", // 无效：不支持的存储类型
    retention_days: -5, // 无效：小于0
    metrics_enabled: true,
    logs_enabled: true,
    traces_enabled: true
  }
  
  let invalid_errors = validate_config(invalid_config)
  
  // 验证无效配置的错误检测
  assert_eq(invalid_errors.length(), 3)
  assert_true(invalid_errors.contains("采样率必须在0.0到1.0之间"))
  assert_true(invalid_errors.contains("批量大小必须大于0"))
  assert_true(invalid_errors.contains("存储类型必须是file、database或cloud"))
}

// 测试2: 动态配置更新
test "动态配置更新" {
  // 模拟当前配置
  let mut current_config = {
    sampling_rate: 1.0,
    batch_size: 100,
    flush_interval: 5000,
    retry_attempts: 3,
    timeout: 30000,
    compression: true,
    encryption: false,
    storage_type: "file",
    retention_days: 30
  }
  
  // 模拟配置更新历史
  let mut config_history = []
  
  // 配置更新函数
  let update_config = fn(current_config, updates, history) {
    let mut updated_config = current_config
    
    // 应用更新
    for (key, value) in updates {
      match key {
        "sampling_rate" => updated_config.sampling_rate = value
        "batch_size" => updated_config.batch_size = value
        "flush_interval" => updated_config.flush_interval = value
        "retry_attempts" => updated_config.retry_attempts = value
        "timeout" => updated_config.timeout = value
        "compression" => updated_config.compression = value
        "encryption" => updated_config.encryption = value
        "storage_type" => updated_config.storage_type = value
        "retention_days" => updated_config.retention_days = value
        _ => () // 忽略未知字段
      }
    }
    
    // 记录更新历史
    let mut updated_history = history
    updated_history = updated_history.push({
      timestamp: 1640995200,
      changes: updates,
      previous_config: current_config,
      new_config: updated_config
    })
    
    { config: updated_config, history: updated_history }
  }
  
  // 第一次配置更新
  let updates1 = {
    "sampling_rate": 0.5,
    "batch_size": 200
  }
  
  let result1 = update_config(current_config, updates1, config_history)
  current_config = result1.config
  config_history = result1.history
  
  // 验证第一次更新结果
  assert_eq(current_config.sampling_rate, 0.5)
  assert_eq(current_config.batch_size, 200)
  assert_eq(current_config.flush_interval, 5000) // 未更改
  assert_eq(config_history.length(), 1)
  
  // 第二次配置更新
  let updates2 = {
    "encryption": true,
    "storage_type": "database",
    "retention_days": 60
  }
  
  let result2 = update_config(current_config, updates2, config_history)
  current_config = result2.config
  config_history = result2.history
  
  // 验证第二次更新结果
  assert_eq(current_config.encryption, true)
  assert_eq(current_config.storage_type, "database")
  assert_eq(current_config.retention_days, 60)
  assert_eq(current_config.sampling_rate, 0.5) // 保持第一次更新
  assert_eq(current_config.batch_size, 200) // 保持第一次更新
  assert_eq(config_history.length(), 2)
  
  // 验证配置历史记录
  let history_entry1 = config_history[0]
  assert_eq(history_entry1.timestamp, 1640995200)
  assert_eq(history_entry1.changes["sampling_rate"], 0.5)
  assert_eq(history_entry1.changes["batch_size"], 200)
  
  let history_entry2 = config_history[1]
  assert_eq(history_entry2.timestamp, 1640995200)
  assert_eq(history_entry2.changes["encryption"], true)
  assert_eq(history_entry2.changes["storage_type"], "database")
  assert_eq(history_entry2.changes["retention_days"], 60)
  
  // 配置回滚函数
  let rollback_config = fn(history, steps) {
    if history.length() >= steps {
      let target_entry = history[history.length() - steps]
      target_entry.previous_config
    } else {
      // 如果回滚步数超过历史记录，返回最初配置
      history[0].previous_config
    }
  }
  
  // 回滚到上一个配置
  let rollback_config_result = rollback_config(config_history, 1)
  
  // 验证回滚结果
  assert_eq(rollback_config_result.sampling_rate, 0.5)
  assert_eq(rollback_config_result.batch_size, 200)
  assert_eq(rollback_config_result.encryption, false) // 回滚到第二次更新前的状态
  assert_eq(rollback_config_result.storage_type, "file") // 回滚到第二次更新前的状态
  assert_eq(rollback_config_result.retention_days, 30) // 回滚到第二次更新前的状态
}

// 测试3: 环境特定配置
test "环境特定配置" {
  // 模拟基础配置
  let base_config = {
    sampling_rate: 1.0,
    batch_size: 100,
    flush_interval: 5000,
    retry_attempts: 3,
    timeout: 30000,
    compression: true,
    encryption: false,
    storage_type: "file",
    retention_days: 30,
    debug_mode: false
  }
  
  // 模拟环境特定配置
  let environment_configs = {
    "development": {
      sampling_rate: 1.0,
      batch_size: 10,
      flush_interval: 1000,
      debug_mode: true,
      storage_type: "memory"
    },
    "testing": {
      sampling_rate: 1.0,
      batch_size: 50,
      flush_interval: 2000,
      debug_mode: true,
      retention_days: 7
    },
    "staging": {
      sampling_rate: 0.8,
      batch_size: 100,
      flush_interval: 5000,
      encryption: true,
      storage_type: "database"
    },
    "production": {
      sampling_rate: 0.1,
      batch_size: 500,
      flush_interval: 10000,
      encryption: true,
      storage_type: "database",
      retention_days: 90
    }
  }
  
  // 环境配置加载函数
  let load_environment_config = fn(base_config, env_config) {
    let mut merged_config = base_config
    
    // 应用环境特定配置
    for (key, value) in env_config {
      match key {
        "sampling_rate" => merged_config.sampling_rate = value
        "batch_size" => merged_config.batch_size = value
        "flush_interval" => merged_config.flush_interval = value
        "retry_attempts" => merged_config.retry_attempts = value
        "timeout" => merged_config.timeout = value
        "compression" => merged_config.compression = value
        "encryption" => merged_config.encryption = value
        "storage_type" => merged_config.storage_type = value
        "retention_days" => merged_config.retention_days = value
        "debug_mode" => merged_config.debug_mode = value
        _ => () // 忽略未知字段
      }
    }
    
    merged_config
  }
  
  // 加载开发环境配置
  let dev_config = load_environment_config(base_config, environment_configs["development"])
  
  // 验证开发环境配置
  assert_eq(dev_config.sampling_rate, 1.0) // 环境配置
  assert_eq(dev_config.batch_size, 10) // 环境配置
  assert_eq(dev_config.flush_interval, 1000) // 环境配置
  assert_eq(dev_config.retry_attempts, 3) // 基础配置
  assert_eq(dev_config.debug_mode, true) // 环境配置
  assert_eq(dev_config.storage_type, "memory") // 环境配置
  assert_eq(dev_config.encryption, false) // 基础配置
  
  // 加载测试环境配置
  let test_config = load_environment_config(base_config, environment_configs["testing"])
  
  // 验证测试环境配置
  assert_eq(test_config.sampling_rate, 1.0) // 环境配置
  assert_eq(test_config.batch_size, 50) // 环境配置
  assert_eq(test_config.flush_interval, 2000) // 环境配置
  assert_eq(test_config.debug_mode, true) // 环境配置
  assert_eq(test_config.retention_days, 7) // 环境配置
  assert_eq(test_config.storage_type, "file") // 基础配置
  
  // 加载生产环境配置
  let prod_config = load_environment_config(base_config, environment_configs["production"])
  
  // 验证生产环境配置
  assert_eq(prod_config.sampling_rate, 0.1) // 环境配置
  assert_eq(prod_config.batch_size, 500) // 环境配置
  assert_eq(prod_config.flush_interval, 10000) // 环境配置
  assert_eq(prod_config.encryption, true) // 环境配置
  assert_eq(prod_config.storage_type, "database") // 环境配置
  assert_eq(prod_config.retention_days, 90) // 环境配置
  assert_eq(prod_config.debug_mode, false) // 基础配置
  
  // 环境配置比较函数
  let compare_configs = fn(config1, config2) {
    let mut differences = []
    
    if config1.sampling_rate != config2.sampling_rate {
      differences = differences.push("sampling_rate")
    }
    if config1.batch_size != config2.batch_size {
      differences = differences.push("batch_size")
    }
    if config1.flush_interval != config2.flush_interval {
      differences = differences.push("flush_interval")
    }
    if config1.encryption != config2.encryption {
      differences = differences.push("encryption")
    }
    if config1.storage_type != config2.storage_type {
      differences = differences.push("storage_type")
    }
    if config1.retention_days != config2.retention_days {
      differences = differences.push("retention_days")
    }
    if config1.debug_mode != config2.debug_mode {
      differences = differences.push("debug_mode")
    }
    
    differences
  }
  
  // 比较开发环境和生产环境配置
  let env_differences = compare_configs(dev_config, prod_config)
  
  // 验证环境差异
  assert_true(env_differences.length() >= 5) // 至少有5个不同配置
  assert_true(env_differences.contains("sampling_rate"))
  assert_true(env_differences.contains("batch_size"))
  assert_true(env_differences.contains("flush_interval"))
  assert_true(env_differences.contains("storage_type"))
  assert_true(env_differences.contains("debug_mode"))
}

// 测试4: 配置模板与继承
test "配置模板与继承" {
  // 模拟配置模板
  let config_templates = {
    "basic": {
      sampling_rate: 1.0,
      batch_size: 100,
      flush_interval: 5000,
      compression: true,
      storage_type: "file"
    },
    "advanced": {
      sampling_rate: 0.8,
      batch_size: 200,
      flush_interval: 3000,
      compression: true,
      encryption: true,
      storage_type: "database",
      retention_days: 60
    },
    "high_performance": {
      sampling_rate: 0.5,
      batch_size: 500,
      flush_interval: 1000,
      compression: true,
      encryption: true,
      storage_type: "memory",
      retention_days: 7
    }
  }
  
  // 模拟具体配置（继承自模板）
  let service_configs = [
    {
      name: "auth-service",
      template: "basic",
      overrides: {
        batch_size: 50,
        retention_days: 30
      }
    },
    {
      name: "payment-service",
      template: "advanced",
      overrides: {
        sampling_rate: 1.0,
        encryption: true,
        retention_days: 90
      }
    },
    {
      name: "analytics-service",
      template: "high_performance",
      overrides: {
        batch_size: 1000,
        flush_interval: 500
      }
    }
  ]
  
  // 配置继承函数
  let inherit_config = fn(templates, template_name, overrides) {
    let template = templates[template_name]
    match template {
      Some(base_config) => {
        let mut inherited_config = base_config
        
        // 应用覆盖配置
        for (key, value) in overrides {
          match key {
            "sampling_rate" => inherited_config.sampling_rate = value
            "batch_size" => inherited_config.batch_size = value
            "flush_interval" => inherited_config.flush_interval = value
            "compression" => inherited_config.compression = value
            "encryption" => inherited_config.encryption = value
            "storage_type" => inherited_config.storage_type = value
            "retention_days" => inherited_config.retention_days = value
            _ => () // 忽略未知字段
          }
        }
        
        inherited_config
      }
      None => {
        // 如果模板不存在，返回默认配置
        {
          sampling_rate: 1.0,
          batch_size: 100,
          flush_interval: 5000,
          compression: false,
          encryption: false,
          storage_type: "file",
          retention_days: 30
        }
      }
    }
  }
  
  // 生成所有服务的最终配置
  let mut final_configs = []
  
  for service_config in service_configs {
    let final_config = inherit_config(config_templates, service_config.template, service_config.overrides)
    final_configs = final_configs.push({
      service: service_config.name,
      template: service_config.template,
      config: final_config
    })
  }
  
  // 验证auth-service配置
  let auth_config = final_configs[0]
  assert_eq(auth_config.service, "auth-service")
  assert_eq(auth_config.template, "basic")
  assert_eq(auth_config.config.sampling_rate, 1.0) // 继承自basic模板
  assert_eq(auth_config.config.batch_size, 50) // 覆盖配置
  assert_eq(auth_config.config.flush_interval, 5000) // 继承自basic模板
  assert_eq(auth_config.config.compression, true) // 继承自basic模板
  assert_eq(auth_config.config.storage_type, "file") // 继承自basic模板
  assert_eq(auth_config.config.retention_days, 30) // 覆盖配置
  assert_eq(auth_config.config.encryption, false) // 基础值（模板和覆盖中都没有）
  
  // 验证payment-service配置
  let payment_config = final_configs[1]
  assert_eq(payment_config.service, "payment-service")
  assert_eq(payment_config.template, "advanced")
  assert_eq(payment_config.config.sampling_rate, 1.0) // 覆盖配置
  assert_eq(payment_config.config.batch_size, 200) // 继承自advanced模板
  assert_eq(payment_config.config.encryption, true) // 继承自advanced模板（与覆盖值相同）
  assert_eq(payment_config.config.storage_type, "database") // 继承自advanced模板
  assert_eq(payment_config.config.retention_days, 90) // 覆盖配置
  
  // 验证analytics-service配置
  let analytics_config = final_configs[2]
  assert_eq(analytics_config.service, "analytics-service")
  assert_eq(analytics_config.template, "high_performance")
  assert_eq(analytics_config.config.sampling_rate, 0.5) // 继承自high_performance模板
  assert_eq(analytics_config.config.batch_size, 1000) // 覆盖配置
  assert_eq(analytics_config.config.flush_interval, 500) // 覆盖配置
  assert_eq(analytics_config.config.storage_type, "memory") // 继承自high_performance模板
  assert_eq(analytics_config.config.retention_days, 7) // 继承自high_performance模板
  
  // 模板使用统计
  let mut template_usage = {}
  
  for final_config in final_configs {
    let template_name = final_config.template
    let current_count = template_usage[template_name]
    match current_count {
      Some(count) => {
        template_usage[template_name] = Some(count + 1)
      }
      None => {
        template_usage[template_name] = Some(1)
      }
    }
  }
  
  // 验证模板使用统计
  assert_eq(template_usage["basic"], Some(1))
  assert_eq(template_usage["advanced"], Some(1))
  assert_eq(template_usage["high_performance"], Some(1))
}

// 测试5: 配置热重载
test "配置热重载" {
  // 模拟当前运行配置
  let mut runtime_config = {
    sampling_rate: 1.0,
    batch_size: 100,
    flush_interval: 5000,
    retry_attempts: 3,
    timeout: 30000,
    compression: true,
    encryption: false,
    storage_type: "file",
    retention_days: 30
  }
  
  // 模拟配置监听器
  let mut config_listeners = []
  
  // 配置变更通知函数
  let notify_config_change = fn(listeners, old_config, new_config) {
    let mut updated_listeners = []
    
    for listener in listeners {
      // 模拟监听器处理配置变更
      let notification_result = {
        listener_id: listener.id,
        old_config: old_config,
        new_config: new_config,
        status: "notified",
        timestamp: 1640995200
      }
      
      updated_listeners = updated_listeners.push(notification_result)
    }
    
    updated_listeners
  }
  
  // 添加配置监听器
  config_listeners = config_listeners.push({ id: "telemetry_collector", callback: "handle_telemetry_config_change" })
  config_listeners = config_listeners.push({ id: "storage_manager", callback: "handle_storage_config_change" })
  config_listeners = config_listeners.push({ id: "metrics_processor", callback: "handle_metrics_config_change" })
  
  // 保存当前配置（用于热重载前的备份）
  let backup_config = runtime_config
  
  // 模拟从外部源加载新配置
  let external_config = {
    sampling_rate: 0.8,
    batch_size: 200,
    flush_interval: 3000,
    encryption: true,
    storage_type: "database",
    retention_days: 60
  }
  
  // 热重载函数
  let hot_reload_config = fn(current_config, new_config, listeners) {
    // 保存旧配置
    let old_config = current_config
    
    // 应用新配置
    let mut updated_config = current_config
    for (key, value) in new_config {
      match key {
        "sampling_rate" => updated_config.sampling_rate = value
        "batch_size" => updated_config.batch_size = value
        "flush_interval" => updated_config.flush_interval = value
        "retry_attempts" => updated_config.retry_attempts = value
        "timeout" => updated_config.timeout = value
        "compression" => updated_config.compression = value
        "encryption" => updated_config.encryption = value
        "storage_type" => updated_config.storage_type = value
        "retention_days" => updated_config.retention_days = value
        _ => () // 忽略未知字段
      }
    }
    
    // 通知监听器
    let notifications = notify_config_change(listeners, old_config, updated_config)
    
    { config: updated_config, notifications: notifications }
  }
  
  // 执行热重载
  let reload_result = hot_reload_config(runtime_config, external_config, config_listeners)
  runtime_config = reload_result.config
  let notifications = reload_result.notifications
  
  // 验证热重载结果
  assert_eq(runtime_config.sampling_rate, 0.8) // 新配置
  assert_eq(runtime_config.batch_size, 200) // 新配置
  assert_eq(runtime_config.flush_interval, 3000) // 新配置
  assert_eq(runtime_config.retry_attempts, 3) // 保持原配置
  assert_eq(runtime_config.timeout, 30000) // 保持原配置
  assert_eq(runtime_config.compression, true) // 保持原配置
  assert_eq(runtime_config.encryption, true) // 新配置
  assert_eq(runtime_config.storage_type, "database") // 新配置
  assert_eq(runtime_config.retention_days, 60) // 新配置
  
  // 验证通知结果
  assert_eq(notifications.length(), 3) // 3个监听器
  
  // 验证telemetry_collector通知
  let telemetry_notification = notifications[0]
  assert_eq(telemetry_notification.listener_id, "telemetry_collector")
  assert_eq(telemetry_notification.status, "notified")
  assert_eq(telemetry_notification.old_config, backup_config)
  assert_eq(telemetry_notification.new_config, runtime_config)
  
  // 验证storage_manager通知
  let storage_notification = notifications[1]
  assert_eq(storage_notification.listener_id, "storage_manager")
  assert_eq(storage_notification.status, "notified")
  
  // 验证metrics_processor通知
  let metrics_notification = notifications[2]
  assert_eq(metrics_notification.listener_id, "metrics_processor")
  assert_eq(metrics_notification.status, "notified")
  
  // 配置回滚函数（热重载失败时使用）
  let rollback_hot_reload = fn(current_config, backup_config, listeners) {
    // 恢复备份配置
    let restored_config = backup_config
    
    // 通知监听器回滚
    let notifications = notify_config_change(listeners, current_config, restored_config)
    
    { config: restored_config, notifications: notifications }
  }
  
  // 模拟热重载失败，执行回滚
  let rollback_result = rollback_hot_reload(runtime_config, backup_config, config_listeners)
  let restored_config = rollback_result.config
  let rollback_notifications = rollback_result.notifications
  
  // 验证回滚结果
  assert_eq(restored_config, backup_config)
  assert_eq(restored_config.sampling_rate, 1.0) // 恢复到原始值
  assert_eq(restored_config.batch_size, 100) // 恢复到原始值
  assert_eq(restored_config.encryption, false) // 恢复到原始值
  assert_eq(restored_config.storage_type, "file") // 恢复到原始值
  
  // 验证回滚通知
  assert_eq(rollback_notifications.length(), 3) // 3个监听器收到回滚通知
}

// 测试6: 配置加密与安全
test "配置加密与安全" {
  // 模拟敏感配置信息
  let sensitive_config = {
    api_key: "sk-1234567890abcdef",
    database_password: "super_secret_password",
    encryption_key: "encryption_key_12345",
    storage_credentials: {
      access_key: "AKIAIOSFODNN7EXAMPLE",
      secret_key: "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
    },
    webhook_urls: [
      "https://api.example.com/webhooks/telemetry",
      "https://alerts.example.com/webhooks/notifications"
    ]
  }
  
  // 简化的加密函数（模拟）
  let encrypt_data = fn(data, key) {
    // 在实际实现中，这里会使用真正的加密算法
    // 这里仅模拟加密过程
    let mut encrypted = ""
    for i in range(0, data.length()) {
      let char_code = data[i].to_int()
      let key_char_code = key[i % key.length()].to_int()
      let encrypted_char_code = (char_code + key_char_code) % 256
      encrypted = encrypted + encrypted_char_code.to_char()
    }
    encrypted
  }
  
  // 简化的解密函数（模拟）
  let decrypt_data = fn(encrypted_data, key) {
    // 在实际实现中，这里会使用真正的解密算法
    // 这里仅模拟解密过程
    let mut decrypted = ""
    for i in range(0, encrypted_data.length()) {
      let char_code = encrypted_data[i].to_int()
      let key_char_code = key[i % key.length()].to_int()
      let decrypted_char_code = (char_code - key_char_code + 256) % 256
      decrypted = decrypted + decrypted_char_code.to_char()
    }
    decrypted
  }
  
  // 配置加密函数
  let encrypt_config = fn(config, encryption_key) {
    let mut encrypted_config = {}
    
    // 加密字符串字段
    let encrypted_api_key = encrypt_data(config.api_key, encryption_key)
    encrypted_config["api_key"] = Some(encrypted_api_key)
    
    let encrypted_db_password = encrypt_data(config.database_password, encryption_key)
    encrypted_config["database_password"] = Some(encrypted_db_password)
    
    let encrypted_encryption_key = encrypt_data(config.encryption_key, encryption_key)
    encrypted_config["encryption_key"] = Some(encrypted_encryption_key)
    
    // 加密嵌套对象
    let mut encrypted_credentials = {}
    let encrypted_access_key = encrypt_data(config.storage_credentials.access_key, encryption_key)
    encrypted_credentials["access_key"] = Some(encrypted_access_key)
    
    let encrypted_secret_key = encrypt_data(config.storage_credentials.secret_key, encryption_key)
    encrypted_credentials["secret_key"] = Some(encrypted_secret_key)
    
    encrypted_config["storage_credentials"] = Some(encrypted_credentials)
    
    // 加密URL数组
    let mut encrypted_urls = []
    for url in config.webhook_urls {
      let encrypted_url = encrypt_data(url, encryption_key)
      encrypted_urls = encrypted_urls.push(encrypted_url)
    }
    encrypted_config["webhook_urls"] = Some(encrypted_urls)
    
    encrypted_config
  }
  
  // 配置解密函数
  let decrypt_config = fn(encrypted_config, encryption_key) {
    let mut decrypted_config = {}
    
    // 解密字符串字段
    let encrypted_api_key = encrypted_config["api_key"]
    match encrypted_api_key {
      Some(encrypted) => {
        let decrypted = decrypt_data(encrypted, encryption_key)
        decrypted_config["api_key"] = Some(decrypted)
      }
      None => ()
    }
    
    let encrypted_db_password = encrypted_config["database_password"]
    match encrypted_db_password {
      Some(encrypted) => {
        let decrypted = decrypt_data(encrypted, encryption_key)
        decrypted_config["database_password"] = Some(decrypted)
      }
      None => ()
    }
    
    let encrypted_encryption_key = encrypted_config["encryption_key"]
    match encrypted_encryption_key {
      Some(encrypted) => {
        let decrypted = decrypt_data(encrypted, encryption_key)
        decrypted_config["encryption_key"] = Some(decrypted)
      }
      None => ()
    }
    
    // 解密嵌套对象
    let encrypted_credentials = encrypted_config["storage_credentials"]
    match encrypted_credentials {
      Some(credentials) => {
        let mut decrypted_credentials = {}
        
        let encrypted_access_key = credentials["access_key"]
        match encrypted_access_key {
          Some(encrypted) => {
            let decrypted = decrypt_data(encrypted, encryption_key)
            decrypted_credentials["access_key"] = Some(decrypted)
          }
          None => ()
        }
        
        let encrypted_secret_key = credentials["secret_key"]
        match encrypted_secret_key {
          Some(encrypted) => {
            let decrypted = decrypt_data(encrypted, encryption_key)
            decrypted_credentials["secret_key"] = Some(decrypted)
          }
          None => ()
        }
        
        decrypted_config["storage_credentials"] = Some(decrypted_credentials)
      }
      None => ()
    }
    
    // 解密URL数组
    let encrypted_urls = encrypted_config["webhook_urls"]
    match encrypted_urls {
      Some(urls) => {
        let mut decrypted_urls = []
        for encrypted_url in urls {
          let decrypted = decrypt_data(encrypted_url, encryption_key)
          decrypted_urls = decrypted_urls.push(decrypted)
        }
        decrypted_config["webhook_urls"] = Some(decrypted_urls)
      }
      None => ()
    }
    
    decrypted_config
  }
  
  // 执行配置加密
  let encryption_key = "my_secret_key"
  let encrypted_config = encrypt_config(sensitive_config, encryption_key)
  
  // 验证加密结果
  assert_not_eq(encrypted_config["api_key"], Some(sensitive_config.api_key)) // 已加密
  assert_not_eq(encrypted_config["database_password"], Some(sensitive_config.database_password)) // 已加密
  assert_not_eq(encrypted_config["encryption_key"], Some(sensitive_config.encryption_key)) // 已加密
  
  // 验证加密后的值与原始值不同
  let encrypted_api_key = encrypted_config["api_key"]
  match encrypted_api_key {
    Some(encrypted) => {
      assert_not_eq(encrypted, sensitive_config.api_key)
      assert_eq(encrypted.length(), sensitive_config.api_key.length()) // 长度相同
    }
    None => assert_true(false)
  }
  
  // 执行配置解密
  let decrypted_config = decrypt_config(encrypted_config, encryption_key)
  
  // 验证解密结果
  assert_eq(decrypted_config["api_key"], Some(sensitive_config.api_key))
  assert_eq(decrypted_config["database_password"], Some(sensitive_config.database_password))
  assert_eq(decrypted_config["encryption_key"], Some(sensitive_config.encryption_key))
  
  // 验证嵌套对象解密
  let decrypted_credentials = decrypted_config["storage_credentials"]
  match decrypted_credentials {
    Some(credentials) => {
      assert_eq(credentials["access_key"], Some(sensitive_config.storage_credentials.access_key))
      assert_eq(credentials["secret_key"], Some(sensitive_config.storage_credentials.secret_key))
    }
    None => assert_true(false)
  }
  
  // 验证URL数组解密
  let decrypted_urls = decrypted_config["webhook_urls"]
  match decrypted_urls {
    Some(urls) => {
      assert_eq(urls.length(), sensitive_config.webhook_urls.length())
      assert_eq(urls[0], sensitive_config.webhook_urls[0])
      assert_eq(urls[1], sensitive_config.webhook_urls[1])
    }
    None => assert_true(false)
  }
  
  // 验证使用错误密钥解密失败
  let wrong_key = "wrong_key"
  let incorrectly_decrypted = decrypt_config(encrypted_config, wrong_key)
  
  // 验证错误密钥解密结果与原始值不同
  assert_not_eq(incorrectly_decrypted["api_key"], Some(sensitive_config.api_key))
  assert_not_eq(incorrectly_decrypted["database_password"], Some(sensitive_config.database_password))
}