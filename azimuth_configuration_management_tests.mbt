// Azimuth 配置管理测试用例
// 测试配置加载、验证、更新和动态配置功能

test "配置加载和解析" {
  // 创建配置源
  let config_source = @azimuth.ConfigurationSource {
    type: @azimuth.ConfigSourceType.File,
    location: "/etc/azimuth/config.json",
    format: @azimuth.ConfigFormat.Json,
    priority: 1,
    watch_changes: true,
    encryption_enabled: false
  }
  
  // 模拟配置数据
  let config_data = @azimuth.ConfigurationData {
    values: [
      ("telemetry.service.name", @azimuth.ConfigValue.StringValue("azimuth-service")),
      ("telemetry.service.version", @azimuth.ConfigValue.StringValue("1.0.0")),
      ("telemetry.endpoint.url", @azimuth.ConfigValue.StringValue("https://telemetry.example.com/api/v1")),
      ("telemetry.endpoint.timeout_ms", @azimuth.ConfigValue.IntValue(5000)),
      ("telemetry.batch.size", @azimuth.ConfigValue.IntValue(100)),
      ("telemetry.batch.flush_interval_ms", @azimuth.ConfigValue.IntValue(1000)),
      ("telemetry.sampling.probability", @azimuth.ConfigValue.FloatValue(0.1)),
      ("telemetry.metrics.enabled", @azimuth.ConfigValue.BoolValue(true)),
      ("telemetry.tracing.enabled", @azimuth.ConfigValue.BoolValue(true)),
      ("telemetry.logging.enabled", @azimuth.ConfigValue.BoolValue(true)),
      ("telemetry.logging.level", @azimuth.ConfigValue.StringValue("INFO")),
      ("telemetry.retry.max_attempts", @azimuth.ConfigValue.IntValue(3)),
      ("telemetry.retry.initial_delay_ms", @azimuth.ConfigValue.IntValue(1000)),
      ("telemetry.retry.max_delay_ms", @azimuth.ConfigValue.IntValue(10000)),
      ("telemetry.retry.backoff_multiplier", @azimuth.ConfigValue.FloatValue(2.0))
    ]
  }
  
  // 创建配置管理器
  let config_manager = @azimuth.ConfigurationManager {
    sources: [config_source],
    config: config_data,
    validators: [],
    watchers: [],
    last_updated: @azimuth.Timestamp(1640995200)
  }
  
  // 验证配置加载
  assert_eq(config_manager.sources.length(), 1)
  assert_eq(config_manager.config.values.length(), 15)
  assert_eq(config_manager.sources[0].type, @azimuth.ConfigSourceType.File)
  assert_eq(config_manager.sources[0].format, @azimuth.ConfigFormat.Json)
  assert_true(config_manager.sources[0].watch_changes)
  
  // 验证配置值
  let service_name = config_manager.config.values.find(fn(kv) { kv.0 == "telemetry.service.name" })
  let service_version = config_manager.config.values.find(fn(kv) { kv.0 == "telemetry.service.version" })
  let endpoint_url = config_manager.config.values.find(fn(kv) { kv.0 == "telemetry.endpoint.url" })
  let timeout_ms = config_manager.config.values.find(fn(kv) { kv.0 == "telemetry.endpoint.timeout_ms" })
  let batch_size = config_manager.config.values.find(fn(kv) { kv.0 == "telemetry.batch.size" })
  let sampling_probability = config_manager.config.values.find(fn(kv) { kv.0 == "telemetry.sampling.probability" })
  let metrics_enabled = config_manager.config.values.find(fn(kv) { kv.0 == "telemetry.metrics.enabled" })
  
  match service_name {
    Some((_, @azimuth.ConfigValue.StringValue(name))) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
  
  match service_version {
    Some((_, @azimuth.ConfigValue.StringValue(version))) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  match endpoint_url {
    Some((_, @azimuth.ConfigValue.StringValue(url))) => assert_eq(url, "https://telemetry.example.com/api/v1")
    _ => assert_true(false)
  }
  
  match timeout_ms {
    Some((_, @azimuth.ConfigValue.IntValue(timeout))) => assert_eq(timeout, 5000)
    _ => assert_true(false)
  }
  
  match batch_size {
    Some((_, @azimuth.ConfigValue.IntValue(size))) => assert_eq(size, 100)
    _ => assert_true(false)
  }
  
  match sampling_probability {
    Some((_, @azimuth.ConfigValue.FloatValue(probability))) => assert_eq(probability, 0.1)
    _ => assert_true(false)
  }
  
  match metrics_enabled {
    Some((_, @azimuth.ConfigValue.BoolValue(enabled))) => assert_true(enabled)
    _ => assert_true(false)
  }
}

test "配置验证和默认值" {
  // 创建配置验证器
  let config_validators = [
    @azimuth.ConfigValidator {
      key: "telemetry.endpoint.url",
      validator_type: @azimuth.ValidatorType.Url,
      required: true,
      default_value: Some(@azimuth.ConfigValue.StringValue("https://default.telemetry.com/api/v1"))
    },
    @azimuth.ConfigValidator {
      key: "telemetry.endpoint.timeout_ms",
      validator_type: @azimuth.ValidatorType.Range,
      required: true,
      default_value: Some(@azimuth.ConfigValue.IntValue(3000)),
      constraints: @azimuth.ValidationConstraints {
        min_value: Some(@azimuth.ConfigValue.IntValue(1000)),
        max_value: Some(@azimuth.ConfigValue.IntValue(30000))
      }
    },
    @azimuth.ConfigValidator {
      key: "telemetry.sampling.probability",
      validator_type: @azimuth.ValidatorType.Range,
      required: true,
      default_value: Some(@azimuth.ConfigValue.FloatValue(0.1)),
      constraints: @azimuth.ValidationConstraints {
        min_value: Some(@azimuth.ConfigValue.FloatValue(0.0)),
        max_value: Some(@azimuth.ConfigValue.FloatValue(1.0))
      }
    },
    @azimuth.ConfigValidator {
      key: "telemetry.logging.level",
      validator_type: @azimuth.ValidatorType.Enum,
      required: true,
      default_value: Some(@azimuth.ConfigValue.StringValue("INFO")),
      constraints: @azimuth.ValidationConstraints {
        allowed_values: [
          @azimuth.ConfigValue.StringValue("DEBUG"),
          @azimuth.ConfigValue.StringValue("INFO"),
          @azimuth.ConfigValue.StringValue("WARN"),
          @azimuth.ConfigValue.StringValue("ERROR")
        ]
      }
    },
    @azimuth.ConfigValidator {
      key: "telemetry.optional.feature",
      validator_type: @azimuth.ValidatorType.Any,
      required: false,
      default_value: Some(@azimuth.ConfigValue.BoolValue(false))
    }
  ]
  
  // 创建部分配置（缺少一些必需项）
  let partial_config = @azimuth.ConfigurationData {
    values: [
      ("telemetry.endpoint.url", @azimuth.ConfigValue.StringValue("https://custom.telemetry.com/api")),
      ("telemetry.endpoint.timeout_ms", @azimuth.ConfigValue.IntValue(500)), // 超出范围
      ("telemetry.sampling.probability", @azimuth.ConfigValue.FloatValue(1.5)), // 超出范围
      ("telemetry.logging.level", @azimuth.ConfigValue.StringValue("VERBOSE")) // 不在枚举中
    ]
  }
  
  // 验证配置并应用默认值
  let mut validated_config = partial_config.values
  
  for validator in config_validators {
    let config_value = validated_config.find(fn(kv) { kv.0 == validator.key })
    
    match config_value {
      Some((key, value)) => {
        // 验证现有值
        let is_valid = match validator.validator_type {
          @azimuth.ValidatorType.Url => {
            match value {
              @azimuth.ConfigValue.StringValue(url) => url.starts_with("http://") || url.starts_with("https://")
              _ => false
            }
          }
          @azimuth.ValidatorType.Range => {
            match (value, validator.constraints.min_value, validator.constraints.max_value) {
              (@azimuth.ConfigValue.IntValue(v), Some(@azimuth.ConfigValue.IntValue(min)), Some(@azimuth.ConfigValue.IntValue(max))) => 
                v >= min && v <= max
              (@azimuth.ConfigValue.FloatValue(v), Some(@azimuth.ConfigValue.FloatValue(min)), Some(@azimuth.ConfigValue.FloatValue(max))) => 
                v >= min && v <= max
              _ => true
            }
          }
          @azimuth.ValidatorType.Enum => {
            match (value, validator.constraints.allowed_values) {
              (@azimuth.ConfigValue.StringValue(v), Some(allowed)) => 
                allowed.contains(@azimuth.ConfigValue.StringValue(v))
              _ => true
            }
          }
          @azimuth.ValidatorType.Any => true
        }
        
        // 如果无效且有默认值，则使用默认值
        if !is_valid && validator.default_value.is_some() {
          validated_config = validated_config.map(fn(kv) {
            if kv.0 == validator.key {
              (validator.key, validator.default_value.unwrap())
            } else {
              kv
            }
          })
        }
      }
      None => {
        // 如果配置不存在但是必需的，则使用默认值
        if validator.required && validator.default_value.is_some() {
          validated_config = validated_config + [(validator.key, validator.default_value.unwrap())]
        }
      }
    }
  }
  
  // 验证最终配置
  let final_url = validated_config.find(fn(kv) { kv.0 == "telemetry.endpoint.url" })
  let final_timeout = validated_config.find(fn(kv) { kv.0 == "telemetry.endpoint.timeout_ms" })
  let final_sampling = validated_config.find(fn(kv) { kv.0 == "telemetry.sampling.probability" })
  let final_level = validated_config.find(fn(kv) { kv.0 == "telemetry.logging.level" })
  let final_optional = validated_config.find(fn(kv) { kv.0 == "telemetry.optional.feature" })
  
  // URL应该保持原值（有效）
  match final_url {
    Some((_, @azimuth.ConfigValue.StringValue(url))) => assert_eq(url, "https://custom.telemetry.com/api")
    _ => assert_true(false)
  }
  
  // Timeout应该使用默认值（无效）
  match final_timeout {
    Some((_, @azimuth.ConfigValue.IntValue(timeout))) => assert_eq(timeout, 3000)
    _ => assert_true(false)
  }
  
  // Sampling应该使用默认值（无效）
  match final_sampling {
    Some((_, @azimuth.ConfigValue.FloatValue(sampling))) => assert_eq(sampling, 0.1)
    _ => assert_true(false)
  }
  
  // Level应该使用默认值（无效）
  match final_level {
    Some((_, @azimuth.ConfigValue.StringValue(level))) => assert_eq(level, "INFO")
    _ => assert_true(false)
  }
  
  // Optional应该使用默认值（不存在）
  match final_optional {
    Some((_, @azimuth.ConfigValue.BoolValue(optional))) => assert_false(optional)
    _ => assert_true(false)
  }
}

test "动态配置更新" {
  // 创建配置管理器
  let config_manager = @azimuth.ConfigurationManager {
    sources: [],
    config: @azimuth.ConfigurationData { values: [] },
    validators: [],
    watchers: [],
    last_updated: @azimuth.Timestamp(1640995200)
  }
  
  // 创建配置变更监听器
  let config_watchers = [
    @azimuth.ConfigWatcher {
      id: "watcher-001",
      patterns: ["telemetry.endpoint.*"],
      callback: fn(key, old_value, new_value) {
        // 模拟配置变更回调
        @azimuth.WatcherResult.Success
      }
    },
    @azimuth.ConfigWatcher {
      id: "watcher-002",
      patterns: ["telemetry.batch.*"],
      callback: fn(key, old_value, new_value) {
        // 模拟配置变更回调
        @azimuth.WatcherResult.Success
      }
    }
  ]
  
  // 模拟配置更新
  let config_updates = [
    @azimuth.ConfigUpdate {
      key: "telemetry.endpoint.url",
      old_value: Some(@azimuth.ConfigValue.StringValue("https://old.telemetry.com/api")),
      new_value: @azimuth.ConfigValue.StringValue("https://new.telemetry.com/api"),
      timestamp: @azimuth.Timestamp(1640995300),
      source: "file"
    },
    @azimuth.ConfigUpdate {
      key: "telemetry.batch.size",
      old_value: Some(@azimuth.ConfigValue.IntValue(100)),
      new_value: @azimuth.ConfigValue.IntValue(200),
      timestamp: @azimuth.Timestamp(1640995400),
      source: "file"
    },
    @azimuth.ConfigUpdate {
      key: "telemetry.sampling.probability",
      old_value: Some(@azimuth.ConfigValue.FloatValue(0.1)),
      new_value: @azimuth.ConfigValue.FloatValue(0.2),
      timestamp: @azimuth.Timestamp(1640995500),
      source: "env"
    }
  ]
  
  // 验证配置更新
  assert_eq(config_updates.length(), 3)
  
  // 检查哪些监听器应该被触发
  let endpoint_update = config_updates[0]
  let batch_update = config_updates[1]
  let sampling_update = config_updates[2]
  
  // Endpoint更新应该触发watcher-001
  let endpoint_matches_watcher1 = config_watchers[0].patterns.any(fn(pattern) {
    endpoint_update.key.starts_with(pattern.replace("*", ""))
  })
  assert_true(endpoint_matches_watcher1)
  
  // Batch更新应该触发watcher-002
  let batch_matches_watcher2 = config_watchers[1].patterns.any(fn(pattern) {
    batch_update.key.starts_with(pattern.replace("*", ""))
  })
  assert_true(batch_matches_watcher2)
  
  // Sampling更新不应该触发任何监听器
  let sampling_matches_watcher1 = config_watchers[0].patterns.any(fn(pattern) {
    sampling_update.key.starts_with(pattern.replace("*", ""))
  })
  let sampling_matches_watcher2 = config_watchers[1].patterns.any(fn(pattern) {
    sampling_update.key.starts_with(pattern.replace("*", ""))
  })
  assert_false(sampling_matches_watcher1)
  assert_false(sampling_matches_watcher2)
  
  // 计算配置变更统计
  let updates_by_source = [
    ("file", config_updates.filter(fn(u) { u.source == "file" }).length()),
    ("env", config_updates.filter(fn(u) { u.source == "env" }).length())
  ]
  
  assert_eq(updates_by_source[0].1, 2) // 2个来自file的更新
  assert_eq(updates_by_source[1].1, 1) // 1个来自env的更新
  
  // 验证时间顺序
  let updates_sorted = config_updates.sort(fn(a, b) {
    if a.timestamp < b.timestamp { -1 } 
    else if a.timestamp > b.timestamp { 1 } 
    else { 0 }
  })
  
  assert_eq(updates_sorted[0].key, "telemetry.endpoint.url")
  assert_eq(updates_sorted[1].key, "telemetry.batch.size")
  assert_eq(updates_sorted[2].key, "telemetry.sampling.probability")
}

test "配置层级和继承" {
  // 创建配置层级
  let config_layers = [
    @azimuth.ConfigLayer {
      name: "base",
      priority: 1,
      data: @azimuth.ConfigurationData {
        values: [
          ("telemetry.service.name", @azimuth.ConfigValue.StringValue("azimuth-service")),
          ("telemetry.service.version", @azimuth.ConfigValue.StringValue("1.0.0")),
          ("telemetry.endpoint.url", @azimuth.ConfigValue.StringValue("https://default.telemetry.com/api")),
          ("telemetry.endpoint.timeout_ms", @azimuth.ConfigValue.IntValue(5000)),
          ("telemetry.batch.size", @azimuth.ConfigValue.IntValue(100)),
          ("telemetry.sampling.probability", @azimuth.ConfigValue.FloatValue(0.1)),
          ("telemetry.logging.level", @azimuth.ConfigValue.StringValue("INFO"))
        ]
      }
    },
    @azimuth.ConfigLayer {
      name: "environment",
      priority: 2,
      data: @azimuth.ConfigurationData {
        values: [
          ("telemetry.service.version", @azimuth.ConfigValue.StringValue("1.1.0")),
          ("telemetry.endpoint.url", @azimuth.ConfigValue.StringValue("https://env.telemetry.com/api")),
          ("telemetry.sampling.probability", @azimuth.ConfigValue.FloatValue(0.2))
        ]
      }
    },
    @azimuth.ConfigLayer {
      name: "override",
      priority: 3,
      data: @azimuth.ConfigurationData {
        values: [
          ("telemetry.endpoint.timeout_ms", @azimuth.ConfigValue.IntValue(10000)),
          ("telemetry.batch.size", @azimuth.ConfigValue.IntValue(200))
        ]
      }
    }
  ]
  
  // 按优先级排序配置层
  let sorted_layers = config_layers.sort(fn(a, b) {
    if a.priority < b.priority { -1 } 
    else if a.priority > b.priority { 1 } 
    else { 0 }
  })
  
  // 合并配置层（高优先级覆盖低优先级）
  let mut merged_config = @azimuth.ConfigurationData { values: [] }
  
  for layer in sorted_layers {
    for config_item in layer.data.values {
      let existing_index = merged_config.values.index_of(fn(item) { item.0 == config_item.0 })
      
      match existing_index {
        Some(index) => {
          // 覆盖现有值
          merged_config.values[index] = config_item
        }
        None => {
          // 添加新值
          merged_config.values = merged_config.values + [config_item]
        }
      }
    }
  }
  
  // 验证配置合并结果
  let service_name = merged_config.values.find(fn(kv) { kv.0 == "telemetry.service.name" })
  let service_version = merged_config.values.find(fn(kv) { kv.0 == "telemetry.service.version" })
  let endpoint_url = merged_config.values.find(fn(kv) { kv.0 == "telemetry.endpoint.url" })
  let endpoint_timeout = merged_config.values.find(fn(kv) { kv.0 == "telemetry.endpoint.timeout_ms" })
  let batch_size = merged_config.values.find(fn(kv) { kv.0 == "telemetry.batch.size" })
  let sampling_probability = merged_config.values.find(fn(kv) { kv.0 == "telemetry.sampling.probability" })
  let logging_level = merged_config.values.find(fn(kv) { kv.0 == "telemetry.logging.level" })
  
  // 验证最终值（高优先级覆盖低优先级）
  match service_name {
    Some((_, @azimuth.ConfigValue.StringValue(name))) => assert_eq(name, "azimuth-service") // 来自base层
    _ => assert_true(false)
  }
  
  match service_version {
    Some((_, @azimuth.ConfigValue.StringValue(version))) => assert_eq(version, "1.1.0") // 来自environment层
    _ => assert_true(false)
  }
  
  match endpoint_url {
    Some((_, @azimuth.ConfigValue.StringValue(url))) => assert_eq(url, "https://env.telemetry.com/api") // 来自environment层
    _ => assert_true(false)
  }
  
  match endpoint_timeout {
    Some((_, @azimuth.ConfigValue.IntValue(timeout))) => assert_eq(timeout, 10000) // 来自override层
    _ => assert_true(false)
  }
  
  match batch_size {
    Some((_, @azimuth.ConfigValue.IntValue(size))) => assert_eq(size, 200) // 来自override层
    _ => assert_true(false)
  }
  
  match sampling_probability {
    Some((_, @azimuth.ConfigValue.FloatValue(probability))) => assert_eq(probability, 0.2) // 来自environment层
    _ => assert_true(false)
  }
  
  match logging_level {
    Some((_, @azimuth.ConfigValue.StringValue(level))) => assert_eq(level, "INFO") // 来自base层
    _ => assert_true(false)
  }
  
  // 验证配置来源追踪
  let config_sources = [
    ("telemetry.service.name", "base"),
    ("telemetry.service.version", "environment"),
    ("telemetry.endpoint.url", "environment"),
    ("telemetry.endpoint.timeout_ms", "override"),
    ("telemetry.batch.size", "override"),
    ("telemetry.sampling.probability", "environment"),
    ("telemetry.logging.level", "base")
  ]
  
  assert_eq(config_sources.length(), 7)
  assert_eq(config_sources.filter(fn(s) { s.1 == "base" }).length(), 2)
  assert_eq(config_sources.filter(fn(s) { s.1 == "environment" }).length(), 3)
  assert_eq(config_sources.filter(fn(s) { s.1 == "override" }).length(), 2)
}