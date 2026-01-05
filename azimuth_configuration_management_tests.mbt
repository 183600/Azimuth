// Azimuth 配置管理测试
// 专注于配置加载、验证、更新和管理功能

// 测试1: 配置文件加载和解析
test "配置文件加载和解析基础功能" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 配置支持的文件格式
  ConfigManager::register_format(config_manager, "yaml", YamlConfigParser::new())
  ConfigManager::register_format(config_manager, "json", JsonConfigParser::new())
  ConfigManager::register_format(config_manager, "toml", TomlConfigParser::new())
  ConfigManager::register_format(config_manager, "properties", PropertiesConfigParser::new())
  
  // 创建YAML配置内容
  let yaml_config_content = "
# Azimuth Telemetry Configuration
service:
  name: \"payment-service\"
  version: \"1.2.3\"
  environment: \"production\"
  
telemetry:
  enabled: true
  sampling_rate: 0.1
  
endpoints:
  primary: \"https://telemetry.example.com\"
  fallback: \"https://telemetry-backup.example.com\"
  
database:
  host: \"db.example.com\"
  port: 5432
  name: \"payments\"
  username: \"payment_user\"
  password: \"${DB_PASSWORD}\"  # 环境变量替换
  pool_size: 20
  timeout: 30000
  
logging:
  level: \"info\"
  format: \"json\"
  outputs:
    - type: \"file\"
      path: \"/var/log/payment-service.log\"
      rotation: \"daily\"
    - type: \"console\"
      colored: true
      
metrics:
  enabled: true
  interval: 60000  # 60秒
  endpoints:
    - \"/metrics\"
    - \"/health/metrics\"
    
features:
  - name: \"advanced_tracing\"
    enabled: true
    config:
      max_spans: 1000
      timeout: 5000
  - name: \"custom_metrics\"
    enabled: false
    config: {}
"
  
  // 创建JSON配置内容
  let json_config_content = "{
  \"service\": {
    \"name\": \"order-service\",
    \"version\": \"2.1.0\",
    \"environment\": \"staging\"
  },
  \"telemetry\": {
    \"enabled\": true,
    \"sampling_rate\": 0.2
  },
  \"cache\": {
    \"type\": \"redis\",
    \"host\": \"cache.example.com\",
    \"port\": 6379,
    \"ttl\": 3600
  },
  \"api\": {
    \"port\": 8080,
    \"timeout\": 30000,
    \"rate_limit\": {
      \"requests_per_minute\": 1000,
      \"burst\": 100
    }
  }
}"
  
  // 创建TOML配置内容
  let toml_config_content = "
[service]
name = \"user-service\"
version = \"3.0.1\"
environment = \"development\"

[database]
host = \"localhost\"
port = 5432
name = \"users_dev\"
username = \"dev_user\"
password = \"dev_password\"
pool_size = 10

[redis]
host = \"localhost\"
port = 6379
db = 0

[logging]
level = \"debug\"
format = \"text\"

[features]
advanced_search = true
user_analytics = false
beta_features = true
"
  
  // 创建Properties配置内容
  let properties_config_content = "
service.name=notification-service
service.version=1.5.2
service.environment=test

smtp.host=smtp.example.com
smtp.port=587
smtp.username=notifications@example.com
smtp.password=${SMTP_PASSWORD}

sms.provider=twilio
sms.api_key=${SMS_API_KEY}
sms.from_number=+1234567890

push.enabled=true
push.firebase.config=${FIREBASE_CONFIG}
"
  
  // 测试YAML配置加载
  let yaml_result = ConfigManager::load_from_string(config_manager, yaml_config_content, "yaml")
  assert_true(yaml_result.success)
  
  let yaml_config = yaml_result.config
  assert_eq(yaml_config.get("service.name"), Some("payment-service"))
  assert_eq(yaml_config.get("service.version"), Some("1.2.3"))
  assert_eq(yaml_config.get("service.environment"), Some("production"))
  assert_eq(yaml_config.get("telemetry.enabled"), Some("true"))
  assert_eq(yaml_config.get("telemetry.sampling_rate"), Some("0.1"))
  assert_eq(yaml_config.get("database.port"), Some("5432"))
  assert_eq(yaml_config.get("database.pool_size"), Some("20"))
  
  // 测试嵌套配置访问
  let logging_outputs = yaml_config.get_array("logging.outputs")
  assert_true(logging_outputs != None)
  
  match logging_outputs {
    Some(outputs) => {
      assert_eq(outputs.length(), 2)
      
      let file_output = outputs[0]
      assert_eq(file_output.get("type"), Some("file"))
      assert_eq(file_output.get("path"), Some("/var/log/payment-service.log"))
      
      let console_output = outputs[1]
      assert_eq(console_output.get("type"), Some("console"))
      assert_eq(console_output.get("colored"), Some("true"))
    }
    None => assert_true(false)
  }
  
  // 测试特性配置
  let features = yaml_config.get_array("features")
  assert_true(features != None)
  
  match features {
    Some(feature_list) => {
      assert_eq(feature_list.length(), 2)
      
      let advanced_tracing = feature_list[0]
      assert_eq(advanced_tracing.get("name"), Some("advanced_tracing"))
      assert_eq(advanced_tracing.get("enabled"), Some("true"))
      assert_eq(advanced_tracing.get("config.max_spans"), Some("1000"))
      
      let custom_metrics = feature_list[1]
      assert_eq(custom_metrics.get("name"), Some("custom_metrics"))
      assert_eq(custom_metrics.get("enabled"), Some("false"))
    }
    None => assert_true(false)
  }
  
  // 测试JSON配置加载
  let json_result = ConfigManager::load_from_string(config_manager, json_config_content, "json")
  assert_true(json_result.success)
  
  let json_config = json_result.config
  assert_eq(json_config.get("service.name"), Some("order-service"))
  assert_eq(json_config.get("service.version"), Some("2.1.0"))
  assert_eq(json_config.get("telemetry.sampling_rate"), Some("0.2"))
  assert_eq(json_config.get("cache.type"), Some("redis"))
  assert_eq(json_config.get("api.port"), Some("8080"))
  
  // 测试TOML配置加载
  let toml_result = ConfigManager::load_from_string(config_manager, toml_config_content, "toml")
  assert_true(toml_result.success)
  
  let toml_config = toml_result.config
  assert_eq(toml_config.get("service.name"), Some("user-service"))
  assert_eq(toml_config.get("service.version"), Some("3.0.1"))
  assert_eq(toml_config.get("database.port"), Some("5432"))
  assert_eq(toml_config.get("redis.db"), Some("0"))
  
  // 测试Properties配置加载
  let properties_result = ConfigManager::load_from_string(config_manager, properties_config_content, "properties")
  assert_true(properties_result.success)
  
  let properties_config = properties_result.config
  assert_eq(properties_config.get("service.name"), Some("notification-service"))
  assert_eq(properties_config.get("service.version"), Some("1.5.2"))
  assert_eq(properties_config.get("smtp.port"), Some("587"))
  assert_eq(properties_config.get("sms.provider"), Some("twilio"))
  assert_eq(properties_config.get("push.enabled"), Some("true"))
  
  // 测试配置合并
  let merged_config = ConfigManager::merge(config_manager, [
    yaml_config,
    json_config,
    toml_config,
    properties_config
  ])
  
  // 验证合并后的配置
  assert_eq(merged_config.get("service.name"), Some("notification-service"))  // 最后一个覆盖
  assert_eq(merged_config.get("service.version"), Some("1.5.2"))
  assert_eq(merged_config.get("telemetry.enabled"), Some("true"))
  assert_eq(merged_config.get("telemetry.sampling_rate"), Some("0.2"))  // JSON覆盖YAML
  assert_eq(merged_config.get("database.port"), Some("5432"))
  assert_eq(merged_config.get("cache.type"), Some("redis"))
  assert_eq(merged_config.get("redis.db"), Some("0"))
  assert_eq(merged_config.get("smtp.port"), Some("587"))
  
  // 测试配置验证
  let validation_rules = [
    {
      path: "service.name",
      required: true,
      type: "string",
      min_length: 1,
      max_length: 100
    },
    {
      path: "service.version",
      required: true,
      type: "string",
      pattern: "^\\d+\\.\\d+\\.\\d+$"
    },
    {
      path: "telemetry.sampling_rate",
      required: false,
      type: "float",
      min_value: 0.0,
      max_value: 1.0
    },
    {
      path: "database.port",
      required: false,
      type: "int",
      min_value: 1,
      max_value: 65535
    }
  ]
  
  let validation_result = ConfigManager::validate(config_manager, merged_config, validation_rules)
  assert_true(validation_result.valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // 测试无效配置验证
  let invalid_config = ConfigManager::create_empty(config_manager)
  ConfigManager::set(invalid_config, "service.name", "")  // 空字符串
  ConfigManager::set(invalid_config, "service.version", "invalid")  // 无效版本格式
  ConfigManager::set(invalid_config, "telemetry.sampling_rate", "1.5")  // 超出范围
  ConfigManager::set(invalid_config, "database.port", "70000")  // 超出端口范围
  
  let invalid_validation_result = ConfigManager::validate(config_manager, invalid_config, validation_rules)
  assert_false(invalid_validation_result.valid)
  assert_true(invalid_validation_result.errors.length() >= 4)
  
  // 验证具体错误
  let has_name_error = invalid_validation_result.errors.any(fn(e) { e.contains("service.name") })
  let has_version_error = invalid_validation_result.errors.any(fn(e) { e.contains("service.version") })
  let has_sampling_rate_error = invalid_validation_result.errors.any(fn(e) { e.contains("sampling_rate") })
  let has_port_error = invalid_validation_result.errors.any(fn(e) { e.contains("database.port") })
  
  assert_true(has_name_error)
  assert_true(has_version_error)
  assert_true(has_sampling_rate_error)
  assert_true(has_port_error)
}

// 测试2: 环境变量和配置替换
test "环境变量和配置替换功能" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  ConfigManager::register_format(config_manager, "yaml", YamlConfigParser::new())
  
  // 设置环境变量
  let env_vars = {
    "SERVICE_NAME": "env-service",
    "SERVICE_VERSION": "2.0.0",
    "DB_HOST": "env-db.example.com",
    "DB_PASSWORD": "env-secret-password",
    "API_PORT": "9090",
    "LOG_LEVEL": "debug",
    "ENABLE_METRICS": "true",
    "CACHE_TTL": "7200",
    "FEATURE_FLAGS": "feature1,feature2,feature3",
    "CONFIG_JSON": "{\"nested\": {\"value\": \"from_env\"}}"
  }
  
  // 配置环境变量提供者
  ConfigManager::set_environment_provider(config_manager, {
    get: fn(key) {
      env_vars.get(key)
    },
    list: fn() {
      env_vars.keys()
    }
  })
  
  // 创建包含环境变量占位符的配置
  let config_with_placeholders = "
service:
  name: \"${SERVICE_NAME}\"
  version: \"${SERVICE_VERSION}\"
  environment: \"${ENVIRONMENT:production}\"  # 带默认值
  
database:
  host: \"${DB_HOST}\"
  port: 5432
  username: \"db_user\"
  password: \"${DB_PASSWORD}\"
  
api:
  port: ${API_PORT:8080}
  timeout: 30000
  
logging:
  level: \"${LOG_LEVEL:info}\"
  
metrics:
  enabled: ${ENABLE_METRICS:false}
  interval: 60000
  
cache:
  ttl: ${CACHE_TTL:3600}
  
features:
  flags: \"${FEATURE_FLAGS}\"
  
nested_config: ${CONFIG_JSON}
"
  
  // 加载配置并替换环境变量
  let config_result = ConfigManager::load_from_string_with_env_substitution(config_manager, config_with_placeholders, "yaml")
  assert_true(config_result.success)
  
  let config = config_result.config
  
  // 验证环境变量替换
  assert_eq(config.get("service.name"), Some("env-service"))
  assert_eq(config.get("service.version"), Some("2.0.0"))
  assert_eq(config.get("service.environment"), Some("production"))  // 使用默认值
  assert_eq(config.get("database.host"), Some("env-db.example.com"))
  assert_eq(config.get("database.password"), Some("env-secret-password"))
  assert_eq(config.get("api.port"), Some("9090"))
  assert_eq(config.get("logging.level"), Some("debug"))
  assert_eq(config.get("metrics.enabled"), Some("true"))
  assert_eq(config.get("cache.ttl"), Some("7200"))
  assert_eq(config.get("features.flags"), Some("feature1,feature2,feature3"))
  
  // 验证JSON环境变量替换
  let nested_config = config.get("nested_config")
  assert_true(nested_config != None)
  match nested_config {
    Some(config_str) => {
      assert_true(config_str.contains("from_env"))
    }
    None => assert_true(false)
  }
  
  // 测试复合环境变量表达式
  let complex_config = "
service:
  full_name: \"${SERVICE_NAME}-${SERVICE_VERSION}\"
  instance_id: \"${SERVICE_NAME}-${INSTANCE_ID:default-instance}\"
  
database:
  url: \"postgresql://${DB_USER:default_user}:${DB_PASSWORD}@${DB_HOST:localhost}:${DB_PORT:5432}/${DB_NAME:default_db}\"
  
paths:
  data_dir: \"/data/${SERVICE_NAME}\"
  log_dir: \"/logs/${SERVICE_NAME}/${ENVIRONMENT:production}\"
"
  
  let complex_config_result = ConfigManager::load_from_string_with_env_substitution(config_manager, complex_config, "yaml")
  assert_true(complex_config_result.success)
  
  let complex_config_loaded = complex_config_result.config
  
  // 验证复合表达式
  assert_eq(complex_config_loaded.get("service.full_name"), Some("env-service-2.0.0"))
  assert_eq(complex_config_loaded.get("service.instance_id"), Some("env-service-default-instance"))
  assert_eq(complex_config_loaded.get("database.url"), Some("postgresql://default_user:env-secret-password@env-db.example.com:5432/default_db"))
  assert_eq(complex_config_loaded.get("paths.data_dir"), Some("/data/env-service"))
  assert_eq(complex_config_loaded.get("paths.log_dir"), Some("/logs/env-service/production"))
  
  // 测试条件环境变量替换
  let conditional_config = "
service:
  name: \"${SERVICE_NAME}\"
  
features:
  debug_mode: ${DEBUG_MODE:false}
  verbose_logging: ${LOG_LEVEL:info} == \"debug\"
  production_mode: ${ENVIRONMENT:production} == \"production\"
  
monitoring:
  enabled: ${MONITORING_ENABLED:true}
  level: ${MONITORING_LEVEL:basic}
  
scaling:
  auto_scale: ${ENVIRONMENT} == \"production\"
  min_instances: ${MIN_INSTANCES:2}
  max_instances: ${MAX_INSTANCES:10}
"
  
  let conditional_config_result = ConfigManager::load_from_string_with_env_substitution(config_manager, conditional_config, "yaml")
  assert_true(conditional_config_result.success)
  
  let conditional_config_loaded = conditional_config_result.config
  
  // 验证条件表达式
  assert_eq(conditional_config_loaded.get("features.debug_mode"), Some("false"))
  assert_eq(conditional_config_loaded.get("features.verbose_logging"), Some("true"))  // debug == "debug"
  assert_eq(conditional_config_loaded.get("features.production_mode"), Some("true"))  // production == "production"
  assert_eq(conditional_config_loaded.get("monitoring.enabled"), Some("true"))
  assert_eq(conditional_config_loaded.get("scaling.auto_scale"), Some("true"))  // production == "production"
  
  // 测试环境变量验证
  let env_validation_rules = [
    {
      variable: "SERVICE_NAME",
      required: true,
      pattern: "^[a-z0-9-]+$"
    },
    {
      variable: "SERVICE_VERSION",
      required: true,
      pattern: "^\\d+\\.\\d+\\.\\d+$"
    },
    {
      variable: "DB_PASSWORD",
      required: true,
      min_length: 8
    },
    {
      variable: "API_PORT",
      required: false,
      type: "int",
      min_value: 1024,
      max_value: 65535
    }
  ]
  
  let env_validation_result = ConfigManager::validate_environment(config_manager, env_validation_rules)
  assert_true(env_validation_result.valid)
  assert_eq(env_validation_result.errors.length(), 0)
  
  // 测试环境变量缺失情况
  let incomplete_env_vars = {
    "SERVICE_NAME": "test-service",
    "SERVICE_VERSION": "1.0.0"
    // 缺少 DB_PASSWORD
  }
  
  ConfigManager::set_environment_provider(config_manager, {
    get: fn(key) {
      incomplete_env_vars.get(key)
    },
    list: fn() {
      incomplete_env_vars.keys()
    }
  })
  
  let incomplete_env_validation_result = ConfigManager::validate_environment(config_manager, env_validation_rules)
  assert_false(incomplete_env_validation_result.valid)
  assert_true(incomplete_env_validation_result.errors.length() > 0)
  
  // 验证具体错误
  let has_missing_password_error = incomplete_env_validation_result.errors.any(fn(e) { e.contains("DB_PASSWORD") and e.contains("required") })
  assert_true(has_missing_password_error)
  
  // 测试环境变量变更监听
  let mut change_events = []
  
  ConfigManager::add_environment_change_listener(config_manager, {
    on_change: fn(key, old_value, new_value) {
      change_events.push((key, old_value, new_value))
    }
  })
  
  // 模拟环境变量变更
  ConfigManager::set_environment_variable(config_manager, "SERVICE_NAME", "updated-service")
  ConfigManager::set_environment_variable(config_manager, "LOG_LEVEL", "warn")
  
  // 验证变更事件
  assert_eq(change_events.length(), 2)
  assert_eq(change_events[0], ("SERVICE_NAME", Some("test-service"), Some("updated-service")))
  assert_eq(change_events[1], ("LOG_LEVEL", Some("info"), Some("warn")))
  
  // 测试配置热重载
  let hot_reload_config = "
service:
  name: \"${SERVICE_NAME}\"
  reloadable: true
  
dynamic:
  value: \"${DYNAMIC_VALUE:default}\"
  timestamp: ${TIMESTAMP:0}
"
  
  let hot_reload_result = ConfigManager::load_from_string_with_env_substitution(config_manager, hot_reload_config, "yaml")
  assert_true(hot_reload_result.success)
  
  let hot_reload_config_loaded = hot_reload_result.config
  assert_eq(hot_reload_config_loaded.get("service.name"), Some("updated-service"))  // 使用更新后的值
  
  // 更新环境变量并重新加载
  ConfigManager::set_environment_variable(config_manager, "DYNAMIC_VALUE", "updated_value")
  ConfigManager::set_environment_variable(config_manager, "TIMESTAMP", "1234567890")
  
  let reloaded_result = ConfigManager::reload_with_env_substitution(config_manager, hot_reload_config_loaded)
  assert_true(reloaded_result.success)
  
  let reloaded_config = reloaded_result.config
  assert_eq(reloaded_config.get("dynamic.value"), Some("updated_value"))
  assert_eq(reloaded_config.get("dynamic.timestamp"), Some("1234567890"))
  
  // 测试环境变量导出
  let exported_env = ConfigManager::export_environment_variables(config_manager, hot_reload_config_loaded)
  assert_true(exported_env.contains("SERVICE_NAME=updated-service"))
  assert_true(exported_env.contains("DYNAMIC_VALUE=updated_value"))
  assert_true(exported_env.contains("TIMESTAMP=1234567890"))
}

// 测试3: 配置验证和模式检查
test "配置验证和模式检查功能" {
  // 创建配置验证器
  let config_validator = ConfigValidator::new()
  
  // 定义JSON Schema
  let telemetry_schema = {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type": "object",
    "title": "Azimuth Telemetry Configuration",
    "description": "Configuration schema for Azimuth telemetry system",
    "required": ["service", "telemetry"],
    "properties": {
      "service": {
        "type": "object",
        "description": "Service configuration",
        "required": ["name", "version", "environment"],
        "properties": {
          "name": {
            "type": "string",
            "description": "Service name",
            "minLength": 1,
            "maxLength": 100,
            "pattern": "^[a-z0-9-]+$"
          },
          "version": {
            "type": "string",
            "description": "Service version",
            "pattern": "^\\d+\\.\\d+\\.\\d+(-[a-zA-Z0-9]+)?$"
          },
          "environment": {
            "type": "string",
            "description": "Deployment environment",
            "enum": ["development", "staging", "production", "test"]
          },
          "namespace": {
            "type": "string",
            "description": "Service namespace",
            "minLength": 1,
            "maxLength": 100
          },
          "tags": {
            "type": "array",
            "description": "Service tags",
            "items": {
              "type": "string",
              "minLength": 1,
              "maxLength": 50
            },
            "maxItems": 20
          }
        },
        "additionalProperties": false
      },
      "telemetry": {
        "type": "object",
        "description": "Telemetry configuration",
        "required": ["enabled"],
        "properties": {
          "enabled": {
            "type": "boolean",
            "description": "Enable telemetry collection"
          },
          "sampling_rate": {
            "type": "number",
            "description": "Sampling rate for traces",
            "minimum": 0.0,
            "maximum": 1.0,
            "default": 0.1
          },
          "endpoints": {
            "type": "array",
            "description": "Telemetry endpoints",
            "items": {
              "type": "object",
              "required": ["url"],
              "properties": {
                "url": {
                  "type": "string",
                  "format": "uri",
                  "description": "Endpoint URL"
                },
                "timeout": {
                  "type": "integer",
                  "description": "Request timeout in milliseconds",
                  "minimum": 1000,
                  "maximum": 60000,
                  "default": 5000
                },
                "headers": {
                  "type": "object",
                  "description": "Additional headers",
                  "additionalProperties": {
                    "type": "string"
                  }
                }
              }
            },
            "minItems": 1,
            "maxItems": 5
          },
          "batch_size": {
            "type": "integer",
            "description": "Batch size for sending telemetry data",
            "minimum": 1,
            "maximum": 1000,
            "default": 100
          },
          "flush_interval": {
            "type": "integer",
            "description": "Flush interval in milliseconds",
            "minimum": 1000,
            "maximum": 300000,
            "default": 5000
          }
        }
      },
      "database": {
        "type": "object",
        "description": "Database configuration",
        "required": ["host", "port", "name"],
        "properties": {
          "host": {
            "type": "string",
            "description": "Database host",
            "minLength": 1,
            "maxLength": 255
          },
          "port": {
            "type": "integer",
            "description": "Database port",
            "minimum": 1,
            "maximum": 65535,
            "default": 5432
          },
          "name": {
            "type": "string",
            "description": "Database name",
            "minLength": 1,
            "maxLength": 64
          },
          "username": {
            "type": "string",
            "description": "Database username",
            "minLength": 1,
            "maxLength": 64
          },
          "password": {
            "type": "string",
            "description": "Database password",
            "minLength": 8,
            "maxLength": 255
          },
          "pool_size": {
            "type": "integer",
            "description": "Connection pool size",
            "minimum": 1,
            "maximum": 100,
            "default": 10
          },
          "ssl": {
            "type": "object",
            "description": "SSL configuration",
            "properties": {
              "enabled": {
                "type": "boolean",
                "default": false
              },
              "cert_path": {
                "type": "string",
                "description": "SSL certificate path"
              },
              "key_path": {
                "type": "string",
                "description": "SSL key path"
              }
            }
          }
        }
      },
      "features": {
        "type": "object",
        "description": "Feature flags",
        "patternProperties": {
          "^[a-z][a-z0-9_]*$": {
            "type": "object",
            "required": ["enabled"],
            "properties": {
              "enabled": {
                "type": "boolean",
                "description": "Feature enabled status"
              },
              "config": {
                "type": "object",
                "description": "Feature-specific configuration"
              }
            }
          }
        },
        "additionalProperties": false
      }
    },
    "additionalProperties": false
  }
  
  // 注册Schema
  ConfigValidator::register_schema(config_validator, "telemetry", telemetry_schema)
  
  // 创建有效配置
  let valid_config = {
    "service": {
      "name": "payment-service",
      "version": "1.2.3",
      "environment": "production",
      "namespace": "finance",
      "tags": ["payment", "critical", "finance"]
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.15,
      "endpoints": [
        {
          "url": "https://telemetry.example.com/api/v1/traces",
          "timeout": 10000,
          "headers": {
            "Authorization": "Bearer token123",
            "X-Service": "payment-service"
          }
        }
      ],
      "batch_size": 200,
      "flush_interval": 10000
    },
    "database": {
      "host": "db.example.com",
      "port": 5432,
      "name": "payments",
      "username": "payment_user",
      "password": "secure_password_123",
      "pool_size": 20,
      "ssl": {
        "enabled": true,
        "cert_path": "/etc/ssl/certs/db.crt",
        "key_path": "/etc/ssl/private/db.key"
      }
    },
    "features": {
      "advanced_tracing": {
        "enabled": true,
        "config": {
          "max_spans": 1000,
          "timeout": 5000
        }
      },
      "custom_metrics": {
        "enabled": false
      },
      "experimental_ui": {
        "enabled": true,
        "config": {
          "beta_features": ["new_dashboard", "enhanced_charts"]
        }
      }
    }
  }
  
  // 验证有效配置
  let valid_validation_result = ConfigValidator::validate_with_schema(config_validator, valid_config, "telemetry")
  assert_true(valid_validation_result.valid)
  assert_eq(valid_validation_result.errors.length(), 0)
  
  // 创建无效配置
  let invalid_config = {
    "service": {
      "name": "",  // 空字符串，违反minLength
      "version": "1.2",  // 不符合版本模式
      "environment": "invalid_env",  // 不在枚举值中
      "tags": ["a".repeat(51)]  // 超过最大长度
    },
    "telemetry": {
      "enabled": "true",  // 字符串而非布尔值
      "sampling_rate": 1.5,  // 超出最大值
      "endpoints": [],  // 空数组，违反minItems
      "batch_size": 0  // 小于最小值
    },
    "database": {
      "host": "db.example.com",
      "port": 70000,  // 超出端口范围
      "name": "payments",
      "username": "user",
      "password": "123"  // 密码太短
    },
    "features": {
      "invalid feature name": {  // 无效的特性名称
        "enabled": true
      },
      "valid_feature": {
        "enabled": "maybe"  // 应该是布尔值
      }
    }
  }
  
  // 验证无效配置
  let invalid_validation_result = ConfigValidator::validate_with_schema(config_validator, invalid_config, "telemetry")
  assert_false(invalid_validation_result.valid)
  assert_true(invalid_validation_result.errors.length() > 0)
  
  // 验证具体错误
  let errors = invalid_validation_result.errors
  
  let has_name_error = errors.any(fn(e) { e.contains("service.name") and e.contains("minLength") })
  let has_version_error = errors.any(fn(e) { e.contains("service.version") and e.contains("pattern") })
  let has_env_error = errors.any(fn(e) { e.contains("service.environment") and e.contains("enum") })
  let has_enabled_error = errors.any(fn(e) { e.contains("telemetry.enabled") and e.contains("boolean") })
  let has_sampling_rate_error = errors.any(fn(e) { e.contains("sampling_rate") and e.contains("maximum") })
  let has_endpoints_error = errors.any(fn(e) { e.contains("endpoints") and e.contains("minItems") })
  let has_port_error = errors.any(fn(e) { e.contains("database.port") and e.contains("maximum") })
  let has_password_error = errors.any(fn(e) { e.contains("database.password") and e.contains("minLength") })
  
  assert_true(has_name_error)
  assert_true(has_version_error)
  assert_true(has_env_error)
  assert_true(has_enabled_error)
  assert_true(has_sampling_rate_error)
  assert_true(has_endpoints_error)
  assert_true(has_port_error)
  assert_true(has_password_error)
  
  // 测试自定义验证规则
  ConfigValidator::add_custom_rule(config_validator, {
    name: "consistent_service_names",
    description: "Ensure service names are consistent across different sections",
    validator: fn(config) {
      let service_name = config.get("service.name")
      let telemetry_service = config.get("telemetry.service_name")
      
      match (service_name, telemetry_service) {
        (Some(name), Some(telemetry_name)) => {
          if name == telemetry_name {
            ValidationResult::valid()
          } else {
            ValidationResult::invalid("Service name in telemetry section must match service.name")
          }
        }
        _ => ValidationResult::valid()  // 如果任一为空，则跳过验证
      }
    }
  })
  
  ConfigValidator::add_custom_rule(config_validator, {
    name: "reasonable_pool_sizes",
    description: "Ensure database pool size is reasonable for the environment",
    validator: fn(config) {
      let environment = config.get("service.environment")
      let pool_size = config.get("database.pool_size")
      
      match (environment, pool_size) {
        (Some(env), Some(size_str)) => {
          match size_str.parse_int() {
            Some(size) => {
              if env == "production" and size < 10 {
                ValidationResult::invalid("Production environment requires pool size of at least 10")
              } else if env == "development" and size > 20 {
                ValidationResult::invalid("Development environment should not use pool size greater than 20")
              } else {
                ValidationResult::valid()
              }
            }
            None => ValidationResult::invalid("Invalid pool size format")
          }
        }
        _ => ValidationResult::valid()
      }
    }
  })
  
  // 测试自定义验证规则
  let config_with_inconsistent_names = {
    "service": {
      "name": "payment-service",
      "version": "1.2.3",
      "environment": "production"
    },
    "telemetry": {
      "enabled": true,
      "service_name": "different-service"  // 不一致的服务名称
    },
    "database": {
      "host": "db.example.com",
      "port": 5432,
      "name": "payments",
      "pool_size": 5  // 生产环境下池大小太小
    }
  }
  
  let custom_validation_result = ConfigValidator::validate_with_custom_rules(config_validator, config_with_inconsistent_names)
  assert_false(custom_validation_result.valid)
  assert_true(custom_validation_result.errors.length() >= 2)
  
  // 验证自定义错误
  let has_consistency_error = custom_validation_result.errors.any(fn(e) { e.contains("consistent_service_names") })
  let has_pool_size_error = custom_validation_result.errors.any(fn(e) { e.contains("reasonable_pool_sizes") })
  
  assert_true(has_consistency_error)
  assert_true(has_pool_size_error)
  
  // 测试条件验证
  ConfigValidator::add_conditional_rule(config_validator, {
    name: "ssl_config_validation",
    description: "Validate SSL configuration only when SSL is enabled",
    condition: fn(config) {
      config.get("database.ssl.enabled") == Some("true")
    },
    rule: {
      "database.ssl.cert_path": {
        required: true,
        type: "string",
        min_length: 1
      },
      "database.ssl.key_path": {
        required: true,
        type: "string",
        min_length: 1
      }
    }
  })
  
  // 测试条件验证 - SSL启用但缺少证书路径
  let ssl_incomplete_config = {
    "service": {
      "name": "test-service",
      "version": "1.0.0",
      "environment": "staging"
    },
    "telemetry": {
      "enabled": true
    },
    "database": {
      "host": "db.example.com",
      "port": 5432,
      "name": "test",
      "ssl": {
        "enabled": true,
        "cert_path": "",  // 空证书路径
        // 缺少 key_path
      }
    }
  }
  
  let conditional_validation_result = ConfigValidator::validate_with_conditional_rules(config_validator, ssl_incomplete_config)
  assert_false(conditional_validation_result.valid)
  
  // 测试Schema生成
  let generated_schema = ConfigValidator::generate_schema_from_sample(config_validator, valid_config)
  assert_true(generated_schema.contains("\"type\": \"object\""))
  assert_true(generated_schema.contains("\"service\""))
  assert_true(generated_schema.contains("\"telemetry\""))
  
  // 测试Schema版本兼容性
  let schema_v1 = {
    "type": "object",
    "properties": {
      "service": {
        "type": "object",
        "properties": {
          "name": {"type": "string"},
          "version": {"type": "string"}
        },
        "required": ["name", "version"]
      }
    }
  }
  
  let schema_v2 = {
    "type": "object",
    "properties": {
      "service": {
        "type": "object",
        "properties": {
          "name": {"type": "string"},
          "version": {"type": "string"},
          "environment": {"type": "string", "enum": ["development", "production"]}  // 新增字段
        },
        "required": ["name", "version"]  // environment不是必需的
      }
    }
  }
  
  let compatibility_result = ConfigValidator::check_schema_compatibility(config_validator, schema_v1, schema_v2)
  assert_true(compatibility_result.compatible)
  assert_true(compatibility_result.added_properties.contains("service.environment"))
  
  // 测试验证报告生成
  let validation_report = ConfigValidator::generate_validation_report(config_validator, invalid_config, "telemetry")
  assert_true(validation_report.summary.contains("invalid"))
  assert_true(validation_report.errors.length() > 0)
  assert_true(validation_report.suggestions.length() > 0)
  
  // 验证建议内容
  let has_name_suggestion = validation_report.suggestions.any(fn(s) { s.contains("service.name") })
  let has_version_suggestion = validation_report.suggestions.any(fn(s) { s.contains("service.version") })
  
  assert_true(has_name_suggestion)
  assert_true(has_version_suggestion)
}

// 测试4: 动态配置更新和热重载
test "动态配置更新和热重载功能" {
  // 创建动态配置管理器
  let dynamic_config_manager = DynamicConfigManager::new()
  
  // 配置动态更新选项
  DynamicConfigManager::configure(dynamic_config_manager, {
    watch_file_changes: true,
    reload_on_change: true,
    validation_on_reload: true,
    backup_on_change: true,
    max_backup_files: 10,
    change_notification: true
  })
  
  // 初始配置
  let initial_config_content = "
service:
  name: \"dynamic-service\"
  version: \"1.0.0\"
  environment: \"development\"
  
telemetry:
  enabled: true
  sampling_rate: 0.1
  endpoints:
    - url: \"https://telemetry.example.com\"
      timeout: 5000
      
database:
  host: \"localhost\"
  port: 5432
  name: \"dynamic_db\"
  pool_size: 10
  
features:
  new_ui: false
  advanced_search: false
  real_time_updates: false
  
logging:
  level: \"info\"
  format: \"json\"
"
  
  // 加载初始配置
  let initial_load_result = DynamicConfigManager::load_from_string(dynamic_config_manager, initial_config_content, "yaml")
  assert_true(initial_load_result.success)
  
  let initial_config = initial_load_result.config
  
  // 验证初始配置
  assert_eq(initial_config.get("service.name"), Some("dynamic-service"))
  assert_eq(initial_config.get("telemetry.sampling_rate"), Some("0.1"))
  assert_eq(initial_config.get("database.pool_size"), Some("10"))
  assert_eq(initial_config.get("features.new_ui"), Some("false"))
  assert_eq(initial_config.get("logging.level"), Some("info"))
  
  // 注册配置变更监听器
  let mut change_events = []
  
  DynamicConfigManager::add_change_listener(dynamic_config_manager, {
    id: "test-listener",
    on_change: fn(old_config, new_config, changes) {
      change_events.push((old_config, new_config, changes))
    }
  })
  
  // 创建更新后的配置
  let updated_config_content = "
service:
  name: \"dynamic-service\"
  version: \"1.1.0\"  # 版本更新
  environment: \"staging\"  # 环境变更
  
telemetry:
  enabled: true
  sampling_rate: 0.2  # 采样率变更
  endpoints:
    - url: \"https://telemetry.example.com\"
      timeout: 10000  # 超时时间变更
    - url: \"https://telemetry-backup.example.com\"  # 新增端点
      timeout: 5000
      
database:
  host: \"db.example.com\"  # 主机变更
  port: 5432
  name: \"dynamic_db\"
  pool_size: 20  # 连接池大小变更
  ssl:  # 新增SSL配置
    enabled: true
    cert_path: \"/etc/ssl/certs/db.crt\"
    
features:
  new_ui: true  # 功能启用
  advanced_search: true  # 功能启用
  real_time_updates: false
  beta_features:  # 新增功能组
    enabled: true
    features: [\"feature_a\", \"feature_b\"]
    
logging:
  level: \"debug\"  # 日志级别变更
  format: \"text\"  # 格式变更
  outputs:  # 新增输出配置
    - type: \"file\"
      path: \"/var/log/dynamic-service.log\"
"
  
  // 应用配置更新
  let update_result = DynamicConfigManager::update_config(dynamic_config_manager, updated_config_content, "yaml")
  assert_true(update_result.success)
  
  let updated_config = update_result.config
  
  // 验证更新后的配置
  assert_eq(updated_config.get("service.version"), Some("1.1.0"))
  assert_eq(updated_config.get("service.environment"), Some("staging"))
  assert_eq(updated_config.get("telemetry.sampling_rate"), Some("0.2"))
  assert_eq(updated_config.get("telemetry.endpoints.1.url"), Some("https://telemetry-backup.example.com"))
  assert_eq(updated_config.get("database.host"), Some("db.example.com"))
  assert_eq(updated_config.get("database.pool_size"), Some("20"))
  assert_eq(updated_config.get("database.ssl.enabled"), Some("true"))
  assert_eq(updated_config.get("features.new_ui"), Some("true"))
  assert_eq(updated_config.get("features.advanced_search"), Some("true"))
  assert_eq(updated_config.get("features.beta_features.enabled"), Some("true"))
  assert_eq(updated_config.get("logging.level"), Some("debug"))
  assert_eq(updated_config.get("logging.format"), Some("text"))
  
  // 验证变更事件
  assert_eq(change_events.length(), 1)
  
  let (old_config, new_config, changes) = change_events[0]
  assert_eq(old_config.get("service.version"), Some("1.0.0"))
  assert_eq(new_config.get("service.version"), Some("1.1.0"))
  assert_true(changes.length() > 0)
  
  // 验证具体变更
  let has_version_change = changes.any(fn(c) { c.path == "service.version" and c.old_value == Some("1.0.0") and c.new_value == Some("1.1.0") })
  let has_sampling_rate_change = changes.any(fn(c) { c.path == "telemetry.sampling_rate" and c.old_value == Some("0.1") and c.new_value == Some("0.2") })
  let has_pool_size_change = changes.any(fn(c) { c.path == "database.pool_size" and c.old_value == Some("10") and c.new_value == Some("20") })
  let has_new_ui_change = changes.any(fn(c) { c.path == "features.new_ui" and c.old_value == Some("false") and c.new_value == Some("true") })
  
  assert_true(has_version_change)
  assert_true(has_sampling_rate_change)
  assert_true(has_pool_size_change)
  assert_true(has_new_ui_change)
  
  // 测试配置回滚
  let rollback_result = DynamicConfigManager::rollback_config(dynamic_config_manager, 1)  // 回滚到上一个版本
  assert_true(rollback_result.success)
  
  let rolled_back_config = rollback_result.config
  assert_eq(rolled_back_config.get("service.version"), Some("1.0.0"))
  assert_eq(rolled_back_config.get("telemetry.sampling_rate"), Some("0.1"))
  assert_eq(rolled_back_config.get("database.pool_size"), Some("10"))
  
  // 验证回滚事件
  assert_eq(change_events.length(), 2)
  let (_, _, rollback_changes) = change_events[1]
  assert_true(rollback_changes.any(fn(c) { c.is_rollback }))
  
  // 重新应用更新
  DynamicConfigManager::update_config(dynamic_config_manager, updated_config_content, "yaml")
  
  // 测试部分配置更新
  let partial_update = {
    "telemetry": {
      "sampling_rate": 0.3
    },
    "features": {
      "real_time_updates": true
    }
  }
  
  let partial_update_result = DynamicConfigManager::partial_update(dynamic_config_manager, partial_update)
  assert_true(partial_update_result.success)
  
  let partially_updated_config = partial_update_result.config
  assert_eq(partially_updated_config.get("service.version"), Some("1.1.0"))  // 未变更
  assert_eq(partially_updated_config.get("telemetry.sampling_rate"), Some("0.3"))  // 已更新
  assert_eq(partially_updated_config.get("features.real_time_updates"), Some("true"))  // 已更新
  
  // 测试配置验证失败时的处理
  let invalid_partial_update = {
    "telemetry": {
      "sampling_rate": 1.5  // 无效值，超出范围
    }
  }
  
  let invalid_update_result = DynamicConfigManager::partial_update(dynamic_config_manager, invalid_partial_update)
  assert_false(invalid_update_result.success)
  assert_true(invalid_update_result.error_message.contains("validation"))
  
  // 验证配置未变更
  let current_config = DynamicConfigManager::get_current_config(dynamic_config_manager)
  assert_eq(current_config.get("telemetry.sampling_rate"), Some("0.3"))  // 保持之前的有效值
  
  // 测试配置文件监控
  let file_monitor_result = DynamicConfigManager::enable_file_watching(dynamic_config_manager, "/tmp/dynamic-config.yaml")
  assert_true(file_monitor_result.success)
  
  // 模拟文件变更
  let file_changed_content = "
service:
  name: \"dynamic-service\"
  version: \"1.2.0\"  # 文件监控版本
  environment: \"production\"
  
telemetry:
  enabled: true
  sampling_rate: 0.05
  
database:
  host: \"prod-db.example.com\"
  port: 5432
  name: \"dynamic_db\"
  pool_size: 50
"
  
  // 模拟文件变更事件
  DynamicConfigManager::simulate_file_change(dynamic_config_manager, "/tmp/dynamic-config.yaml", file_changed_content)
  
  // 验证文件监控触发的重载
  let file_reloaded_config = DynamicConfigManager::get_current_config(dynamic_config_manager)
  assert_eq(file_reloaded_config.get("service.version"), Some("1.2.0"))
  assert_eq(file_reloaded_config.get("service.environment"), Some("production"))
  assert_eq(file_reloaded_config.get("telemetry.sampling_rate"), Some("0.05"))
  assert_eq(file_reloaded_config.get("database.host"), Some("prod-db.example.com"))
  assert_eq(file_reloaded_config.get("database.pool_size"), Some("50"))
  
  // 测试配置模板
  let config_template = "
service:
  name: \"{{SERVICE_NAME}}\"
  version: \"{{SERVICE_VERSION}}\"
  environment: \"{{ENVIRONMENT}}\"
  
telemetry:
  enabled: {{TELEMETRY_ENABLED}}
  sampling_rate: {{TELEMETRY_SAMPLING_RATE}}
  
database:
  host: {{DB_HOST}}
  port: {{DB_PORT}}
  name: {{DB_NAME}}
  pool_size: {{DB_POOL_SIZE}}
"
  
  let template_variables = {
    "SERVICE_NAME": "template-service",
    "SERVICE_VERSION": "2.0.0",
    "ENVIRONMENT": "production",
    "TELEMETRY_ENABLED": true,
    "TELEMETRY_SAMPLING_RATE": 0.15,
    "DB_HOST": "template-db.example.com",
    "DB_PORT": 5432,
    "DB_NAME": "template_db",
    "DB_POOL_SIZE": 25
  }
  
  let template_result = DynamicConfigManager::render_template(dynamic_config_manager, config_template, template_variables)
  assert_true(template_result.success)
  
  let rendered_config = template_result.config
  assert_eq(rendered_config.get("service.name"), Some("template-service"))
  assert_eq(rendered_config.get("service.version"), Some("2.0.0"))
  assert_eq(rendered_config.get("telemetry.enabled"), Some("true"))
  assert_eq(rendered_config.get("telemetry.sampling_rate"), Some("0.15"))
  assert_eq(rendered_config.get("database.pool_size"), Some("25"))
  
  // 测试配置导出
  let export_result = DynamicConfigManager::export_config(dynamic_config_manager, "yaml")
  assert_true(export_result.success)
  assert_true(export_result.content.length() > 0)
  assert_true(export_result.content.contains("service:"))
  assert_true(export_result.content.contains("telemetry:"))
  
  // 测试配置历史
  let config_history = DynamicConfigManager::get_config_history(dynamic_config_manager)
  assert_true(config_history.length() >= 3)  // 初始、更新、回滚
  
  // 验证历史记录
  let initial_history = config_history[config_history.length() - 1]
  assert_eq(initial_history.config.get("service.version"), Some("1.0.0"))
  
  let updated_history = config_history[config_history.length() - 2]
  assert_eq(updated_history.config.get("service.version"), Some("1.1.0"))
  
  // 测试配置比较
  let config_diff = DynamicConfigManager::compare_configs(dynamic_config_manager, initial_config, updated_config)
  assert_true(config_diff.added.length() > 0)
  assert_true(config_diff.modified.length() > 0)
  assert_true(config_diff.removed.length() >= 0)
  
  // 验证具体差异
  let has_version_diff = config_diff.modified.any(fn(d) { d.path == "service.version" })
  let has_sampling_rate_diff = config_diff.modified.any(fn(d) { d.path == "telemetry.sampling_rate" })
  let has_ssl_added = config_diff.added.any(fn(d) { d.path == "database.ssl" })
  
  assert_true(has_version_diff)
  assert_true(has_sampling_rate_diff)
  assert_true(has_ssl_added)
  
  // 测试配置统计
  let config_stats = DynamicConfigManager::get_config_stats(dynamic_config_manager)
  assert_true(config_stats.total_updates >= 3)
  assert_true(config_stats.total_rollbacks >= 1)
  assert_true(config_stats.total_file_reloads >= 1)
  assert_true(config_stats.change_listeners >= 1)
}

// 测试5: 配置加密和安全
test "配置加密和安全功能" {
  // 创建安全配置管理器
  let secure_config_manager = SecureConfigManager::new()
  
  // 配置安全选项
  SecureConfigManager::configure(secure_config_manager, {
    encryption_enabled: true,
    encryption_algorithm: "aes256_gcm",
    key_derivation: "pbkdf2",
    hash_algorithm: "sha256",
    secure_storage: true,
    access_logging: true,
    audit_trail: true
  })
  
  // 设置加密密钥
  SecureConfigManager::set_encryption_key(secure_config_manager, {
    key_id: "config-encryption-key-2023",
    key_data: "32-byte-encryption-key-123456789012",
    algorithm: "aes256",
    created_at: 1640995200,
    expires_at: 1672531200
  })
  
  // 创建包含敏感信息的配置
  let sensitive_config_content = "
service:
  name: \"secure-service\"
  version: \"1.0.0\"
  environment: \"production\"
  
database:
  host: \"db.example.com\"
  port: 5432
  name: \"secure_db\"
  username: \"secure_user\"
  password: \"{{encrypted:U2FsdGVkX1+vupppZksvRf5pq5g5XjFRIipRkwB0K1Y96Qsv2Lm+31cmzaAILwyt\"}}\"
  ssl:
    enabled: true
    cert_path: \"{{encrypted:U2FsdGVkX1+vupppZksvRf5pq5g5XjFRIipRkwB0K1Y96Qsv2Lm+31cmzaAILwzu\"}}\"
    key_path: \"{{encrypted:U2FsdGVkX1+vupppZksvRf5pq5g5XjFRIipRkwB0K1Y96Qsv2Lm+31cmzaAILwzv\"}}\"
    
api_keys:
  external_service: \"{{encrypted:U2FsdGVkX1+vupppZksvRf5pq5g5XjFRIipRkwB0K1Y96Qsv2Lm+31cmzaAILwxw\"}}\"
  payment_gateway: \"{{encrypted:U2FsdGVkX1+vupppZksvRf5pq5g5XjFRIipRkwB0K1Y96Qsv2Lm+31cmzaAILwxw\"}}\"
  
secrets:
  jwt_secret: \"{{encrypted:U2FsdGVkX1+vupppZksvRf5pq5g5XjFRIipRkwB0K1Y96Qsv2Lm+31cmzaAILwyA\"}}\"
  webhook_secret: \"{{encrypted:U2FsdGVkX1+vupppZksvRf5pq5g5XjFRIipRkwB0K1Y96Qsv2Lm+31cmzaAILwyB\"}}\"
  
oauth:
  client_id: \"oauth_client_123\"
  client_secret: \"{{encrypted:U2FsdGVkX1+vupppZksvRf5pq5g5XjFRIipRkwB0K1Y96Qsv2Lm+31cmzaAILwyC\"}}\"
  
non_sensitive:
  debug: false
  log_level: \"info\"
  max_connections: 100
"
  
  // 加载并解密敏感配置
  let secure_load_result = SecureConfigManager::load_from_string(secure_config_manager, sensitive_config_content, "yaml")
  assert_true(secure_load_result.success)
  
  let secure_config = secure_load_result.config
  
  // 验证非敏感配置
  assert_eq(secure_config.get("service.name"), Some("secure-service"))
  assert_eq(secure_config.get("service.version"), Some("1.0.0"))
  assert_eq(secure_config.get("database.host"), Some("db.example.com"))
  assert_eq(secure_config.get("database.username"), Some("secure_user"))
  assert_eq(secure_config.get("non_sensitive.debug"), Some("false"))
  assert_eq(secure_config.get("non_sensitive.max_connections"), Some("100"))
  
  // 验证敏感配置已解密
  let db_password = secure_config.get("database.password")
  assert_true(db_password != None)
  assert_false(db_password == Some("{{encrypted:U2FsdGVkX1+vupppZksvRf5pq5g5XjFRIipRkwB0K1Y96Qsv2Lm+31cmzaAILwyt}}"))  // 不再是加密格式
  
  let jwt_secret = secure_config.get("secrets.jwt_secret")
  assert_true(jwt_secret != None)
  assert_false(jwt_secret == Some("{{encrypted:U2FsdGVkX1+vupppZksvRf5pq5g5XjFRIipRkwB0K1Y96Qsv2Lm+31cmzaAILwyA}}"))
  
  let oauth_client_secret = secure_config.get("oauth.client_secret")
  assert_true(oauth_client_secret != None)
  assert_false(oauth_client_secret == Some("{{encrypted:U2FsdGVkX1+vupppZksvRf5pq5g5XjFRIipRkwB0K1Y96Qsv2Lm+31cmzaAILwyC}}"))
  
  // 测试敏感值加密
  let encrypt_result = SecureConfigManager::encrypt_value(secure_config_manager, "super_secret_password")
  assert_true(encrypt_result.success)
  assert_true(encrypt_result.encrypted_value.length() > 0)
  assert_true(encrypt_result.encrypted_value.starts_with("{{encrypted:"))
  
  // 测试敏感值解密
  let decrypt_result = SecureConfigManager::decrypt_value(secure_config_manager, encrypt_result.encrypted_value)
  assert_true(decrypt_result.success)
  assert_eq(decrypt_result.decrypted_value, "super_secret_password")
  
  // 测试配置保存时的加密
  let config_to_save = {
    "service": {
      "name": "secure-service",
      "version": "1.1.0"
    },
    "database": {
      "host": "db.example.com",
      "password": "new_secret_password"  # 应该被加密
    },
    "secrets": {
      "jwt_secret": "new_jwt_secret_key"  # 应该被加密
    },
    "non_sensitive": {
      "debug": true
    }
  }
  
  let secure_save_result = SecureConfigManager::save_config(secure_config_manager, config_to_save, "yaml", {
    encrypt_sensitive: true,
    sensitive_patterns: ["password", "secret", "key", "token"]
  })
  
  assert_true(secure_save_result.success)
  let saved_config_content = secure_save_result.content
  
  // 验证敏感值已加密
  assert_true(saved_config_content.contains("{{encrypted:"))
  assert_false(saved_config_content.contains("new_secret_password"))
  assert_false(saved_config_content.contains("new_jwt_secret_key"))
  
  // 验证非敏感值未加密
  assert_true(saved_config_content.contains("secure-service"))
  assert_true(saved_config_content.contains("debug: true"))
  
  // 测试访问控制
  SecureConfigManager::configure_access_control(secure_config_manager, {
    role_based_access: true,
    default_permissions: ["read"],
    audit_access: true
  })
  
  // 定义角色和权限
  SecureConfigManager::define_role(secure_config_manager, {
    name: "admin",
    permissions: ["read", "write", "decrypt", "encrypt"],
    path_patterns: ["*"]
  })
  
  SecureConfigManager::define_role(secure_config_manager, {
    name: "operator",
    permissions: ["read", "write"],
    path_patterns: ["service.*", "database.*", "non_sensitive.*"],
    denied_patterns: ["secrets.*", "api_keys.*", "oauth.*"]
  })
  
  SecureConfigManager::define_role(secure_config_manager, {
    name: "viewer",
    permissions: ["read"],
    path_patterns: ["service.*", "non_sensitive.*"],
    denied_patterns: ["database.password", "secrets.*", "api_keys.*", "oauth.*"]
  })
  
  // 测试不同角色的访问
  let admin_access_result = SecureConfigManager::check_access(secure_config_manager, {
    role: "admin",
    path: "database.password",
    action: "read"
  })
  assert_true(admin_access_result.allowed)
  
  let operator_access_result = SecureConfigManager::check_access(secure_config_manager, {
    role: "operator",
    path: "database.password",
    action: "read"
  })
  assert_false(operator_access_result.allowed)  // 操作员不能访问密码
  
  let viewer_access_result = SecureConfigManager::check_access(secure_config_manager, {
    role: "viewer",
    path: "service.name",
    action: "read"
  })
  assert_true(viewer_access_result.allowed)  // 查看者可以访问服务名称
  
  let viewer_secrets_access_result = SecureConfigManager::check_access(secure_config_manager, {
    role: "viewer",
    path: "secrets.jwt_secret",
    action: "read"
  })
  assert_false(viewer_secrets_access_result.allowed)  // 查看者不能访问密钥
  
  // 测试配置过滤
  let filtered_config_for_operator = SecureConfigManager::filter_config(secure_config_manager, secure_config, "operator")
  
  // 验证操作员可以看到的内容
  assert_eq(filtered_config_for_operator.get("service.name"), Some("secure-service"))
  assert_eq(filtered_config_for_operator.get("database.host"), Some("db.example.com"))
  assert_eq(filtered_config_for_operator.get("non_sensitive.debug"), Some("false"))
  
  // 验证操作员不能看到的内容
  assert_eq(filtered_config_for_operator.get("database.password"), None)
  assert_eq(filtered_config_for_operator.get("secrets.jwt_secret"), None)
  assert_eq(filtered_config_for_operator.get("api_keys.external_service"), None)
  
  let filtered_config_for_viewer = SecureConfigManager::filter_config(secure_config_manager, secure_config, "viewer")
  
  // 验证查看者可以看到的内容
  assert_eq(filtered_config_for_viewer.get("service.name"), Some("secure-service"))
  assert_eq(filtered_config_for_viewer.get("non_sensitive.debug"), Some("false"))
  
  // 验证查看者不能看到的内容
  assert_eq(filtered_config_for_viewer.get("database.host"), None)  // 数据库配置不在允许路径中
  assert_eq(filtered_config_for_viewer.get("database.password"), None)
  assert_eq(filtered_config_for_viewer.get("secrets.jwt_secret"), None)
  
  // 测试审计日志
  let audit_log = SecureConfigManager::get_audit_log(secure_config_manager)
  assert_true(audit_log.length() > 0)
  
  // 验证审计日志包含关键操作
  let has_load_event = audit_log.any(fn(entry) { entry.operation == "load_config" })
  let has_encrypt_event = audit_log.any(fn(entry) { entry.operation == "encrypt_value" })
  let has_decrypt_event = audit_log.any(fn(entry) { entry.operation == "decrypt_value" })
  let has_access_check_event = audit_log.any(fn(entry) { entry.operation == "access_check" })
  
  assert_true(has_load_event)
  assert_true(has_encrypt_event)
  assert_true(has_decrypt_event)
  assert_true(has_access_check_event)
  
  // 测试密钥轮换
  let new_encryption_key = {
    key_id: "config-encryption-key-2024",
    key_data: "32-byte-new-encryption-key-1234567890",
    algorithm: "aes256",
    created_at: 1640995200,
    expires_at: 1672531200
  }
  
  SecureConfigManager::add_encryption_key(secure_config_manager, new_encryption_key)
  SecureConfigManager::set_primary_key(secure_config_manager, "config-encryption-key-2024")
  
  // 使用新密钥加密值
  let new_key_encrypt_result = SecureConfigManager::encrypt_value(secure_config_manager, "test_with_new_key")
  assert_true(new_key_encrypt_result.success)
  
  // 验证新密钥加密的数据可以解密
  let new_key_decrypt_result = SecureConfigManager::decrypt_value(secure_config_manager, new_key_encrypt_result.encrypted_value)
  assert_true(new_key_decrypt_result.success)
  assert_eq(new_key_decrypt_result.decrypted_value, "test_with_new_key")
  
  // 验证旧密钥加密的数据仍然可以解密（密钥轮换兼容性）
  let old_key_decrypt_result = SecureConfigManager::decrypt_value(secure_config_manager, encrypt_result.encrypted_value)
  assert_true(old_key_decrypt_result.success)
  assert_eq(old_key_decrypt_result.decrypted_value, "super_secret_password")
  
  // 测试配置完整性验证
  let integrity_check_result = SecureConfigManager::verify_integrity(secure_config_manager, secure_config)
  assert_true(integrity_check_result.valid)
  assert_eq(integrity_check_result.checksum.length() > 0)
  
  // 模拟配置篡改
  let tampered_config = SecureConfigManager::clone_config(secure_config)
  SecureConfigManager::set(tampered_config, "service.name", "tampered-service")
  
  let tampered_check_result = SecureConfigManager::verify_integrity(secure_config_manager, tampered_config)
  assert_false(tampered_check_result.valid)
  assert_true(tampered_check_result.error_message.contains("integrity"))
  
  // 测试安全配置统计
  let security_stats = SecureConfigManager::get_security_stats(secure_config_manager)
  assert_true(security_stats.total_encryptions > 0)
  assert_true(security_stats.total_decryptions > 0)
  assert_true(security_stats.access_checks > 0)
  assert_true(security_stats.audit_entries > 0)
  assert_true(security_stats.key_rotations >= 1)
  assert_true(security_stats.integrity_checks > 0)
}

// 测试6: 多环境配置管理
test "多环境配置管理功能" {
  // 创建多环境配置管理器
  let multi_env_config_manager = MultiEnvConfigManager::new()
  
  // 配置环境层次结构
  MultiEnvConfigManager::configure_environments(multi_env_config_manager, {
    environments: ["development", "staging", "production", "test"],
    inheritance_order: ["base", "development", "staging", "production"],
    default_environment: "development"
  })
  
  // 基础配置
  let base_config = {
    "service": {
      "name": "multi-env-service",
      "version": "1.0.0"
    },
    "database": {
      "port": 5432,
      "pool_size": 10
    },
    "logging": {
      "format": "json"
    },
    "features": {
      "basic_feature": true,
      "advanced_feature": false
    },
    "monitoring": {
      "enabled": true,
      "interval": 60000
    }
  }
  
  // 开发环境配置
  let development_config = {
    "service": {
      "environment": "development"
    },
    "database": {
      "host": "localhost",
      "name": "multi_env_dev",
      "pool_size": 5
    },
    "logging": {
      "level": "debug"
    },
    "features": {
      "debug_mode": true,
      "hot_reload": true
    },
    "monitoring": {
      "interval": 30000
    }
  }
  
  // 测试环境配置
  let test_config = {
    "service": {
      "environment": "test"
    },
    "database": {
      "host": "test-db.example.com",
      "name": "multi_env_test"
    },
    "logging": {
      "level": "info"
    },
    "features": {
      "mock_external_services": true
    }
  }
  
  // 预发布环境配置
  let staging_config = {
    "service": {
      "environment": "staging"
    },
    "database": {
      "host": "staging-db.example.com",
      "name": "multi_env_staging",
      "pool_size": 15
    },
    "logging": {
      "level": "info"
    },
    "features": {
      "advanced_feature": true
    },
    "monitoring": {
      "interval": 45000
    }
  }
  
  // 生产环境配置
  let production_config = {
    "service": {
      "environment": "production"
    },
    "database": {
      "host": "prod-db.example.com",
      "name": "multi_env_prod",
      "pool_size": 25,
      "ssl": {
        "enabled": true
      }
    },
    "logging": {
      "level": "warn"
    },
    "features": {
      "debug_mode": false,
      "advanced_feature": true
    },
    "monitoring": {
      "interval": 30000,
      "alerts": {
        "enabled": true,
        "thresholds": {
          "error_rate": 0.05,
          "response_time": 1000
        }
      }
    }
  }
  
  // 注册环境配置
  MultiEnvConfigManager::register_environment_config(multi_env_config_manager, "base", base_config)
  MultiEnvConfigManager::register_environment_config(multi_env_config_manager, "development", development_config)
  MultiEnvConfigManager::register_environment_config(multi_env_config_manager, "test", test_config)
  MultiEnvConfigManager::register_environment_config(multi_env_config_manager, "staging", staging_config)
  MultiEnvConfigManager::register_environment_config(multi_env_config_manager, "production", production_config)
  
  // 测试开发环境配置合并
  let dev_config_result = MultiEnvConfigManager::get_environment_config(multi_env_config_manager, "development")
  assert_true(dev_config_result.success)
  
  let dev_config = dev_config_result.config
  
  // 验证基础配置继承
  assert_eq(dev_config.get("service.name"), Some("multi-env-service"))
  assert_eq(dev_config.get("service.version"), Some("1.0.0"))
  assert_eq(dev_config.get("database.port"), Some("5432"))
  assert_eq(dev_config.get("logging.format"), Some("json"))
  assert_eq(dev_config.get("features.basic_feature"), Some("true"))
  assert_eq(dev_config.get("monitoring.enabled"), Some("true"))
  
  // 验证开发环境配置覆盖
  assert_eq(dev_config.get("service.environment"), Some("development"))
  assert_eq(dev_config.get("database.host"), Some("localhost"))
  assert_eq(dev_config.get("database.name"), Some("multi_env_dev"))
  assert_eq(dev_config.get("database.pool_size"), Some("5"))  // 开发环境覆盖
  assert_eq(dev_config.get("logging.level"), Some("debug"))
  assert_eq(dev_config.get("features.debug_mode"), Some("true"))
  assert_eq(dev_config.get("features.hot_reload"), Some("true"))
  assert_eq(dev_config.get("monitoring.interval"), Some("30000"))  // 开发环境覆盖
  
  // 测试测试环境配置合并
  let test_config_result = MultiEnvConfigManager::get_environment_config(multi_env_config_manager, "test")
  assert_true(test_config_result.success)
  
  let test_config_loaded = test_config_result.config
  
  // 验证基础配置继承
  assert_eq(test_config_loaded.get("service.name"), Some("multi-env-service"))
  assert_eq(test_config_loaded.get("database.port"), Some("5432"))
  assert_eq(test_config_loaded.get("database.pool_size"), Some("10"))  // 基础配置值
  
  // 验证测试环境配置覆盖
  assert_eq(test_config_loaded.get("service.environment"), Some("test"))
  assert_eq(test_config_loaded.get("database.host"), Some("test-db.example.com"))
  assert_eq(test_config_loaded.get("database.name"), Some("multi_env_test"))
  assert_eq(test_config_loaded.get("logging.level"), Some("info"))
  assert_eq(test_config_loaded.get("features.mock_external_services"), Some("true"))
  
  // 测试预发布环境配置合并
  let staging_config_result = MultiEnvConfigManager::get_environment_config(multi_env_config_manager, "staging")
  assert_true(staging_config_result.success)
  
  let staging_config_loaded = staging_config_result.config
  
  // 验证预发布环境配置
  assert_eq(staging_config_loaded.get("service.environment"), Some("staging"))
  assert_eq(staging_config_loaded.get("database.host"), Some("staging-db.example.com"))
  assert_eq(staging_config_loaded.get("database.name"), Some("multi_env_staging"))
  assert_eq(staging_config_loaded.get("database.pool_size"), Some("15"))
  assert_eq(staging_config_loaded.get("logging.level"), Some("info"))
  assert_eq(staging_config_loaded.get("features.advanced_feature"), Some("true"))
  assert_eq(staging_config_loaded.get("monitoring.interval"), Some("45000"))
  
  // 测试生产环境配置合并
  let prod_config_result = MultiEnvConfigManager::get_environment_config(multi_env_config_manager, "production")
  assert_true(prod_config_result.success)
  
  let prod_config_loaded = prod_config_result.config
  
  // 验证生产环境配置
  assert_eq(prod_config_loaded.get("service.environment"), Some("production"))
  assert_eq(prod_config_loaded.get("database.host"), Some("prod-db.example.com"))
  assert_eq(prod_config_loaded.get("database.name"), Some("multi_env_prod"))
  assert_eq(prod_config_loaded.get("database.pool_size"), Some("25"))
  assert_eq(prod_config_loaded.get("database.ssl.enabled"), Some("true"))
  assert_eq(prod_config_loaded.get("logging.level"), Some("warn"))
  assert_eq(prod_config_loaded.get("features.debug_mode"), Some("false"))
  assert_eq(prod_config_loaded.get("features.advanced_feature"), Some("true"))
  assert_eq(prod_config_loaded.get("monitoring.interval"), Some("30000"))
  assert_eq(prod_config_loaded.get("monitoring.alerts.enabled"), Some("true"))
  assert_eq(prod_config_loaded.get("monitoring.alerts.thresholds.error_rate"), Some("0.05"))
  
  // 测试环境配置比较
  let env_comparison = MultiEnvConfigManager::compare_environments(multi_env_config_manager, "development", "production")
  assert_true(env_comparison.differences.length() > 0)
  
  // 验证具体差异
  let has_db_host_diff = env_comparison.differences.any(fn(d) { d.path == "database.host" })
  let has_pool_size_diff = env_comparison.differences.any(fn(d) { d.path == "database.pool_size" })
  let has_log_level_diff = env_comparison.differences.any(fn(d) { d.path == "logging.level" })
  let has_debug_mode_diff = env_comparison.differences.any(fn(d) { d.path == "features.debug_mode" })
  
  assert_true(has_db_host_diff)
  assert_true(has_pool_size_diff)
  assert_true(has_log_level_diff)
  assert_true(has_debug_mode_diff)
  
  // 测试环境配置验证
  let env_validation_rules = {
    "development": [
      {
        path: "database.host",
        required: true,
        pattern: "localhost|127\\.0\\.0\\.1"
      },
      {
        path: "logging.level",
        required: true,
        enum: ["debug", "info", "warn", "error"]
      }
    ],
    "production": [
      {
        path: "database.host",
        required: true,
        pattern: "^[a-zA-Z0-9.-]+\\.example\\.com$"
      },
      {
        path: "database.ssl.enabled",
        required: true,
        type: "boolean",
        value: true
      },
      {
        path: "logging.level",
        required: true,
        enum: ["info", "warn", "error"]
      },
      {
        path: "features.debug_mode",
        required: true,
        type: "boolean",
        value: false
      }
    ]
  }
  
  let dev_validation_result = MultiEnvConfigManager::validate_environment_config(multi_env_config_manager, "development", env_validation_rules)
  assert_true(dev_validation_result.valid)
  
  let prod_validation_result = MultiEnvConfigManager::validate_environment_config(multi_env_config_manager, "production", env_validation_rules)
  assert_true(prod_validation_result.valid)
  
  // 测试无效生产环境配置
  let invalid_prod_config = {
    "service": {
      "environment": "production"
    },
    "database": {
      "host": "localhost",  // 违反生产环境规则
      "ssl": {
        "enabled": false  // 违反生产环境规则
      }
    },
    "logging": {
      "level": "debug"  // 违反生产环境规则
    },
    "features": {
      "debug_mode": true  // 违反生产环境规则
    }
  }
  
  MultiEnvConfigManager::register_environment_config(multi_env_config_manager, "invalid_production", invalid_prod_config)
  
  let invalid_prod_validation_result = MultiEnvConfigManager::validate_environment_config(multi_env_config_manager, "invalid_production", env_validation_rules)
  assert_false(invalid_prod_validation_result.valid)
  assert_true(invalid_prod_validation_result.errors.length() >= 4)
  
  // 测试环境配置模板
  let env_template = {
    "service": {
      "name": "{{SERVICE_NAME}}",
      "environment": "{{ENVIRONMENT}}"
    },
    "database": {
      "host": "{{DB_HOST}}",
      "port": {{DB_PORT}},
      "name": "{{DB_NAME}}"
    },
    "logging": {
      "level": "{{LOG_LEVEL}}"
    }
  }
  
  let template_variables = {
    "SERVICE_NAME": "templated-service",
    "ENVIRONMENT": "staging",
    "DB_HOST": "template-db.example.com",
    "DB_PORT": 5432,
    "DB_NAME": "template_db",
    "LOG_LEVEL": "info"
  }
  
  let template_config_result = MultiEnvConfigManager::create_environment_from_template(multi_env_config_manager, "template_env", env_template, template_variables)
  assert_true(template_config_result.success)
  
  let template_config_loaded = template_config_result.config
  assert_eq(template_config_loaded.get("service.name"), Some("templated-service"))
  assert_eq(template_config_loaded.get("service.environment"), Some("staging"))
  assert_eq(template_config_loaded.get("database.host"), Some("template-db.example.com"))
  assert_eq(template_config_loaded.get("database.port"), Some("5432"))
  assert_eq(template_config_loaded.get("logging.level"), Some("info"))
  
  // 测试环境配置导出
  let export_result = MultiEnvConfigManager::export_environment_configs(multi_env_config_manager, ["development", "production"], "yaml")
  assert_true(export_result.success)
  assert_true(export_result.content.contains("development:"))
  assert_true(export_result.content.contains("production:"))
  
  // 测试环境配置迁移
  let migration_plan = {
    "from_environment": "development",
    "to_environment": "staging",
    "mappings": {
      "database.host": {
        "from": "localhost",
        "to": "staging-db.example.com"
      },
      "database.name": {
        "from": "multi_env_dev",
        "to": "multi_env_staging"
      },
      "logging.level": {
        "from": "debug",
        "to": "info"
      }
    }
  }
  
  let migration_result = MultiEnvConfigManager::migrate_config(multi_env_config_manager, migration_plan)
  assert_true(migration_result.success)
  
  let migrated_config = migration_result.config
  assert_eq(migrated_config.get("service.environment"), Some("staging"))
  assert_eq(migrated_config.get("database.host"), Some("staging-db.example.com"))
  assert_eq(migrated_config.get("database.name"), Some("multi_env_staging"))
  assert_eq(migrated_config.get("logging.level"), Some("info"))
  
  // 测试环境配置统计
  let env_stats = MultiEnvConfigManager::get_environment_stats(multi_env_config_manager)
  assert_eq(env_stats.total_environments, 5)  // base, development, test, staging, production
  assert_eq(env_stats.registered_configs, 6)  // + template_env
  assert_true(env_stats.config_sizes.contains("development"))
  assert_true(env_stats.config_sizes.contains("production"))
  assert_true(env_stats.inheritance_depths.contains("production"))
  assert_true(env_stats.validation_results.contains("development"))
}

// 测试7: 配置依赖和关系管理
test "配置依赖和关系管理功能" {
  // 创建配置依赖管理器
  let dependency_config_manager = DependencyConfigManager::new()
  
  // 定义配置依赖关系
  let config_dependencies = [
    {
      "source": "database.connection_string",
      "depends_on": ["database.host", "database.port", "database.name", "database.username", "database.password"],
      "transform": "build_connection_string"
    },
    {
      "source": "cache.connection_string",
      "depends_on": ["cache.host", "cache.port", "cache.db"],
      "transform": "build_cache_connection_string"
    },
    {
      "source": "service.full_name",
      "depends_on": ["service.name", "service.version"],
      "transform": "concat_name_version"
    },
    {
      "source": "telemetry.endpoint_urls",
      "depends_on": ["telemetry.endpoints"],
      "transform": "extract_urls"
    },
    {
      "source": "monitoring.health_check_url",
      "depends_on": ["service.host", "service.port", "service.health_path"],
      "transform": "build_health_check_url"
    }
  ]
  
  // 注册转换函数
  DependencyConfigManager::register_transform_function(dependency_config_manager, "build_connection_string", fn(dependencies) {
    let host = dependencies.get("database.host")
    let port = dependencies.get("database.port")
    let name = dependencies.get("database.name")
    let username = dependencies.get("database.username")
    let password = dependencies.get("database.password")
    
    match (host, port, name, username, password) {
      (Some(h), Some(p), Some(n), Some(u), Some(pwd)) => {
        Some("postgresql://" + u + ":" + pwd + "@" + h + ":" + p + "/" + n)
      }
      _ => None
    }
  })
  
  DependencyConfigManager::register_transform_function(dependency_config_manager, "build_cache_connection_string", fn(dependencies) {
    let host = dependencies.get("cache.host")
    let port = dependencies.get("cache.port")
    let db = dependencies.get("cache.db")
    
    match (host, port, db) {
      (Some(h), Some(p), Some(d)) => {
        Some("redis://" + h + ":" + p + "/" + d)
      }
      _ => None
    }
  })
  
  DependencyConfigManager::register_transform_function(dependency_config_manager, "concat_name_version", fn(dependencies) {
    let name = dependencies.get("service.name")
    let version = dependencies.get("service.version")
    
    match (name, version) {
      (Some(n), Some(v)) => {
        Some(n + "-" + v)
      }
      _ => None
    }
  })
  
  DependencyConfigManager::register_transform_function(dependency_config_manager, "extract_urls", fn(dependencies) {
    let endpoints = dependencies.get("telemetry.endpoints")
    
    match endpoints {
      Some(endpoint_list) => {
        // 假设endpoint_list是一个JSON数组字符串
        Some(endpoint_list)  // 简化实现
      }
      None => None
    }
  })
  
  DependencyConfigManager::register_transform_function(dependency_config_manager, "build_health_check_url", fn(dependencies) {
    let host = dependencies.get("service.host")
    let port = dependencies.get("service.port")
    let health_path = dependencies.get("service.health_path")
    
    match (host, port, health_path) {
      (Some(h), Some(p), Some(path)) => {
        Some("http://" + h + ":" + p + path)
      }
      _ => None
    }
  })
  
  // 注册依赖关系
  for dependency in config_dependencies {
    DependencyConfigManager::register_dependency(dependency_config_manager, dependency)
  }
  
  // 创建基础配置
  let base_config = {
    "service": {
      "name": "dependency-service",
      "version": "1.0.0",
      "host": "api.example.com",
      "port": 8080,
      "health_path": "/health"
    },
    "database": {
      "host": "db.example.com",
      "port": "5432",
      "name": "dependency_db",
      "username": "db_user",
      "password": "secure_password"
    },
    "cache": {
      "host": "cache.example.com",
      "port": "6379",
      "db": "0"
    },
    "telemetry": {
      "endpoints": "[{\"url\": \"https://telemetry.example.com/api/v1/traces\", \"timeout\": 5000}]"
    }
  }
  
  // 解析依赖关系
  let resolve_result = DependencyConfigManager::resolve_dependencies(dependency_config_manager, base_config)
  assert_true(resolve_result.success)
  
  let resolved_config = resolve_result.config
  
  // 验证解析后的依赖值
  let connection_string = resolved_config.get("database.connection_string")
  assert_true(connection_string != None)
  match connection_string {
    Some(conn_str) => {
      assert_true(conn_str.contains("postgresql://"))
      assert_true(conn_str.contains("db_user"))
      assert_true(conn_str.contains("secure_password"))
      assert_true(conn_str.contains("db.example.com"))
      assert_true(conn_str.contains("5432"))
      assert_true(conn_str.contains("dependency_db"))
    }
    None => assert_true(false)
  }
  
  let cache_connection_string = resolved_config.get("cache.connection_string")
  assert_true(cache_connection_string != None)
  match cache_connection_string {
    Some(conn_str) => {
      assert_eq(conn_str, "redis://cache.example.com:6379/0")
    }
    None => assert_true(false)
  }
  
  let service_full_name = resolved_config.get("service.full_name")
  assert_true(service_full_name != None)
  match service_full_name {
    Some(full_name) => {
      assert_eq(full_name, "dependency-service-1.0.0")
    }
    None => assert_true(false)
  }
  
  let telemetry_endpoint_urls = resolved_config.get("telemetry.endpoint_urls")
  assert_true(telemetry_endpoint_urls != None)
  
  let health_check_url = resolved_config.get("monitoring.health_check_url")
  assert_true(health_check_url != None)
  match health_check_url {
    Some(url) => {
      assert_eq(url, "http://api.example.com:8080/health")
    }
    None => assert_true(false)
  }
  
  // 测试依赖变更传播
  let updated_config = {
    "service": {
      "name": "dependency-service",
      "version": "1.1.0",  // 版本变更
      "host": "api.example.com",
      "port": 8080,
      "health_path": "/health"
    },
    "database": {
      "host": "new-db.example.com",  // 主机变更
      "port": "5432",
      "name": "dependency_db",
      "username": "db_user",
      "password": "secure_password"
    },
    "cache": {
      "host": "cache.example.com",
      "port": "6379",
      "db": "1"  // 数据库变更
    },
    "telemetry": {
      "endpoints": "[{\"url\": \"https://telemetry.example.com/api/v1/traces\", \"timeout\": 5000}]"
    }
  }
  
  let propagation_result = DependencyConfigManager::propagate_changes(dependency_config_manager, base_config, updated_config)
  assert_true(propagation_result.success)
  
  let propagated_config = propagation_result.config
  
  // 验证变更传播
  let updated_service_full_name = propagated_config.get("service.full_name")
  match updated_service_full_name {
    Some(full_name) => {
      assert_eq(full_name, "dependency-service-1.1.0")  // 版本变更已传播
    }
    None => assert_true(false)
  }
  
  let updated_connection_string = propagated_config.get("database.connection_string")
  match updated_connection_string {
    Some(conn_str) => {
      assert_true(conn_str.contains("new-db.example.com"))  // 主机变更已传播
    }
    None => assert_true(false)
  }
  
  let updated_cache_connection_string = propagated_config.get("cache.connection_string")
  match updated_cache_connection_string {
    Some(conn_str) => {
      assert_eq(conn_str, "redis://cache.example.com:6379/1")  // 数据库变更已传播
    }
    None => assert_true(false)
  }
  
  // 测试循环依赖检测
  let circular_dependencies = [
    {
      "source": "config.a",
      "depends_on": ["config.b"],
      "transform": "identity"
    },
    {
      "source": "config.b",
      "depends_on": ["config.c"],
      "transform": "identity"
    },
    {
      "source": "config.c",
      "depends_on": ["config.a"],  // 循环依赖
      "transform": "identity"
    }
  ]
  
  for dependency in circular_dependencies {
    DependencyConfigManager::register_dependency(dependency_config_manager, dependency)
  }
  
  let circular_config = {
    "config": {
      "a": "value_a",
      "b": "value_b",
      "c": "value_c"
    }
  }
  
  let circular_resolve_result = DependencyConfigManager::resolve_dependencies(dependency_config_manager, circular_config)
  assert_false(circular_resolve_result.success)
  assert_true(circular_resolve_result.error_message.contains("circular"))
  
  // 测试缺失依赖检测
  let incomplete_config = {
    "service": {
      "name": "dependency-service",
      "version": "1.0.0"
      // 缺少 host, port, health_path
    },
    "database": {
      "host": "db.example.com",
      "port": "5432"
      // 缺少 name, username, password
    }
  }
  
  let incomplete_resolve_result = DependencyConfigManager::resolve_dependencies(dependency_config_manager, incomplete_config)
  assert_false(incomplete_resolve_result.success)
  assert_true(incomplete_resolve_result.error_message.contains("missing"))
  
  // 测试依赖验证
  let dependency_validation_rules = [
    {
      "dependency": "database.connection_string",
      "rules": [
        {
          "type": "format",
          "pattern": "^postgresql://[^:]+:[^@]+@[^:]+:\\d+/[^/]+$"
        }
      ]
    },
    {
      "dependency": "cache.connection_string",
      "rules": [
        {
          "type": "format",
          "pattern": "^redis://[^:]+:\\d+/\\d+$"
        }
      ]
    }
  ]
  
  let validation_result = DependencyConfigManager::validate_dependencies(dependency_config_manager, resolved_config, dependency_validation_rules)
  assert_true(validation_result.valid)
  
  // 测试依赖可视化
  let dependency_graph = DependencyConfigManager::generate_dependency_graph(dependency_config_manager, base_config)
  assert_true(dependency_graph.nodes.length() > 0)
  assert_true(dependency_graph.edges.length() > 0)
  
  // 验证图结构
  let has_database_node = dependency_graph.nodes.any(fn(n) { n.id == "database.connection_string" })
  let has_service_node = dependency_graph.nodes.any(fn(n) { n.id == "service.full_name" })
  
  assert_true(has_database_node)
  assert_true(has_service_node)
  
  // 测试依赖影响分析
  let impact_analysis = DependencyConfigManager::analyze_change_impact(dependency_config_manager, base_config, ["database.host"])
  assert_true(impact_analysis.affected_dependencies.length() > 0)
  
  // 验证影响分析结果
  let has_connection_string_impact = impact_analysis.affected_dependencies.any(fn(d) { d == "database.connection_string" })
  assert_true(has_connection_string_impact)
  
  // 测试依赖统计
  let dependency_stats = DependencyConfigManager::get_dependency_stats(dependency_config_manager)
  assert_true(dependency_stats.total_dependencies > 0)
  assert_true(dependency_stats.transform_functions > 0)
  assert_true(dependency_stats.resolution_attempts > 0)
  assert_true(dependency_stats.successful_resolutions > 0)
  assert_true(dependency_stats.failed_resolutions > 0)
}

// 测试8: 配置模板和生成器
test "配置模板和生成器功能" {
  // 创建配置模板管理器
  let template_config_manager = TemplateConfigManager::new()
  
  // 定义配置模板
  let service_template = {
    "name": "service_template",
    "description": "通用服务配置模板",
    "variables": [
      {
        "name": "SERVICE_NAME",
        "type": "string",
        "required": true,
        "description": "服务名称",
        "pattern": "^[a-z0-9-]+$"
      },
      {
        "name": "SERVICE_VERSION",
        "type": "string",
        "required": true,
        "description": "服务版本",
        "pattern": "^\\d+\\.\\d+\\.\\d+$"
      },
      {
        "name": "ENVIRONMENT",
        "type": "enum",
        "required": true,
        "description": "部署环境",
        "values": ["development", "staging", "production"],
        "default": "development"
      },
      {
        "name": "PORT",
        "type": "integer",
        "required": false,
        "description": "服务端口",
        "min": 1024,
        "max": 65535,
        "default": 8080
      },
      {
        "name": "ENABLE_TELEMETRY",
        "type": "boolean",
        "required": false,
        "description": "启用遥测",
        "default": true
      }
    ],
    "template": "
service:
  name: \"{{SERVICE_NAME}}\"
  version: \"{{SERVICE_VERSION}}\"
  environment: \"{{ENVIRONMENT}}\"
  port: {{PORT}}
  
telemetry:
  enabled: {{ENABLE_TELEMETRY}}
  sampling_rate: {{#if (eq ENVIRONMENT \"production\")}}0.1{{else}}0.5{{/if}}
  
logging:
  level: {{#if (eq ENVIRONMENT \"development\")}}debug{{else if (eq ENVIRONMENT \"staging\")}}info{{else}}warn{{/if}}
  format: json
  
health_check:
  enabled: true
  path: \"/health\"
  interval: 30000
"
  }
  
  let database_template = {
    "name": "database_template",
    "description": "数据库配置模板",
    "variables": [
      {
        "name": "DB_TYPE",
        "type": "enum",
        "required": true,
        "description": "数据库类型",
        "values": ["postgresql", "mysql", "mongodb"]
      },
      {
        "name": "DB_HOST",
        "type": "string",
        "required": true,
        "description": "数据库主机"
      },
      {
        "name": "DB_PORT",
        "type": "integer",
        "required": false,
        "description": "数据库端口",
        "default": 5432
      },
      {
        "name": "DB_NAME",
        "type": "string",
        "required": true,
        "description": "数据库名称"
      },
      {
        "name": "DB_USERNAME",
        "type": "string",
        "required": true,
        "description": "数据库用户名"
      },
      {
        "name": "DB_PASSWORD",
        "type": "string",
        "required": true,
        "description": "数据库密码",
        "sensitive": true
      },
      {
        "name": "DB_POOL_SIZE",
        "type": "integer",
        "required": false,
        "description": "连接池大小",
        "min": 1,
        "max": 100,
        "default": 10
      },
      {
        "name": "ENABLE_SSL",
        "type": "boolean",
        "required": false,
        "description": "启用SSL",
        "default": false
      }
    ],
    "template": "
database:
  type: \"{{DB_TYPE}}\"
  host: \"{{DB_HOST}}\"
  port: {{DB_PORT}}
  name: \"{{DB_NAME}}\"
  username: \"{{DB_USERNAME}}\"
  password: \"{{DB_PASSWORD}}\"
  pool_size: {{DB_POOL_SIZE}}
  
  {{#if ENABLE_SSL}}
  ssl:
    enabled: true
    cert_path: \"/etc/ssl/certs/db.crt\"
    key_path: \"/etc/ssl/private/db.key\"
  {{/if}}
  
  {{#if (eq DB_TYPE \"postgresql\")}}
  connection_string: \"postgresql://{{DB_USERNAME}}:{{DB_PASSWORD}}@{{DB_HOST}}:{{DB_PORT}}/{{DB_NAME}}\"
  {{else if (eq DB_TYPE \"mysql\")}}
  connection_string: \"{{DB_USERNAME}}:{{DB_PASSWORD}}@tcp({{DB_HOST}}:{{DB_PORT}})/{{DB_NAME}}\"
  {{else if (eq DB_TYPE \"mongodb\")}}
  connection_string: \"mongodb://{{DB_USERNAME}}:{{DB_PASSWORD}}@{{DB_HOST}}:{{DB_PORT}}/{{DB_NAME}}\"
  {{/if}}
"
  }
  
  let monitoring_template = {
    "name": "monitoring_template",
    "description": "监控配置模板",
    "variables": [
      {
        "name": "METRICS_ENABLED",
        "type": "boolean",
        "required": false,
        "description": "启用指标收集",
        "default": true
      },
      {
        "name": "METRICS_PORT",
        "type": "integer",
        "required": false,
        "description": "指标端口",
        "default": 9090
      },
      {
        "name": "TRACING_ENABLED",
        "type": "boolean",
        "required": false,
        "description": "启用分布式追踪",
        "default": true
      },
      {
        "name": "TRACING_ENDPOINT",
        "type": "string",
        "required": false,
        "description": "追踪端点",
        "default": "http://jaeger:14268/api/traces"
      },
      {
        "name": "ALERT_WEBHOOK_URL",
        "type": "string",
        "required": false,
        "description": "告警Webhook URL"
      }
    ],
    "template": "
monitoring:
  metrics:
    enabled: {{METRICS_ENABLED}}
    port: {{METRICS_PORT}}
    path: \"/metrics\"
    interval: 60000
    
  tracing:
    enabled: {{TRACING_ENABLED}}
    {{#if TRACING_ENABLED}}
    endpoint: \"{{TRACING_ENDPOINT}}\"
    sampling_rate: 0.1
    {{/if}}
    
  {{#if ALERT_WEBHOOK_URL}}
  alerts:
    webhook_url: \"{{ALERT_WEBHOOK_URL}}\"
    enabled: true
  {{/if}}
"
  }
  
  // 注册模板
  TemplateConfigManager::register_template(template_config_manager, service_template)
  TemplateConfigManager::register_template(template_config_manager, database_template)
  TemplateConfigManager::register_template(template_config_manager, monitoring_template)
  
  // 创建模板变量
  let service_variables = {
    "SERVICE_NAME": "user-service",
    "SERVICE_VERSION": "1.2.3",
    "ENVIRONMENT": "production",
    "PORT": 8080,
    "ENABLE_TELEMETRY": true
  }
  
  let database_variables = {
    "DB_TYPE": "postgresql",
    "DB_HOST": "db.example.com",
    "DB_PORT": 5432,
    "DB_NAME": "users",
    "DB_USERNAME": "user_service",
    "DB_PASSWORD": "secure_password_123",
    "DB_POOL_SIZE": 20,
    "ENABLE_SSL": true
  }
  
  let monitoring_variables = {
    "METRICS_ENABLED": true,
    "METRICS_PORT": 9090,
    "TRACING_ENABLED": true,
    "TRACING_ENDPOINT": "http://jaeger.example.com:14268/api/traces",
    "ALERT_WEBHOOK_URL": "https://alerts.example.com/webhook"
  }
  
  // 渲染服务模板
  let service_render_result = TemplateConfigManager::render_template(template_config_manager, "service_template", service_variables)
  assert_true(service_render_result.success)
  
  let service_config = service_render_result.config
  
  // 验证服务配置
  assert_eq(service_config.get("service.name"), Some("user-service"))
  assert_eq(service_config.get("service.version"), Some("1.2.3"))
  assert_eq(service_config.get("service.environment"), Some("production"))
  assert_eq(service_config.get("service.port"), Some("8080"))
  assert_eq(service_config.get("telemetry.enabled"), Some("true"))
  assert_eq(service_config.get("telemetry.sampling_rate"), Some("0.1"))  // 生产环境采样率
  assert_eq(service_config.get("logging.level"), Some("warn"))  // 生产环境日志级别
  
  // 渲染数据库模板
  let database_render_result = TemplateConfigManager::render_template(template_config_manager, "database_template", database_variables)
  assert_true(database_render_result.success)
  
  let database_config = database_render_result.config
  
  // 验证数据库配置
  assert_eq(database_config.get("database.type"), Some("postgresql"))
  assert_eq(database_config.get("database.host"), Some("db.example.com"))
  assert_eq(database_config.get("database.port"), Some("5432"))
  assert_eq(database_config.get("database.name"), Some("users"))
  assert_eq(database_config.get("database.username"), Some("user_service"))
  assert_eq(database_config.get("database.password"), Some("secure_password_123"))
  assert_eq(database_config.get("database.pool_size"), Some("20"))
  assert_eq(database_config.get("database.ssl.enabled"), Some("true"))
  assert_eq(database_config.get("database.connection_string"), Some("postgresql://user_service:secure_password_123@db.example.com:5432/users"))
  
  // 渲染监控模板
  let monitoring_render_result = TemplateConfigManager::render_template(template_config_manager, "monitoring_template", monitoring_variables)
  assert_true(monitoring_render_result.success)
  
  let monitoring_config = monitoring_render_result.config
  
  // 验证监控配置
  assert_eq(monitoring_config.get("monitoring.metrics.enabled"), Some("true"))
  assert_eq(monitoring_config.get("monitoring.metrics.port"), Some("9090"))
  assert_eq(monitoring_config.get("monitoring.tracing.enabled"), Some("true"))
  assert_eq(monitoring_config.get("monitoring.tracing.endpoint"), Some("http://jaeger.example.com:14268/api/traces"))
  assert_eq(monitoring_config.get("monitoring.alerts.webhook_url"), Some("https://alerts.example.com/webhook"))
  
  // 测试模板组合
  let combined_config = TemplateConfigManager::combine_configs(template_config_manager, [
    service_config,
    database_config,
    monitoring_config
  ])
  
  // 验证组合配置
  assert_eq(combined_config.get("service.name"), Some("user-service"))
  assert_eq(combined_config.get("database.type"), Some("postgresql"))
  assert_eq(combined_config.get("monitoring.metrics.enabled"), Some("true"))
  
  // 测试模板验证
  let template_validation_result = TemplateConfigManager::validate_template_variables(template_config_manager, "service_template", service_variables)
  assert_true(template_validation_result.valid)
  
  // 测试无效变量验证
  let invalid_service_variables = {
    "SERVICE_NAME": "",  // 空字符串，违反模式
    "SERVICE_VERSION": "1.2",  // 无效版本格式
    "ENVIRONMENT": "invalid_env",  // 无效枚举值
    "PORT": 100,  // 端口太小
    "ENABLE_TELEMETRY": "not_boolean"  // 无效布尔值
  }
  
  let invalid_validation_result = TemplateConfigManager::validate_template_variables(template_config_manager, "service_template", invalid_service_variables)
  assert_false(invalid_validation_result.valid)
  assert_true(invalid_validation_result.errors.length() >= 5)
  
  // 测试模板继承
  let base_service_template = {
    "name": "base_service_template",
    "description": "基础服务模板",
    "variables": [
      {
        "name": "SERVICE_NAME",
        "type": "string",
        "required": true
      },
      {
        "name": "SERVICE_VERSION",
        "type": "string",
        "required": true
      }
    ],
    "template": "
service:
  name: \"{{SERVICE_NAME}}\"
  version: \"{{SERVICE_VERSION}}\"
  
logging:
  level: info
  format: json
"
  }
  
  let extended_service_template = {
    "name": "extended_service_template",
    "description": "扩展服务模板",
    "extends": "base_service_template",
    "variables": [
      {
        "name": "ENVIRONMENT",
        "type": "enum",
        "required": true,
        "values": ["development", "staging", "production"]
      },
      {
        "name": "PORT",
        "type": "integer",
        "required": false,
        "default": 8080
      }
    ],
    "template": "
service:
  port: {{PORT}}
  environment: \"{{ENVIRONMENT}}\"
  
telemetry:
  enabled: true
"
  }
  
  TemplateConfigManager::register_template(template_config_manager, base_service_template)
  TemplateConfigManager::register_template(template_config_manager, extended_service_template)
  
  let extended_variables = {
    "SERVICE_NAME": "extended-service",
    "SERVICE_VERSION": "2.0.0",
    "ENVIRONMENT": "staging",
    "PORT": 9090
  }
  
  let extended_render_result = TemplateConfigManager::render_template(template_config_manager, "extended_service_template", extended_variables)
  assert_true(extended_render_result.success)
  
  let extended_config = extended_render_result.config
  
  // 验证继承的配置
  assert_eq(extended_config.get("service.name"), Some("extended-service"))
  assert_eq(extended_config.get("service.version"), Some("2.0.0"))
  assert_eq(extended_config.get("service.port"), Some("9090"))  // 来自扩展模板
  assert_eq(extended_config.get("service.environment"), Some("staging"))  // 来自扩展模板
  assert_eq(extended_config.get("logging.level"), Some("info"))  // 来自基础模板
  assert_eq(extended_config.get("telemetry.enabled"), Some("true"))  // 来自扩展模板
  
  // 测试模板函数
  TemplateConfigManager::register_template_function(template_config_manager, "generate_secret", fn(args) {
    let length = args.get("length")
    match length {
      Some(len_str) => {
        match len_str.parse_int() {
          Some(len) => {
            // 生成指定长度的随机字符串
            "generated_secret_" + len.to_string()
          }
          None => "default_secret"
        }
      }
      None => "default_secret"
    }
  })
  
  let template_with_function = {
    "name": "function_template",
    "description": "使用函数的模板",
    "variables": [
      {
        "name": "SECRET_LENGTH",
        "type": "integer",
        "required": false,
        "default": 16
      }
    ],
    "template": "
secrets:
  api_key: \"{{generate_secret length=SECRET_LENGTH}}\"
  jwt_secret: \"{{generate_secret length=32}}\"
"
  }
  
  TemplateConfigManager::register_template(template_config_manager, template_with_function)
  
  let function_variables = {
    "SECRET_LENGTH": 20
  }
  
  let function_render_result = TemplateConfigManager::render_template(template_config_manager, "function_template", function_variables)
  assert_true(function_render_result.success)
  
  let function_config = function_render_result.config
  assert_eq(function_config.get("secrets.api_key"), Some("generated_secret_20"))
  assert_eq(function_config.get("secrets.jwt_secret"), Some("generated_secret_32"))
  
  // 测试模板导出
  let template_export_result = TemplateConfigManager::export_template(template_config_manager, "service_template", "yaml")
  assert_true(template_export_result.success)
  assert_true(template_export_result.content.contains("name: service_template"))
  assert_true(template_export_result.content.contains("variables:"))
  assert_true(template_export_result.content.contains("template:"))
  
  // 测试模板统计
  let template_stats = TemplateConfigManager::get_template_stats(template_config_manager)
  assert_true(template_stats.total_templates >= 4)  // service, database, monitoring, function
  assert_true(template_stats.total_variables > 0)
  assert_true(template_stats.total_renders > 0)
  assert_true(template_stats.template_usage.contains("service_template"))
}