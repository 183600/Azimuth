// Azimuth 配置管理测试用例
// 专注于系统配置的加载、验证、更新和分发功能

// 测试1: 配置加载和解析
test "配置加载和解析功能" {
  // 模拟配置文件内容
  let config_content = {
    "application": {
      "name": "azimuth-telemetry",
      "version": "1.2.3",
      "environment": "production",
      "debug": false,
      "log_level": "info"
    },
    "server": {
      "host": "0.0.0.0",
      "port": 8080,
      "read_timeout_ms": 30000,
      "write_timeout_ms": 30000,
      "max_connections": 1000
    },
    "database": {
      "host": "db.example.com",
      "port": 5432,
      "name": "azimuth_db",
      "username": "azimuth_user",
      "password": "secure_password",
      "max_connections": 20,
      "connection_timeout_ms": 5000
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.1,
      "export_interval_ms": 10000,
      "metrics": {
        "cpu": true,
        "memory": true,
        "disk": true,
        "network": true
      },
      "tracing": {
        "enabled": true,
        "jaeger_endpoint": "http://jaeger:14268/api/traces"
      }
    },
    "features": {
      "new_dashboard": true,
      "advanced_analytics": false,
      "beta_features": {
        "ml_predictions": true,
        "auto_scaling": false
      }
    }
  }
  
  // 验证配置结构
  assert_eq(config_content["application"]["name"], "azimuth-telemetry")
  assert_eq(config_content["application"]["version"], "1.2.3")
  assert_eq(config_content["application"]["environment"], "production")
  assert_eq(config_content["application"]["debug"], false)
  assert_eq(config_content["application"]["log_level"], "info")
  
  // 验证服务器配置
  assert_eq(config_content["server"]["host"], "0.0.0.0")
  assert_eq(config_content["server"]["port"], 8080)
  assert_eq(config_content["server"]["read_timeout_ms"], 30000)
  assert_eq(config_content["server"]["write_timeout_ms"], 30000)
  assert_eq(config_content["server"]["max_connections"], 1000)
  
  // 验证数据库配置
  assert_eq(config_content["database"]["host"], "db.example.com")
  assert_eq(config_content["database"]["port"], 5432)
  assert_eq(config_content["database"]["name"], "azimuth_db")
  assert_eq(config_content["database"]["username"], "azimuth_user")
  assert_eq(config_content["database"]["password"], "secure_password")
  assert_eq(config_content["database"]["max_connections"], 20)
  assert_eq(config_content["database"]["connection_timeout_ms"], 5000)
  
  // 验证遥测配置
  assert_eq(config_content["telemetry"]["enabled"], true)
  assert_eq(config_content["telemetry"]["sampling_rate"], 0.1)
  assert_eq(config_content["telemetry"]["export_interval_ms"], 10000)
  assert_eq(config_content["telemetry"]["metrics"]["cpu"], true)
  assert_eq(config_content["telemetry"]["metrics"]["memory"], true)
  assert_eq(config_content["telemetry"]["metrics"]["disk"], true)
  assert_eq(config_content["telemetry"]["metrics"]["network"], true)
  assert_eq(config_content["telemetry"]["tracing"]["enabled"], true)
  assert_eq(config_content["telemetry"]["tracing"]["jaeger_endpoint"], "http://jaeger:14268/api/traces")
  
  // 验证功能开关
  assert_eq(config_content["features"]["new_dashboard"], true)
  assert_eq(config_content["features"]["advanced_analytics"], false)
  assert_eq(config_content["features"]["beta_features"]["ml_predictions"], true)
  assert_eq(config_content["features"]["beta_features"]["auto_scaling"], false)
  
  // 实现配置解析器
  let parse_config = fn(raw_config) {
    // 验证必需字段
    let required_fields = [
      "application.name",
      "application.version",
      "server.host",
      "server.port",
      "database.host",
      "database.name"
    ]
    
    let mut validation_errors = []
    
    for field_path in required_fields {
      let path_parts = field_path.split(".")
      let mut current = raw_config
      let mut field_exists = true
      
      for part in path_parts {
        match current.get(part) {
          Some(value) => {
            match value {
              Object(obj) => current = obj,
              _ => {}
            }
          },
          None => {
            field_exists = false
            break
          }
        }
      }
      
      if !field_exists {
        validation_errors = validation_errors.push("Missing required field: " + field_path)
      }
    }
    
    // 数据类型验证
    let type_validations = [
      {"path": "server.port", "expected_type": "Int"},
      {"path": "server.max_connections", "expected_type": "Int"},
      {"path": "database.port", "expected_type": "Int"},
      {"path": "database.max_connections", "expected_type": "Int"},
      {"path": "telemetry.sampling_rate", "expected_type": "Float"},
      {"path": "application.debug", "expected_type": "Bool"},
      {"path": "telemetry.enabled", "expected_type": "Bool"}
    ]
    
    for validation in type_validations {
      let path_parts = validation["path"].split(".")
      let mut current = raw_config
      let mut field_value = None
      
      for part in path_parts {
        match current.get(part) {
          Some(value) => {
            match value {
              Object(obj) => current = obj,
              _ => field_value = Some(value)
            }
          },
          None => {
            field_value = None
            break
          }
        }
      }
      
      match field_value {
        Some(value) => {
          let type_matches = match validation["expected_type"] {
            "Int" => value is Int,
            "Float" => value is Float,
            "Bool" => value is Bool,
            "String" => value is String,
            _ => false
          }
          
          if !type_matches {
            validation_errors = validation_errors.push(
              "Type mismatch for field " + validation["path"] + 
              ": expected " + validation["expected_type"]
            )
          }
        },
        None => {
          validation_errors = validation_errors.push(
            "Field not found for type validation: " + validation["path"]
          )
        }
      }
    }
    
    {
      "config": raw_config,
      "valid": validation_errors.length() == 0,
      "errors": validation_errors
    }
  }
  
  // 解析配置
  let parsed_config = parse_config(config_content)
  
  // 验证解析结果
  assert_eq(parsed_config["valid"], true)
  assert_eq(parsed_config["errors"].length(), 0)
}

// 测试2: 配置验证和默认值
test "配置验证和默认值处理" {
  // 模拟不完整的配置
  let incomplete_config = {
    "application": {
      "name": "azimuth-telemetry",
      // 缺少version字段
      "environment": "production",
      "debug": false
    },
    "server": {
      "host": "0.0.0.0",
      "port": "8080", // 错误类型：应该是Int
      "read_timeout_ms": 30000
      // 缺少write_timeout_ms和max_connections字段
    },
    "database": {
      "host": "db.example.com",
      // 缺少port字段
      "name": "azimuth_db",
      "username": "azimuth_user",
      "password": "secure_password"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 1.5, // 超出有效范围(0-1)
      "export_interval_ms": 10000
      // 缺少metrics和tracing配置
    }
  }
  
  // 定义默认配置
  let default_config = {
    "application": {
      "version": "1.0.0",
      "log_level": "info"
    },
    "server": {
      "write_timeout_ms": 30000,
      "max_connections": 100
    },
    "database": {
      "port": 5432,
      "max_connections": 10,
      "connection_timeout_ms": 5000
    },
    "telemetry": {
      "sampling_rate": 0.1,
      "metrics": {
        "cpu": true,
        "memory": true,
        "disk": true,
        "network": true
      },
      "tracing": {
        "enabled": false,
        "jaeger_endpoint": ""
      }
    }
  }
  
  // 实现配置合并和验证
  let merge_and_validate_config = fn(user_config, defaults) {
    // 递归合并配置
    let merge_objects = fn(user, default) {
      let mut merged = default
      
      for (key, value) in user {
        match default.get(key) {
          Some(default_value) => {
            match (value, default_value) {
              (Object(user_obj), Object(default_obj)) => {
                merged = merged.set(key, merge_objects(user_obj, default_obj))
              },
              _ => {
                merged = merged.set(key, value)
              }
            }
          },
          None => {
            merged = merged.set(key, value)
          }
        }
      }
      
      merged
    }
    
    let merged_config = merge_objects(user_config, defaults)
    
    // 验证配置值
    let mut validation_errors = []
    let mut warnings = []
    
    // 验证采样率范围
    let sampling_rate = merged_config["telemetry"]["sampling_rate"]
    if sampling_rate < 0.0 || sampling_rate > 1.0 {
      validation_errors = validation_errors.push(
        "telemetry.sampling_rate must be between 0.0 and 1.0, got: " + sampling_rate.to_string()
      )
    }
    
    // 验证端口号范围
    let server_port = merged_config["server"]["port"]
    if server_port is String {
      validation_errors = validation_errors.push(
        "server.port must be an integer, got: " + server_port.to_string()
      )
    } else if server_port < 1 || server_port > 65535 {
      validation_errors = validation_errors.push(
        "server.port must be between 1 and 65535, got: " + server_port.to_string()
      )
    }
    
    // 验证数据库端口号
    let db_port = merged_config["database"]["port"]
    if db_port < 1 || db_port > 65535 {
      validation_errors = validation_errors.push(
        "database.port must be between 1 and 65535, got: " + db_port.to_string()
      )
    }
    
    // 生成警告
    let debug_mode = merged_config["application"]["debug"]
    let environment = merged_config["application"]["environment"]
    if debug_mode && environment == "production" {
      warnings = warnings.push(
        "Debug mode is enabled in production environment"
      )
    }
    
    let max_connections = merged_config["server"]["max_connections"]
    if max_connections > 10000 {
      warnings = warnings.push(
        "High max_connections value may impact performance: " + max_connections.to_string()
      )
    }
    
    {
      "config": merged_config,
      "valid": validation_errors.length() == 0,
      "errors": validation_errors,
      "warnings": warnings
    }
  }
  
  // 合并和验证配置
  let result = merge_and_validate_config(incomplete_config, default_config)
  
  // 验证结果
  assert_eq(result["valid"], false)
  assert_eq(result["errors"].length(), 2)
  assert_eq(result["warnings"].length(), 0)
  
  // 验证错误信息
  assert_true(result["errors"][0].contains("server.port must be an integer"))
  assert_true(result["errors"][1].contains("telemetry.sampling_rate must be between 0.0 and 1.0"))
  
  // 验证默认值已合并
  assert_eq(result["config"]["application"]["version"], "1.0.0")
  assert_eq(result["config"]["server"]["write_timeout_ms"], 30000)
  assert_eq(result["config"]["server"]["max_connections"], 100)
  assert_eq(result["config"]["database"]["port"], 5432)
  assert_eq(result["config"]["database"]["max_connections"], 10)
  assert_eq(result["config"]["telemetry"]["metrics"]["cpu"], true)
  assert_eq(result["config"]["telemetry"]["tracing"]["enabled"], false)
}

// 测试3: 动态配置更新
test "动态配置更新功能" {
  // 初始配置
  let initial_config = {
    "application": {
      "name": "azimuth-telemetry",
      "version": "1.2.3",
      "log_level": "info",
      "debug": false
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.1,
      "export_interval_ms": 10000
    },
    "features": {
      "new_dashboard": true,
      "advanced_analytics": false,
      "beta_features": {
        "ml_predictions": true,
        "auto_scaling": false
      }
    }
  }
  
  // 配置更新
  let config_updates = [
    {
      "timestamp": 1640995200,
      "path": "application.log_level",
      "old_value": "info",
      "new_value": "debug",
      "requires_restart": false
    },
    {
      "timestamp": 1640995300,
      "path": "telemetry.sampling_rate",
      "old_value": 0.1,
      "new_value": 0.2,
      "requires_restart": false
    },
    {
      "timestamp": 1640995400,
      "path": "features.new_dashboard",
      "old_value": true,
      "new_value": false,
      "requires_restart": false
    },
    {
      "timestamp": 1640995500,
      "path": "features.beta_features.auto_scaling",
      "old_value": false,
      "new_value": true,
      "requires_restart": false
    },
    {
      "timestamp": 1640995600,
      "path": "application.version",
      "old_value": "1.2.3",
      "new_value": "1.3.0",
      "requires_restart": true
    }
  ]
  
  // 验证初始配置
  assert_eq(initial_config["application"]["log_level"], "info")
  assert_eq(initial_config["telemetry"]["sampling_rate"], 0.1)
  assert_eq(initial_config["features"]["new_dashboard"], true)
  assert_eq(initial_config["features"]["beta_features"]["auto_scaling"], false)
  assert_eq(initial_config["application"]["version"], "1.2.3")
  
  // 实现动态配置更新
  let update_config = fn(config, updates) {
    let mut updated_config = config
    let mut applied_updates = []
    let mut restart_required = false
    
    for update in updates {
      let path_parts = update["path"].split(".")
      let mut current = updated_config
      let mut parent_path = []
      
      // 导航到父对象
      for i in 0..=(path_parts.length() - 2) {
        parent_path = parent_path.push(path_parts[i])
        
        match current.get(path_parts[i]) {
          Some(value) => {
            match value {
              Object(obj) => current = obj,
              _ => {}
            }
          },
          None => {}
        }
      }
      
      // 获取最后一个键
      let final_key = path_parts[path_parts.length() - 1]
      
      // 验证当前值是否匹配旧值
      let mut current_value = None
      match current.get(final_key) {
        Some(value) => current_value = Some(value),
        None => {}
      }
      
      match current_value {
        Some(value) => {
          if value == update["old_value"] {
            // 应用更新
            // 注意：这是一个简化的实现，实际中需要更复杂的对象更新逻辑
            updated_config = deep_set(updated_config, update["path"], update["new_value"])
            applied_updates = applied_updates.push(update)
            
            if update["requires_restart"] {
              restart_required = true
            }
          }
        },
        None => {}
      }
    }
    
    {
      "config": updated_config,
      "applied_updates": applied_updates,
      "restart_required": restart_required
    }
  }
  
  // 深度设置对象属性（简化实现）
  let deep_set = fn(obj, path, value) {
    let path_parts = path.split(".")
    let mut current = obj
    
    // 这是一个简化的实现，实际中需要递归创建嵌套对象
    if path_parts.length() == 2 && path_parts[0] == "application" {
      current = current.set("application", {
        ...current["application"],
        path_parts[1]: value
      })
    } else if path_parts.length() == 2 && path_parts[0] == "telemetry" {
      current = current.set("telemetry", {
        ...current["telemetry"],
        path_parts[1]: value
      })
    } else if path_parts.length() == 2 && path_parts[0] == "features" {
      current = current.set("features", {
        ...current["features"],
        path_parts[1]: value
      })
    } else if path_parts.length() == 3 && path_parts[0] == "features" && path_parts[1] == "beta_features" {
      current = current.set("features", {
        ...current["features"],
        "beta_features": {
          ...current["features"]["beta_features"],
          path_parts[2]: value
        }
      })
    }
    
    current
  }
  
  // 应用配置更新
  let update_result = update_config(initial_config, config_updates)
  
  // 验证更新结果
  assert_eq(update_result["applied_updates"].length(), 5)
  assert_eq(update_result["restart_required"], true)
  
  // 验证更新后的配置
  let final_config = update_result["config"]
  assert_eq(final_config["application"]["log_level"], "debug")
  assert_eq(final_config["telemetry"]["sampling_rate"], 0.2)
  assert_eq(final_config["features"]["new_dashboard"], false)
  assert_eq(final_config["features"]["beta_features"]["auto_scaling"], true)
  assert_eq(final_config["application"]["version"], "1.3.0")
}

// 测试4: 配置版本管理和回滚
test "配置版本管理和回滚功能" {
  // 配置历史记录
  let config_history = [
    {
      "version": 1,
      "timestamp": 1640995000,
      "config": {
        "application": {
          "name": "azimuth-telemetry",
          "version": "1.0.0",
          "log_level": "info"
        },
        "telemetry": {
          "enabled": true,
          "sampling_rate": 0.1
        }
      },
      "change_description": "Initial configuration"
    },
    {
      "version": 2,
      "timestamp": 1640995200,
      "config": {
        "application": {
          "name": "azimuth-telemetry",
          "version": "1.1.0",
          "log_level": "debug"
        },
        "telemetry": {
          "enabled": true,
          "sampling_rate": 0.2
        }
      },
      "change_description": "Increased sampling rate and enabled debug logging"
    },
    {
      "version": 3,
      "timestamp": 1640995400,
      "config": {
        "application": {
          "name": "azimuth-telemetry",
          "version": "1.2.0",
          "log_level": "info"
        },
        "telemetry": {
          "enabled": true,
          "sampling_rate": 0.15
        }
      },
      "change_description": "Adjusted sampling rate and disabled debug logging"
    }
  ]
  
  // 当前配置版本
  let current_version = 3
  
  // 验证配置历史
  assert_eq(config_history.length(), 3)
  assert_eq(config_history[0]["version"], 1)
  assert_eq(config_history[1]["version"], 2)
  assert_eq(config_history[2]["version"], 3)
  assert_eq(current_version, 3)
  
  // 实现配置回滚功能
  let rollback_config = fn(history, from_version, to_version) {
    // 查找目标版本配置
    let target_version = history.filter(fn(entry) { 
      entry["version"] == to_version 
    })
    
    if target_version.length() == 0 {
      {
        "success": false,
        "error": "Target version not found: " + to_version.to_string(),
        "config": None
      }
    } else {
      let target_config = target_version[0]["config"]
      
      // 创建回滚记录
      let rollback_entry = {
        "version": history[history.length() - 1]["version"] + 1,
        "timestamp": 1640995600,
        "config": target_config,
        "change_description": "Rollback from version " + from_version.to_string() + 
                             " to version " + to_version.to_string(),
        "rollback_from": from_version,
        "rollback_to": to_version
      }
      
      // 添加到历史记录
      let updated_history = history.push(rollback_entry)
      
      {
        "success": true,
        "error": None,
        "config": Some(target_config),
        "new_version": rollback_entry["version"],
        "updated_history": updated_history
      }
    }
  }
  
  // 回滚到版本2
  let rollback_result = rollback_config(config_history, current_version, 2)
  
  // 验证回滚结果
  assert_eq(rollback_result["success"], true)
  match rollback_result["config"] {
    Some(config) => {
      assert_eq(config["application"]["version"], "1.1.0")
      assert_eq(config["application"]["log_level"], "debug")
      assert_eq(config["telemetry"]["sampling_rate"], 0.2)
    },
    None => assert_true(false)
  }
  assert_eq(rollback_result["new_version"], 4)
  assert_eq(rollback_result["updated_history"].length(), 4)
  
  // 验证回滚记录
  let rollback_entry = rollback_result["updated_history"][3]
  assert_eq(rollback_entry["version"], 4)
  assert_eq(rollback_entry["rollback_from"], 3)
  assert_eq(rollback_entry["rollback_to"], 2)
  assert_true(rollback_entry["change_description"].contains("Rollback from version 3 to version 2"))
  
  // 实现配置比较功能
  let compare_configs = fn(config1, config2) {
    let differences = []
    
    // 比较应用配置
    if config1["application"]["version"] != config2["application"]["version"] {
      differences.push({
        "path": "application.version",
        "old_value": config1["application"]["version"],
        "new_value": config2["application"]["version"]
      })
    }
    
    if config1["application"]["log_level"] != config2["application"]["log_level"] {
      differences.push({
        "path": "application.log_level",
        "old_value": config1["application"]["log_level"],
        "new_value": config2["application"]["log_level"]
      })
    }
    
    // 比较遥测配置
    if config1["telemetry"]["sampling_rate"] != config2["telemetry"]["sampling_rate"] {
      differences.push({
        "path": "telemetry.sampling_rate",
        "old_value": config1["telemetry"]["sampling_rate"],
        "new_value": config2["telemetry"]["sampling_rate"]
      })
    }
    
    differences
  }
  
  // 比较当前配置和回滚后的配置
  let current_config = config_history[2]["config"]
  let rolled_back_config = rollback_result["config"].unwrap()
  let config_differences = compare_configs(current_config, rolled_back_config)
  
  // 验证配置差异
  assert_eq(config_differences.length(), 2)
  
  // 验证版本差异
  let version_diff = config_differences.filter(fn(diff) { 
    diff["path"] == "application.version" 
  })[0]
  assert_eq(version_diff["old_value"], "1.2.0")
  assert_eq(version_diff["new_value"], "1.1.0")
  
  // 验证日志级别差异
  let log_level_diff = config_differences.filter(fn(diff) { 
    diff["path"] == "application.log_level" 
  })[0]
  assert_eq(log_level_diff["old_value"], "info")
  assert_eq(log_level_diff["new_value"], "debug")
  
  // 验证采样率差异
  let sampling_rate_diff = config_differences.filter(fn(diff) { 
    diff["path"] == "telemetry.sampling_rate" 
  })[0]
  assert_eq(sampling_rate_diff["old_value"], 0.15)
  assert_eq(sampling_rate_diff["new_value"], 0.2)
}

// 测试5: 配置模板和环境变量替换
test "配置模板和环境变量替换" {
  // 配置模板
  let config_template = {
    "application": {
      "name": "${APP_NAME:azimuth-telemetry}",
      "version": "${APP_VERSION:1.0.0}",
      "environment": "${ENVIRONMENT:development}",
      "debug": "${DEBUG:false}"
    },
    "server": {
      "host": "${SERVER_HOST:0.0.0.0}",
      "port": "${SERVER_PORT:8080}",
      "max_connections": "${MAX_CONNECTIONS:100}"
    },
    "database": {
      "host": "${DB_HOST:localhost}",
      "port": "${DB_PORT:5432}",
      "name": "${DB_NAME:azimuth_db}",
      "username": "${DB_USER:azimuth}",
      "password": "${DB_PASSWORD:password}"
    },
    "telemetry": {
      "enabled": "${TELEMETRY_ENABLED:true}",
      "sampling_rate": "${SAMPLING_RATE:0.1}",
      "endpoint": "${TELEMETRY_ENDPOINT:http://localhost:14268/api/traces}"
    }
  }
  
  // 环境变量
  let environment_variables = {
    "APP_NAME": "azimuth-telemetry-prod",
    "APP_VERSION": "1.2.3",
    "ENVIRONMENT": "production",
    "SERVER_HOST": "10.0.0.5",
    "SERVER_PORT": "9090",
    "DB_HOST": "db.production.example.com",
    "DB_NAME": "azimuth_prod_db",
    "TELEMETRY_ENABLED": "true",
    "SAMPLING_RATE": "0.2"
    // 注意：DEBUG, MAX_CONNECTIONS, DB_PORT, DB_USER, DB_PASSWORD, TELEMETRY_ENDPOINT 未设置
  }
  
  // 验证模板
  assert_eq(config_template["application"]["name"], "${APP_NAME:azimuth-telemetry}")
  assert_eq(config_template["server"]["host"], "${SERVER_HOST:0.0.0.0}")
  assert_eq(config_template["database"]["password"], "${DB_PASSWORD:password}")
  
  // 实现环境变量替换
  let substitute_variables = fn(template, env_vars) {
    let substitute_string = fn(str) {
      if str is String && str.starts_with("${") && str.ends_with("}") {
        // 提取变量名和默认值
        let without_prefix = str.substring(2, str.length() - 2) // 移除${和}
        let parts = without_prefix.split(":")
        
        if parts.length() >= 1 {
          let var_name = parts[0]
          let default_value = if parts.length() > 1 {
            parts.slice(1).join(":")
          } else {
            ""
          }
          
          // 查找环境变量
          match env_vars.get(var_name) {
            Some(value) => value,
            None => default_value
          }
        } else {
          str
        }
      } else {
        str
      }
    }
    
    // 递归替换对象中的字符串
    let substitute_object = fn(obj) {
      let mut result = {}
      
      for (key, value) in obj {
        match value {
          String(str) => {
            result = result.set(key, substitute_string(str))
          },
          Object(nested_obj) => {
            result = result.set(key, substitute_object(nested_obj))
          },
          _ => {
            result = result.set(key, value)
          }
        }
      }
      
      result
    }
    
    substitute_object(template)
  }
  
  // 应用环境变量替换
  let resolved_config = substitute_variables(config_template, environment_variables)
  
  // 验证替换结果
  assert_eq(resolved_config["application"]["name"], "azimuth-telemetry-prod")
  assert_eq(resolved_config["application"]["version"], "1.2.3")
  assert_eq(resolved_config["application"]["environment"], "production")
  assert_eq(resolved_config["application"]["debug"], "false") // 使用默认值
  
  assert_eq(resolved_config["server"]["host"], "10.0.0.5")
  assert_eq(resolved_config["server"]["port"], "9090")
  assert_eq(resolved_config["server"]["max_connections"], "100") // 使用默认值
  
  assert_eq(resolved_config["database"]["host"], "db.production.example.com")
  assert_eq(resolved_config["database"]["port"], "5432") // 使用默认值
  assert_eq(resolved_config["database"]["name"], "azimuth_prod_db")
  assert_eq(resolved_config["database"]["username"], "azimuth") // 使用默认值
  assert_eq(resolved_config["database"]["password"], "password") // 使用默认值
  
  assert_eq(resolved_config["telemetry"]["enabled"], "true")
  assert_eq(resolved_config["telemetry"]["sampling_rate"], "0.2")
  assert_eq(resolved_config["telemetry"]["endpoint"], "http://localhost:14268/api/traces") // 使用默认值
  
  // 实现类型转换
  let convert_types = fn(config) {
    let convert_value = fn(value) {
      // 尝试转换为整数
      match value.parse_int() {
        Some(int_val) => int_val,
        None => {
          // 尝试转换为浮点数
          match value.parse_float() {
            Some(float_val) => float_val,
            None => {
              // 尝试转换为布尔值
              if value == "true" {
                true
              } else if value == "false" {
                false
              } else {
                value // 保持为字符串
              }
            }
          }
        }
      }
    }
    
    let convert_object = fn(obj) {
      let mut result = {}
      
      for (key, value) in obj {
        match value {
          String(str) => {
            result = result.set(key, convert_value(str))
          },
          Object(nested_obj) => {
            result = result.set(key, convert_object(nested_obj))
          },
          _ => {
            result = result.set(key, value)
          }
        }
      }
      
      result
    }
    
    convert_object(config)
  }
  
  // 应用类型转换
  let typed_config = convert_types(resolved_config)
  
  // 验证类型转换
  assert_eq(typed_config["application"]["name"], "azimuth-telemetry-prod") // 字符串
  assert_eq(typed_config["application"]["debug"], false) // 布尔值
  assert_eq(typed_config["server"]["port"], 9090) // 整数
  assert_eq(typed_config["server"]["max_connections"], 100) // 整数
  assert_eq(typed_config["database"]["port"], 5432) // 整数
  assert_eq(typed_config["telemetry"]["enabled"], true) // 布尔值
  assert_eq(typed_config["telemetry"]["sampling_rate"], 0.2) // 浮点数
}