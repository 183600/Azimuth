// Azimuth Configuration Validation and Defaults Tests
// 配置验证和默认值测试用例 - 专注于配置管理和验证功能

// Test 1: 基本配置加载和验证
test "basic configuration loading and validation" {
  let config_manager = ConfigManager::new()
  
  // 定义配置模式
  let config_schema = {
    "server": {
      "host": { "type": "string", "required": true, "default": "localhost" },
      "port": { "type": "integer", "required": true, "min": 1, "max": 65535, "default": 8080 },
      "ssl_enabled": { "type": "boolean", "required": false, "default": false }
    },
    "database": {
      "url": { "type": "string", "required": true, "pattern": "^[a-zA-Z]+://.+" },
      "timeout": { "type": "integer", "required": false, "min": 1, "default": 30 },
      "pool_size": { "type": "integer", "required": false, "min": 1, "max": 100, "default": 10 }
    },
    "logging": {
      "level": { "type": "string", "required": false, "enum": ["debug", "info", "warn", "error"], "default": "info" },
      "file": { "type": "string", "required": false, "default": "azimuth.log" }
    }
  }
  
  // 注册配置模式
  let schema_result = ConfigManager::register_schema(config_manager, "azimuth", config_schema)
  match schema_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 测试有效配置
  let valid_config = {
    "server": {
      "host": "example.com",
      "port": 9000,
      "ssl_enabled": true
    },
    "database": {
      "url": "postgresql://localhost:5432/azimuth",
      "timeout": 60,
      "pool_size": 20
    },
    "logging": {
      "level": "debug",
      "file": "custom.log"
    }
  }
  
  let validation_result = ConfigManager::validate_config(config_manager, "azimuth", valid_config)
  match validation_result {
    Ok(_) => assert_true(true)
    Err(errors) => {
      // 打印验证错误以便调试
      for error in errors {
        assert_true(false)
      }
    }
  }
  
  // 测试使用默认值
  let partial_config = {
    "server": {
      "host": "partial.example.com"
      // 缺少port和ssl_enabled，应该使用默认值
    },
    "database": {
      "url": "mysql://localhost:3306/azimuth"
      // 缺少timeout和pool_size，应该使用默认值
    }
    // 缺少整个logging配置，应该使用所有默认值
  }
  
  let merge_result = ConfigManager::merge_with_defaults(config_manager, "azimuth", partial_config)
  match merge_result {
    Ok(merged_config) => {
      assert_eq(merged_config.server.host, "partial.example.com")
      assert_eq(merged_config.server.port, 8080) // 默认值
      assert_eq(merged_config.server.ssl_enabled, false) // 默认值
      assert_eq(merged_config.database.url, "mysql://localhost:3306/azimuth")
      assert_eq(merged_config.database.timeout, 30) // 默认值
      assert_eq(merged_config.database.pool_size, 10) // 默认值
      assert_eq(merged_config.logging.level, "info") // 默认值
      assert_eq(merged_config.logging.file, "azimuth.log") // 默认值
    }
    Err(_) => assert_true(false)
  }
  
  // 测试无效配置
  let invalid_config = {
    "server": {
      "host": "example.com",
      "port": 70000, // 超出范围
      "ssl_enabled": "true" // 错误类型
    },
    "database": {
      "url": "invalid_url", // 不匹配模式
      "timeout": -5, // 小于最小值
      "pool_size": 150 // 超出最大值
    },
    "logging": {
      "level": "verbose" // 不在枚举值中
    }
  }
  
  let invalid_result = ConfigManager::validate_config(config_manager, "azimuth", invalid_config)
  match invalid_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(errors) => {
      assert_true(errors.length() >= 5) // 至少5个错误
      
      // 验证特定错误
      let has_port_error = errors.any(fn(error) { String::contains(error, "port") })
      let has_ssl_error = errors.any(fn(error) { String::contains(error, "ssl_enabled") })
      let has_url_error = errors.any(fn(error) { String::contains(error, "url") })
      let has_timeout_error = errors.any(fn(error) { String::contains(error, "timeout") })
      let has_level_error = errors.any(fn(error) { String::contains(error, "level") })
      
      assert_true(has_port_error)
      assert_true(has_ssl_error)
      assert_true(has_url_error)
      assert_true(has_timeout_error)
      assert_true(has_level_error)
    }
  }
}

// Test 2: 环境特定配置管理
test "environment-specific configuration management" {
  let env_config_manager = EnvConfigManager::new()
  
  // 定义基础配置
  let base_config = {
    "app_name": "Azimuth",
    "version": "1.0.0",
    "debug": false,
    "server": {
      "host": "localhost",
      "port": 8080
    },
    "database": {
      "host": "localhost",
      "port": 5432,
      "name": "azimuth_dev"
    }
  }
  
  // 定义环境特定配置
  let dev_config = {
    "debug": true,
    "server": {
      "port": 8081
    },
    "database": {
      "name": "azimuth_dev"
    },
    "logging": {
      "level": "debug"
    }
  }
  
  let prod_config = {
    "debug": false,
    "server": {
      "host": "0.0.0.0",
      "port": 80
    },
    "database": {
      "host": "prod-db.example.com",
      "name": "azimuth_prod"
    },
    "logging": {
      "level": "warn"
    }
  }
  
  // 注册配置
  EnvConfigManager::set_base_config(env_config_manager, base_config)
  EnvConfigManager::set_environment_config(env_config_manager, "development", dev_config)
  EnvConfigManager::set_environment_config(env_config_manager, "production", prod_config)
  
  // 测试开发环境配置
  let dev_result = EnvConfigManager::get_config_for_environment(env_config_manager, "development")
  match dev_result {
    Ok(config) => {
      assert_eq(config.app_name, "Azimuth") // 来自基础配置
      assert_eq(config.version, "1.0.0") // 来自基础配置
      assert_true(config.debug) // 来自开发环境配置
      assert_eq(config.server.host, "localhost") // 来自基础配置
      assert_eq(config.server.port, 8081) // 来自开发环境配置
      assert_eq(config.database.host, "localhost") // 来自基础配置
      assert_eq(config.database.name, "azimuth_dev") // 来自开发环境配置
      assert_eq(config.logging.level, "debug") // 来自开发环境配置
    }
    Err(_) => assert_true(false)
  }
  
  // 测试生产环境配置
  let prod_result = EnvConfigManager::get_config_for_environment(env_config_manager, "production")
  match prod_result {
    Ok(config) => {
      assert_eq(config.app_name, "Azimuth") // 来自基础配置
      assert_eq(config.version, "1.0.0") // 来自基础配置
      assert_false(config.debug) // 来自生产环境配置
      assert_eq(config.server.host, "0.0.0.0") // 来自生产环境配置
      assert_eq(config.server.port, 80) // 来自生产环境配置
      assert_eq(config.database.host, "prod-db.example.com") // 来自生产环境配置
      assert_eq(config.database.name, "azimuth_prod") // 来自生产环境配置
      assert_eq(config.logging.level, "warn") // 来自生产环境配置
    }
    Err(_) => assert_true(false)
  }
  
  // 测试环境配置验证
  let env_schema = {
    "debug": { "type": "boolean", "required": true },
    "server": {
      "host": { "type": "string", "required": true },
      "port": { "type": "integer", "required": true, "min": 1, "max": 65535 }
    },
    "database": {
      "host": { "type": "string", "required": true },
      "name": { "type": "string", "required": true }
    }
  }
  
  EnvConfigManager::set_validation_schema(env_config_manager, env_schema)
  
  // 验证开发环境配置
  let dev_validation = EnvConfigManager::validate_environment_config(env_config_manager, "development")
  match dev_validation {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证生产环境配置
  let prod_validation = EnvConfigManager::validate_environment_config(env_config_manager, "production")
  match prod_validation {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 测试无效环境配置
  let invalid_env_config = {
    "debug": "not_boolean",
    "server": {
      "host": "",
      "port": 70000
    },
    "database": {
      "host": "valid_host",
      // 缺少name字段
    }
  }
  
  EnvConfigManager::set_environment_config(env_config_manager, "invalid", invalid_env_config)
  let invalid_validation = EnvConfigManager::validate_environment_config(env_config_manager, "invalid")
  match invalid_validation {
    Ok(_) => assert_true(false) // 应该失败
    Err(errors) => {
      assert_true(errors.length() >= 3) // 至少3个错误
    }
  }
}

// Test 3: 配置热重载和动态更新
test "configuration hot reload and dynamic updates" {
  let hot_reload_manager = HotReloadConfigManager::new()
  
  // 初始配置
  let initial_config = {
    "refresh_interval": 30,
    "max_connections": 100,
    "feature_flags": {
      "new_ui": false,
      "advanced_analytics": false
    }
  }
  
  // 设置初始配置
  let init_result = HotReloadConfigManager::set_config(hot_reload_manager, initial_config)
  match init_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证初始配置
  let current_config = HotReloadConfigManager::get_current_config(hot_reload_manager)
  assert_eq(current_config.refresh_interval, 30)
  assert_eq(current_config.max_connections, 100)
  assert_false(current_config.feature_flags.new_ui)
  assert_false(current_config.feature_flags.advanced_analytics)
  
  // 注册配置变更监听器
  let mut change_detected = false
  let mut old_config = {}
  let mut new_config = {}
  
  HotReloadConfigManager::add_change_listener(hot_reload_manager, fn(old, new) {
    change_detected = true
    old_config = old
    new_config = new
  })
  
  // 测试部分配置更新
  let partial_update = {
    "refresh_interval": 60,
    "feature_flags": {
      "new_ui": true
    }
  }
  
  let update_result = HotReloadConfigManager::update_config(hot_reload_manager, partial_update)
  match update_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证变更监听器被调用
  assert_true(change_detected)
  assert_eq(old_config.refresh_interval, 30)
  assert_eq(new_config.refresh_interval, 60)
  assert_false(old_config.feature_flags.new_ui)
  assert_true(new_config.feature_flags.new_ui)
  
  // 验证配置已更新
  let updated_config = HotReloadConfigManager::get_current_config(hot_reload_manager)
  assert_eq(updated_config.refresh_interval, 60) // 已更新
  assert_eq(updated_config.max_connections, 100) // 保持不变
  assert_true(updated_config.feature_flags.new_ui) // 已更新
  assert_false(updated_config.feature_flags.advanced_analytics) // 保持不变
  
  // 测试配置回滚
  let rollback_result = HotReloadConfigManager::rollback_config(hot_reload_manager, 1) // 回滚到上一个版本
  match rollback_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证回滚结果
  let rolled_back_config = HotReloadConfigManager::get_current_config(hot_reload_manager)
  assert_eq(rolled_back_config.refresh_interval, 30) // 已回滚
  assert_eq(rolled_back_config.max_connections, 100)
  assert_false(rolled_back_config.feature_flags.new_ui) // 已回滚
  assert_false(rolled_back_config.feature_flags.advanced_analytics)
  
  // 测试配置历史
  let history = HotReloadConfigManager::get_config_history(hot_reload_manager)
  assert_true(history.length() >= 2) // 至少有初始配置和更新后的配置
  
  // 验证历史记录
  let latest_version = history[history.length() - 1]
  assert_eq(latest_version.config.refresh_interval, 30)
  assert_eq(latest_version.version, 1) // 回滚后的版本
  
  // 测试配置验证和更新
  let validation_rules = {
    "refresh_interval": { "type": "integer", "min": 1, "max": 300 },
    "max_connections": { "type": "integer", "min": 10, "max": 1000 },
    "feature_flags": {
      "new_ui": { "type": "boolean" },
      "advanced_analytics": { "type": "boolean" }
    }
  }
  
  HotReloadConfigManager::set_validation_rules(hot_reload_manager, validation_rules)
  
  // 测试有效更新
  let valid_update = {
    "refresh_interval": 120,
    "max_connections": 200
  }
  
  let valid_update_result = HotReloadConfigManager::validate_and_update(hot_reload_manager, valid_update)
  match valid_update_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 测试无效更新
  let invalid_update = {
    "refresh_interval": 500, // 超出最大值
    "max_connections": 5 // 小于最小值
  }
  
  let invalid_update_result = HotReloadConfigManager::validate_and_update(hot_reload_manager, invalid_update)
  match invalid_update_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(errors) => {
      assert_true(errors.length() >= 2)
    }
  }
  
  // 验证无效更新后配置未改变
  let unchanged_config = HotReloadConfigManager::get_current_config(hot_reload_manager)
  assert_eq(unchanged_config.refresh_interval, 120) // 上一次有效更新
  assert_eq(unchanged_config.max_connections, 200)
}

// Test 4: 配置依赖和引用解析
test "configuration dependencies and reference resolution" {
  let dependency_manager = ConfigDependencyManager::new()
  
  // 定义配置模板
  let config_template = {
    "app": {
      "name": "Azimuth",
      "version": "1.0.0",
      "base_url": "${protocol}://${host}:${port}",
      "assets_path": "${base_path}/assets",
      "log_file": "${log_dir}/${app.name}.log"
    },
    "server": {
      "protocol": "https",
      "host": "localhost",
      "port": 8080
    },
    "paths": {
      "base_path": "/opt/azimuth",
      "log_dir": "${base_path}/logs",
      "data_dir": "${base_path}/data"
    },
    "database": {
      "url": "${server.protocol}://${server.host}:5432/${db_name}",
      "connection_pool": "${server.port * 2}" // 表达式引用
    },
    "computed": {
      "total_workers": "${server.port / 100}",
      "cache_size": "${server.port * 10}",
      "app_full_name": "${app.name} v${app.version}"
    }
  }
  
  // 设置配置变量
  let variables = {
    "db_name": "azimuth_prod"
  }
  
  // 解析配置引用
  let resolve_result = ConfigDependencyManager::resolve_references(dependency_manager, config_template, variables)
  match resolve_result {
    Ok(resolved_config) => {
      // 验证简单引用解析
      assert_eq(resolved_config.app.base_url, "https://localhost:8080")
      assert_eq(resolved_config.app.assets_path, "/opt/azimuth/assets")
      assert_eq(resolved_config.app.log_file, "/opt/azimuth/logs/Azimuth.log")
      assert_eq(resolved_config.paths.log_dir, "/opt/azimuth/logs")
      assert_eq(resolved_config.paths.data_dir, "/opt/azimuth/data")
      
      // 验证跨配置节引用
      assert_eq(resolved_config.database.url, "https://localhost:5432/azimuth_prod")
      
      // 验证表达式计算
      assert_eq(resolved_config.database.connection_pool, 16160) // 8080 * 2
      assert_eq(resolved_config.computed.total_workers, 80) // 8080 / 100
      assert_eq(resolved_config.computed.cache_size, 80800) // 8080 * 10
      assert_eq(resolved_config.computed.app_full_name, "Azimuth v1.0.0")
    }
    Err(_) => assert_true(false)
  }
  
  // 测试循环依赖检测
  let circular_config = {
    "a": "${b}",
    "b": "${c}",
    "c": "${a}" // 循环依赖
  }
  
  let circular_result = ConfigDependencyManager::resolve_references(dependency_manager, circular_config, {})
  match circular_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(error) => {
      assert_true(String::contains(error, "circular"))
    }
  }
  
  // 测试缺失引用检测
  let missing_ref_config = {
    "present": "value",
    "missing_ref": "${nonexistent}" // 引用不存在的变量
  }
  
  let missing_result = ConfigDependencyManager::resolve_references(dependency_manager, missing_ref_config, {})
  match missing_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(error) => {
      assert_true(String::contains(error, "nonexistent"))
    }
  }
  
  // 测试配置依赖图
  let dependency_graph = ConfigDependencyManager::build_dependency_graph(dependency_manager, config_template)
  assert_true(dependency_graph.nodes.length() > 0)
  assert_true(dependency_graph.edges.length() > 0)
  
  // 验证依赖关系
  let base_url_deps = dependency_graph.edges.filter(fn(edge) { 
    edge.to == "app.base_url" 
  })
  assert_true(base_url_deps.length() == 3) // protocol, host, port
  
  // 测试拓扑排序
  let sorted_nodes = ConfigDependencyManager::topological_sort(dependency_manager, dependency_graph)
  assert_true(sorted_nodes.length() == dependency_graph.nodes.length())
  
  // 验证排序正确性：被依赖的节点应该排在前面
  let protocol_index = sorted_nodes.index_of("server.protocol")
  let base_url_index = sorted_nodes.index_of("app.base_url")
  assert_true(protocol_index < base_url_index)
}

// Test 5: 配置加密和安全存储
test "configuration encryption and secure storage" {
  let secure_config_manager = SecureConfigManager::new()
  
  // 生成加密密钥
  let key_result = SecureConfigManager::generate_encryption_key(secure_config_manager)
  match key_result {
    Ok(key) => {
      // 敏感配置数据
      let sensitive_config = {
        "database": {
          "password": "super_secret_password",
          "api_key": "sk-1234567890abcdef",
          "connection_string": "Server=db.example.com;Database=azimuth;User=admin;Password=super_secret_password;"
        },
        "services": {
          "auth_service": {
            "client_secret": "auth_client_secret_12345",
            "private_key": "-----BEGIN RSA PRIVATE KEY-----\nMIIEpAIBAAKCAQEA...\n-----END RSA PRIVATE KEY-----"
          },
          "payment_service": {
            "merchant_id": "merchant_12345",
            "api_secret": "payment_secret_67890"
          }
        },
        "licenses": {
          "product_key": "PRODUCT-KEY-12345-ABCDE-67890-FGHIJ"
        }
      }
      
      // 加密配置
      let encrypt_result = SecureConfigManager::encrypt_config(secure_config_manager, sensitive_config, key)
      match encrypt_result {
        Ok(encrypted_config) => {
          // 验证配置已加密（原始敏感数据不应出现在加密配置中）
          let config_str = JSON::stringify(encrypted_config)
          assert_false(String::contains(config_str, "super_secret_password"))
          assert_false(String::contains(config_str, "sk-1234567890abcdef"))
          assert_false(String::contains(config_str, "auth_client_secret_12345"))
          
          // 解密配置
          let decrypt_result = SecureConfigManager::decrypt_config(secure_config_manager, encrypted_config, key)
          match decrypt_result {
            Ok(decrypted_config) => {
              // 验证解密后的配置与原始配置相同
              assert_eq(decrypted_config.database.password, "super_secret_password")
              assert_eq(decrypted_config.database.api_key, "sk-1234567890abcdef")
              assert_eq(decrypted_config.services.auth_service.client_secret, "auth_client_secret_12345")
              assert_eq(decrypted_config.services.payment_service.merchant_id, "merchant_12345")
              assert_eq(decrypted_config.licenses.product_key, "PRODUCT-KEY-12345-ABCDE-67890-FGHIJ")
            }
            Err(_) => assert_true(false)
          }
          
          // 测试使用错误密钥解密
          let wrong_key = SecureConfigManager::generate_encryption_key(secure_config_manager)
          match wrong_key {
            Ok(wrong_encryption_key) => {
              let wrong_decrypt_result = SecureConfigManager::decrypt_config(secure_config_manager, encrypted_config, wrong_encryption_key)
              match wrong_decrypt_result {
                Ok(_) => assert_true(false) // 应该失败
                Err(_) => assert_true(true)
              }
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
      
      // 测试安全存储
      let storage_result = SecureConfigManager::store_secure_config(secure_config_manager, "production", encrypted_config)
      match storage_result {
        Ok(storage_id) => {
          // 从安全存储中检索
          let retrieve_result = SecureConfigManager::retrieve_secure_config(secure_config_manager, storage_id)
          match retrieve_result {
            Ok(retrieved_config) => {
              // 解密检索的配置
              let decrypt_retrieved = SecureConfigManager::decrypt_config(secure_config_manager, retrieved_config, key)
              match decrypt_retrieved {
                Ok(retrieved_decrypted) => {
                  // 验证检索的配置与原始配置相同
                  assert_eq(retrieved_decrypted.database.password, "super_secret_password")
                }
                Err(_) => assert_true(false)
              }
            }
            Err(_) => assert_true(false)
          }
          
          // 测试配置访问控制
          SecureConfigManager::set_access_policy(secure_config_manager, storage_id, {
            "allowed_users": ["admin", "config_manager"],
            "require_authentication": true,
            "audit_access": true
          })
          
          // 测试访问验证
          let admin_access = SecureConfigManager::check_access(secure_config_manager, storage_id, "admin")
          let user_access = SecureConfigManager::check_access(secure_config_manager, storage_id, "regular_user")
          
          assert_true(admin_access)
          assert_false(user_access)
          
          // 测试配置轮换
          let rotation_result = SecureConfigManager::rotate_encryption(secure_config_manager, storage_id, key)
          match rotation_result {
            Ok(new_storage_id) => {
              // 使用新密钥解密
              let new_key = SecureConfigManager::generate_encryption_key(secure_config_manager)
              match new_key {
                Ok(rotated_key) => {
                  let new_retrieve = SecureConfigManager::retrieve_secure_config(secure_config_manager, new_storage_id)
                  match new_retrieve {
                    Ok(new_config) => {
                      let new_decrypt = SecureConfigManager::decrypt_config(secure_config_manager, new_config, rotated_key)
                      match new_decrypt {
                        Ok(rotated_decrypted) => {
                          assert_eq(rotated_decrypted.database.password, "super_secret_password")
                        }
                        Err(_) => assert_true(false)
                      }
                    }
                    Err(_) => assert_true(false)
                  }
                }
                Err(_) => assert_true(false)
              }
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 6: 配置模板和继承
test "configuration templates and inheritance" {
  let template_manager = ConfigTemplateManager::new()
  
  // 定义基础模板
  let base_template = {
    "name": "base",
    "description": "基础配置模板",
    "config": {
      "app": {
        "name": "Azimuth",
        "version": "1.0.0",
        "debug": false
      },
      "server": {
        "host": "localhost",
        "port": 8080,
        "workers": 4
      },
      "logging": {
        "level": "info",
        "format": "json"
      }
    }
  }
  
  // 定义服务模板
  let service_template = {
    "name": "service",
    "description": "服务配置模板",
    "parent": "base",
    "config": {
      "server": {
        "port": 9000,
        "ssl": true
      },
      "discovery": {
        "enabled": true,
        "service_type": "web"
      }
    }
  }
  
  // 定义数据库模板
  let database_template = {
    "name": "database",
    "description": "数据库配置模板",
    "parent": "base",
    "config": {
      "database": {
        "type": "postgresql",
        "host": "localhost",
        "port": 5432,
        "name": "azimuth"
      },
      "server": {
        "port": 8081 // 覆盖基础模板的端口
      }
    }
  }
  
  // 注册模板
  let templates = [base_template, service_template, database_template]
  for template in templates {
    let register_result = ConfigTemplateManager::register_template(template_manager, template)
    match register_result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  // 测试模板继承
  let service_config_result = ConfigTemplateManager::build_from_template(template_manager, "service")
  match service_config_result {
    Ok(service_config) => {
      // 验证继承自基础模板
      assert_eq(service_config.app.name, "Azimuth")
      assert_eq(service_config.app.version, "1.0.0")
      assert_eq(service_config.app.debug, false)
      assert_eq(service_config.logging.level, "info")
      
      // 验证服务模板的覆盖
      assert_eq(service_config.server.host, "localhost") // 来自基础模板
      assert_eq(service_config.server.port, 9000) // 来自服务模板，覆盖基础模板
      assert_eq(service_config.server.workers, 4) // 来自基础模板
      assert_eq(service_config.server.ssl, true) // 来自服务模板
      
      // 验证服务模板的新增
      assert_true(service_config.discovery.enabled)
      assert_eq(service_config.discovery.service_type, "web")
    }
    Err(_) => assert_true(false)
  }
  
  // 测试数据库模板继承
  let database_config_result = ConfigTemplateManager::build_from_template(template_manager, "database")
  match database_config_result {
    Ok(database_config) => {
      // 验证继承自基础模板
      assert_eq(database_config.app.name, "Azimuth")
      assert_eq(database_config.server.host, "localhost")
      
      // 验证数据库模板的覆盖
      assert_eq(database_config.server.port, 8081) // 来自数据库模板
      
      // 验证数据库模板的新增
      assert_eq(database_config.database.type, "postgresql")
      assert_eq(database_config.database.host, "localhost")
      assert_eq(database_config.database.port, 5432)
      assert_eq(database_config.database.name, "azimuth")
    }
    Err(_) => assert_true(false)
  }
  
  // 测试多层继承
  let web_service_template = {
    "name": "web_service",
    "description": "Web服务配置模板",
    "parent": "service",
    "config": {
      "server": {
        "port": 80,
        "routes": ["/api", "/health"]
      },
      "middleware": {
        "cors": true,
        "rate_limiting": true
      }
    }
  }
  
  let web_register_result = ConfigTemplateManager::register_template(template_manager, web_service_template)
  match web_register_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  let web_config_result = ConfigTemplateManager::build_from_template(template_manager, "web_service")
  match web_config_result {
    Ok(web_config) => {
      // 验证多层继承
      assert_eq(web_config.app.name, "Azimuth") // 来自base模板
      assert_eq(web_config.server.ssl, true) // 来自service模板
      assert_eq(web_config.discovery.enabled, true) // 来自service模板
      assert_eq(web_config.server.port, 80) // 来自web_service模板，覆盖service模板
      
      // 验证web_service模板的新增
      assert_eq(web_config.server.routes, ["/api", "/health"])
      assert_true(web_config.middleware.cors)
      assert_true(web_config.middleware.rate_limiting)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试模板参数化
  let parameterized_template = {
    "name": "parameterized",
    "description": "参数化配置模板",
    "parent": "base",
    "parameters": {
      "service_name": { "type": "string", "required": true },
      "service_port": { "type": "integer", "default": 8080 },
      "enable_ssl": { "type": "boolean", "default": false }
    },
    "config": {
      "app": {
        "name": "${service_name}"
      },
      "server": {
        "port": "${service_port}",
        "ssl": "${enable_ssl}"
      }
    }
  }
  
  let param_register_result = ConfigTemplateManager::register_template(template_manager, parameterized_template)
  match param_register_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 使用参数构建配置
  let parameters = {
    "service_name": "CustomService",
    "service_port": 9000,
    "enable_ssl": true
  }
  
  let param_config_result = ConfigTemplateManager::build_from_template_with_params(template_manager, "parameterized", parameters)
  match param_config_result {
    Ok(param_config) => {
      assert_eq(param_config.app.name, "CustomService")
      assert_eq(param_config.server.port, 9000)
      assert_true(param_config.server.ssl)
      
      // 验证继承自基础模板的其他配置
      assert_eq(param_config.app.version, "1.0.0")
      assert_eq(param_config.logging.level, "info")
    }
    Err(_) => assert_true(false)
  }
  
  // 测试模板验证
  let template_schema = {
    "app": {
      "name": { "type": "string", "required": true },
      "version": { "type": "string", "required": true }
    },
    "server": {
      "host": { "type": "string", "required": true },
      "port": { "type": "integer", "required": true, "min": 1, "max": 65535 }
    }
  }
  
  ConfigTemplateManager::set_validation_schema(template_manager, template_schema)
  
  // 验证有效模板
  let valid_template_result = ConfigTemplateManager::validate_template(template_manager, "base")
  match valid_template_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 测试模板依赖图
  let dependency_graph = ConfigTemplateManager::build_template_dependency_graph(template_manager)
  assert_true(dependency_graph.nodes.length() >= 4) // base, service, database, web_service
  
  // 验证依赖关系
  let service_deps = dependency_graph.get_dependencies("service")
  assert_true(service_deps.contains("base"))
  
  let web_service_deps = dependency_graph.get_dependencies("web_service")
  assert_true(web_service_deps.contains("service"))
  assert_true(web_service_deps.contains("base"))
}

// Test 7: 配置导入导出和迁移
test "configuration import, export and migration" {
  let migration_manager = ConfigMigrationManager::new()
  
  // 定义源配置格式（版本1）
  let v1_config = {
    "app_name": "Azimuth",
    "app_version": "1.0.0",
    "server_host": "localhost",
    "server_port": 8080,
    "database_url": "postgresql://localhost:5432/azimuth",
    "log_level": "info",
    "debug_mode": false
  }
  
  // 定义目标配置格式（版本2）
  let v2_config_template = {
    "application": {
      "name": "",
      "version": "",
      "environment": "development"
    },
    "server": {
      "host": "",
      "port": 0,
      "ssl": false
    },
    "database": {
      "connection": {
        "url": "",
        "timeout": 30
      },
      "pool": {
        "size": 10,
        "max_overflow": 20
      }
    },
    "logging": {
      "level": "",
      "format": "json",
      "outputs": ["console"]
    },
    "features": {
      "debug": false,
      "metrics": true
    }
  }
  
  // 定义迁移规则
  let migration_rules = [
    {
      "from": "app_name",
      "to": "application.name",
      "transform": "identity"
    },
    {
      "from": "app_version",
      "to": "application.version",
      "transform": "identity"
    },
    {
      "from": "server_host",
      "to": "server.host",
      "transform": "identity"
    },
    {
      "from": "server_port",
      "to": "server.port",
      "transform": "identity"
    },
    {
      "from": "database_url",
      "to": "database.connection.url",
      "transform": "identity"
    },
    {
      "from": "log_level",
      "to": "logging.level",
      "transform": "identity"
    },
    {
      "from": "debug_mode",
      "to": "features.debug",
      "transform": "identity"
    },
    {
      "to": "features.metrics",
      "transform": "constant",
      "value": true
    },
    {
      "to": "database.pool.size",
      "transform": "constant",
      "value": 10
    },
    {
      "to": "database.pool.max_overflow",
      "transform": "constant",
      "value": 20
    },
    {
      "to": "database.connection.timeout",
      "transform": "constant",
      "value": 30
    },
    {
      "to": "logging.format",
      "transform": "constant",
      "value": "json"
    },
    {
      "to": "logging.outputs",
      "transform": "constant",
      "value": ["console"]
    },
    {
      "to": "server.ssl",
      "transform": "constant",
      "value": false
    },
    {
      "to": "application.environment",
      "transform": "constant",
      "value": "development"
    }
  ]
  
  // 注册迁移
  let migration_result = ConfigMigrationManager::register_migration(migration_manager, "1.0", "2.0", migration_rules)
  match migration_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 执行迁移
  let migrate_result = ConfigMigrationManager::migrate_config(migration_manager, v1_config, "1.0", "2.0")
  match migrate_result {
    Ok(migrated_config) => {
      // 验证迁移结果
      assert_eq(migrated_config.application.name, "Azimuth")
      assert_eq(migrated_config.application.version, "1.0.0")
      assert_eq(migrated_config.application.environment, "development")
      assert_eq(migrated_config.server.host, "localhost")
      assert_eq(migrated_config.server.port, 8080)
      assert_false(migrated_config.server.ssl)
      assert_eq(migrated_config.database.connection.url, "postgresql://localhost:5432/azimuth")
      assert_eq(migrated_config.database.connection.timeout, 30)
      assert_eq(migrated_config.database.pool.size, 10)
      assert_eq(migrated_config.database.pool.max_overflow, 20)
      assert_eq(migrated_config.logging.level, "info")
      assert_eq(migrated_config.logging.format, "json")
      assert_eq(migrated_config.logging.outputs, ["console"])
      assert_false(migrated_config.features.debug)
      assert_true(migrated_config.features.metrics)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试配置导出
  let export_formats = ["json", "yaml", "toml", "xml"]
  for format in export_formats {
    let export_result = ConfigMigrationManager::export_config(migration_manager, migrated_config, format)
    match export_result {
    Ok(exported_data) => {
        assert_true(exported_data.length() > 0)
        
        // 测试导入
        let import_result = ConfigMigrationManager::import_config(migration_manager, exported_data, format)
        match import_result {
          Ok(imported_config) => {
            // 验证导入的配置与原始配置相同
            assert_eq(imported_config.application.name, migrated_config.application.name)
            assert_eq(imported_config.application.version, migrated_config.application.version)
            assert_eq(imported_config.server.host, migrated_config.server.host)
            assert_eq(imported_config.server.port, migrated_config.server.port)
          }
          Err(_) => assert_true(false)
        }
      }
      Err(_) => assert_true(false)
    }
  }
  
  // 测试配置验证和迁移
  let v2_validation_rules = {
    "application": {
      "name": { "type": "string", "required": true },
      "version": { "type": "string", "required": true }
    },
    "server": {
      "host": { "type": "string", "required": true },
      "port": { "type": "integer", "required": true, "min": 1, "max": 65535 }
    }
  }
  
  ConfigMigrationManager::set_validation_schema(migration_manager, "2.0", v2_validation_rules)
  
  // 测试有效配置迁移
  let valid_migration_result = ConfigMigrationManager::validate_and_migrate(migration_manager, v1_config, "1.0", "2.0")
  match valid_migration_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 测试无效配置迁移
  let invalid_v1_config = { ...v1_config, "server_port": -1 } // 无效端口
  let invalid_migration_result = ConfigMigrationManager::validate_and_migrate(migration_manager, invalid_v1_config, "1.0", "2.0")
  match invalid_migration_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(errors) => {
      assert_true(errors.length() >= 1)
    }
  }
  
  // 测试迁移历史
  let migration_history = ConfigMigrationManager::get_migration_history(migration_manager)
  assert_true(migration_history.length() >= 1)
  
  let latest_migration = migration_history[migration_history.length() - 1]
  assert_eq(latest_migration.from_version, "1.0")
  assert_eq(latest_migration.to_version, "2.0")
  assert_true(latest_migration.timestamp > 0)
  
  // 测试回滚迁移
  let rollback_result = ConfigMigrationManager::rollback_migration(migration_manager, migrated_config, "2.0", "1.0")
  match rollback_result {
    Ok(rolled_back_config) => {
      // 验证回滚结果
      assert_eq(rolled_back_config.app_name, "Azimuth")
      assert_eq(rolled_back_config.app_version, "1.0.0")
      assert_eq(rolled_back_config.server_host, "localhost")
      assert_eq(rolled_back_config.server_port, 8080)
    }
    Err(_) => assert_true(false)
  }
}

// Test 8: 配置验证规则和自定义验证器
test "configuration validation rules and custom validators" {
  let validation_manager = ValidationManager::new()
  
  // 定义内置验证规则
  let built_in_rules = {
    "server.host": {
      "type": "string",
      "required": true,
      "pattern": "^[a-zA-Z0-9.-]+$"
    },
    "server.port": {
      "type": "integer",
      "required": true,
      "min": 1,
      "max": 65535
    },
    "database.url": {
      "type": "string",
      "required": true,
      "custom": "database_url"
    },
    "auth.tokens.jwt_secret": {
      "type": "string",
      "required": true,
      "min_length": 32
    },
    "features.*": {
      "type": "boolean",
      "required": false
    }
  }
  
  // 注册内置验证规则
  ValidationManager::register_rules(validation_manager, built_in_rules)
  
  // 注册自定义验证器
  ValidationManager::register_custom_validator(validation_manager, "database_url", fn(value) {
    let url_pattern = "^[a-zA-Z]+://[^:]+:[0-9]+/.+$"
    if Regex::matches(url_pattern, value) {
      Ok(())
    } else {
      Err("Invalid database URL format")
    }
  })
  
  // 注册条件验证器
  ValidationManager::register_conditional_validator(validation_manager, "ssl_required", fn(config) {
    config.server.port == 443 || config.server.ssl_enabled
  }, "SSL is required when using port 443")
  
  // 注册依赖验证器
  ValidationManager::register_dependency_validator(validation_manager, "cache_dependency", fn(config) {
    if config.features.cache_enabled {
      return config.cache.type != "" && config.cache.host != ""
    }
    return true
  }, "Cache configuration is required when cache is enabled")
  
  // 测试有效配置
  let valid_config = {
    "server": {
      "host": "example.com",
      "port": 8080,
      "ssl_enabled": false
    },
    "database": {
      "url": "postgresql://localhost:5432/azimuth"
    },
    "auth": {
      "tokens": {
        "jwt_secret": "this_is_a_very_long_jwt_secret_key_32_chars"
      }
    },
    "features": {
      "analytics": true,
      "debug_mode": false
    },
    "cache": {
      "type": "redis",
      "host": "localhost"
    }
  }
  
  let valid_result = ValidationManager::validate(validation_manager, valid_config)
  match valid_result {
    Ok(_) => assert_true(true)
    Err(errors) => {
      for error in errors {
        assert_true(false)
      }
    }
  }
  
  // 测试无效配置
  let invalid_config = {
    "server": {
      "host": "invalid host with spaces",
      "port": 70000, // 超出范围
      "ssl_enabled": false
    },
    "database": {
      "url": "invalid_url_format" // 不匹配数据库URL模式
    },
    "auth": {
      "tokens": {
        "jwt_secret": "short" // 太短
      }
    },
    "features": {
      "analytics": "not_boolean", // 错误类型
      "debug_mode": false
    },
    "cache": {
      "type": "", // 空类型
      "host": "localhost"
    }
  }
  
  let invalid_result = ValidationManager::validate(validation_manager, invalid_config)
  match invalid_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(errors) => {
      assert_true(errors.length() >= 5)
      
      // 验证特定错误
      let has_host_error = errors.any(fn(error) { String::contains(error, "server.host") })
      let has_port_error = errors.any(fn(error) { String::contains(error, "server.port") })
      let has_db_url_error = errors.any(fn(error) { String::contains(error, "database.url") })
      let has_jwt_error = errors.any(fn(error) { String::contains(error, "jwt_secret") })
      let has_analytics_error = errors.any(fn(error) { String::contains(error, "features.analytics") })
      
      assert_true(has_host_error)
      assert_true(has_port_error)
      assert_true(has_db_url_error)
      assert_true(has_jwt_error)
      assert_true(has_analytics_error)
    }
  }
  
  // 测试条件验证
  let ssl_config = {
    "server": {
      "host": "example.com",
      "port": 443, // HTTPS端口
      "ssl_enabled": false // SSL未启用
    }
  }
  
  let ssl_result = ValidationManager::validate(validation_manager, ssl_config)
  match ssl_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(errors) => {
      let has_ssl_error = errors.any(fn(error) { String::contains(error, "SSL is required") })
      assert_true(has_ssl_error)
    }
  }
  
  // 测试依赖验证
  let cache_config = {
    "features": {
      "cache_enabled": true
    },
    "cache": {
      "type": "", // 空类型
      "host": "" // 空主机
    }
  }
  
  let cache_result = ValidationManager::validate(validation_manager, cache_config)
  match cache_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(errors) => {
      let has_cache_error = errors.any(fn(error) { String::contains(error, "Cache configuration") })
      assert_true(has_cache_error)
    }
  }
  
  // 测试验证规则组合
  let composite_rules = {
    "service.endpoints": {
      "type": "array",
      "required": true,
      "min_items": 1,
      "items": {
        "type": "object",
        "properties": {
          "path": { "type": "string", "required": true, "pattern": "^/" },
          "method": { "type": "string", "required": true, "enum": ["GET", "POST", "PUT", "DELETE"] },
          "handler": { "type": "string", "required": true }
        }
      }
    }
  }
  
  ValidationManager::register_rules(validation_manager, composite_rules)
  
  let endpoint_config = {
    "service": {
      "endpoints": [
        {
          "path": "/api/users",
          "method": "GET",
          "handler": "get_users"
        },
        {
          "path": "/api/users",
          "method": "POST",
          "handler": "create_user"
        }
      ]
    }
  }
  
  let endpoint_result = ValidationManager::validate(validation_manager, endpoint_config)
  match endpoint_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 测试验证报告生成
  let validation_report = ValidationManager::generate_validation_report(validation_manager, invalid_config)
  assert_true(validation_report.contains("Validation Report"))
  assert_true(validation_report.contains("Errors Found"))
  assert_true(validation_report.contains("Validation Summary"))
  
  // 测试验证规则导出
  let exported_rules = ValidationManager::export_rules(validation_manager)
  assert_true(exported_rules.length() > 0)
  
  // 测试验证规则导入
  let import_result = ValidationManager::import_rules(validation_manager, exported_rules)
  match import_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
}

// Test 9: 配置监控和变更通知
test "configuration monitoring and change notifications" {
  let config_monitor = ConfigMonitor::new()
  
  // 初始配置
  let initial_config = {
    "app": {
      "name": "Azimuth",
      "version": "1.0.0"
    },
    "server": {
      "host": "localhost",
      "port": 8080
    },
    "features": {
      "new_ui": false,
      "analytics": true
    }
  }
  
  // 设置监控配置
  ConfigMonitor::set_config(config_monitor, initial_config)
  
  // 注册变更监听器
  let mut change_events = []
  
  ConfigMonitor::add_change_listener(config_monitor, "server.port", fn(old_value, new_value) {
    change_events = change_events.push({
      "path": "server.port",
      "old": old_value,
      "new": new_value,
      "timestamp": Time::now()
    })
  })
  
  ConfigMonitor::add_change_listener(config_monitor, "features.*", fn(old_value, new_value) {
    change_events = change_events.push({
      "path": "features.*",
      "old": old_value,
      "new": new_value,
      "timestamp": Time::now()
    })
  })
  
  // 测试配置变更
  let updates = [
    { "server": { "port": 9000 } },
    { "features": { "new_ui": true } },
    { "features": { "analytics": false } },
    { "app": { "version": "1.1.0" } }
  ]
  
  for update in updates {
    let update_result = ConfigMonitor::update_config(config_monitor, update)
    match update_result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
    
    // 等待变更通知
    ConcurrentProcessor::sleep(100)
  }
  
  // 验证变更事件
  assert_true(change_events.length() >= 3) // port, new_ui, analytics
  
  // 验证特定变更
  let port_change = change_events.find(fn(event) { event.path == "server.port" })
  match port_change {
    Some(change) => {
      assert_eq(change.old, 8080)
      assert_eq(change.new, 9000)
    }
    None => assert_true(false)
  }
  
  let feature_changes = change_events.filter(fn(event) { event.path == "features.*" })
  assert_true(feature_changes.length() >= 2)
  
  // 测试配置快照
  let snapshot_result = ConfigMonitor::create_snapshot(config_monitor, "before_major_update")
  match snapshot_result {
    Ok(snapshot_id) => {
      // 进行更多变更
      let major_update = {
        "app": { "version": "2.0.0" },
        "server": { "host": "0.0.0.0" },
        "features": { "new_ui": true, "analytics": true, "beta_features": true }
      }
      
      ConfigMonitor::update_config(config_monitor, major_update)
      
      // 恢复到快照
      let restore_result = ConfigMonitor::restore_snapshot(config_monitor, snapshot_id)
      match restore_result {
        Ok(_) => {
          // 验证配置已恢复
          let current_config = ConfigMonitor::get_config(config_monitor)
          assert_eq(current_config.app.version, "1.1.0") // 快照前的版本
          assert_eq(current_config.server.host, "localhost")
          assert_false(current_config.features.beta_features)
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试配置差异比较
  let config_a = {
    "server": { "port": 8080, "host": "localhost" },
    "features": { "new_ui": false, "analytics": true }
  }
  
  let config_b = {
    "server": { "port": 9000, "host": "localhost" },
    "features": { "new_ui": true, "analytics": false }
  }
  
  let diff_result = ConfigMonitor::compare_configs(config_monitor, config_a, config_b)
  match diff_result {
    Ok(diff) => {
      assert_true(diff.changed.length() >= 3)
      
      let port_change = diff.changed.find(fn(change) { change.path == "server.port" })
      match port_change {
        Some(change) => {
          assert_eq(change.old_value, 8080)
          assert_eq(change.new_value, 9000)
        }
        None => assert_true(false)
      }
      
      let new_ui_change = diff.changed.find(fn(change) { change.path == "features.new_ui" })
      match new_ui_change {
        Some(change) => {
          assert_eq(change.old_value, false)
          assert_eq(change.new_value, true)
        }
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试配置变更历史
  let history = ConfigMonitor::get_change_history(config_monitor)
  assert_true(history.length() >= 4)
  
  // 验证历史记录按时间排序
  for i in 1..<history.length() {
    assert_true(history[i].timestamp >= history[i-1].timestamp)
  }
  
  // 测试配置回滚
  let rollback_result = ConfigMonitor::rollback_to_version(config_monitor, history[history.length() - 3].version)
  match rollback_result {
    Ok(_) => {
      // 验证已回滚到指定版本
      let current_config = ConfigMonitor::get_config(config_monitor)
      assert_eq(current_config.server.port, 9000) // 第三次变更前的状态
      assert_eq(current_config.features.new_ui, true)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试配置监控指标
  let metrics = ConfigMonitor::get_metrics(config_monitor)
  assert_true(metrics.total_changes >= 4)
  assert_true(metrics.change_listeners >= 2)
  assert_true(metrics.snapshots_created >= 1)
  assert_true(metrics.rollbacks_performed >= 1)
}

// Test 10: 配置性能和优化
test "configuration performance and optimization" {
  let performance_manager = ConfigPerformanceManager::new()
  
  // 创建大型配置
  let large_config = {}
  let sections = ["servers", "databases", "services", "features", "routes", "middleware", "auth", "cache"]
  
  for section in sections {
    let section_config = {}
    for i in 0..<100 {
      let item_key = "item_" + i.to_string()
      let item_value = {
        "id": i,
        "name": "Item " + i.to_string(),
        "enabled": i % 2 == 0,
        "priority": i % 10,
        "metadata": {
          "created": Time::now() - i * 1000,
          "updated": Time::now() - i * 500,
          "version": "1." + i.to_string(),
          "tags": ["tag" + (i % 5).to_string(), "category" + (i % 3).to_string()]
        }
      }
      section_config[item_key] = item_value
    }
    large_config[section] = section_config
  }
  
  // 测试配置加载性能
  let load_start = Time::now()
  let load_result = ConfigPerformanceManager::load_config(performance_manager, large_config)
  let load_time = Time::now() - load_start
  
  match load_result {
    Ok(config_id) => {
      assert_true(load_time < 1000) // 应该在1秒内完成
      
      // 测试配置查询性能
      let query_start = Time::now()
      for i in 0..<1000 {
        let section = sections[i % sections.length()]
        let item = "item_" + (i % 100).to_string()
        let query_result = ConfigPerformanceManager::query_config(performance_manager, config_id, section + "." + item)
        match query_result {
          Some(_) => assert_true(true)
          None => assert_true(false)
        }
      }
      let query_time = Time::now() - query_start
      
      assert_true(query_time < 500) // 1000次查询应该在500ms内完成
      
      // 测试配置更新性能
      let update_start = Time::now()
      for i in 0..<100 {
        let section = sections[i % sections.length()]
        let item = "item_" + i.to_string()
        let update_path = section + "." + item + ".enabled"
        let update_result = ConfigPerformanceManager::update_config_value(performance_manager, config_id, update_path, true)
        match update_result {
          Ok(_) => assert_true(true)
          Err(_) => assert_true(false)
        }
      }
      let update_time = Time::now() - update_start
      
      assert_true(update_time < 1000) // 100次更新应该在1秒内完成
      
      // 测试配置缓存
      ConfigPerformanceManager::enable_caching(performance_manager, config_id, 1000) // 缓存1000个条目
      
      let cache_start = Time::now()
      for i in 0..<1000 {
        let section = sections[i % sections.length()]
        let item = "item_" + (i % 100).to_string()
        let query_result = ConfigPerformanceManager::query_config(performance_manager, config_id, section + "." + item)
        match query_result {
          Some(_) => assert_true(true)
          None => assert_true(false)
        }
      }
      let cache_time = Time::now() - cache_start
      
      assert_true(cache_time < query_time * 0.5) // 缓存查询应该更快
      
      // 测试缓存命中率
      let cache_stats = ConfigPerformanceManager::get_cache_stats(performance_manager, config_id)
      assert_true(cache_stats.hit_rate > 0.8) // 命中率应该超过80%
      
      // 测试配置压缩
      let compress_start = Time::now()
      let compress_result = ConfigPerformanceManager::compress_config(performance_manager, config_id)
      let compress_time = Time::now() - compress_start
      
      match compress_result {
        Ok(compressed_id) => {
          let original_size = ConfigPerformanceManager::get_config_size(performance_manager, config_id)
          let compressed_size = ConfigPerformanceManager::get_config_size(performance_manager, compressed_id)
          
          assert_true(compressed_size < original_size) // 压缩后应该更小
          assert_true(compress_time < 2000) // 压缩应该在2秒内完成
          
          // 测试压缩配置的查询性能
          let decompress_start = Time::now()
          for i in 0..<100 {
            let section = sections[i % sections.length()]
            let item = "item_" + i.to_string()
            let query_result = ConfigPerformanceManager::query_config(performance_manager, compressed_id, section + "." + item)
            match query_result {
              Some(_) => assert_true(true)
              None => assert_true(false)
            }
          }
          let decompress_time = Time::now() - decompress_start
          
          assert_true(decompress_time < 1000) // 解压缩查询应该在1秒内完成
        }
        Err(_) => assert_true(false)
      }
      
      // 测试配置分片
      let shard_start = Time::now()
      let shard_result = ConfigPerformanceManager::shard_config(performance_manager, config_id, sections)
      let shard_time = Time::now() - shard_start
      
      match shard_result {
        Ok(shard_ids) => {
          assert_eq(shard_ids.length(), sections.length())
          assert_true(shard_time < 1000) // 分片应该在1秒内完成
          
          // 测试并行查询分片
          let parallel_start = Time::now()
          let shard_queries = []
          
          for i in 0..<sections.length() {
            let shard_id = shard_ids[i]
            let item = "item_" + (i * 10).to_string()
            let query_path = sections[i] + "." + item
            let query = ConfigPerformanceManager::async_query_config(performance_manager, shard_id, query_path)
            shard_queries = shard_queries.push(query)
          }
          
          // 等待所有查询完成
          let results = []
          for query in shard_queries {
            let result = ConfigPerformanceManager::wait_for_query(query)
            match result {
              Some(value) => results = results.push(value)
              None => assert_true(false)
            }
          }
          let parallel_time = Time::now() - parallel_start
          
          assert_eq(results.length(), sections.length())
          assert_true(parallel_time < 500) // 并行查询应该更快
        }
        Err(_) => assert_true(false)
      }
      
      // 获取性能指标
      let performance_metrics = ConfigPerformanceManager::get_performance_metrics(performance_manager)
      
      assert_true(performance_metrics.load_time_ms > 0)
      assert_true(performance_metrics.query_time_ms > 0)
      assert_true(performance_metrics.update_time_ms > 0)
      assert_true(performance_metrics.cache_hit_rate > 0)
      assert_true(performance_metrics.compression_ratio > 1.0)
      
      // 测试性能优化建议
      let optimization_suggestions = ConfigPerformanceManager::get_optimization_suggestions(performance_manager, config_id)
      assert_true(optimization_suggestions.length() > 0)
      
      // 验证建议内容
      let has_cache_suggestion = optimization_suggestions.any(fn(suggestion) { 
        String::contains(suggestion, "cache")
      })
      let has_compression_suggestion = optimization_suggestions.any(fn(suggestion) { 
        String::contains(suggestion, "compress")
      })
      
      assert_true(has_cache_suggestion || has_compression_suggestion)
    }
    Err(_) => assert_true(false)
  }
}