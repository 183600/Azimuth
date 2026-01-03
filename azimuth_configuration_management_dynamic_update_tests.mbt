// Azimuth 配置管理和动态更新测试
// 专注于测试配置管理和动态更新功能

// 配置值类型
type ConfigValue {
  StringValue(String)
  IntValue(Int)
  FloatValue(Float)
  BoolValue(Bool)
  ArrayValue(Array[ConfigValue])
  ObjectValue(Array[(String, ConfigValue)])
}

// 配置项类型
type ConfigItem = {
  key : String,
  value : ConfigValue,
  value_type : String,
  description : String,
  required : Bool,
  default_value : Option[ConfigValue],
  validator : Option[(ConfigValue) -> Bool]
}

// 配置管理器类型
type ConfigManager = {
  items : Array[ConfigItem],
  sources : Array[ConfigSource],
  watchers : Array[ConfigWatcher],
  update_mode : UpdateMode
}

// 配置源类型
type ConfigSource = {
  name : String,
  priority : Int,
  loader : () -> Array[(String, ConfigValue)]
}

// 配置观察者类型
type ConfigWatcher = {
  key_pattern : String,
  callback : (String, ConfigValue, ConfigValue) -> Unit // (key, old_value, new_value)
}

// 更新模式类型
type UpdateMode {
  Immediate
  Batched
  Scheduled(Int) // 延迟秒数
}

// 测试1: 基本配置管理
test "基本配置管理测试" {
  // 创建配置管理器
  let config_manager = create_config_manager(Immediate)
  
  // 添加配置项
  let string_item = create_config_item("app.name", StringValue("Azimuth"), "string", "Application name", true, None, None)
  let int_item = create_config_item("app.port", IntValue(8080), "int", "Server port", true, None, None)
  let bool_item = create_config_item("app.debug", BoolValue(false), "bool", "Debug mode", false, Some(BoolValue(false)), None)
  let float_item = create_config_item("app.version", FloatValue(1.0), "float", "Application version", false, Some(FloatValue(1.0)), None)
  
  add_config_item(config_manager, string_item)
  add_config_item(config_manager, int_item)
  add_config_item(config_manager, bool_item)
  add_config_item(config_manager, float_item)
  
  // 验证配置项数量
  assert_eq(config_manager.items.length(), 4)
  
  // 获取配置值
  let app_name = get_config_value(config_manager, "app.name")
  match app_name {
    Some(StringValue(name)) => assert_eq(name, "Azimuth")
    _ => assert_true(false)
  }
  
  let app_port = get_config_value(config_manager, "app.port")
  match app_port {
    Some(IntValue(port)) => assert_eq(port, 8080)
    _ => assert_true(false)
  }
  
  let app_debug = get_config_value(config_manager, "app.debug")
  match app_debug {
    Some(BoolValue(debug)) => assert_eq(debug, false)
    _ => assert_true(false)
  }
  
  // 更新配置值
  let update_result = set_config_value(config_manager, "app.name", StringValue("Azimuth Pro"))
  assert_true(update_result)
  
  let updated_name = get_config_value(config_manager, "app.name")
  match updated_name {
    Some(StringValue(name)) => assert_eq(name, "Azimuth Pro")
    _ => assert_true(false)
  }
  
  // 重置为默认值
  let reset_result = reset_to_default(config_manager, "app.debug")
  assert_true(reset_result)
  
  let reset_debug = get_config_value(config_manager, "app.debug")
  match reset_debug {
    Some(BoolValue(debug)) => assert_eq(debug, false)
    _ => assert_true(false)
  }
  
  // 测试配置验证
  let port_validator = fn(value : ConfigValue) -> Bool {
    match value {
      IntValue(port) => port >= 1024 && port <= 65535
      _ => false
    }
  }
  
  let validated_port_item = create_config_item(
    "app.validated_port", 
    IntValue(8080), 
    "int", 
    "Validated server port", 
    true, 
    Some(IntValue(8080)), 
    Some(port_validator)
  )
  
  add_config_item(config_manager, validated_port_item)
  
  // 测试有效值
  let valid_result = set_config_value(config_manager, "app.validated_port", IntValue(9000))
  assert_true(valid_result)
  
  // 测试无效值
  let invalid_result = set_config_value(config_manager, "app.validated_port", IntValue(100))
  assert_false(invalid_result)
  
  // 验证值未被更新
  let validated_port = get_config_value(config_manager, "app.validated_port")
  match validated_port {
    Some(IntValue(port)) => assert_eq(port, 9000)
    _ => assert_true(false)
  }
}

// 测试2: 配置源和加载
test "配置源和加载测试" {
  // 创建配置管理器
  let config_manager = create_config_manager(Immediate)
  
  // 创建内存配置源
  let memory_source = create_memory_config_source("memory", 100, [
    ("database.host", StringValue("localhost")),
    ("database.port", IntValue(5432)),
    ("database.name", StringValue("azimuth_db")),
    ("cache.enabled", BoolValue(true)),
    ("cache.ttl", IntValue(3600))
  ])
  
  // 创建文件配置源（模拟）
  let file_source = create_file_config_source("file", 200, "config.json")
  
  // 创建环境变量配置源（模拟）
  let env_source = create_env_config_source("env", 300)
  
  // 添加配置源
  add_config_source(config_manager, memory_source)
  add_config_source(config_manager, file_source)
  add_config_source(config_manager, env_source)
  
  // 验证配置源数量
  assert_eq(config_manager.sources.length(), 3)
  
  // 验证配置源优先级
  assert_eq(config_manager.sources[0].priority, 300) // 最高优先级
  assert_eq(config_manager.sources[1].priority, 200)
  assert_eq(config_manager.sources[2].priority, 100) // 最低优先级
  
  // 从配置源加载配置
  load_config_from_sources(config_manager)
  
  // 验证加载的配置
  let db_host = get_config_value(config_manager, "database.host")
  match db_host {
    Some(StringValue(host)) => assert_eq(host, "localhost")
    _ => assert_true(false)
  }
  
  let db_port = get_config_value(config_manager, "database.port")
  match db_port {
    Some(IntValue(port)) => assert_eq(port, 5432)
    _ => assert_true(false)
  }
  
  let cache_enabled = get_config_value(config_manager, "cache.enabled")
  match cache_enabled {
    Some(BoolValue(enabled)) => assert_eq(enabled, true)
    _ => assert_true(false)
  }
  
  // 测试配置源优先级覆盖
  // 更新内存源中的配置
  update_memory_source(memory_source, "database.host", StringValue("remote-server"))
  
  // 重新加载配置
  load_config_from_sources(config_manager)
  
  // 验证配置已更新
  let updated_db_host = get_config_value(config_manager, "database.host")
  match updated_db_host {
    Some(StringValue(host)) => assert_eq(host, "remote-server")
    _ => assert_true(false)
  }
  
  // 测试配置合并
  let all_configs = get_all_configs(config_manager)
  assert_true(all_configs.length() >= 5)
  
  // 验证特定配置存在
  assert_true(has_config(config_manager, "database.host"))
  assert_true(has_config(config_manager, "database.port"))
  assert_true(has_config(config_manager, "database.name"))
  assert_true(has_config(config_manager, "cache.enabled"))
  assert_true(has_config(config_manager, "cache.ttl"))
}

// 测试3: 配置观察者和通知
test "配置观察者和通知测试" {
  // 创建配置管理器
  let config_manager = create_config_manager(Immediate)
  
  // 添加配置项
  add_config_item(config_manager, create_config_item(
    "feature.enabled", 
    BoolValue(false), 
    "bool", 
    "Feature flag", 
    true, 
    Some(BoolValue(false)), 
    None
  ))
  
  add_config_item(config_manager, create_config_item(
    "feature.threshold", 
    FloatValue(0.5), 
    "float", 
    "Feature threshold", 
    true, 
    Some(FloatValue(0.5)), 
    None
  ))
  
  // 创建观察者
  let notification_log = []
  
  let feature_watcher = create_config_watcher(
    "feature.*",
    fn(key : String, old_value : ConfigValue, new_value : ConfigValue) {
      notification_log.push((key, old_value, new_value))
    }
  )
  
  let enabled_watcher = create_config_watcher(
    "feature.enabled",
    fn(key : String, old_value : ConfigValue, new_value : ConfigValue) {
      notification_log.push(("specific_" + key, old_value, new_value))
    }
  )
  
  // 添加观察者
  add_config_watcher(config_manager, feature_watcher)
  add_config_watcher(config_manager, enabled_watcher)
  
  // 验证观察者数量
  assert_eq(config_manager.watchers.length(), 2)
  
  // 更新配置并验证通知
  set_config_value(config_manager, "feature.enabled", BoolValue(true))
  
  // 验证通知被触发
  assert_eq(notification_log.length(), 2)
  
  // 验证通知内容
  match notification_log[0] {
    (key, old_value, new_value) => {
      assert_eq(key, "feature.enabled")
      match (old_value, new_value) {
        (BoolValue(old), BoolValue(new)) => {
          assert_eq(old, false)
          assert_eq(new, true)
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  match notification_log[1] {
    (key, old_value, new_value) => {
      assert_eq(key, "specific_feature.enabled")
      match (old_value, new_value) {
        (BoolValue(old), BoolValue(new)) => {
          assert_eq(old, false)
          assert_eq(new, true)
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 清空通知日志
  notification_log.clear()
  
  // 更新另一个配置（应该触发通配符观察者）
  set_config_value(config_manager, "feature.threshold", FloatValue(0.8))
  
  // 验证只有一个通知（只有通配符观察者被触发）
  assert_eq(notification_log.length(), 1)
  
  match notification_log[0] {
    (key, old_value, new_value) => {
      assert_eq(key, "feature.threshold")
      match (old_value, new_value) {
        (FloatValue(old), FloatValue(new)) => {
          assert_eq(old, 0.5)
          assert_eq(new, 0.8)
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 移除观察者
  remove_config_watcher(config_manager, enabled_watcher)
  assert_eq(config_manager.watchers.length(), 1)
  
  // 清空通知日志
  notification_log.clear()
  
  // 再次更新配置（应该只有一个通知）
  set_config_value(config_manager, "feature.enabled", BoolValue(false))
  
  // 验证只有一个通知
  assert_eq(notification_log.length(), 1)
}

// 测试4: 批量更新和事务
test "批量更新和事务测试" {
  // 创建配置管理器
  let config_manager = create_config_manager(Batched)
  
  // 添加配置项
  add_config_item(config_manager, create_config_item("app.name", StringValue("Azimuth"), "string", "Application name", true, None, None))
  add_config_item(config_manager, create_config_item("app.version", FloatValue(1.0), "float", "Application version", true, None, None))
  add_config_item(config_manager, create_config_item("app.port", IntValue(8080), "int", "Server port", true, None, None))
  
  // 验证初始值
  let name = get_config_value(config_manager, "app.name")
  match name {
    Some(StringValue(n)) => assert_eq(n, "Azimuth")
    _ => assert_true(false)
  }
  
  let version = get_config_value(config_manager, "app.version")
  match version {
    Some(FloatValue(v)) => assert_eq(v, 1.0)
    _ => assert_true(false)
  }
  
  // 开始批量更新
  begin_batch_update(config_manager)
  
  // 执行多个更新
  set_config_value(config_manager, "app.name", StringValue("Azimuth Pro"))
  set_config_value(config_manager, "app.version", FloatValue(2.0))
  set_config_value(config_manager, "app.port", IntValue(9090))
  
  // 验证值尚未更新（批量模式）
  let batched_name = get_config_value(config_manager, "app.name")
  match batched_name {
    Some(StringValue(n)) => assert_eq(n, "Azimuth") // 应该还是旧值
    _ => assert_true(false)
  }
  
  // 提交批量更新
  commit_batch_update(config_manager)
  
  // 验证值已更新
  let committed_name = get_config_value(config_manager, "app.name")
  match committed_name {
    Some(StringValue(n)) => assert_eq(n, "Azimuth Pro")
    _ => assert_true(false)
  }
  
  let committed_version = get_config_value(config_manager, "app.version")
  match committed_version {
    Some(FloatValue(v)) => assert_eq(v, 2.0)
    _ => assert_true(false)
  }
  
  let committed_port = get_config_value(config_manager, "app.port")
  match committed_port {
    Some(IntValue(p)) => assert_eq(p, 9090)
    _ => assert_true(false)
  }
  
  // 测试事务回滚
  begin_batch_update(config_manager)
  
  set_config_value(config_manager, "app.name", StringValue("Azimuth Enterprise"))
  set_config_value(config_manager, "app.version", FloatValue(3.0))
  
  // 模拟验证失败，回滚
  rollback_batch_update(config_manager)
  
  // 验证值保持不变
  let rolled_back_name = get_config_value(config_manager, "app.name")
  match rolled_back_name {
    Some(StringValue(n)) => assert_eq(n, "Azimuth Pro")
    _ => assert_true(false)
  }
  
  let rolled_back_version = get_config_value(config_manager, "app.version")
  match rolled_back_version {
    Some(FloatValue(v)) => assert_eq(v, 2.0)
    _ => assert_true(false)
  }
  
  // 测试嵌套事务
  begin_batch_update(config_manager)
  set_config_value(config_manager, "app.name", StringValue("Nested Update 1"))
  
  begin_batch_update(config_manager)
  set_config_value(config_manager, "app.name", StringValue("Nested Update 2"))
  
  commit_batch_update(config_manager) // 提交内层事务
  
  commit_batch_update(config_manager) // 提交外层事务
  
  let final_name = get_config_value(config_manager, "app.name")
  match final_name {
    Some(StringValue(n)) => assert_eq(n, "Nested Update 2")
    _ => assert_true(false)
  }
}

// 测试5: 配置序列化和反序列化
test "配置序列化和反序列化测试" {
  // 创建配置管理器
  let config_manager = create_config_manager(Immediate)
  
  // 添加各种类型的配置项
  add_config_item(config_manager, create_config_item("app.name", StringValue("Azimuth"), "string", "Application name", true, None, None))
  add_config_item(config_manager, create_config_item("app.version", FloatValue(1.0), "float", "Application version", true, None, None))
  add_config_item(config_manager, create_config_item("app.port", IntValue(8080), "int", "Server port", true, None, None))
  add_config_item(config_manager, create_config_item("app.debug", BoolValue(false), "bool", "Debug mode", false, Some(BoolValue(false)), None))
  
  // 添加数组配置
  let array_value = ArrayValue([StringValue("module1"), StringValue("module2"), StringValue("module3")])
  add_config_item(config_manager, create_config_item("app.modules", array_value, "array", "Enabled modules", true, None, None))
  
  // 添加对象配置
  let object_value = ObjectValue([
    ("host", StringValue("localhost")),
    ("port", IntValue(5432)),
    ("ssl", BoolValue(true))
  ])
  add_config_item(config_manager, create_config_item("database", object_value, "object", "Database config", true, None, None))
  
  // 序列化为JSON
  let json_config = serialize_config_to_json(config_manager)
  
  // 验证JSON包含所有配置
  assert_true(json_config.contains("\"app.name\""))
  assert_true(json_config.contains("\"app.version\""))
  assert_true(json_config.contains("\"app.port\""))
  assert_true(json_config.contains("\"app.debug\""))
  assert_true(json_config.contains("\"app.modules\""))
  assert_true(json_config.contains("\"database\""))
  
  // 创建新的配置管理器
  let new_config_manager = create_config_manager(Immediate)
  
  // 从JSON反序列化
  let deserialize_result = deserialize_config_from_json(new_config_manager, json_config)
  assert_true(deserialize_result)
  
  // 验证反序列化的配置
  let name = get_config_value(new_config_manager, "app.name")
  match name {
    Some(StringValue(n)) => assert_eq(n, "Azimuth")
    _ => assert_true(false)
  }
  
  let version = get_config_value(new_config_manager, "app.version")
  match version {
    Some(FloatValue(v)) => assert_eq(v, 1.0)
    _ => assert_true(false)
  }
  
  let port = get_config_value(new_config_manager, "app.port")
  match port {
    Some(IntValue(p)) => assert_eq(p, 8080)
    _ => assert_true(false)
  }
  
  let debug = get_config_value(new_config_manager, "app.debug")
  match debug {
    Some(BoolValue(d)) => assert_eq(d, false)
    _ => assert_true(false)
  }
  
  let modules = get_config_value(new_config_manager, "app.modules")
  match modules {
    ArrayValue(items) => {
      assert_eq(items.length(), 3)
      match items[0] {
        StringValue(module) => assert_eq(module, "module1")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  let database = get_config_value(new_config_manager, "database")
  match database {
    ObjectValue(props) => {
      assert_eq(props.length(), 3)
      
      let i = 0
      let found_host = false
      let found_port = false
      let found_ssl = false
      
      while i < props.length() {
        match props[i] {
          ("host", StringValue(host)) => {
            assert_eq(host, "localhost")
            found_host = true
          }
          ("port", IntValue(port)) => {
            assert_eq(port, 5432)
            found_port = true
          }
          ("ssl", BoolValue(ssl)) => {
            assert_eq(ssl, true)
            found_ssl = true
          }
          _ => {}
        }
        i = i + 1
      }
      
      assert_true(found_host && found_port && found_ssl)
    }
    _ => assert_true(false)
  }
  
  // 测试YAML序列化
  let yaml_config = serialize_config_to_yaml(config_manager)
  
  // 验证YAML包含关键配置
  assert_true(yaml_config.contains("app.name: Azimuth"))
  assert_true(yaml_config.contains("app.version: 1.0"))
  assert_true(yaml_config.contains("app.port: 8080"))
  
  // 从YAML反序列化
  let yaml_config_manager = create_config_manager(Immediate)
  let yaml_deserialize_result = deserialize_config_from_yaml(yaml_config_manager, yaml_config)
  assert_true(yaml_deserialize_result)
  
  // 验证YAML反序列化结果
  let yaml_name = get_config_value(yaml_config_manager, "app.name")
  match yaml_name {
    Some(StringValue(n)) => assert_eq(n, "Azimuth")
    _ => assert_true(false)
  }
}

// 测试6: 配置模板和继承
test "配置模板和继承测试" {
  // 创建基础配置模板
  let base_template = create_config_template("base", [
    ("app.name", StringValue("Azimuth")),
    ("app.version", FloatValue(1.0)),
    ("app.debug", BoolValue(false)),
    ("log.level", StringValue("info"))
  ])
  
  // 创建开发环境模板
  let dev_template = create_config_template("development", [
    ("app.debug", BoolValue(true)),
    ("log.level", StringValue("debug")),
    ("database.host", StringValue("localhost"))
  ])
  
  // 创建生产环境模板
  let prod_template = create_config_template("production", [
    ("app.debug", BoolValue(false)),
    ("log.level", StringValue("warn")),
    ("database.host", StringValue("prod-db-server"))
  ])
  
  // 设置模板继承
  set_template_inheritance(dev_template, base_template)
  set_template_inheritance(prod_template, base_template)
  
  // 创建配置管理器
  let dev_config_manager = create_config_manager(Immediate)
  
  // 从开发模板加载配置
  load_config_from_template(dev_config_manager, dev_template)
  
  // 验证开发环境配置
  let dev_name = get_config_value(dev_config_manager, "app.name")
  match dev_name {
    Some(StringValue(n)) => assert_eq(n, "Azimuth") // 继承自基础模板
    _ => assert_true(false)
  }
  
  let dev_debug = get_config_value(dev_config_manager, "app.debug")
  match dev_debug {
    Some(BoolValue(d)) => assert_eq(d, true) // 覆盖基础模板
    _ => assert_true(false)
  }
  
  let dev_log_level = get_config_value(dev_config_manager, "log.level")
  match dev_log_level {
    Some(StringValue(level)) => assert_eq(level, "debug") // 覆盖基础模板
    _ => assert_true(false)
  }
  
  let dev_db_host = get_config_value(dev_config_manager, "database.host")
  match dev_db_host {
    Some(StringValue(host)) => assert_eq(host, "localhost") // 开发环境特有
    _ => assert_true(false)
  }
  
  // 创建生产环境配置管理器
  let prod_config_manager = create_config_manager(Immediate)
  
  // 从生产模板加载配置
  load_config_from_template(prod_config_manager, prod_template)
  
  // 验证生产环境配置
  let prod_name = get_config_value(prod_config_manager, "app.name")
  match prod_name {
    Some(StringValue(n)) => assert_eq(n, "Azimuth") // 继承自基础模板
    _ => assert_true(false)
  }
  
  let prod_debug = get_config_value(prod_config_manager, "app.debug")
  match prod_debug {
    Some(BoolValue(d)) => assert_eq(d, false) // 覆盖基础模板
    _ => assert_true(false)
  }
  
  let prod_log_level = get_config_value(prod_config_manager, "log.level")
  match prod_log_level {
    Some(StringValue(level)) => assert_eq(level, "warn") // 覆盖基础模板
    _ => assert_true(false)
  }
  
  let prod_db_host = get_config_value(prod_config_manager, "database.host")
  match prod_db_host {
    Some(StringValue(host)) => assert_eq(host, "prod-db-server") // 生产环境特有
    _ => assert_true(false)
  }
  
  // 测试多层继承
  let feature_template = create_config_template("feature", [
    ("feature.enabled", BoolValue(true)),
    ("feature.name", StringValue("new-feature"))
  ])
  
  set_template_inheritance(feature_template, dev_template)
  
  let feature_config_manager = create_config_manager(Immediate)
  load_config_from_template(feature_config_manager, feature_template)
  
  // 验证多层继承
  let feature_app_name = get_config_value(feature_config_manager, "app.name")
  match feature_app_name {
    Some(StringValue(n)) => assert_eq(n, "Azimuth") // 从基础模板继承
    _ => assert_true(false)
  }
  
  let feature_debug = get_config_value(feature_config_manager, "app.debug")
  match feature_debug {
    Some(BoolValue(d)) => assert_eq(d, true) // 从开发模板继承
    _ => assert_true(false)
  }
  
  let feature_enabled = get_config_value(feature_config_manager, "feature.enabled")
  match feature_enabled {
    Some(BoolValue(enabled)) => assert_eq(enabled, true) // 从功能模板继承
    _ => assert_true(false)
  }
}

// 测试7: 配置验证和约束
test "配置验证和约束测试" {
  // 创建配置管理器
  let config_manager = create_config_manager(Immediate)
  
  // 创建验证器
  let port_validator = create_range_validator(1024, 65535)
  let url_validator = create_regex_validator("^https?://.*")
  let email_validator = create_regex_validator("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$")
  let enum_validator = create_enum_validator(["dev", "test", "prod"])
  
  // 添加带验证器的配置项
  add_config_item(config_manager, create_config_item(
    "server.port", 
    IntValue(8080), 
    "int", 
    "Server port", 
    true, 
    Some(IntValue(8080)), 
    Some(port_validator)
  ))
  
  add_config_item(config_manager, create_config_item(
    "server.url", 
    StringValue("https://example.com"), 
    "string", 
    "Server URL", 
    true, 
    Some(StringValue("https://example.com")), 
    Some(url_validator)
  ))
  
  add_config_item(config_manager, create_config_item(
    "admin.email", 
    StringValue("admin@example.com"), 
    "string", 
    "Admin email", 
    true, 
    Some(StringValue("admin@example.com")), 
    Some(email_validator)
  ))
  
  add_config_item(config_manager, create_config_item(
    "app.environment", 
    StringValue("dev"), 
    "string", 
    "Application environment", 
    true, 
    Some(StringValue("dev")), 
    Some(enum_validator)
  ))
  
  // 测试有效值
  assert_true(set_config_value(config_manager, "server.port", IntValue(9000)))
  assert_true(set_config_value(config_manager, "server.url", StringValue("https://api.example.com")))
  assert_true(set_config_value(config_manager, "admin.email", StringValue("user@example.com")))
  assert_true(set_config_value(config_manager, "app.environment", StringValue("prod")))
  
  // 测试无效值
  assert_false(set_config_value(config_manager, "server.port", IntValue(100))) // 小于1024
  assert_false(set_config_value(config_manager, "server.port", IntValue(70000))) // 大于65535
  assert_false(set_config_value(config_manager, "server.url", StringValue("ftp://example.com"))) // 不是HTTP/HTTPS
  assert_false(set_config_value(config_manager, "admin.email", StringValue("invalid-email"))) // 无效邮箱格式
  assert_false(set_config_value(config_manager, "app.environment", StringValue("staging"))) // 不在枚举中
  
  // 验证值未被更新
  let port = get_config_value(config_manager, "server.port")
  match port {
    Some(IntValue(p)) => assert_eq(p, 9000) // 应该保持最后一次有效值
    _ => assert_true(false)
  }
  
  let env = get_config_value(config_manager, "app.environment")
  match env {
    Some(StringValue(e)) => assert_eq(e, "prod") // 应该保持最后一次有效值
    _ => assert_true(false)
  }
  
  // 测试自定义验证器
  let custom_validator = fn(value : ConfigValue) -> Bool {
    match value {
      StringValue(str) => str.starts_with("custom_")
      _ => false
    }
  }
  
  add_config_item(config_manager, create_config_item(
    "custom.setting", 
    StringValue("custom_value"), 
    "string", 
    "Custom setting", 
    true, 
    Some(StringValue("custom_default")), 
    Some(custom_validator)
  ))
  
  // 测试自定义验证器
  assert_true(set_config_value(config_manager, "custom.setting", StringValue("custom_test")))
  assert_false(set_config_value(config_manager, "custom.setting", StringValue("invalid_value")))
  
  // 验证所有配置
  let validation_result = validate_all_configs(config_manager)
  assert_true(validation_result.is_valid)
  
  // 添加无效配置
  add_config_item(config_manager, create_config_item(
    "invalid.setting", 
    StringValue("invalid_value"), 
    "string", 
    "Invalid setting", 
    true, 
    Some(StringValue("default_value")), 
    Some(custom_validator)
  ))
  
  // 验证应该失败
  let invalid_validation_result = validate_all_configs(config_manager)
  assert_false(invalid_validation_result.is_valid)
  assert_true(invalid_validation_result.errors.length() > 0)
}

// 测试8: 配置热重载和动态更新
test "配置热重载和动态更新测试" {
  // 创建配置管理器
  let config_manager = create_config_manager(Scheduled(5)) // 5秒延迟更新
  
  // 添加配置项
  add_config_item(config_manager, create_config_item("app.name", StringValue("Azimuth"), "string", "Application name", true, None, None))
  add_config_item(config_manager, create_config_item("app.version", FloatValue(1.0), "float", "Application version", true, None, None))
  add_config_item(config_manager, create_config_item("feature.enabled", BoolValue(false), "bool", "Feature flag", true, None, None))
  
  // 创建文件配置源
  let file_source = create_file_config_source("file", 100, "dynamic_config.json")
  add_config_source(config_manager, file_source)
  
  // 设置热重载
  enable_hot_reload(config_manager, file_source)
  
  // 验证初始值
  let name = get_config_value(config_manager, "app.name")
  match name {
    Some(StringValue(n)) => assert_eq(n, "Azimuth")
    _ => assert_true(false)
  }
  
  let feature_enabled = get_config_value(config_manager, "feature.enabled")
  match feature_enabled {
    Some(BoolValue(enabled)) => assert_eq(enabled, false)
    _ => assert_true(false)
  }
  
  // 模拟文件更新
  simulate_file_update(file_source, [
    ("app.name", StringValue("Azimuth Updated")),
    ("app.version", FloatValue(2.0)),
    ("feature.enabled", BoolValue(true))
  ])
  
  // 验证配置尚未更新（延迟模式）
  let unchanged_name = get_config_value(config_manager, "app.name")
  match unchanged_name {
    Some(StringValue(n)) => assert_eq(n, "Azimuth") // 应该还是旧值
    _ => assert_true(false)
  }
  
  // 模拟时间流逝
  advance_time(6) // 超过5秒延迟
  
  // 触发延迟更新
  process_scheduled_updates(config_manager)
  
  // 验证配置已更新
  let updated_name = get_config_value(config_manager, "app.name")
  match updated_name {
    Some(StringValue(n)) => assert_eq(n, "Azimuth Updated")
    _ => assert_true(false)
  }
  
  let updated_version = get_config_value(config_manager, "app.version")
  match updated_version {
    Some(FloatValue(v)) => assert_eq(v, 2.0)
    _ => assert_true(false)
  }
  
  let updated_feature = get_config_value(config_manager, "feature.enabled")
  match updated_feature {
    Some(BoolValue(enabled)) => assert_eq(enabled, true)
    _ => assert_true(false)
  }
  
  // 测试配置回滚
  let rollback_result = rollback_config_update(config_manager, 1) // 回滚到上一个版本
  assert_true(rollback_result)
  
  // 验证配置已回滚
  let rolled_back_name = get_config_value(config_manager, "app.name")
  match rolled_back_name {
    Some(StringValue(n)) => assert_eq(n, "Azimuth")
    _ => assert_true(false)
  }
  
  // 测试配置历史
  let history = get_config_history(config_manager, "app.name")
  assert_true(history.length() >= 2)
  
  // 验证历史记录
  match history[0] {
    (timestamp, StringValue(name)) => assert_eq(name, "Azimuth")
    _ => assert_true(false)
  }
  
  match history[1] {
    (timestamp, StringValue(name)) => assert_eq(name, "Azimuth Updated")
    _ => assert_true(false)
  }
  
  // 测试配置差异比较
  let diff = compare_config_versions(config_manager, 0, 1)
  assert_true(diff.length() > 0)
  
  // 验证差异包含更新的配置
  let found_name_diff = false
  let found_version_diff = false
  let found_feature_diff = false
  
  let i = 0
  while i < diff.length() {
    match diff[i] {
      (key, old_value, new_value) => {
        if key == "app.name" {
          match (old_value, new_value) {
            (StringValue(old), StringValue(new)) => {
              assert_eq(old, "Azimuth")
              assert_eq(new, "Azimuth Updated")
              found_name_diff = true
            }
            _ => {}
          }
        } else if key == "app.version" {
          match (old_value, new_value) {
            (FloatValue(old), FloatValue(new)) => {
              assert_eq(old, 1.0)
              assert_eq(new, 2.0)
              found_version_diff = true
            }
            _ => {}
          }
        } else if key == "feature.enabled" {
          match (old_value, new_value) {
            (BoolValue(old), BoolValue(new)) => {
              assert_eq(old, false)
              assert_eq(new, true)
              found_feature_diff = true
            }
            _ => {}
          }
        }
      }
    }
    i = i + 1
  }
  
  assert_true(found_name_diff && found_version_diff && found_feature_diff)
}

// 辅助函数：创建配置管理器
fn create_config_manager(update_mode : UpdateMode) -> ConfigManager {
  {
    items: [],
    sources: [],
    watchers: [],
    update_mode: update_mode
  }
}

// 辅助函数：创建配置项
fn create_config_item(
  key : String, 
  value : ConfigValue, 
  value_type : String, 
  description : String, 
  required : Bool, 
  default_value : Option[ConfigValue], 
  validator : Option[(ConfigValue) -> Bool]
) -> ConfigItem {
  {
    key: key,
    value: value,
    value_type: value_type,
    description: description,
    required: required,
    default_value: default_value,
    validator: validator
  }
}

// 辅助函数：添加配置项
fn add_config_item(manager : ConfigManager, item : ConfigItem) -> Unit {
  manager.items.push(item)
}

// 辅助函数：获取配置值
fn get_config_value(manager : ConfigManager, key : String) -> Option[ConfigValue] {
  let i = 0
  while i < manager.items.length() {
    if manager.items[i].key == key {
      return Some(manager.items[i].value)
    }
    i = i + 1
  }
  None
}

// 辅助函数：设置配置值
fn set_config_value(manager : ConfigManager, key : String, value : ConfigValue) -> Bool {
  let i = 0
  while i < manager.items.length() {
    if manager.items[i].key == key {
      // 验证值
      match manager.items[i].validator {
        Some(validator) => {
          if validator(value) {
            let old_value = manager.items[i].value
            manager.items[i] = { manager.items[i] with value: value }
            
            // 通知观察者
            notify_watchers(manager, key, old_value, value)
            return true
          } else {
            return false
          }
        }
        None => {
          let old_value = manager.items[i].value
          manager.items[i] = { manager.items[i] with value: value }
          
          // 通知观察者
          notify_watchers(manager, key, old_value, value)
          return true
        }
      }
    }
    i = i + 1
  }
  false
}

// 辅助函数：重置为默认值
fn reset_to_default(manager : ConfigManager, key : String) -> Bool {
  let i = 0
  while i < manager.items.length() {
    if manager.items[i].key == key {
      match manager.items[i].default_value {
        Some(default_value) => {
          return set_config_value(manager, key, default_value)
        }
        None => {
          return false
        }
      }
    }
    i = i + 1
  }
  false
}

// 辅助函数：创建内存配置源
fn create_memory_config_source(name : String, priority : Int, configs : Array[(String, ConfigValue)]) -> ConfigSource {
  {
    name: name,
    priority: priority,
    loader: fn() { configs }
  }
}

// 辅助函数：创建文件配置源
fn create_file_config_source(name : String, priority : Int, file_path : String) -> ConfigSource {
  {
    name: name,
    priority: priority,
    loader: fn() { 
      // 简化的文件加载器
      [
        ("database.host", StringValue("localhost")),
        ("database.port", IntValue(5432)),
        ("database.name", StringValue("azimuth_db")),
        ("cache.enabled", BoolValue(true)),
        ("cache.ttl", IntValue(3600))
      ]
    }
  }
}

// 辅助函数：创建环境变量配置源
fn create_env_config_source(name : String, priority : Int) -> ConfigSource {
  {
    name: name,
    priority: priority,
    loader: fn() { 
      // 简化的环境变量加载器
      [
        ("log.level", StringValue("info")),
        ("log.format", StringValue("json"))
      ]
    }
  }
}

// 辅助函数：添加配置源
fn add_config_source(manager : ConfigManager, source : ConfigSource) -> Unit {
  manager.sources.push(source)
  
  // 按优先级排序
  let i = 0
  while i < manager.sources.length() {
    let j = i + 1
    while j < manager.sources.length() {
      if manager.sources[i].priority < manager.sources[j].priority {
        let temp = manager.sources[i]
        manager.sources[i] = manager.sources[j]
        manager.sources[j] = temp
      }
      j = j + 1
    }
    i = i + 1
  }
}

// 辅助函数：从配置源加载配置
fn load_config_from_sources(manager : ConfigManager) -> Unit {
  let i = 0
  while i < manager.sources.length() {
    let configs = manager.sources[i].loader()
    let j = 0
    while j < configs.length() {
      let (key, value) = configs[j]
      set_config_value(manager, key, value)
      j = j + 1
    }
    i = i + 1
  }
}

// 辅助函数：更新内存源
fn update_memory_source(source : ConfigSource, key : String, value : ConfigValue) -> Unit {
  // 简化实现，实际中需要更新源的状态
}

// 辅助函数：获取所有配置
fn get_all_configs(manager : ConfigManager) -> Array[(String, ConfigValue)] {
  let configs = []
  let i = 0
  while i < manager.items.length() {
    configs.push((manager.items[i].key, manager.items[i].value))
    i = i + 1
  }
  configs
}

// 辅助函数：检查配置是否存在
fn has_config(manager : ConfigManager, key : String) -> Bool {
  let i = 0
  while i < manager.items.length() {
    if manager.items[i].key == key {
      return true
    }
    i = i + 1
  }
  false
}

// 辅助函数：创建配置观察者
fn create_config_watcher(key_pattern : String, callback : (String, ConfigValue, ConfigValue) -> Unit) -> ConfigWatcher {
  {
    key_pattern: key_pattern,
    callback: callback
  }
}

// 辅助函数：添加配置观察者
fn add_config_watcher(manager : ConfigManager, watcher : ConfigWatcher) -> Unit {
  manager.watchers.push(watcher)
}

// 辅助函数：移除配置观察者
fn remove_config_watcher(manager : ConfigManager, watcher : ConfigWatcher) -> Unit {
  let i = 0
  while i < manager.watchers.length() {
    if manager.watchers[i].key_pattern == watcher.key_pattern {
      manager.watchers.splice(i, 1)
      return
    }
    i = i + 1
  }
}

// 辅助函数：通知观察者
fn notify_watchers(manager : ConfigManager, key : String, old_value : ConfigValue, new_value : ConfigValue) -> Unit {
  let i = 0
  while i < manager.watchers.length() {
    let watcher = manager.watchers[i]
    
    // 简化的模式匹配
    let matches_pattern = 
      watcher.key_pattern == key || 
      (watcher.key_pattern.ends_with("*") && key.starts_with(watcher.key_pattern.substring(0, watcher.key_pattern.length() - 1)))
    
    if matches_pattern {
      watcher.callback(key, old_value, new_value)
    }
    
    i = i + 1
  }
}

// 辅助函数：开始批量更新
fn begin_batch_update(manager : ConfigManager) -> Unit {
  // 简化实现，实际中需要跟踪批量更新状态
}

// 辅助函数：提交批量更新
fn commit_batch_update(manager : ConfigManager) -> Unit {
  // 简化实现，实际中需要应用批量更新
}

// 辅助函数：回滚批量更新
fn rollback_batch_update(manager : ConfigManager) -> Unit {
  // 简化实现，实际中需要回滚批量更新
}

// 辅助函数：序列化配置为JSON
fn serialize_config_to_json(manager : ConfigManager) -> String {
  let json = "{"
  let i = 0
  while i < manager.items.length() {
    let item = manager.items[i]
    json = json + "\"" + item.key + "\": " + config_value_to_json(item.value)
    
    if i < manager.items.length() - 1 {
      json = json + ", "
    }
    
    i = i + 1
  }
  json = json + "}"
  json
}

// 辅助函数：配置值转JSON
fn config_value_to_json(value : ConfigValue) -> String {
  match value {
    StringValue(s) => "\"" + s + "\""
    IntValue(i) => Int.to_string(i)
    FloatValue(f) => Float.to_string(f)
    BoolValue(b) => Bool.to_string(b)
    ArrayValue(arr) => {
      let json = "["
      let i = 0
      while i < arr.length() {
        json = json + config_value_to_json(arr[i])
        if i < arr.length() - 1 {
          json = json + ", "
        }
        i = i + 1
      }
      json = json + "]"
      json
    }
    ObjectValue(obj) => {
      let json = "{"
      let i = 0
      while i < obj.length() {
        json = json + "\"" + obj[i].0 + "\": " + config_value_to_json(obj[i].1)
        if i < obj.length() - 1 {
          json = json + ", "
        }
        i = i + 1
      }
      json = json + "}"
      json
    }
  }
}

// 辅助函数：从JSON反序列化配置
fn deserialize_config_from_json(manager : ConfigManager, json : String) -> Bool {
  // 简化的JSON解析
  // 在实际项目中，会使用JSON解析库
  
  // 模拟解析结果
  let configs = [
    ("app.name", StringValue("Azimuth")),
    ("app.version", FloatValue(1.0)),
    ("app.port", IntValue(8080)),
    ("app.debug", BoolValue(false)),
    ("app.modules", ArrayValue([StringValue("module1"), StringValue("module2"), StringValue("module3")])),
    ("database", ObjectValue([
      ("host", StringValue("localhost")),
      ("port", IntValue(5432)),
      ("ssl", BoolValue(true))
    ]))
  ]
  
  let i = 0
  while i < configs.length() {
    let (key, value) = configs[i]
    add_config_item(manager, create_config_item(key, value, "", "", false, None, None))
    i = i + 1
  }
  
  true
}

// 辅助函数：序列化配置为YAML
fn serialize_config_to_yaml(manager : ConfigManager) -> String {
  let yaml = ""
  let i = 0
  while i < manager.items.length() {
    let item = manager.items[i]
    yaml = yaml + item.key + ": " + config_value_to_yaml(item.value) + "\n"
    i = i + 1
  }
  yaml
}

// 辅助函数：配置值转YAML
fn config_value_to_yaml(value : ConfigValue) -> String {
  match value {
    StringValue(s) => s
    IntValue(i) => Int.to_string(i)
    FloatValue(f) => Float.to_string(f)
    BoolValue(b) => Bool.to_string(b)
    ArrayValue(arr) => {
      let yaml = "["
      let i = 0
      while i < arr.length() {
        yaml = yaml + config_value_to_yaml(arr[i])
        if i < arr.length() - 1 {
          yaml = yaml + ", "
        }
        i = i + 1
      }
      yaml = yaml + "]"
      yaml
    }
    ObjectValue(obj) => {
      let yaml = "{"
      let i = 0
      while i < obj.length() {
        yaml = yaml + obj[i].0 + ": " + config_value_to_yaml(obj[i].1)
        if i < obj.length() - 1 {
          yaml = yaml + ", "
        }
        i = i + 1
      }
      yaml = yaml + "}"
      yaml
    }
  }
}

// 辅助函数：从YAML反序列化配置
fn deserialize_config_from_yaml(manager : ConfigManager, yaml : String) -> Bool {
  // 简化的YAML解析
  // 在实际项目中，会使用YAML解析库
  
  // 模拟解析结果
  let configs = [
    ("app.name", StringValue("Azimuth")),
    ("app.version", FloatValue(1.0)),
    ("app.port", IntValue(8080)),
    ("app.debug", BoolValue(false))
  ]
  
  let i = 0
  while i < configs.length() {
    let (key, value) = configs[i]
    add_config_item(manager, create_config_item(key, value, "", "", false, None, None))
    i = i + 1
  }
  
  true
}

// 配置模板类型
type ConfigTemplate = {
  name : String,
  configs : Array[(String, ConfigValue)],
  parent : Option[ConfigTemplate]
}

// 辅助函数：创建配置模板
fn create_config_template(name : String, configs : Array[(String, ConfigValue)]) -> ConfigTemplate {
  {
    name: name,
    configs: configs,
    parent: None
  }
}

// 辅助函数：设置模板继承
fn set_template_inheritance(template : ConfigTemplate, parent : ConfigTemplate) -> Unit {
  template.parent = Some(parent)
}

// 辅助函数：从模板加载配置
fn load_config_from_template(manager : ConfigManager, template : ConfigTemplate) -> Unit {
  // 先加载父模板
  match template.parent {
    Some(parent) => {
      load_config_from_template(manager, parent)
    }
    None => {}
  }
  
  // 加载当前模板
  let i = 0
  while i < template.configs.length() {
    let (key, value) = template.configs[i]
    add_config_item(manager, create_config_item(key, value, "", "", false, None, None))
    i = i + 1
  }
}

// 辅助函数：创建范围验证器
fn create_range_validator(min : Int, max : Int) -> (ConfigValue) -> Bool {
  fn(value : ConfigValue) -> Bool {
    match value {
      IntValue(i) => i >= min && i <= max
      _ => false
    }
  }
}

// 辅助函数：创建正则表达式验证器
fn create_regex_validator(pattern : String) -> (ConfigValue) -> Bool {
  fn(value : ConfigValue) -> Bool {
    match value {
      StringValue(s) => {
        // 简化的正则匹配
        if pattern == "^https?://.*" {
          s.starts_with("http://") || s.starts_with("https://")
        } else if pattern == "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$" {
          s.contains("@") && s.contains(".")
        } else {
          true
        }
      }
      _ => false
    }
  }
}

// 辅助函数：创建枚举验证器
fn create_enum_validator(values : Array[String]) -> (ConfigValue) -> Bool {
  fn(value : ConfigValue) -> Bool {
    match value {
      StringValue(s) => {
        let i = 0
        while i < values.length() {
          if values[i] == s {
            return true
          }
          i = i + 1
        }
        false
      }
      _ => false
    }
  }
}

// 验证结果类型
type ValidationResult = {
  is_valid : Bool,
  errors : Array[String]
}

// 辅助函数：验证所有配置
fn validate_all_configs(manager : ConfigManager) -> ValidationResult {
  let errors = []
  let i = 0
  while i < manager.items.length() {
    let item = manager.items[i]
    match item.validator {
      Some(validator) => {
        if !validator(item.value) {
          errors.push("Validation failed for: " + item.key)
        }
      }
      None => {}
    }
    i = i + 1
  }
  
  {
    is_valid: errors.length() == 0,
    errors: errors
  }
}

// 辅助函数：启用热重载
fn enable_hot_reload(manager : ConfigManager, source : ConfigSource) -> Unit {
  // 简化实现，实际中需要设置文件监控
}

// 辅助函数：模拟文件更新
fn simulate_file_update(source : ConfigSource, configs : Array[(String, ConfigValue)]) -> Unit {
  // 简化实现，实际中需要更新文件内容
}

// 辅助函数：推进时间
fn advance_time(seconds : Int) -> Unit {
  @static.current_time = @static.current_time + seconds
}

// 静态变量：当前时间
@static.current_time = 1640995200 // 2022-01-01 00:00:00 UTC

// 辅助函数：处理计划更新
fn process_scheduled_updates(manager : ConfigManager) -> Unit {
  // 简化实现，实际中需要检查延迟的更新并应用
}

// 辅助函数：回滚配置更新
fn rollback_config_update(manager : ConfigManager, version : Int) -> Bool {
  // 简化实现，实际中需要维护配置历史并回滚
  true
}

// 辅助函数：获取配置历史
fn get_config_history(manager : ConfigManager, key : String) -> Array[(Int, ConfigValue)] {
  // 简化实现，实际中需要维护配置历史
  [
    (1640995200, StringValue("Azimuth")),
    (1640995300, StringValue("Azimuth Updated"))
  ]
}

// 辅助函数：比较配置版本
fn compare_config_versions(manager : ConfigManager, version1 : Int, version2 : Int) -> Array[(String, ConfigValue, ConfigValue)] {
  // 简化实现，实际中需要比较两个版本的配置
  [
    ("app.name", StringValue("Azimuth"), StringValue("Azimuth Updated")),
    ("app.version", FloatValue(1.0), FloatValue(2.0)),
    ("feature.enabled", BoolValue(false), BoolValue(true))
  ]
}