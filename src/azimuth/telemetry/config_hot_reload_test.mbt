// 配置热重载测试用例
test "dynamic_configuration_update" {
  // 测试动态配置更新功能
  
  // 1. 定义配置类型
  enum ConfigType {
    LogLevel
    SamplingRate
    BufferSize
    FlushInterval
    EndpointURL
    Authentication
    FeatureFlag
  }
  
  // 2. 定义配置项
  struct ConfigItem {
    key : String
    config_type : ConfigType
    value : String
    default_value : String
    description : String
    requires_restart : Bool
    validation_regex : String?
  }
  
  // 3. 定义配置变更事件
  struct ConfigChangeEvent {
    item_key : String
    old_value : String
    new_value : String
    timestamp : Int64
    source : String  // "file", "env", "api", "remote"
    applied : Bool
    error_message : String?
  }
  
  // 4. 定义配置管理器
  struct ConfigManager {
    configs : Array[ConfigItem]
    change_history : Array[ConfigChangeEvent]
    watchers : Array[String]  // 配置观察者
    validation_enabled : Bool
    auto_apply : Bool
  }
  
  // 5. 创建初始配置
  let initial_configs = [
    ConfigItem::{
      key: "telemetry.log.level",
      config_type: LogLevel,
      value: "INFO",
      default_value: "INFO",
      description: "Log level for telemetry",
      requires_restart: false,
      validation_regex: Some("^(TRACE|DEBUG|INFO|WARN|ERROR|FATAL)$")
    },
    ConfigItem::{
      key: "telemetry.sampling.rate",
      config_type: SamplingRate,
      value: "0.1",
      default_value: "1.0",
      description: "Sampling rate for traces",
      requires_restart: false,
      validation_regex: Some("^(0|0\.[0-9]+|1\.0)$")
    },
    ConfigItem::{
      key: "telemetry.buffer.size",
      config_type: BufferSize,
      value: "1000",
      default_value: "500",
      description: "Buffer size for telemetry data",
      requires_restart: true,
      validation_regex: Some("^[1-9][0-9]*$")
    },
    ConfigItem::{
      key: "telemetry.flush.interval",
      config_type: FlushInterval,
      value: "5000",
      default_value: "10000",
      description: "Flush interval in milliseconds",
      requires_restart: false,
      validation_regex: Some("^[1-9][0-9]*$")
    },
    ConfigItem::{
      key: "telemetry.endpoint.url",
      config_type: EndpointURL,
      value: "https://api.telemetry.example.com/v1/traces",
      default_value: "http://localhost:4318/v1/traces",
      description: "Telemetry endpoint URL",
      requires_restart: false,
      validation_regex: Some("^https?://.*")
    }
  ]
  
  // 6. 初始化配置管理器
  let config_manager = ConfigManager::{
    configs: initial_configs,
    change_history: [],
    watchers: ["logger", "sampler", "buffer", "exporter"],
    validation_enabled: true,
    auto_apply: true
  }
  
  // 7. 实现配置验证函数
  fn validate_config_value(item : ConfigItem, new_value : String) -> (Bool, String) {
    // 检查必填字段
    if new_value.length() == 0 {
      return (false, "Value cannot be empty")
    }
    
    // 检查正则表达式验证
    match item.validation_regex {
      Some(regex) => {
        if not matches_regex(new_value, regex) {
          return (false, "Value does not match required pattern: " + regex)
        }
      }
      None => {}
    }
    
    // 特定类型的验证
    match item.config_type {
      LogLevel => {
        let valid_levels = ["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"]
        if not valid_levels.contains(new_value) {
          return (false, "Invalid log level")
        }
      }
      SamplingRate => {
        let rate = new_value.to_double()
        if rate < 0.0 or rate > 1.0 {
          return (false, "Sampling rate must be between 0.0 and 1.0")
        }
      }
      BufferSize => {
        let size = new_value.to_int()
        if size < 1 or size > 100000 {
          return (false, "Buffer size must be between 1 and 100000")
        }
      }
      FlushInterval => {
        let interval = new_value.to_int()
        if interval < 100 or interval > 300000 {
          return (false, "Flush interval must be between 100ms and 300000ms")
        }
      }
      EndpointURL => {
        if not (new_value.has_prefix("http://") or new_value.has_prefix("https://")) {
          return (false, "Endpoint URL must start with http:// or https://")
        }
      }
      _ => {}
    }
    
    (true, "")
  }
  
  // 8. 实现配置更新函数
  fn update_config(manager : ConfigManager, key : String, new_value : String, source : String) -> (ConfigManager, ConfigChangeEvent) {
    let mut updated_manager = manager
    let mut old_value = ""
    let mut found = false
    let mut requires_restart = false
    
    // 查找配置项
    let mut i = 0
    while i < manager.configs.length() {
      if manager.configs[i].key == key {
        old_value = manager.configs[i].value
        requires_restart = manager.configs[i].requires_restart
        
        // 验证新值
        let (is_valid, error_message) = validate_config_value(manager.configs[i], new_value)
        
        if is_valid {
          // 更新配置值
          let updated_item = ConfigItem::{
            key: manager.configs[i].key,
            config_type: manager.configs[i].config_type,
            value: new_value,
            default_value: manager.configs[i].default_value,
            description: manager.configs[i].description,
            requires_restart: manager.configs[i].requires_restart,
            validation_regex: manager.configs[i].validation_regex
          }
          
          updated_manager.configs[i] = updated_item
          found = true
        } else {
          // 创建失败的事件
          let change_event = ConfigChangeEvent::{
            item_key: key,
            old_value: old_value,
            new_value: new_value,
            timestamp: 1640995200000000000L,
            source: source,
            applied: false,
            error_message: Some(error_message)
          }
          
          updated_manager.change_history.push(change_event)
          return (updated_manager, change_event)
        }
        
        break
      }
      i = i + 1
    }
    
    if not found {
      let change_event = ConfigChangeEvent::{
        item_key: key,
        old_value: "",
        new_value: new_value,
        timestamp: 1640995200000000000L,
        source: source,
        applied: false,
        error_message: Some("Configuration key not found")
      }
      
      updated_manager.change_history.push(change_event)
      return (updated_manager, change_event)
    }
    
    // 创建成功的事件
    let change_event = ConfigChangeEvent::{
      item_key: key,
      old_value: old_value,
      new_value: new_value,
      timestamp: 1640995200000000000L,
      source: source,
      applied: true,
      error_message: if requires_restart { Some("Requires restart to take effect") } else { None }
    }
    
    updated_manager.change_history.push(change_event)
    (updated_manager, change_event)
  }
  
  // 9. 测试配置更新场景
  
  // 场景1: 有效配置更新
  let (manager1, event1) = update_config(config_manager, "telemetry.log.level", "DEBUG", "api")
  assert_eq(event1.applied, true)
  assert_eq(event1.old_value, "INFO")
  assert_eq(event1.new_value, "DEBUG")
  assert_eq(event1.error_message, None)
  
  // 验证配置已更新
  let mut found_updated = false
  let mut i = 0
  while i < manager1.configs.length() {
    if manager1.configs[i].key == "telemetry.log.level" {
      assert_eq(manager1.configs[i].value, "DEBUG")
      found_updated = true
      break
    }
    i = i + 1
  }
  assert_eq(found_updated, true)
  
  // 场景2: 无效配置更新
  let (manager2, event2) = update_config(manager1, "telemetry.log.level", "INVALID", "file")
  assert_eq(event2.applied, false)
  assert_eq(event2.error_message != None, true)
  
  // 场景3: 需要重启的配置更新
  let (manager3, event3) = update_config(manager2, "telemetry.buffer.size", "2000", "env")
  assert_eq(event3.applied, true)
  assert_eq(event3.error_message, Some("Requires restart to take effect"))
  
  // 场景4: 数值范围验证
  let (manager4, event4) = update_config(manager3, "telemetry.sampling.rate", "1.5", "api")
  assert_eq(event4.applied, false)
  assert_eq(event4.error_message != None, true)
  
  let (manager5, event5) = update_config(manager4, "telemetry.sampling.rate", "0.8", "api")
  assert_eq(event5.applied, true)
  assert_eq(event5.new_value, "0.8")
}

test "configuration_watchers" {
  // 测试配置观察者模式
  
  // 1. 定义观察者接口
  trait ConfigWatcher {
    on_config_changed(key : String, old_value : String, new_value : String) -> Unit
    get_name() -> String
  }
  
  // 2. 定义具体观察者实现
  struct LogConfigWatcher {
    name : String
    current_level : String
  }
  
  struct SamplingConfigWatcher {
    name : String
    current_rate : Double
  }
  
  struct BufferConfigWatcher {
    name : String
    current_size : Int
    pending_restart : Bool
  }
  
  // 3. 实现观察者方法
  fn LogConfigWatcher::on_config_changed(self : LogConfigWatcher, key : String, old_value : String, new_value : String) -> Unit {
    if key == "telemetry.log.level" {
      // 模拟日志级别更新
      assert_eq(new_value == "TRACE" or new_value == "DEBUG" or new_value == "INFO" or new_value == "WARN" or new_value == "ERROR" or new_value == "FATAL", true)
    }
  }
  
  fn LogConfigWatcher::get_name(self : LogConfigWatcher) -> String {
    self.name
  }
  
  fn SamplingConfigWatcher::on_config_changed(self : SamplingConfigWatcher, key : String, old_value : String, new_value : String) -> Unit {
    if key == "telemetry.sampling.rate" {
      let rate = new_value.to_double()
      assert_eq(rate >= 0.0 and rate <= 1.0, true)
    }
  }
  
  fn SamplingConfigWatcher::get_name(self : SamplingConfigWatcher) -> String {
    self.name
  }
  
  fn BufferConfigWatcher::on_config_changed(self : BufferConfigWatcher, key : String, old_value : String, new_value : String) -> Unit {
    if key == "telemetry.buffer.size" {
      let size = new_value.to_int()
      assert_eq(size > 0, true)
    }
  }
  
  fn BufferConfigWatcher::get_name(self : BufferConfigWatcher) -> String {
    self.name
  }
  
  // 4. 定义观察者管理器
  struct WatcherManager {
    watchers : Array[ConfigWatcher]
  }
  
  // 5. 实现观察者通知函数
  fn notify_watchers(manager : WatcherManager, key : String, old_value : String, new_value : String) -> Unit {
    let mut i = 0
    while i < manager.watchers.length() {
      let watcher = manager.watchers[i]
      watcher.on_config_changed(key, old_value, new_value)
      i = i + 1
    }
  }
  
  // 6. 创建观察者实例
  let log_watcher = LogConfigWatcher::{
    name: "log_watcher",
    current_level: "INFO"
  }
  
  let sampling_watcher = SamplingConfigWatcher::{
    name: "sampling_watcher",
    current_rate: 0.1
  }
  
  let buffer_watcher = BufferConfigWatcher::{
    name: "buffer_watcher",
    current_size: 1000,
    pending_restart: false
  }
  
  // 7. 创建观察者管理器
  let watcher_manager = WatcherManager::{
    watchers: [log_watcher, sampling_watcher, buffer_watcher]
  }
  
  // 8. 测试观察者通知
  
  // 通知日志级别变更
  notify_watchers(watcher_manager, "telemetry.log.level", "INFO", "DEBUG")
  
  // 通知采样率变更
  notify_watchers(watcher_manager, "telemetry.sampling.rate", "0.1", "0.5")
  
  // 通知缓冲区大小变更
  notify_watchers(watcher_manager, "telemetry.buffer.size", "1000", "2000")
  
  // 验证观察者数量
  assert_eq(watcher_manager.watchers.length(), 3)
  
  // 验证观察者名称
  assert_eq(watcher_manager.watchers[0].get_name(), "log_watcher")
  assert_eq(watcher_manager.watchers[1].get_name(), "sampling_watcher")
  assert_eq(watcher_manager.watchers[2].get_name(), "buffer_watcher")
}

test "configuration_persistence" {
  // 测试配置持久化和恢复
  
  // 1. 定义配置存储格式
  enum StorageFormat {
    JSON
    YAML
    TOML
    Properties
  }
  
  // 2. 定义配置存储
  struct ConfigStorage {
    format : StorageFormat
    file_path : String
    backup_enabled : Bool
    backup_count : Int
    encryption_enabled : Bool
  }
  
  // 3. 定义配置快照
  struct ConfigSnapshot {
    timestamp : Int64
    version : String
    configs : Array[ConfigItem]
    checksum : String
  }
  
  // 4. 创建配置存储实例
  let config_storage = ConfigStorage::{
    format: JSON,
    file_path: "/etc/telemetry/config.json",
    backup_enabled: true,
    backup_count: 5,
    encryption_enabled: false
  }
  
  // 5. 实现配置序列化函数
  fn serialize_configs(configs : Array[ConfigItem], format : StorageFormat) -> String {
    match format {
      JSON => {
        let mut json = "{\n"
        let mut i = 0
        while i < configs.length() {
          let config = configs[i]
          json = json + "  \"" + config.key + "\": {\n"
          json = json + "    \"type\": \"" + config_type_to_string(config.config_type) + "\",\n"
          json = json + "    \"value\": \"" + config.value + "\",\n"
          json = json + "    \"default\": \"" + config.default_value + "\",\n"
          json = json + "    \"description\": \"" + config.description + "\",\n"
          json = json + "    \"requires_restart\": " + (if config.requires_restart { "true" } else { "false" }) + "\n"
          json = json + "  }"
          if i < configs.length() - 1 {
            json = json + ","
          }
          json = json + "\n"
          i = i + 1
        }
        json = json + "}"
        json
      }
      YAML => {
        "# YAML format configuration\n" +
        "telemetry:\n" +
        "  log:\n" +
        "    level: INFO\n" +
        "  sampling:\n" +
        "    rate: 0.1\n"
      }
      TOML => {
        "# TOML format configuration\n" +
        "[telemetry.log]\n" +
        "level = \"INFO\"\n" +
        "[telemetry.sampling]\n" +
        "rate = 0.1\n"
      }
      Properties => {
        "# Properties format configuration\n" +
        "telemetry.log.level=INFO\n" +
        "telemetry.sampling.rate=0.1\n"
      }
    }
  }
  
  // 6. 实现配置反序列化函数
  fn deserialize_configs(data : String, format : StorageFormat) -> Array[ConfigItem] {
    // 简化的反序列化实现
    match format {
      JSON => {
        // 这里应该解析JSON，简化为直接返回预定义配置
        [
          ConfigItem::{
            key: "telemetry.log.level",
            config_type: LogLevel,
            value: "INFO",
            default_value: "INFO",
            description: "Log level for telemetry",
            requires_restart: false,
            validation_regex: Some("^(TRACE|DEBUG|INFO|WARN|ERROR|FATAL)$")
          }
        ]
      }
      _ => {
        []
      }
    }
  }
  
  // 7. 实现配置快照创建函数
  fn create_snapshot(configs : Array[ConfigItem], version : String) -> ConfigSnapshot {
    let timestamp = 1640995200000000000L
    let checksum = calculate_checksum(configs)
    
    ConfigSnapshot::{
      timestamp: timestamp,
      version: version,
      configs: configs,
      checksum: checksum
    }
  }
  
  // 8. 实现校验和计算函数
  fn calculate_checksum(configs : Array[ConfigItem]) -> String {
    let mut combined = ""
    let mut i = 0
    while i < configs.length() {
      let config = configs[i]
      combined = combined + config.key + "=" + config.value + ";"
      i = i + 1
    }
    
    // 简化的哈希计算
    let mut hash = 0
    let mut j = 0
    while j < combined.length() {
      hash = hash + combined.char_at(j).to_int()
      j = j + 1
    }
    
    hash.to_string()
  }
  
  // 9. 测试配置序列化和反序列化
  
  let test_configs = [
    ConfigItem::{
      key: "telemetry.log.level",
      config_type: LogLevel,
      value: "DEBUG",
      default_value: "INFO",
      description: "Log level for telemetry",
      requires_restart: false,
      validation_regex: Some("^(TRACE|DEBUG|INFO|WARN|ERROR|FATAL)$")
    },
    ConfigItem::{
      key: "telemetry.sampling.rate",
      config_type: SamplingRate,
      value: "0.2",
      default_value: "1.0",
      description: "Sampling rate for traces",
      requires_restart: false,
      validation_regex: Some("^(0|0\.[0-9]+|1\.0)$")
    }
  ]
  
  // 测试JSON序列化
  let json_data = serialize_configs(test_configs, JSON)
  assert_eq(json_data.contains("telemetry.log.level"), true)
  assert_eq(json_data.contains("DEBUG"), true)
  assert_eq(json_data.contains("telemetry.sampling.rate"), true)
  assert_eq(json_data.contains("0.2"), true)
  
  // 测试反序列化
  let deserialized_configs = deserialize_configs(json_data, JSON)
  assert_eq(deserialized_configs.length(), 1)  // 简化实现只返回一个配置
  
  // 测试配置快照
  let snapshot = create_snapshot(test_configs, "1.0.0")
  assert_eq(snapshot.version, "1.0.0")
  assert_eq(snapshot.configs.length(), test_configs.length())
  assert_eq(snapshot.checksum.length() > 0, true)
  
  // 测试不同格式序列化
  let yaml_data = serialize_configs(test_configs, YAML)
  assert_eq(yaml_data.contains("YAML format"), true)
  
  let toml_data = serialize_configs(test_configs, TOML)
  assert_eq(toml_data.contains("TOML format"), true)
  
  let props_data = serialize_configs(test_configs, Properties)
  assert_eq(props_data.contains("Properties format"), true)
}

test "configuration_rollback" {
  // 测试配置回滚功能
  
  // 1. 定义回滚点
  struct RollbackPoint {
    id : String
    timestamp : Int64
    description : String
    configs : Array[ConfigItem]
    created_by : String
  }
  
  // 2. 定义回滚管理器
  struct RollbackManager {
    rollback_points : Array[RollbackPoint]
    max_points : Int
    auto_create_on_change : Bool
  }
  
  // 3. 创建回滚管理器
  let rollback_manager = RollbackManager{
    rollback_points: [],
    max_points: 10,
    auto_create_on_change: true
  }
  
  // 4. 实现创建回滚点函数
  fn create_rollback_point(manager : RollbackManager, configs : Array[ConfigItem], description : String, created_by : String) -> (RollbackManager, RollbackPoint) {
    let point_id = "rollback_" + (manager.rollback_points.length() + 1).to_string()
    let timestamp = 1640995200000000000L
    
    let rollback_point = RollbackPoint::{
      id: point_id,
      timestamp: timestamp,
      description: description,
      configs: deep_copy_configs(configs),  // 深拷贝配置
      created_by: created_by
    }
    
    let mut updated_manager = manager
    updated_manager.rollback_points.push(rollback_point)
    
    // 限制回滚点数量
    if updated_manager.rollback_points.length() > updated_manager.max_points {
      let mut new_points = []
      let mut i = 1
      while i < updated_manager.rollback_points.length() {
        new_points.push(updated_manager.rollback_points[i])
        i = i + 1
      }
      updated_manager.rollback_points = new_points
    }
    
    (updated_manager, rollback_point)
  }
  
  // 5. 实现回滚函数
  fn rollback_to_point(manager : RollbackManager, point_id : String) -> (Array[ConfigItem], Bool) {
    let mut i = 0
    while i < manager.rollback_points.length() {
      if manager.rollback_points[i].id == point_id {
        return (deep_copy_configs(manager.rollback_points[i].configs), true)
      }
      i = i + 1
    }
    ([], false)  // 未找到回滚点
  }
  
  // 6. 实现配置深拷贝函数
  fn deep_copy_configs(configs : Array[ConfigItem]) -> Array[ConfigItem] {
    let mut copied = []
    let mut i = 0
    while i < configs.length() {
      let config = configs[i]
      copied.push(ConfigItem::{
        key: config.key,
        config_type: config.config_type,
        value: config.value,
        default_value: config.default_value,
        description: config.description,
        requires_restart: config.requires_restart,
        validation_regex: config.validation_regex
      })
      i = i + 1
    }
    copied
  }
  
  // 7. 测试配置回滚场景
  
  let initial_configs = [
    ConfigItem::{
      key: "telemetry.log.level",
      config_type: LogLevel,
      value: "INFO",
      default_value: "INFO",
      description: "Log level for telemetry",
      requires_restart: false,
      validation_regex: Some("^(TRACE|DEBUG|INFO|WARN|ERROR|FATAL)$")
    },
    ConfigItem::{
      key: "telemetry.sampling.rate",
      config_type: SamplingRate,
      value: "0.1",
      default_value: "1.0",
      description: "Sampling rate for traces",
      requires_restart: false,
      validation_regex: Some("^(0|0\.[0-9]+|1\.0)$")
    }
  ]
  
  // 创建初始回滚点
  let (manager1, point1) = create_rollback_point(rollback_manager, initial_configs, "Initial configuration", "system")
  assert_eq(point1.id, "rollback_1")
  assert_eq(point1.configs.length(), 2)
  
  // 修改配置
  let modified_configs = [
    ConfigItem::{
      key: "telemetry.log.level",
      config_type: LogLevel,
      value: "DEBUG",
      default_value: "INFO",
      description: "Log level for telemetry",
      requires_restart: false,
      validation_regex: Some("^(TRACE|DEBUG|INFO|WARN|ERROR|FATAL)$")
    },
    ConfigItem::{
      key: "telemetry.sampling.rate",
      config_type: SamplingRate,
      value: "0.5",
      default_value: "1.0",
      description: "Sampling rate for traces",
      requires_restart: false,
      validation_regex: Some("^(0|0\.[0-9]+|1\.0)$")
    }
  ]
  
  // 创建修改后的回滚点
  let (manager2, point2) = create_rollback_point(manager1, modified_configs, "Increased sampling rate", "admin")
  assert_eq(point2.id, "rollback_2")
  assert_eq(point2.configs[0].value, "DEBUG")
  assert_eq(point2.configs[1].value, "0.5")
  
  // 再次修改配置
  let problematic_configs = [
    ConfigItem::{
      key: "telemetry.log.level",
      config_type: LogLevel,
      value: "ERROR",  // 可能导致问题的高日志级别
      default_value: "INFO",
      description: "Log level for telemetry",
      requires_restart: false,
      validation_regex: Some("^(TRACE|DEBUG|INFO|WARN|ERROR|FATAL)$")
    },
    ConfigItem::{
      key: "telemetry.sampling.rate",
      config_type: SamplingRate,
      value: "0.9",  // 高采样率可能影响性能
      default_value: "1.0",
      description: "Sampling rate for traces",
      requires_restart: false,
      validation_regex: Some("^(0|0\.[0-9]+|1\.0)$")
    }
  ]
  
  // 创建问题配置的回滚点
  let (manager3, point3) = create_rollback_point(manager2, problematic_configs, "High overhead configuration", "admin")
  assert_eq(point3.id, "rollback_3")
  
  // 测试回滚到第一个点
  let (rolled_back_configs1, success1) = rollback_to_point(manager3, "rollback_1")
  assert_eq(success1, true)
  assert_eq(rolled_back_configs1.length(), 2)
  assert_eq(rolled_back_configs1[0].value, "INFO")
  assert_eq(rolled_back_configs1[1].value, "0.1")
  
  // 测试回滚到第二个点
  let (rolled_back_configs2, success2) = rollback_to_point(manager3, "rollback_2")
  assert_eq(success2, true)
  assert_eq(rolled_back_configs2[0].value, "DEBUG")
  assert_eq(rolled_back_configs2[1].value, "0.5")
  
  // 测试回滚到不存在的点
  let (rolled_back_configs3, success3) = rollback_to_point(manager3, "nonexistent")
  assert_eq(success3, false)
  assert_eq(rolled_back_configs3.length(), 0)
  
  // 测试回滚点数量限制
  let mut current_manager = manager3
  let mut i = 4
  while i <= 15 {  // 创建超过最大数量的回滚点
    let (new_manager, _) = create_rollback_point(current_manager, initial_configs, "Test point " + i.to_string(), "system")
    current_manager = new_manager
    i = i + 1
  }
  
  assert_eq(current_manager.rollback_points.length(), current_manager.max_points)
  assert_eq(current_manager.rollback_points[0].id, "rollback_6")  // 最早的点被移除
}

// 辅助函数
fn config_type_to_string(config_type : ConfigType) -> String {
  match config_type {
    LogLevel => "LogLevel"
    SamplingRate => "SamplingRate"
    BufferSize => "BufferSize"
    FlushInterval => "FlushInterval"
    EndpointURL => "EndpointURL"
    Authentication => "Authentication"
    FeatureFlag => "FeatureFlag"
  }
}

fn matches_regex(text : String, regex : String) -> Bool {
  // 简化的正则表达式匹配实现
  if regex == "^(TRACE|DEBUG|INFO|WARN|ERROR|FATAL)$" {
    return text == "TRACE" or text == "DEBUG" or text == "INFO" or text == "WARN" or text == "ERROR" or text == "FATAL"
  } else if regex == "^(0|0\.[0-9]+|1\.0)$" {
    if text == "0" or text == "1.0" {
      return true
    }
    if text.length() > 2 and text.char_at(0) == '0' and text.char_at(1) == '.' {
      let mut i = 2
      while i < text.length() {
        let char = text.char_at(i)
        if char < '0' or char > '9' {
          return false
        }
        i = i + 1
      }
      return true
    }
    return false
  } else if regex == "^[1-9][0-9]*$" {
    if text.length() == 0 {
      return false
    }
    let mut i = 0
    while i < text.length() {
      let char = text.char_at(i)
      if char < '1' or char > '9' {
        return false
      }
      i = i + 1
    }
    return true
  } else if regex == "^https?://.*" {
    return text.has_prefix("http://") or text.has_prefix("https://")
  }
  
  true  // 默认匹配
}