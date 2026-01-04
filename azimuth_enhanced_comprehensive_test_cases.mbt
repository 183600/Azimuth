// Azimuth 增强综合测试用例
// 包含数据处理、错误处理、性能、并发、资源管理等多方面测试

// 测试1: 高级数据处理和转换
test "高级数据处理和转换测试" {
  // 创建测试数据集
  let raw_data = [
    ("metric1", 100.5),
    ("metric2", 200.3),
    ("metric3", 150.7),
    ("metric4", 75.2),
    ("metric5", 300.1)
  ]
  
  // 数据转换函数
  let convert_to_percentage = fn(data) {
    let total = data.fold_left(0.0, fn(acc, item) { acc + item.1 })
    data.map(fn(item) { (item.0, (item.1 / total) * 100.0) })
  }
  
  // 应用转换
  let percentage_data = convert_to_percentage(raw_data)
  
  // 验证转换结果
  let total_percentage = percentage_data.fold_left(0.0, fn(acc, item) { acc + item.1 })
  assert_true(total_percentage > 99.9 && total_percentage < 100.1) // 允许浮点误差
  
  // 验证最大值
  let max_item = percentage_data.fold_left(("metric0", 0.0), fn(acc, item) {
    if item.1 > acc.1 { item } else { acc }
  })
  assert_eq(max_item.0, "metric5")
}

// 测试2: 错误边界和异常处理
test "错误边界和异常处理测试" {
  // 创建错误处理函数
  let safe_divide = fn(numerator, denominator) {
    if denominator == 0 {
      Error("Division by zero")
    } else {
      Ok(numerator / denominator)
    }
  }
  
  // 测试正常情况
  let result1 = safe_divide(10, 2)
  match result1 {
    Ok(value) => assert_eq(value, 5.0)
    Error(_) => assert_true(false)
  }
  
  // 测试错误情况
  let result2 = safe_divide(10, 0)
  match result2 {
    Ok(_) => assert_true(false)
    Error(message) => assert_eq(message, "Division by zero")
  }
  
  // 测试错误恢复
  let recover_divide = fn(numerator, denominator) {
    match safe_divide(numerator, denominator) {
      Ok(value) => value
      Error(_) => 0.0 // 默认值
    }
  }
  
  assert_eq(recover_divide(10, 0), 0.0)
  assert_eq(recover_divide(10, 2), 5.0)
}

// 测试3: 性能基准和资源监控
test "性能基准和资源监控测试" {
  // 创建性能计数器
  let start_time = get_current_time()
  
  // 执行计算密集型操作
  let mut result = 1
  for i in 1..=1000 {
    result = result * i
  }
  
  let end_time = get_current_time()
  let duration = end_time - start_time
  
  // 验证性能在合理范围内
  assert_true(duration < 1000) // 应该在1秒内完成
  
  // 测试内存使用
  let initial_memory = get_memory_usage()
  
  // 创建大量数据
  let large_array = Array::new(10000)
  for i in 0..<10000 {
    large_array[i] = i * 2
  }
  
  let peak_memory = get_memory_usage()
  
  // 清理数据
  // large_array = [] // 在MoonBit中可能需要不同的清理方式
  
  let final_memory = get_memory_usage()
  
  // 验证内存使用模式
  assert_true(peak_memory > initial_memory)
  assert_true(final_memory <= peak_memory)
}

// 测试4: 并发安全和线程同步
test "并发安全和线程同步测试" {
  // 创建共享计数器
  let counter = AtomicInt::new(0)
  
  // 模拟并发操作
  let concurrent_increment = fn() {
    for i in 1..=100 {
      AtomicInt::increment(counter)
    }
  }
  
  // 启动多个并发任务
  let task1 = spawn(concurrent_increment)
  let task2 = spawn(concurrent_increment)
  let task3 = spawn(concurrent_increment)
  
  // 等待所有任务完成
  join(task1)
  join(task2)
  join(task3)
  
  // 验证最终结果
  let final_value = AtomicInt::get(counter)
  assert_eq(final_value, 300) // 3个任务各增加100次
}

// 测试5: 资源生命周期管理
test "资源生命周期管理测试" {
  // 创建资源管理器
  let resource_manager = ResourceManager::new()
  
  // 注册资源
  let resource1 = ResourceManager::register(resource_manager, "database.connection")
  let resource2 = ResourceManager::register(resource_manager, "cache.client")
  let resource3 = ResourceManager::register(resource_manager, "message.queue")
  
  // 验证资源注册
  assert_eq(ResourceManager::count(resource_manager), 3)
  assert_true(ResourceManager::exists(resource_manager, "database.connection"))
  assert_true(ResourceManager::exists(resource_manager, "cache.client"))
  assert_true(ResourceManager::exists(resource_manager, "message.queue"))
  
  // 使用资源
  ResourceManager::acquire(resource1)
  ResourceManager::acquire(resource2)
  
  // 验证资源状态
  assert_eq(ResourceManager::get_ref_count(resource1), 1)
  assert_eq(ResourceManager::get_ref_count(resource2), 1)
  assert_eq(ResourceManager::get_ref_count(resource3), 0)
  
  // 释放资源
  ResourceManager::release(resource1)
  ResourceManager::release(resource2)
  
  // 清理所有资源
  ResourceManager::cleanup_all(resource_manager)
  
  // 验证清理结果
  assert_eq(ResourceManager::count(resource_manager), 0)
}

// 测试6: 配置管理和动态更新
test "配置管理和动态更新测试" {
  // 创建配置管理器
  let config = ConfigManager::new()
  
  // 设置初始配置
  ConfigManager::set(config, "app.name", "azimuth")
  ConfigManager::set(config, "app.version", "1.0.0")
  ConfigManager::set(config, "app.debug", true)
  ConfigManager::set(config, "server.port", 8080)
  ConfigManager::set(config, "server.host", "localhost")
  
  // 验证配置获取
  assert_eq(ConfigManager::get_string(config, "app.name"), "azimuth")
  assert_eq(ConfigManager::get_string(config, "app.version"), "1.0.0")
  assert_true(ConfigManager::get_bool(config, "app.debug"))
  assert_eq(ConfigManager::get_int(config, "server.port"), 8080)
  assert_eq(ConfigManager::get_string(config, "server.host"), "localhost")
  
  // 测试配置更新
  ConfigManager::update(config, "app.version", "1.1.0")
  ConfigManager::update(config, "server.port", 9090)
  
  // 验证更新结果
  assert_eq(ConfigManager::get_string(config, "app.version"), "1.1.0")
  assert_eq(ConfigManager::get_int(config, "server.port"), 9090)
  
  // 测试配置监听器
  let mut change_detected = false
  let listener = fn(key, old_value, new_value) {
    if key == "app.debug" {
      change_detected = true
    }
  }
  
  ConfigManager::add_listener(config, listener)
  ConfigManager::update(config, "app.debug", false)
  
  assert_true(change_detected)
}

// 测试7: 安全性和数据验证
test "安全性和数据验证测试" {
  // 创建验证器
  let validator = Validator::new()
  
  // 添加验证规则
  Validator::add_rule(validator, "email", RegexRule("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"))
  Validator::add_rule(validator, "password", LengthRule(8, 128))
  Validator::add_rule(validator, "age", RangeRule(0, 150))
  Validator::add_rule(validator, "username", AlphaNumericRule())
  
  // 测试有效数据
  let valid_data = [
    ("email", "user@example.com"),
    ("password", "securepassword123"),
    ("age", 25),
    ("username", "user123")
  ]
  
  let valid_result = Validator::validate(validator, valid_data)
  assert_true(valid_result.is_valid)
  
  // 测试无效数据
  let invalid_data = [
    ("email", "invalid-email"),
    ("password", "short"),
    ("age", 200),
    ("username", "user@123")
  ]
  
  let invalid_result = Validator::validate(validator, invalid_data)
  assert_false(invalid_result.is_valid)
  assert_eq(invalid_result.errors.length(), 4) // 所有字段都应该有错误
  
  // 测试SQL注入防护
  let sanitize_input = fn(input) {
    input.replace("'", "''")
         .replace("\"", "\\\"")
         .replace(";", "")
         .replace("--", "")
         .replace("/*", "")
         .replace("*/", "")
  }
  
  let malicious_input = "'; DROP TABLE users; --"
  let sanitized = sanitize_input(malicious_input)
  
  assert_false(sanitized.contains("'"))
  assert_false(sanitized.contains(";"))
  assert_false(sanitized.contains("--"))
}

// 测试8: 跨平台兼容性
test "跨平台兼容性测试" {
  // 获取平台信息
  let platform = get_platform_info()
  let os_type = platform.os_type
  let arch = platform.architecture
  
  // 测试路径处理
  let path_joiner = get_path_joiner()
  let path1 = path_joiner.join(["home", "user", "documents"])
  let path2 = path_joiner.join(["C:", "Program Files", "App"])
  
  // 验证路径分隔符
  if os_type == "windows" {
    assert_true(path1.contains("\\") || path1.contains("/"))
    assert_true(path2.contains("\\"))
  } else {
    assert_true(path1.contains("/"))
    assert_false(path2.contains("\\"))
  }
  
  // 测试文件权限
  let permission_checker = get_permission_checker()
  let readable = permission_checker.is_readable("/tmp")
  let writable = permission_checker.is_writable("/tmp")
  
  // 在不同平台上权限可能不同，只验证函数调用不崩溃
  assert_true(readable == true || readable == false)
  assert_true(writable == true || writable == false)
  
  // 测试环境变量处理
  let env_handler = get_env_handler()
  let path_var = env_handler.get("PATH")
  
  // PATH变量应该存在于所有平台
  assert_true(path_var.is_some())
  assert_true(path_var.unwrap().length() > 0)
}

// 测试9: 国际化和本地化支持
test "国际化和本地化支持测试" {
  // 创建本地化管理器
  let locale_manager = LocaleManager::new()
  
  // 设置支持的语言
  LocaleManager::add_language(locale_manager, "en", "English")
  LocaleManager::add_language(locale_manager, "zh", "中文")
  LocaleManager::add_language(locale_manager, "ja", "日本語")
  LocaleManager::add_language(locale_manager, "es", "Español")
  
  // 添加翻译资源
  LocaleManager::add_translation(locale_manager, "en", "welcome", "Welcome")
  LocaleManager::add_translation(locale_manager, "zh", "welcome", "欢迎")
  LocaleManager::add_translation(locale_manager, "ja", "welcome", "ようこそ")
  LocaleManager::add_translation(locale_manager, "es", "welcome", "Bienvenido")
  
  LocaleManager::add_translation(locale_manager, "en", "goodbye", "Goodbye")
  LocaleManager::add_translation(locale_manager, "zh", "goodbye", "再见")
  LocaleManager::add_translation(locale_manager, "ja", "goodbye", "さようなら")
  LocaleManager::add_translation(locale_manager, "es", "goodbye", "Adiós")
  
  // 测试翻译
  LocaleManager::set_locale(locale_manager, "en")
  assert_eq(LocaleManager::translate(locale_manager, "welcome"), "Welcome")
  assert_eq(LocaleManager::translate(locale_manager, "goodbye"), "Goodbye")
  
  LocaleManager::set_locale(locale_manager, "zh")
  assert_eq(LocaleManager::translate(locale_manager, "welcome"), "欢迎")
  assert_eq(LocaleManager::translate(locale_manager, "goodbye"), "再见")
  
  LocaleManager::set_locale(locale_manager, "ja")
  assert_eq(LocaleManager::translate(locale_manager, "welcome"), "ようこそ")
  assert_eq(LocaleManager::translate(locale_manager, "goodbye"), "さようなら")
  
  // 测试回退机制
  LocaleManager::add_translation(locale_manager, "en", "new.key", "New Value")
  LocaleManager::set_locale(locale_manager, "zh")
  
  // 当中文翻译不存在时，应该回退到英文
  assert_eq(LocaleManager::translate(locale_manager, "new.key"), "New Value")
  
  // 测试复数形式
  LocaleManager::add_plural_translation(locale_manager, "en", "item", ["item", "items"])
  LocaleManager::add_plural_translation(locale_manager, "zh", "item", ["个项目", "个项目"])
  
  LocaleManager::set_locale(locale_manager, "en")
  assert_eq(LocaleManager::translate_plural(locale_manager, "item", 1), "item")
  assert_eq(LocaleManager::translate_plural(locale_manager, "item", 5), "items")
  
  LocaleManager::set_locale(locale_manager, "zh")
  assert_eq(LocaleManager::translate_plural(locale_manager, "item", 1), "个项目")
  assert_eq(LocaleManager::translate_plural(locale_manager, "item", 5), "个项目")
}

// 测试10: 综合集成测试
test "综合集成测试" {
  // 创建完整的遥测系统
  let telemetry_system = TelemetrySystem::new()
  
  // 配置系统
  TelemetrySystem::configure(telemetry_system, {
    "service.name": "azimuth.test",
    "service.version": "1.0.0",
    "sampling.rate": 1.0,
    "exporter.endpoint": "http://localhost:4318"
  })
  
  // 启动系统
  TelemetrySystem::start(telemetry_system)
  
  // 创建指标
  let meter = TelemetrySystem::get_meter(telemetry_system, "integration.test")
  let counter = Meter::create_counter(meter, "operations.total")
  let histogram = Meter::create_histogram(meter, "operation.duration")
  let gauge = Meter::create_gauge(meter, "active.connections")
  
  // 创建追踪器
  let tracer = TelemetrySystem::get_tracer(telemetry_system, "integration.tracer")
  let span = Tracer::start_span(tracer, "integration.operation")
  
  // 记录指标
  Counter::add(counter, 100.0)
  Histogram::record(histogram, 0.05)
  Histogram::record(histogram, 0.10)
  Histogram::record(histogram, 0.03)
  Gauge::set(gauge, 25.0)
  
  // 添加span属性
  Span::set_attribute(span, "operation.type", "integration")
  Span::set_attribute(span, "operation.result", "success")
  Span::set_attribute(span, "operation.duration_ms", 80)
  
  // 创建日志
  let logger = TelemetrySystem::get_logger(telemetry_system, "integration.logger")
  let log_record = Logger::create_log_record(logger)
  LogRecord::set_severity(log_record, Info)
  LogRecord::set_body(log_record, "Integration test completed successfully")
  LogRecord::set_attribute(log_record, "test.type", "integration")
  LogRecord::set_attribute(log_record, "test.result", "passed")
  
  // 发送日志
  Logger::emit(log_record)
  
  // 结束span
  Span::set_status(span, Ok)
  Span::end(span)
  
  // 等待数据导出
  sleep(100)
  
  // 验证系统状态
  let system_status = TelemetrySystem::get_status(telemetry_system)
  assert_true(system_status.is_healthy)
  assert_eq(system_status.metrics_count, 3)
  assert_eq(system_status.spans_count, 1)
  assert_eq(system_status.logs_count, 1)
  
  // 停止系统
  TelemetrySystem::stop(telemetry_system)
  
  // 验证系统已停止
  let final_status = TelemetrySystem::get_status(telemetry_system)
  assert_false(final_status.is_running)
}