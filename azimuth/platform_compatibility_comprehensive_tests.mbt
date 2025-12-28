// 平台兼容性测试用例
// 测试遥测系统在不同平台和环境下的兼容性

test "cross-platform timestamp handling" {
  // 测试跨平台时间戳处理
  
  let clock = azimuth::Clock::system()
  let timestamp = azimuth::Clock::now_unix_nanos(clock)
  
  // 验证时间戳格式（Unix纳秒时间戳）
  assert_true(timestamp > 0L, "Timestamp should be positive")
  assert_true(timestamp < 9223372036854775807L, "Timestamp should be within Int64 range")
  
  // 验证时间戳的合理性（2025年的时间戳范围）
  let min_2025 = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let max_2025 = 1767225599999999999L  // 2025-12-31 23:59:59 UTC
  
  assert_true(timestamp >= min_2025, "Timestamp should be within 2025")
  assert_true(timestamp <= max_2025, "Timestamp should be within 2025")
  
  // 测试时间戳的一致性
  let timestamp2 = azimuth::Clock::now_unix_nanos(clock)
  assert_true(timestamp2 >= timestamp, "Subsequent timestamps should be greater or equal")
  
  // 验证时间戳精度（纳秒级）
  let timestamp_diff = timestamp2 - timestamp
  assert_true(timestamp_diff >= 0L, "Time difference should be non-negative")
}

test "platform-specific resource attributes" {
  // 测试平台特定的资源属性
  
  let resource = azimuth::Resource::new()
  
  // 通用平台属性
  let common_attrs = [
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0")),
    ("telemetry.sdk.language", azimuth::StringValue("moonbit"))
  ]
  
  let resource_with_common = azimuth::Resource::with_attributes(resource, common_attrs)
  
  // 模拟不同平台的属性
  // Linux平台
  let linux_attrs = [
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("6.11.0-1018-azure")),
    ("os.description", azimuth::StringValue("Linux 6.11.0-1018-azure")),
    ("host.name", azimuth::StringValue("azure-vm")),
    ("host.arch", azimuth::StringValue("x86_64"))
  ]
  
  let linux_resource = azimuth::Resource::with_attributes(resource_with_common, linux_attrs)
  
  // 验证Linux平台属性
  assert_eq(azimuth::Resource::get_attribute(linux_resource, "os.type"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(linux_resource, "host.arch"), Some(azimuth::StringValue("x86_64")))
  
  // Windows平台（模拟）
  let windows_attrs = [
    ("os.type", azimuth::StringValue("windows")),
    ("os.version", azimuth::StringValue("10.0.19042")),
    ("os.description", azimuth::StringValue("Windows 10 Pro")),
    ("host.name", azimuth::StringValue("DESKTOP-ABC123")),
    ("host.arch", azimuth::StringValue("amd64"))
  ]
  
  let windows_resource = azimuth::Resource::with_attributes(resource_with_common, windows_attrs)
  
  // 验证Windows平台属性
  assert_eq(azimuth::Resource::get_attribute(windows_resource, "os.type"), Some(azimuth::StringValue("windows")))
  assert_eq(azimuth::Resource::get_attribute(windows_resource, "host.arch"), Some(azimuth::StringValue("amd64")))
  
  // macOS平台（模拟）
  let macos_attrs = [
    ("os.type", azimuth::StringValue("darwin")),
    ("os.version", azimuth::StringValue("14.2.1")),
    ("os.description", azimuth::StringValue("macOS Sonoma 14.2.1")),
    ("host.name", azimuth::StringValue("MacBook-Pro.local")),
    ("host.arch", azimuth::StringValue("arm64"))
  ]
  
  let macos_resource = azimuth::Resource::with_attributes(resource_with_common, macos_attrs)
  
  // 验证macOS平台属性
  assert_eq(azimuth::Resource::get_attribute(macos_resource, "os.type"), Some(azimuth::StringValue("darwin")))
  assert_eq(azimuth::Resource::get_attribute(macos_resource, "host.arch"), Some(azimuth::StringValue("arm64")))
}

test "platform-specific network configurations" {
  // 测试平台特定的网络配置
  
  // 创建HTTP客户端和请求，测试不同平台的网络行为
  let http_client = azimuth::HttpClient::new()
  
  // 模拟不同平台的HTTP请求头
  let linux_headers = [
    ("User-Agent", azimuth::StringValue("azimuth-telemetry/0.1.0 (Linux)")),
    ("Accept", azimuth::StringValue("application/json")),
    ("X-Platform", azimuth::StringValue("linux"))
  ]
  
  let linux_request = azimuth::HttpRequest::new(
    "GET", 
    "https://api.example.com/telemetry", 
    linux_headers
  )
  
  // 验证Linux请求
  assert_eq(azimuth::HttpRequest::http_method(linux_request), "GET")
  assert_eq(azimuth::HttpRequest::url(linux_request), "https://api.example.com/telemetry")
  
  // Windows平台请求（模拟）
  let windows_headers = [
    ("User-Agent", azimuth::StringValue("azimuth-telemetry/0.1.0 (Windows)")),
    ("Accept", azimuth::StringValue("application/json")),
    ("X-Platform", azimuth::StringValue("windows"))
  ]
  
  let windows_request = azimuth::HttpRequest::new(
    "POST", 
    "https://api.example.com/telemetry", 
    windows_headers,
    Some("platform-specific-data")
  )
  
  // 验证Windows请求
  assert_eq(azimuth::HttpRequest::http_method(windows_request), "POST")
  assert_eq(azimuth::HttpRequest::body(windows_request), Some("platform-specific-data"))
  
  // 模拟不同平台的HTTP响应
  let success_response = azimuth::HttpResponse::new(200, [
    ("Content-Type", azimuth::StringValue("application/json")),
    ("X-Response-Platform", azimuth::StringValue("cross-platform"))
  ], Some("{\"status\":\"success\"}"))
  
  // 验证响应
  assert_eq(azimuth::HttpResponse::status_code(success_response), 200)
  assert_eq(azimuth::HttpResponse::body(success_response), Some("{\"status\":\"success\"}"))
}

test "platform-specific file system paths" {
  // 测试平台特定的文件系统路径处理
  
  // Unix/Linux路径
  let unix_paths = [
    ("/var/log/telemetry/app.log", azimuth::StringValue("unix")),
    ("/opt/azimuth/config.yaml", azimuth::StringValue("unix")),
    ("/tmp/telemetry-cache", azimuth::StringValue("unix")),
    ("/home/user/.azimuth/metrics.db", azimuth::StringValue("unix"))
  ]
  
  // Windows路径
  let windows_paths = [
    ("C:\\Program Files\\Azimuth\\config.yaml", azimuth::StringValue("windows")),
    ("C:\\Users\\User\\AppData\\Local\\Azimuth\\logs\\app.log", azimuth::StringValue("windows")),
    ("D:\\TelemetryData\\metrics.db", azimuth::StringValue("windows")),
    ("C:\\Temp\\telemetry-cache", azimuth::StringValue("windows"))
  ]
  
  // 验证路径格式
  for path in unix_paths {
    assert_true(path.0.starts_with("/"), "Unix paths should start with /")
  }
  
  for path in windows_paths {
    assert_true(path.0.contains(":"), "Windows paths should contain drive letter")
    assert_true(path.0.contains("\\"), "Windows paths should use backslashes")
  }
  
  // 创建包含路径信息的属性
  let attrs = azimuth::Attributes::new()
  
  // 添加Unix路径
  azimuth::Attributes::set(attrs, "log.path.unix", azimuth::StringValue("/var/log/telemetry/app.log"))
  azimuth::Attributes::set(attrs, "config.path.unix", azimuth::StringValue("/opt/azimuth/config.yaml"))
  
  // 添加Windows路径
  azimuth::Attributes::set(attrs, "log.path.windows", azimuth::StringValue("C:\\Program Files\\Azimuth\\logs\\app.log"))
  azimuth::Attributes::set(attrs, "config.path.windows", azimuth::StringValue("C:\\Program Files\\Azimuth\\config.yaml"))
  
  // 验证路径属性
  assert_eq(azimuth::Attributes::get(attrs, "log.path.unix"), Some(azimuth::StringValue("/var/log/telemetry/app.log")))
  assert_eq(azimuth::Attributes::get(attrs, "config.path.windows"), Some(azimuth::StringValue("C:\\Program Files\\Azimuth\\config.yaml")))
}

test "platform-specific environment variables" {
  // 测试平台特定的环境变量处理
  
  // Unix/Linux环境变量
  let unix_env_vars = [
    ("PATH", "/usr/local/bin:/usr/bin:/bin"),
    ("HOME", "/home/user"),
    ("SHELL", "/bin/bash"),
    ("LANG", "en_US.UTF-8"),
    ("AZIMUTH_CONFIG_DIR", "/etc/azimuth")
  ]
  
  // Windows环境变量
  let windows_env_vars = [
    ("Path", "C:\\Windows\\system32;C:\\Program Files\\Azimuth"),
    ("USERPROFILE", "C:\\Users\\User"),
    ("ComSpec", "C:\\Windows\\system32\\cmd.exe"),
    ("AZIMUTH_CONFIG_DIR", "C:\\Program Files\\Azimuth\\config")
  ]
  
  // 创建包含环境变量的属性
  let attrs = azimuth::Attributes::new()
  
  // 添加Unix环境变量
  for env_var in unix_env_vars {
    let attr_name = "env.unix." + env_var.0.to_lower_case()
    azimuth::Attributes::set(attrs, attr_name, azimuth::StringValue(env_var.1))
  }
  
  // 添加Windows环境变量
  for env_var in windows_env_vars {
    let attr_name = "env.windows." + env_var.0.to_lower_case()
    azimuth::Attributes::set(attrs, attr_name, azimuth::StringValue(env_var.1))
  }
  
  // 验证环境变量属性
  assert_eq(azimuth::Attributes::get(attrs, "env.unix.path"), Some(azimuth::StringValue("/usr/local/bin:/usr/bin:/bin")))
  assert_eq(azimuth::Attributes::get(attrs, "env.unix.home"), Some(azimuth::StringValue("/home/user")))
  assert_eq(azimuth::Attributes::get(attrs, "env.windows.path"), Some(azimuth::StringValue("C:\\Windows\\system32;C:\\Program Files\\Azimuth")))
  assert_eq(azimuth::Attributes::get(attrs, "env.windows.userprofile"), Some(azimuth::StringValue("C:\\Users\\User")))
}

test "platform-specific service discovery" {
  // 测试平台特定的服务发现机制
  
  // 创建服务发现的属性
  let attrs = azimuth::Attributes::new()
  
  // Unix/Linux服务发现
  azimuth::Attributes::set(attrs, "service.discovery.unix", azimuth::StringValue("systemd"))
  azimuth::Attributes::set(attrs, "service.manager.unix", azimuth::StringValue("systemd"))
  azimuth::Attributes::set(attrs, "init.system.unix", azimuth::StringValue("systemd"))
  
  // Windows服务发现
  azimuth::Attributes::set(attrs, "service.discovery.windows", azimuth::StringValue("windows-service"))
  azimuth::Attributes::set(attrs, "service.manager.windows", azimuth::StringValue("services.msc"))
  azimuth::Attributes::set(attrs, "init.system.windows", azimuth::StringValue("wininit"))
  
  // macOS服务发现
  azimuth::Attributes::set(attrs, "service.discovery.darwin", azimuth::StringValue("launchd"))
  azimuth::Attributes::set(attrs, "service.manager.darwin", azimuth::StringValue("launchctl"))
  azimuth::Attributes::set(attrs, "init.system.darwin", azimuth::StringValue("launchd"))
  
  // 容器环境服务发现
  azimuth::Attributes::set(attrs, "service.discovery.docker", azimuth::StringValue("docker-swarm"))
  azimuth::Attributes::set(attrs, "service.discovery.kubernetes", azimuth::StringValue("k8s-api"))
  
  // 云平台服务发现
  azimuth::Attributes::set(attrs, "service.discovery.aws", azimuth::StringValue("ecs-task-definition"))
  azimuth::Attributes::set(attrs, "service.discovery.azure", azimuth::StringValue("azure-service-fabric"))
  azimuth::Attributes::set(attrs, "service.discovery.gcp", azimuth::StringValue("gke-service"))
  
  // 验证服务发现属性
  assert_eq(azimuth::Attributes::get(attrs, "service.discovery.unix"), Some(azimuth::StringValue("systemd")))
  assert_eq(azimuth::Attributes::get(attrs, "service.discovery.windows"), Some(azimuth::StringValue("windows-service")))
  assert_eq(azimuth::Attributes::get(attrs, "service.discovery.darwin"), Some(azimuth::StringValue("launchd")))
  assert_eq(azimuth::Attributes::get(attrs, "service.discovery.kubernetes"), Some(azimuth::StringValue("k8s-api")))
}

test "platform-specific logging configurations" {
  // 测试平台特定的日志配置
  
  let logger = azimuth::LoggerProvider::get_logger(
    azimuth::LoggerProvider::default(), 
    "platform-logger"
  )
  
  // Unix/Linux日志配置
  let unix_log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(unix_log_attrs, "log.system", azimuth::StringValue("syslog"))
  azimuth::Attributes::set(unix_log_attrs, "log.facility", azimuth::StringValue("user"))
  azimuth::Attributes::set(unix_log_attrs, "log.format", azimuth::StringValue("rfc3164"))
  azimuth::Attributes::set(unix_log_attrs, "log.path", azimuth::StringValue("/var/log/azimuth/app.log"))
  
  let unix_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Unix/Linux platform log message"),
    Some(unix_log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    None,
    None,
    None
  )
  
  // Windows日志配置
  let windows_log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(windows_log_attrs, "log.system", azimuth::StringValue("event-log"))
  azimuth::Attributes::set(windows_log_attrs, "log.source", azimuth::StringValue("Azimuth Telemetry"))
  azimuth::Attributes::set(windows_log_attrs, "log.format", azimuth::StringValue("xml"))
  azimuth::Attributes::set(windows_log_attrs, "log.path", azimuth::StringValue("C:\\ProgramData\\Azimuth\\logs\\app.log"))
  
  let windows_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Windows platform log message"),
    Some(windows_log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    None,
    None,
    None
  )
  
  // macOS日志配置
  let macos_log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(macos_log_attrs, "log.system", azimuth::StringValue("os-log"))
  azimuth::Attributes::set(macos_log_attrs, "log.subsystem", azimuth::StringValue("com.azimuth.telemetry"))
  azimuth::Attributes::set(macos_log_attrs, "log.category", azimuth::StringValue("general"))
  azimuth::Attributes::set(macos_log_attrs, "log.path", azimuth::StringValue("/var/log/azimuth/app.log"))
  
  let macos_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("macOS platform log message"),
    Some(macos_log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    None,
    None,
    None
  )
  
  // 发射所有平台的日志
  azimuth::Logger::emit(logger, unix_log)
  azimuth::Logger::emit(logger, windows_log)
  azimuth::Logger::emit(logger, macos_log)
  
  // 验证日志内容
  assert_eq(azimuth::LogRecord::body(unix_log), Some("Unix/Linux platform log message"))
  assert_eq(azimuth::LogRecord::body(windows_log), Some("Windows platform log message"))
  assert_eq(azimuth::LogRecord::body(macos_log), Some("macOS platform log message"))
}

test "platform-specific performance counters" {
  // 测试平台特定的性能计数器
  
  let meter = azimuth::MeterProvider::get_meter(
    azimuth::MeterProvider::default(), 
    "platform-meter"
  )
  
  // Unix/Linux性能计数器
  let unix_cpu_counter = azimuth::Meter::create_counter(
    meter, 
    "system.cpu.time.unix",
    Some("Unix/Linux CPU time"),
    Some("seconds")
  )
  
  let unix_memory_gauge = azimuth::Meter::create_gauge(
    meter, 
    "system.memory.used.unix",
    Some("Unix/Linux memory usage"),
    Some("bytes")
  )
  
  // Windows性能计数器
  let windows_cpu_counter = azimuth::Meter::create_counter(
    meter, 
    "system.cpu.time.windows",
    Some("Windows CPU time"),
    Some("percent")
  )
  
  let windows_memory_gauge = azimuth::Meter::create_gauge(
    meter, 
    "system.memory.used.windows",
    Some("Windows memory usage"),
    Some("bytes")
  )
  
  // macOS性能计数器
  let macos_cpu_counter = azimuth::Meter::create_counter(
    meter, 
    "system.cpu.time.darwin",
    Some("macOS CPU time"),
    Some("seconds")
  )
  
  let macos_memory_gauge = azimuth::Meter::create_gauge(
    meter, 
    "system.memory.used.darwin",
    Some("macOS memory usage"),
    Some("bytes")
  )
  
  // 记录不同平台的性能数据
  azimuth::Counter::add(unix_cpu_counter, 120.5)
  azimuth::Gauge::record(unix_memory_gauge, 2147483648.0)
  
  azimuth::Counter::add(windows_cpu_counter, 75.3)
  azimuth::Gauge::record(windows_memory_gauge, 4294967296.0)
  
  azimuth::Counter::add(macos_cpu_counter, 95.7)
  azimuth::Gauge::record(macos_memory_gauge, 8589934592.0)
  
  // 验证性能计数器的名称和描述
  assert_eq(unix_cpu_counter.name, "system.cpu.time.unix")
  assert_eq(unix_cpu_counter.description, Some("Unix/Linux CPU time"))
  assert_eq(unix_cpu_counter.unit, Some("seconds"))
  
  assert_eq(windows_cpu_counter.name, "system.cpu.time.windows")
  assert_eq(windows_cpu_counter.description, Some("Windows CPU time"))
  assert_eq(windows_cpu_counter.unit, Some("percent"))
  
  assert_eq(macos_cpu_counter.name, "system.cpu.time.darwin")
  assert_eq(macos_cpu_counter.description, Some("macOS CPU time"))
  assert_eq(macos_cpu_counter.unit, Some("seconds"))
}