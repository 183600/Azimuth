// 跨平台兼容性测试 - 用于测试遥测系统在不同平台和环境下的兼容性

test "platform_detection_and_abstraction" {
  // 测试平台检测和抽象层
  
  enum Platform {
    Windows
    Linux
    MacOS
    FreeBSD
    Android
    IOS
    Wasm
    Unknown
  }
  
  enum Architecture {
    X86
    X64
    Arm32
    Arm64
    Wasm32
    Wasm64
    Unknown
  }
  
  struct PlatformInfo {
    platform : Platform
    architecture : Architecture
    endianness : String
    word_size : Int
    pointer_size : Int
  }
  
  // 模拟不同平台的信息
  let platform_configs = [
    {
      platform: Windows,
      architecture: X64,
      endianness: "little",
      word_size: 64,
      pointer_size: 8
    },
    {
      platform: Linux,
      architecture: X64,
      endianness: "little",
      word_size: 64,
      pointer_size: 8
    },
    {
      platform: MacOS,
      architecture: Arm64,
      endianness: "little",
      word_size: 64,
      pointer_size: 8
    },
    {
      platform: Android,
      architecture: Arm64,
      endianness: "little",
      word_size: 64,
      pointer_size: 8
    },
    {
      platform: IOS,
      architecture: Arm64,
      endianness: "little",
      word_size: 64,
      pointer_size: 8
    },
    {
      platform: Wasm,
      architecture: Wasm32,
      endianness: "little",
      word_size: 32,
      pointer_size: 4
    }
  ]
  
  // 验证平台配置
  for config in platform_configs {
    assert_eq(config.word_size > 0, true)
    assert_eq(config.pointer_size > 0, true)
    assert_eq(config.endianness == "little" || config.endianness == "big", true)
    
    match config.architecture {
      X86 => assert_eq(config.word_size, 32)
      X64 => assert_eq(config.word_size, 64)
      Arm32 => assert_eq(config.word_size, 32)
      Arm64 => assert_eq(config.word_size, 64)
      Wasm32 => assert_eq(config.word_size, 32)
      Wasm64 => assert_eq(config.word_size, 64)
      Unknown => ()
    }
  }
  
  // 测试平台特定功能检测
  let detect_platform_features = fn(platform : Platform) : Array<String> {
    match platform {
      Windows => ["registry", "event_log", "performance_counters", "windows_services"]
      Linux => ["proc_fs", "sysctl", "systemd", "cgroups"]
      MacOS => ["launchd", "core_foundation", "foundation", "cocoa"]
      Android => ["content_resolver", "activity_manager", "broadcast_receiver"]
      IOS => ["ns_user_defaults", "core_data", "keychain", "notification_center"]
      Wasm => ["web_storage", "indexed_db", "web_workers", "fetch_api"]
      _ => ["basic_filesystem", "environment_variables"]
    }
  }
  
  // 验证平台功能检测
  for config in platform_configs {
    let features = detect_platform_features(config.platform)
    assert_eq(features.length() > 0, true)
    
    for feature in features {
      assert_eq(feature.length() > 0, true)
      assert_eq(feature.contains(" "), false)  // 功能名称不应包含空格
    }
  }
}

test "cross_platform_file_system_operations" {
  // 测试跨平台文件系统操作
  
  enum PathSeparator {
    ForwardSlash    // Unix-like: /
    BackSlash       // Windows: \
  }
  
  struct FileSystemConfig {
    platform : Platform
    path_separator : PathSeparator
    case_sensitive : Bool
    max_path_length : Int
    temp_directory : String
    home_directory : String
  }
  
  let fs_configs = [
    {
      platform: Windows,
      path_separator: BackSlash,
      case_sensitive: false,
      max_path_length: 260,
      temp_directory: "C:\\Users\\User\\AppData\\Local\\Temp",
      home_directory: "C:\\Users\\User"
    },
    {
      platform: Linux,
      path_separator: ForwardSlash,
      case_sensitive: true,
      max_path_length: 4096,
      temp_directory: "/tmp",
      home_directory: "/home/user"
    },
    {
      platform: MacOS,
      path_separator: ForwardSlash,
      case_sensitive: true,
      max_path_length: 1024,
      temp_directory: "/tmp",
      home_directory: "/Users/user"
    },
    {
      platform: Android,
      path_separator: ForwardSlash,
      case_sensitive: true,
      max_path_length: 4096,
      temp_directory: "/data/local/tmp",
      home_directory: "/data/user/0/com.app"
    }
  ]
  
  // 测试路径规范化
  let normalize_path = fn(path : String, config : FileSystemConfig) : String {
    let separator = match config.path_separator {
      ForwardSlash => "/"
      BackSlash => "\\"
    }
    
    // 替换所有分隔符为当前平台的分隔符
    let mut normalized = path
    if separator == "/" {
      normalized = normalized.replace("\\", "/")
    } else {
      normalized = normalized.replace("/", "\\")
    }
    
    normalized
  }
  
  // 测试路径拼接
  let join_paths = fn(paths : Array<String>, config : FileSystemConfig) : String {
    let separator = match config.path_separator {
      ForwardSlash => "/"
      BackSlash => "\\"
    }
    
    let mut result = ""
    for i = 0; i < paths.length(); i = i + 1 {
      let path = paths[i]
      if i > 0 {
        result = result + separator
      }
      result = result + path
    }
    
    result
  }
  
  // 验证文件系统操作
  for config in fs_configs {
    // 测试路径规范化
    let mixed_path = "folder\\subfolder/file.txt"
    let normalized = normalize_path(mixed_path, config)
    
    let expected_separator = match config.path_separator {
      ForwardSlash => "/"
      BackSlash => "\\"
    }
    
    assert_eq(normalized.contains(expected_separator), true)
    assert_eq(normalized.contains(expected_separator == "/" ? "\\" : "/"), false)
    
    // 测试路径拼接
    let paths = ["home", "user", "documents", "file.txt"]
    let joined_path = join_paths(paths, config)
    
    assert_eq(joined_path.contains("home"), true)
    assert_eq(joined_path.contains("user"), true)
    assert_eq(joined_path.contains("documents"), true)
    assert_eq(joined_path.contains("file.txt"), true)
    
    // 验证路径长度限制
    assert_eq(joined_path.length() <= config.max_path_length, true)
    
    // 测试临时目录路径
    let temp_path = join_paths([config.temp_directory, "telemetry.log"], config)
    assert_eq(temp_path.starts_with(config.temp_directory), true)
  }
}

test "cross_platform_networking_compatibility" {
  // 测试跨平台网络兼容性
  
  enum SocketType {
    TCP
    UDP
    Unix
    NamedPipe
  }
  
  struct NetworkConfig {
    platform : Platform
    supported_sockets : Array<SocketType>
    default_timeout_ms : Int
    max_connections : Int
    dns_resolution_supported : Bool
    ipv6_supported : Bool
  }
  
  let network_configs = [
    {
      platform: Windows,
      supported_sockets: [TCP, UDP, NamedPipe],
      default_timeout_ms: 30000,
      max_connections: 2048,
      dns_resolution_supported: true,
      ipv6_supported: true
    },
    {
      platform: Linux,
      supported_sockets: [TCP, UDP, Unix],
      default_timeout_ms: 60000,
      max_connections: 4096,
      dns_resolution_supported: true,
      ipv6_supported: true
    },
    {
      platform: MacOS,
      supported_sockets: [TCP, UDP, Unix],
      default_timeout_ms: 45000,
      max_connections: 2048,
      dns_resolution_supported: true,
      ipv6_supported: true
    },
    {
      platform: Wasm,
      supported_sockets: [TCP],  // 浏览器环境限制
      default_timeout_ms: 15000,
      max_connections: 6,        // 浏览器连接限制
      dns_resolution_supported: true,
      ipv6_supported: true
    }
  ]
  
  // 测试端点解析
  let parse_endpoint = fn(endpoint : String, config : NetworkConfig) : (String, Int) {
    // 简化的端点解析逻辑
    if endpoint.contains(":") {
      let parts = endpoint.split(":")
      let host = parts[0]
      let port = if parts.length() > 1 {
        parts[1].to_int()
      } else {
        80
      }
      (host, port)
    } else {
      (endpoint, 80)
    }
  }
  
  // 测试连接字符串生成
  let generate_connection_string = fn(host : String, port : Int, socket_type : SocketType, 
                                     config : NetworkConfig) : String {
    match socket_type {
      TCP => "tcp://" + host + ":" + port.to_string()
      UDP => "udp://" + host + ":" + port.to_string()
      Unix => {
        if config.platform == Linux || config.platform == MacOS {
          "unix:///tmp/socket_" + port.to_string()
        } else {
          "unsupported"
        }
      }
      NamedPipe => {
        if config.platform == Windows {
          "\\\\.\\pipe\\telemetry_" + port.to_string()
        } else {
          "unsupported"
        }
      }
    }
  }
  
  // 验证网络配置和操作
  for config in network_configs {
    // 验证基本配置
    assert_eq(config.default_timeout_ms > 0, true)
    assert_eq(config.max_connections > 0, true)
    assert_eq(config.supported_sockets.length() > 0, true)
    
    // 测试端点解析
    let (host, port) = parse_endpoint("localhost:4318", config)
    assert_eq(host, "localhost")
    assert_eq(port, 4318)
    
    // 测试连接字符串生成
    for socket_type in config.supported_sockets {
      let conn_str = generate_connection_string("localhost", 4318, socket_type, config)
      assert_eq(conn_str != "unsupported", true)
      
      match socket_type {
        TCP => assert_eq(conn_str.starts_with("tcp://"), true)
        UDP => assert_eq(conn_str.starts_with("udp://"), true)
        Unix => assert_eq(conn_str.starts_with("unix://"), true)
        NamedPipe => assert_eq(conn_str.starts_with("\\\\"), true)
      }
    }
    
    // 测试平台特定的网络特性
    match config.platform {
      Wasm => {
        assert_eq(config.max_connections <= 10, true)  // 浏览器连接限制
        assert_eq(config.supported_sockets.contains(Unix), false)
        assert_eq(config.supported_sockets.contains(NamedPipe), false)
      }
      Windows => {
        assert_eq(config.supported_sockets.contains(NamedPipe), true)
        assert_eq(config.supported_sockets.contains(Unix), false)
      }
      Linux | MacOS => {
        assert_eq(config.supported_sockets.contains(Unix), true)
        assert_eq(config.supported_sockets.contains(NamedPipe), false)
      }
      _ => ()
    }
  }
}

test "cross_platform_time_and_timezone_handling" {
  // 测试跨平台时间和时区处理
  
  enum TimeSource {
    SystemClock
    HighResolutionTimer
    MonotonicClock
    RealtimeClock
  }
  
  struct TimeConfig {
    platform : Platform
    supported_time_sources : Array<TimeSource>
    resolution_ns : Int64
    timezone_database_available : Bool
    daylight_saving_supported : Bool
  }
  
  let time_configs = [
    {
      platform: Windows,
      supported_time_sources: [SystemClock, HighResolutionTimer, RealtimeClock],
      resolution_ns: 100000L,  // 100微秒
      timezone_database_available: true,
      daylight_saving_supported: true
    },
    {
      platform: Linux,
      supported_time_sources: [SystemClock, HighResolutionTimer, MonotonicClock, RealtimeClock],
      resolution_ns: 1L,  // 1纳秒
      timezone_database_available: true,
      daylight_saving_supported: true
    },
    {
      platform: MacOS,
      supported_time_sources: [SystemClock, HighResolutionTimer, MonotonicClock, RealtimeClock],
      resolution_ns: 1L,  // 1纳秒
      timezone_database_available: true,
      daylight_saving_supported: true
    },
    {
      platform: Wasm,
      supported_time_sources: [SystemClock, HighResolutionTimer],
      resolution_ns: 1000000L,  // 1毫秒
      timezone_database_available: false,  // 依赖浏览器
      daylight_saving_supported: false
    }
  ]
  
  // 测试时间戳格式转换
  let format_timestamp = fn(timestamp : Int64, format : String, config : TimeConfig) : String {
    match format {
      "iso8601" => {
        // 简化的ISO8601格式
        let seconds = timestamp / 1000000000L
        let nanos = timestamp % 1000000000L
        seconds.to_string() + "." + (@sprintf("%09d", nanos.to_int())) + "Z"
      }
      "unix" => timestamp.to_string()
      "rfc3339" => {
        // 简化的RFC3339格式
        let seconds = timestamp / 1000000000L
        seconds.to_string() + "Z"
      }
      _ => timestamp.to_string()
    }
  }
  
  // 测试时区转换
  let convert_timezone = fn(timestamp : Int64, from_tz : String, to_tz : String, 
                            config : TimeConfig) : Int64 {
    if !config.timezone_database_available {
      // 如果没有时区数据库，返回原始时间戳
      timestamp
    } else {
      // 简化的时区转换逻辑
      let offset_hours = match (from_tz, to_tz) {
        ("UTC", "America/New_York") => -5
        ("UTC", "Europe/London") => 0
        ("UTC", "Asia/Tokyo") => 9
        ("America/New_York", "UTC") => 5
        ("Europe/London", "UTC") => 0
        ("Asia/Tokyo", "UTC") => -9
        _ => 0
      }
      
      timestamp + (offset_hours * 3600 * 1000000000L)
    }
  }
  
  // 验证时间配置和操作
  for config in time_configs {
    // 验证基本配置
    assert_eq(config.resolution_ns > 0, true)
    assert_eq(config.supported_time_sources.length() > 0, true)
    
    // 测试时间戳格式化
    let test_timestamp = 1609459200000000000L  // 2021-01-01 00:00:00 UTC
    let iso8601_format = format_timestamp(test_timestamp, "iso8601", config)
    let unix_format = format_timestamp(test_timestamp, "unix", config)
    let rfc3339_format = format_timestamp(test_timestamp, "rfc3339", config)
    
    assert_eq(iso8601_format.contains("1609459200"), true)
    assert_eq(iso8601_format.contains("Z"), true)
    assert_eq(unix_format, "1609459200000000000")
    assert_eq(rfc3339_format.contains("1609459200"), true)
    assert_eq(rfc3339_format.contains("Z"), true)
    
    // 测试时区转换
    if config.timezone_database_available {
      let utc_timestamp = test_timestamp
      let ny_timestamp = convert_timezone(utc_timestamp, "UTC", "America/New_York", config)
      let tokyo_timestamp = convert_timezone(utc_timestamp, "UTC", "Asia/Tokyo", config)
      
      assert_eq(ny_timestamp, utc_timestamp - (5 * 3600 * 1000000000L))
      assert_eq(tokyo_timestamp, utc_timestamp + (9 * 3600 * 1000000000L))
    }
    
    // 测试平台特定的时间特性
    match config.platform {
      Wasm => {
        assert_eq(config.resolution_ns >= 1000000L, true)  // 至少1毫秒精度
        assert_eq(config.timezone_database_available, false)
      }
      Linux | MacOS => {
        assert_eq(config.resolution_ns <= 1000L, true)  // 至少1微秒精度
        assert_eq(config.supported_time_sources.contains(MonotonicClock), true)
      }
      Windows => {
        assert_eq(config.supported_time_sources.contains(HighResolutionTimer), true)
        assert_eq(config.supported_time_sources.contains(MonotonicClock), false)
      }
      _ => ()
    }
  }
}

test "cross_platform_memory_management" {
  // 测试跨平台内存管理
  
  enum MemoryType {
    Heap
    Stack
    Mapped
    Shared
  }
  
  struct MemoryConfig {
    platform : Platform
    total_memory_mb : Int64
    available_memory_mb : Int64
    page_size_bytes : Int
    max_heap_size_mb : Int64
    supports_shared_memory : Bool
    supports_memory_mapping : Bool
  }
  
  let memory_configs = [
    {
      platform: Windows,
      total_memory_mb: 16384L,    // 16GB
      available_memory_mb: 8192L, // 8GB
      page_size_bytes: 4096,
      max_heap_size_mb: 2048L,
      supports_shared_memory: true,
      supports_memory_mapping: true
    },
    {
      platform: Linux,
      total_memory_mb: 32768L,    // 32GB
      available_memory_mb: 16384L, // 16GB
      page_size_bytes: 4096,
      max_heap_size_mb: 4096L,
      supports_shared_memory: true,
      supports_memory_mapping: true
    },
    {
      platform: MacOS,
      total_memory_mb: 16384L,    // 16GB
      available_memory_mb: 8192L, // 8GB
      page_size_bytes: 4096,
      max_heap_size_mb: 2048L,
      supports_shared_memory: true,
      supports_memory_mapping: true
    },
    {
      platform: Wasm,
      total_memory_mb: 4096L,     // 4GB (浏览器限制)
      available_memory_mb: 2048L, // 2GB
      page_size_bytes: 65536,     // 64KB pages
      max_heap_size_mb: 1024L,
      supports_shared_memory: false,
      supports_memory_mapping: false
    }
  ]
  
  // 测试内存分配策略
  let calculate_allocation_strategy = fn(requested_size : Int, config : MemoryConfig) : MemoryType {
    if requested_size < 1024 {  // 小于1KB，使用栈
      Stack
    } else if requested_size < 1024 * 1024 {  // 小于1MB，使用堆
      Heap
    } else if config.supports_memory_mapping && requested_size > 10 * 1024 * 1024 {  // 大于10MB，使用内存映射
      Mapped
    } else {
      Heap
    }
  }
  
  // 测试内存限制检查
  let check_memory_limits = fn(allocation_size_mb : Int64, config : MemoryConfig) : Bool {
    allocation_size_mb <= config.max_heap_size_mb && 
    allocation_size_mb <= config.available_memory_mb / 2  // 保留一半内存给系统
  }
  
  // 验证内存配置和操作
  for config in memory_configs {
    // 验证基本配置
    assert_eq(config.total_memory_mb > 0, true)
    assert_eq(config.available_memory_mb > 0, true)
    assert_eq(config.available_memory_mb <= config.total_memory_mb, true)
    assert_eq(config.page_size_bytes > 0, true)
    assert_eq(config.max_heap_size_mb > 0, true)
    
    // 测试分配策略
    let small_allocation = calculate_allocation_strategy(512, config)
    let medium_allocation = calculate_allocation_strategy(2048, config)
    let large_allocation = calculate_allocation_strategy(20 * 1024 * 1024, config)
    
    assert_eq(small_allocation, Stack)
    assert_eq(medium_allocation, Heap)
    
    if config.supports_memory_mapping {
      assert_eq(large_allocation, Mapped)
    } else {
      assert_eq(large_allocation, Heap)
    }
    
    // 测试内存限制
    let reasonable_allocation = check_memory_limits(512, config)
    let excessive_allocation = check_memory_limits(config.max_heap_size_mb + 1, config)
    
    assert_eq(reasonable_allocation, true)
    assert_eq(excessive_allocation, false)
    
    // 测试平台特定的内存特性
    match config.platform {
      Wasm => {
        assert_eq(config.supports_shared_memory, false)
        assert_eq(config.supports_memory_mapping, false)
        assert_eq(config.page_size_bytes >= 65536, true)  // WebAssembly使用大页面
      }
      Linux => {
        assert_eq(config.supports_shared_memory, true)
        assert_eq(config.supports_memory_mapping, true)
      }
      Windows => {
        assert_eq(config.supports_shared_memory, true)
        assert_eq(config.supports_memory_mapping, true)
      }
      _ => ()
    }
  }
}

test "cross_platform_error_handling" {
  // 测试跨平台错误处理
  
  enum ErrorType {
    NetworkError
    FileSystemError
    MemoryError
    PermissionError
    TimeoutError
    PlatformError
  }
  
  struct PlatformError {
    error_type : ErrorType
    platform_code : Int
    message : String
    is_recoverable : Bool
    suggested_action : String
  }
  
  struct ErrorHandlingConfig {
    platform : Platform
    error_codes : Array<(ErrorType, Int)>
    default_retry_attempts : Int
    supports_error_dialogs : Bool
    logging_mechanism : String
  }
  
  let error_configs = [
    {
      platform: Windows,
      error_codes: [
        (NetworkError, 10060),    // Connection timed out
        (FileSystemError, 2),     // File not found
        (MemoryError, 8),         // Not enough memory
        (PermissionError, 5),     // Access denied
        (TimeoutError, 258)       // Wait timeout
      ],
      default_retry_attempts: 3,
      supports_error_dialogs: true,
      logging_mechanism: "event_log"
    },
    {
      platform: Linux,
      error_codes: [
        (NetworkError, 110),      // Connection timed out
        (FileSystemError, 2),     // No such file or directory
        (MemoryError, 12),        // Not enough memory
        (PermissionError, 13),    // Permission denied
        (TimeoutError, 62)        // Timer expired
      ],
      default_retry_attempts: 5,
      supports_error_dialogs: false,
      logging_mechanism: "syslog"
    },
    {
      platform: MacOS,
      error_codes: [
        (NetworkError, 60),       // Operation timed out
        (FileSystemError, 2),     // No such file or directory
        (MemoryError, 3),         // Not enough memory
        (PermissionError, 13),    // Permission denied
        (TimeoutError, 78)        // Connection timed out
      ],
      default_retry_attempts: 4,
      supports_error_dialogs: true,
      logging_mechanism: "asl"  // Apple System Log
    },
    {
      platform: Wasm,
      error_codes: [
        (NetworkError, 0),       // Network errors in JavaScript
        (FileSystemError, 0),    // Sandboxed file system
        (MemoryError, 0),        // Out of memory
        (PermissionError, 0),    // Security restrictions
        (TimeoutError, 0)        // Timeout
      ],
      default_retry_attempts: 2,
      supports_error_dialogs: false,
      logging_mechanism: "console"
    }
  ]
  
  // 测试错误映射
  let map_platform_error = fn(platform_code : Int, config : ErrorHandlingConfig) : PlatformError {
    // 查找错误类型
    let mut error_type = PlatformError
    for (etype, code) in config.error_codes {
      if code == platform_code {
        error_type = etype
        break
      }
    }
    
    // 生成平台特定的错误信息
    let message = match (config.platform, error_type) {
      (Windows, NetworkError) => "WinSock error: " + platform_code.to_string()
      (Linux, NetworkError) => "POSIX network error: " + platform_code.to_string()
      (MacOS, NetworkError) => "Darwin network error: " + platform_code.to_string()
      (Wasm, NetworkError) => "JavaScript network error"
      (Windows, FileSystemError) => "Windows file system error: " + platform_code.to_string()
      (Linux, FileSystemError) => "POSIX file system error: " + platform_code.to_string()
      (MacOS, FileSystemError) => "Darwin file system error: " + platform_code.to_string()
      (Wasm, FileSystemError) => "Browser file system error"
      _ => "Platform error: " + platform_code.to_string()
    }
    
    // 确定是否可恢复
    let is_recoverable = match error_type {
      NetworkError | TimeoutError => true
      FileSystemError | MemoryError | PermissionError => false
      PlatformError => true
    }
    
    // 建议操作
    let suggested_action = match error_type {
      NetworkError => "Check network connection and retry"
      FileSystemError => "Verify file path and permissions"
      MemoryError => "Free up memory and reduce allocation size"
      PermissionError => "Run with appropriate privileges"
      TimeoutError => "Increase timeout or retry operation"
      PlatformError => "Check platform-specific requirements"
    }
    
    {
      error_type: error_type,
      platform_code: platform_code,
      message: message,
      is_recoverable: is_recoverable,
      suggested_action: suggested_action
    }
  }
  
  // 验证错误处理配置和操作
  for config in error_configs {
    // 验证基本配置
    assert_eq(config.error_codes.length() > 0, true)
    assert_eq(config.default_retry_attempts > 0, true)
    assert_eq(config.logging_mechanism.length() > 0, true)
    
    // 测试错误映射
    for (error_type, code) in config.error_codes {
      let platform_error = map_platform_error(code, config)
      
      assert_eq(platform_error.error_type, error_type)
      assert_eq(platform_error.platform_code, code)
      assert_eq(platform_error.message.length() > 0, true)
      assert_eq(platform_error.suggested_action.length() > 0, true)
      
      // 验证可恢复性判断
      let expected_recoverable = match error_type {
        NetworkError | TimeoutError => true
        FileSystemError | MemoryError | PermissionError => false
        PlatformError => true
      }
      assert_eq(platform_error.is_recoverable, expected_recoverable)
    }
    
    // 测试平台特定的错误处理特性
    match config.platform {
      Windows => {
        assert_eq(config.supports_error_dialogs, true)
        assert_eq(config.logging_mechanism, "event_log")
      }
      Linux => {
        assert_eq(config.supports_error_dialogs, false)
        assert_eq(config.logging_mechanism, "syslog")
      }
      MacOS => {
        assert_eq(config.supports_error_dialogs, true)
        assert_eq(config.logging_mechanism, "asl")
      }
      Wasm => {
        assert_eq(config.supports_error_dialogs, false)
        assert_eq(config.logging_mechanism, "console")
        assert_eq(config.default_retry_attempts <= 3, true)  // 浏览器环境限制重试
      }
      _ => ()
    }
  }
}