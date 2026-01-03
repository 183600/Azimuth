// Azimuth 跨平台兼容性测试用例
// 专注于不同操作系统、架构和运行时环境下的遥测系统兼容性

// 测试1: 操作系统兼容性检测
test "操作系统兼容性检测" {
  // 操作系统类型
  enum OSType {
    Windows
    Linux
    MacOS
    FreeBSD
    Unknown
  }
  
  // 系统信息
  type SystemInfo = {
    os_type: OSType,
    os_version: String,
    architecture: String,
    hostname: String,
    platform: String
  }
  
  // 平台特定配置
  type PlatformConfig = {
    telemetry_endpoint: String,
    log_file_path: String,
    config_file_path: String,
    temp_dir: String,
    path_separator: String,
    line_ending: String,
    environment_variables: Map[String, String]
  }
  
  // 检测操作系统类型
  let detect_os_type = fn() {
    // 在实际实现中，这里会检查系统环境变量或系统调用
    // 这里简化为返回Linux
    OSType::Linux
  }
  
  // 获取系统信息
  let get_system_info = fn() {
    let os_type = detect_os_type()
    
    {
      os_type: os_type,
      os_version: "5.4.0-42-generic",
      architecture: "x86_64",
      hostname: "test-server",
      platform: match os_type {
        OSType::Windows => "windows"
        OSType::Linux => "linux"
        OSType::MacOS => "darwin"
        OSType::FreeBSD => "freebsd"
        OSType::Unknown => "unknown"
      }
    }
  }
  
  // 获取平台特定配置
  let get_platform_config = fn(system_info: SystemInfo) {
    match system_info.os_type {
      OSType::Windows => {
        {
          telemetry_endpoint: "http://localhost:4318/v1/traces",
          log_file_path: "C:\\ProgramData\\Azimuth\\logs\\telemetry.log",
          config_file_path: "C:\\ProgramData\\Azimuth\\config\\azimuth.json",
          temp_dir: "C:\\Temp\\",
          path_separator: "\\",
          line_ending: "\r\n",
          environment_variables: [
            ("PROGRAMFILES", "C:\\Program Files"),
            ("SYSTEMROOT", "C:\\Windows")
          ]
        }
      }
      OSType::Linux => {
        {
          telemetry_endpoint: "http://localhost:4318/v1/traces",
          log_file_path: "/var/log/azimuth/telemetry.log",
          config_file_path: "/etc/azimuth/azimuth.json",
          temp_dir: "/tmp/",
          path_separator: "/",
          line_ending: "\n",
          environment_variables: [
            ("HOME", "/home/user"),
            ("PATH", "/usr/bin:/bin")
          ]
        }
      }
      OSType::MacOS => {
        {
          telemetry_endpoint: "http://localhost:4318/v1/traces",
          log_file_path: "/Library/Logs/Azimuth/telemetry.log",
          config_file_path: "/Library/Application Support/Azimuth/azimuth.json",
          temp_dir: "/tmp/",
          path_separator: "/",
          line_ending: "\n",
          environment_variables: [
            ("HOME", "/Users/user"),
            ("PATH", "/usr/bin:/bin:/usr/local/bin")
          ]
        }
      }
      OSType::FreeBSD => {
        {
          telemetry_endpoint: "http://localhost:4318/v1/traces",
          log_file_path: "/var/log/azimuth/telemetry.log",
          config_file_path: "/usr/local/etc/azimuth/azimuth.json",
          temp_dir: "/tmp/",
          path_separator: "/",
          line_ending: "\n",
          environment_variables: [
            ("HOME", "/home/user"),
            ("PATH", "/usr/bin:/bin:/usr/local/bin")
          ]
        }
      }
      OSType::Unknown => {
        {
          telemetry_endpoint: "http://localhost:4318/v1/traces",
          log_file_path: "./telemetry.log",
          config_file_path: "./azimuth.json",
          temp_dir: "./",
          path_separator: "/",
          line_ending: "\n",
          environment_variables: []
        }
      }
    }
  }
  
  // 测试系统信息获取
  let system_info = get_system_info()
  assert_eq(system_info.os_type, OSType::Linux)
  assert_eq(system_info.platform, "linux")
  assert_eq(system_info.architecture, "x86_64")
  assert_eq(system_info.hostname, "test-server")
  
  // 测试平台特定配置
  let config = get_platform_config(system_info)
  assert_eq(config.telemetry_endpoint, "http://localhost:4318/v1/traces")
  assert_eq(config.log_file_path, "/var/log/azimuth/telemetry.log")
  assert_eq(config.config_file_path, "/etc/azimuth/azimuth.json")
  assert_eq(config.temp_dir, "/tmp/")
  assert_eq(config.path_separator, "/")
  assert_eq(config.line_ending, "\n")
  
  // 测试Windows配置
  let windows_info = { ...system_info, os_type: OSType::Windows, platform: "windows" }
  let windows_config = get_platform_config(windows_info)
  assert_eq(windows_config.path_separator, "\\")
  assert_eq(windows_config.line_ending, "\r\n")
  assert_true(windows_config.log_file_path.contains("C:\\"))
  assert_true(windows_config.config_file_path.contains("C:\\"))
  
  // 测试macOS配置
  let macos_info = { ...system_info, os_type: OSType::MacOS, platform: "darwin" }
  let macos_config = get_platform_config(macos_info)
  assert_true(macos_config.log_file_path.contains("/Library/Logs"))
  assert_true(macos_config.config_file_path.contains("/Library/Application Support"))
}

// 测试2: 架构兼容性处理
test "架构兼容性处理" {
  // CPU架构类型
  enum Architecture {
    X86_64
    X86
    ARM64
    ARM
    PPC64LE
    S390X
    Unknown
  }
  
  // 架构特定配置
  type ArchConfig = {
    binary_name: String,
    library_extension: String,
    native_libs: Array[String],
    performance_tuning: Map[String, String],
    optimized_features: Array[String]
  }
  
  // 检测CPU架构
  let detect_architecture = fn() {
    // 在实际实现中，这里会检查系统信息
    // 这里简化为返回x86_64
    Architecture::X86_64
  }
  
  // 获取架构特定配置
  let get_arch_config = fn(arch: Architecture) {
    match arch {
      Architecture::X86_64 => {
        {
          binary_name: "azimuth-x86_64",
          library_extension: ".so",
          native_libs: [
            "libjemalloc.so",
            "libunwind.so",
            "libssl.so"
          ],
          performance_tuning: [
            ("cpu_features", "avx2,avx,sse4.2,sse2"),
            ("memory_allocator", "jemalloc"),
            ("thread_stack_size", "8MB")
          ],
          optimized_features: [
            "simd_optimizations",
            "vectorized_processing",
            "parallel_execution"
          ]
        }
      }
      Architecture::ARM64 => {
        {
          binary_name: "azimuth-arm64",
          library_extension: ".so",
          native_libs: [
            "libjemalloc.so",
            "libunwind.so",
            "libssl.so"
          ],
          performance_tuning: [
            ("cpu_features", "neon,asimd"),
            ("memory_allocator", "jemalloc"),
            ("thread_stack_size", "8MB")
          ],
          optimized_features: [
            "neon_optimizations",
            "vectorized_processing",
            "parallel_execution"
          ]
        }
      }
      Architecture::ARM => {
        {
          binary_name: "azimuth-arm",
          library_extension: ".so",
          native_libs: [
            "libjemalloc.so",
            "libunwind.so"
          ],
          performance_tuning: [
            ("cpu_features", "neon"),
            ("memory_allocator", "jemalloc"),
            ("thread_stack_size", "4MB")
          ],
          optimized_features: [
            "neon_optimizations",
            "low_power_mode"
          ]
        }
      }
      Architecture::X86 => {
        {
          binary_name: "azimuth-x86",
          library_extension: ".so",
          native_libs: [
            "libjemalloc.so",
            "libunwind.so"
          ],
          performance_tuning: [
            ("cpu_features", "sse2,sse"),
            ("memory_allocator", "jemalloc"),
            ("thread_stack_size", "4MB")
          ],
          optimized_features: [
            "sse_optimizations",
            "compatibility_mode"
          ]
        }
      }
      Architecture::PPC64LE => {
        {
          binary_name: "azimuth-ppc64le",
          library_extension: ".so",
          native_libs: [
            "libjemalloc.so",
            "libunwind.so"
          ],
          performance_tuning: [
            ("cpu_features", "altivec,vsx"),
            ("memory_allocator", "jemalloc"),
            ("thread_stack_size", "8MB")
          ],
          optimized_features: [
            "altivec_optimizations",
            "vectorized_processing"
          ]
        }
      }
      Architecture::S390X => {
        {
          binary_name: "azimuth-s390x",
          library_extension: ".so",
          native_libs: [
            "libjemalloc.so"
          ],
          performance_tuning: [
            ("cpu_features", "zvector"),
            ("memory_allocator", "jemalloc"),
            ("thread_stack_size", "8MB")
          ],
          optimized_features: [
            "zvector_optimizations",
            "enterprise_features"
          ]
        }
      }
      Architecture::Unknown => {
        {
          binary_name: "azimuth",
          library_extension: ".so",
          native_libs: [],
          performance_tuning: [],
          optimized_features: [
            "compatibility_mode"
          ]
        }
      }
    }
  }
  
  // 测试架构检测
  let arch = detect_architecture()
  assert_eq(arch, Architecture::X86_64)
  
  // 测试x86_64配置
  let x86_64_config = get_arch_config(Architecture::X86_64)
  assert_eq(x86_64_config.binary_name, "azimuth-x86_64")
  assert_eq(x86_64_config.library_extension, ".so")
  assert_true(x86_64_config.native_libs.contains("libjemalloc.so"))
  assert_true(x86_64_config.performance_tuning.contains(("cpu_features", "avx2,avx,sse4.2,sse2")))
  assert_true(x86_64_config.optimized_features.contains("simd_optimizations"))
  
  // 测试ARM64配置
  let arm64_config = get_arch_config(Architecture::ARM64)
  assert_eq(arm64_config.binary_name, "azimuth-arm64")
  assert_true(arm64_config.performance_tuning.contains(("cpu_features", "neon,asimd")))
  assert_true(arm64_config.optimized_features.contains("neon_optimizations"))
  
  // 测试ARM配置
  let arm_config = get_arch_config(Architecture::ARM)
  assert_eq(arm_config.binary_name, "azimuth-arm")
  assert_true(arm_config.performance_tuning.contains(("cpu_features", "neon")))
  assert_true(arm_config.optimized_features.contains("low_power_mode"))
  
  // 测试未知架构
  let unknown_config = get_arch_config(Architecture::Unknown)
  assert_eq(unknown_config.binary_name, "azimuth")
  assert_eq(unknown_config.native_libs.length(), 0)
  assert_eq(unknown_config.performance_tuning.size(), 0)
  assert_true(unknown_config.optimized_features.contains("compatibility_mode"))
}

// 测试3: 运行时环境兼容性
test "运行时环境兼容性" {
  // 运行时类型
  enum RuntimeType {
    NodeJS
    Python
    Java
    Go
    DotNet
    Ruby
    Native
    Unknown
  }
  
  // 运行时信息
  type RuntimeInfo = {
    runtime_type: RuntimeType,
    version: String,
    platform: String,
    architecture: String,
    environment_variables: Map[String, String]
  }
  
  // 运行时特定配置
  type RuntimeConfig = {
    telemetry_library: String,
    initialization_code: String,
    configuration_format: String,
    supported_features: Array[String],
    limitations: Array[String]
  }
  
  // 检测运行时环境
  let detect_runtime = fn() {
    // 在实际实现中，这里会检查环境变量或运行时API
    // 这里简化为返回Native
    RuntimeType::Native
  }
  
  // 获取运行时信息
  let get_runtime_info = fn() {
    let runtime_type = detect_runtime()
    
    {
      runtime_type: runtime_type,
      version: "1.0.0",
      platform: "linux",
      architecture: "x86_64",
      environment_variables: [
        ("PATH", "/usr/bin:/bin"),
        ("HOME", "/home/user")
      ]
    }
  }
  
  // 获取运行时特定配置
  let get_runtime_config = fn(runtime_info: RuntimeInfo) {
    match runtime_info.runtime_type {
      RuntimeType::NodeJS => {
        {
          telemetry_library: "@opentelemetry/api",
          initialization_code: "const { NodeSDK } = require('@opentelemetry/sdk-node');",
          configuration_format: "javascript",
          supported_features: [
            "async_hooks",
            "event_loop_monitoring",
            "gc_tracking",
            "http_tracing"
          ],
          limitations: [
            "single_threaded",
            "memory_limitations"
          ]
        }
      }
      RuntimeType::Python => {
        {
          telemetry_library: "opentelemetry-api",
          initialization_code: "from opentelemetry import trace",
          configuration_format: "yaml",
          supported_features: [
            "threading_support",
            "asyncio_support",
            "gc_tracking",
            "http_tracing"
          ],
          limitations: [
            "gil_limitations",
            "memory_overhead"
          ]
        }
      }
      RuntimeType::Java => {
        {
          telemetry_library: "io.opentelemetry:opentelemetry-api",
          initialization_code: "import io.opentelemetry.api.OpenTelemetry;",
          configuration_format: "properties",
          supported_features: [
            "jit_optimizations",
            "garbage_collection",
            "thread_pool_management",
            "jvm_metrics"
          ],
          limitations: [
            "startup_overhead",
            "memory_consumption"
          ]
        }
      }
      RuntimeType::Go => {
        {
          telemetry_library: "go.opentelemetry.io/otel",
          initialization_code: "import \"go.opentelemetry.io/otel\"",
          configuration_format: "json",
          supported_features: [
            "goroutine_tracking",
            "garbage_collection",
            "channel_monitoring",
            "concurrent_execution"
          ],
          limitations: [
            "binary_size",
            "compile_time"
          ]
        }
      }
      RuntimeType::Native => {
        {
          telemetry_library: "azimuth-native",
          initialization_code: "#include <azimuth/telemetry.h>",
          configuration_format: "toml",
          supported_features: [
            "zero_copy_operations",
            "memory_efficiency",
            "low_latency",
            "system_integration"
          ],
          limitations: [
            "platform_specific",
            "manual_memory_management"
          ]
        }
      }
      _ => {
        {
          telemetry_library: "generic",
          initialization_code: "// Generic initialization",
          configuration_format: "json",
          supported_features: [
            "basic_tracing"
          ],
          limitations: [
            "limited_features"
          ]
        }
      }
    }
  }
  
  // 测试运行时检测
  let runtime_info = get_runtime_info()
  assert_eq(runtime_info.runtime_type, RuntimeType::Native)
  assert_eq(runtime_info.version, "1.0.0")
  assert_eq(runtime_info.platform, "linux")
  assert_eq(runtime_info.architecture, "x86_64")
  
  // 测试Native运行时配置
  let native_config = get_runtime_config(runtime_info)
  assert_eq(native_config.telemetry_library, "azimuth-native")
  assert_eq(native_config.configuration_format, "toml")
  assert_true(native_config.supported_features.contains("zero_copy_operations"))
  assert_true(native_config.limitations.contains("platform_specific"))
  
  // 测试NodeJS运行时配置
  let nodejs_info = { ...runtime_info, runtime_type: RuntimeType::NodeJS }
  let nodejs_config = get_runtime_config(nodejs_info)
  assert_eq(nodejs_config.telemetry_library, "@opentelemetry/api")
  assert_eq(nodejs_config.configuration_format, "javascript")
  assert_true(nodejs_config.supported_features.contains("async_hooks"))
  assert_true(nodejs_config.limitations.contains("single_threaded"))
  
  // 测试Python运行时配置
  let python_info = { ...runtime_info, runtime_type: RuntimeType::Python }
  let python_config = get_runtime_config(python_info)
  assert_eq(python_config.telemetry_library, "opentelemetry-api")
  assert_eq(python_config.configuration_format, "yaml")
  assert_true(python_config.supported_features.contains("threading_support"))
  assert_true(python_config.limitations.contains("gil_limitations"))
}

// 测试4: 网络环境兼容性
test "网络环境兼容性" {
  // 网络协议类型
  enum NetworkProtocol {
    HTTP
    HTTPS
    GRPC
    WebSocket
    TCP
    UDP
    QUIC
  }
  
  // 代理配置
  type ProxyConfig = {
    enabled: Bool,
    http_proxy: String,
    https_proxy: String,
    no_proxy: Array[String],
    auth_required: Bool,
    username: Option[String],
    password: Option[String]
  }
  
  // 网络环境配置
  type NetworkConfig = {
    protocols: Array[NetworkProtocol],
    endpoints: Map[NetworkProtocol, String],
    timeout_ms: Int,
    retry_attempts: Int,
    proxy_config: ProxyConfig,
    tls_config: TLSConfig
  }
  
  // TLS配置
  type TLSConfig = {
    enabled: Bool,
    verify_certificates: Bool,
    ca_cert_path: String,
    client_cert_path: String,
    client_key_path: String,
    min_version: String,
    max_version: String
  }
  
  // 检测网络环境
  let detect_network_environment = fn() {
    // 在实际实现中，这里会检查网络配置和环境变量
    {
      has_internet: true,
      has_proxy: false,
      restricted_network: false,
      corporate_network: false
    }
  }
  
  // 获取网络配置
  let get_network_config = fn(network_env) {
    let base_config = {
      protocols: [NetworkProtocol::HTTP, NetworkProtocol::HTTPS, NetworkProtocol::GRPC],
      endpoints: [
        (NetworkProtocol::HTTP, "http://localhost:4318/v1/traces"),
        (NetworkProtocol::HTTPS, "https://localhost:4318/v1/traces"),
        (NetworkProtocol::GRPC, "grpc://localhost:4317")
      ],
      timeout_ms: 5000,
      retry_attempts: 3,
      proxy_config: {
        enabled: network_env.has_proxy,
        http_proxy: "",
        https_proxy: "",
        no_proxy: [],
        auth_required: false,
        username: None,
        password: None
      },
      tls_config: {
        enabled: true,
        verify_certificates: true,
        ca_cert_path: "/etc/ssl/certs/ca-certificates.crt",
        client_cert_path: "",
        client_key_path: "",
        min_version: "1.2",
        max_version: "1.3"
      }
    }
    
    // 根据网络环境调整配置
    if network_env.restricted_network {
      { ...base_config, timeout_ms: 10000, retry_attempts: 5 }
    } else if network_env.corporate_network {
      let proxy_config = {
        ...base_config.proxy_config,
        enabled: true,
        http_proxy: "http://proxy.company.com:8080",
        https_proxy: "http://proxy.company.com:8080",
        no_proxy: ["localhost", "127.0.0.1"]
      }
      { ...base_config, proxy_config: proxy_config }
    } else {
      base_config
    }
  }
  
  // 测试网络环境检测
  let network_env = detect_network_environment()
  assert_true(network_env.has_internet)
  assert_false(network_env.has_proxy)
  assert_false(network_env.restricted_network)
  assert_false(network_env.corporate_network)
  
  // 测试标准网络配置
  let standard_config = get_network_config(network_env)
  assert_true(standard_config.protocols.contains(NetworkProtocol::HTTPS))
  assert_eq(standard_config.timeout_ms, 5000)
  assert_eq(standard_config.retry_attempts, 3)
  assert_false(standard_config.proxy_config.enabled)
  assert_true(standard_config.tls_config.enabled)
  
  // 测试受限网络配置
  let restricted_env = { ...network_env, restricted_network: true }
  let restricted_config = get_network_config(restricted_env)
  assert_eq(restricted_config.timeout_ms, 10000)
  assert_eq(restricted_config.retry_attempts, 5)
  
  // 测试企业网络配置
  let corporate_env = { ...network_env, corporate_network: true }
  let corporate_config = get_network_config(corporate_env)
  assert_true(corporate_config.proxy_config.enabled)
  assert_eq(corporate_config.proxy_config.http_proxy, "http://proxy.company.com:8080")
  assert_true(corporate_config.proxy_config.no_proxy.contains("localhost"))
}

// 测试5: 文件系统兼容性
test "文件系统兼容性" {
  // 文件系统类型
  enum FileSystemType {
    EXT4
    XFS
    NTFS
    APFS
    ZFS
    BTRFS
    FAT32
    Unknown
  }
  
  // 文件系统特性
  type FileSystemFeatures = {
    case_sensitive: Bool,
    max_filename_length: Int,
    max_path_length: Int,
    supports_permissions: Bool,
    supports_symlinks: Bool,
    supports_hardlinks: Bool,
    supports_acls: Bool,
    supports_xattrs: Bool
  }
  
  // 文件系统配置
  type FileSystemConfig = {
    fs_type: FileSystemType,
    features: FileSystemFeatures,
    telemetry_dir: String,
    log_dir: String,
    temp_dir: String,
    config_dir: String,
    permissions: Map[String, String]
  }
  
  // 检测文件系统类型
  let detect_filesystem_type = fn() {
    // 在实际实现中，这里会检查挂载点信息
    FileSystemType::EXT4
  }
  
  // 获取文件系统特性
  let get_filesystem_features = fn(fs_type: FileSystemType) {
    match fs_type {
      FileSystemType::EXT4 => {
        {
          case_sensitive: true,
          max_filename_length: 255,
          max_path_length: 4096,
          supports_permissions: true,
          supports_symlinks: true,
          supports_hardlinks: true,
          supports_acls: true,
          supports_xattrs: true
        }
      }
      FileSystemType::XFS => {
        {
          case_sensitive: true,
          max_filename_length: 255,
          max_path_length: 4096,
          supports_permissions: true,
          supports_symlinks: true,
          supports_hardlinks: true,
          supports_acls: true,
          supports_xattrs: true
        }
      }
      FileSystemType::NTFS => {
        {
          case_sensitive: false,
          max_filename_length: 255,
          max_path_length: 32767,
          supports_permissions: true,
          supports_symlinks: true,
          supports_hardlinks: true,
          supports_acls: true,
          supports_xattrs: true
        }
      }
      FileSystemType::APFS => {
        {
          case_sensitive: false, // 默认不区分大小写
          max_filename_length: 255,
          max_path_length: 1024,
          supports_permissions: true,
          supports_symlinks: true,
          supports_hardlinks: false,
          supports_acls: true,
          supports_xattrs: true
        }
      }
      FileSystemType::ZFS => {
        {
          case_sensitive: true,
          max_filename_length: 255,
          max_path_length: 1024,
          supports_permissions: true,
          supports_symlinks: true,
          supports_hardlinks: true,
          supports_acls: true,
          supports_xattrs: true
        }
      }
      FileSystemType::BTRFS => {
        {
          case_sensitive: true,
          max_filename_length: 255,
          max_path_length: 4096,
          supports_permissions: true,
          supports_symlinks: true,
          supports_hardlinks: true,
          supports_acls: true,
          supports_xattrs: true
        }
      }
      FileSystemType::FAT32 => {
        {
          case_sensitive: false,
          max_filename_length: 255, // 8.3格式限制
          max_path_length: 260,
          supports_permissions: false,
          supports_symlinks: false,
          supports_hardlinks: false,
          supports_acls: false,
          supports_xattrs: false
        }
      }
      FileSystemType::Unknown => {
        {
          case_sensitive: true,
          max_filename_length: 255,
          max_path_length: 1024,
          supports_permissions: false,
          supports_symlinks: false,
          supports_hardlinks: false,
          supports_acls: false,
          supports_xattrs: false
        }
      }
    }
  }
  
  // 获取文件系统配置
  let get_filesystem_config = fn(fs_type: FileSystemType, os_type: OSType) {
    let features = get_filesystem_features(fs_type)
    
    let base_dirs = match os_type {
      OSType::Windows => {
        (
          "C:\\ProgramData\\Azimuth\\Telemetry",
          "C:\\ProgramData\\Azimuth\\Logs",
          "C:\\Temp\\Azimuth",
          "C:\\ProgramData\\Azimuth\\Config"
        )
      }
      OSType::Linux => {
        (
          "/var/lib/azimuth/telemetry",
          "/var/log/azimuth",
          "/tmp/azimuth",
          "/etc/azimuth"
        )
      }
      OSType::MacOS => {
        (
          "/Library/Application Support/Azimuth/Telemetry",
          "/Library/Logs/Azimuth",
          "/tmp/azimuth",
          "/Library/Application Support/Azimuth/Config"
        )
      }
      _ => {
        (
          "./telemetry",
          "./logs",
          "./tmp",
          "./config"
        )
      }
    }
    
    let (telemetry_dir, log_dir, temp_dir, config_dir) = base_dirs
    
    let permissions = if features.supports_permissions {
      match os_type {
        OSType::Windows => [
          (telemetry_dir, "755"),
          (log_dir, "755"),
          (temp_dir, "777"),
          (config_dir, "644")
        ]
        _ => [
          (telemetry_dir, "755"),
          (log_dir, "755"),
          (temp_dir, "777"),
          (config_dir, "644")
        ]
      }
    } else {
      []
    }
    
    {
      fs_type: fs_type,
      features: features,
      telemetry_dir: telemetry_dir,
      log_dir: log_dir,
      temp_dir: temp_dir,
      config_dir: config_dir,
      permissions: permissions
    }
  }
  
  // 测试文件系统检测
  let fs_type = detect_filesystem_type()
  assert_eq(fs_type, FileSystemType::EXT4)
  
  // 测试EXT4文件系统特性
  let ext4_features = get_filesystem_features(FileSystemType::EXT4)
  assert_true(ext4_features.case_sensitive)
  assert_eq(ext4_features.max_filename_length, 255)
  assert_eq(ext4_features.max_path_length, 4096)
  assert_true(ext4_features.supports_permissions)
  assert_true(ext4_features.supports_symlinks)
  assert_true(ext4_features.supports_hardlinks)
  
  // 测试NTFS文件系统特性
  let ntfs_features = get_filesystem_features(FileSystemType::NTFS)
  assert_false(ntfs_features.case_sensitive)
  assert_eq(ntfs_features.max_path_length, 32767)
  assert_true(ntfs_features.supports_permissions)
  assert_true(ntfs_features.supports_symlinks)
  
  // 测试FAT32文件系统特性
  let fat32_features = get_filesystem_features(FileSystemType::FAT32)
  assert_false(fat32_features.case_sensitive)
  assert_false(fat32_features.supports_permissions)
  assert_false(fat32_features.supports_symlinks)
  assert_false(fat32_features.supports_hardlinks)
  
  // 测试Linux下的EXT4配置
  let linux_ext4_config = get_filesystem_config(FileSystemType::EXT4, OSType::Linux)
  assert_eq(linux_ext4_config.telemetry_dir, "/var/lib/azimuth/telemetry")
  assert_eq(linux_ext4_config.log_dir, "/var/log/azimuth")
  assert_eq(linux_ext4_config.temp_dir, "/tmp/azimuth")
  assert_eq(linux_ext4_config.config_dir, "/etc/azimuth")
  assert_true(linux_ext4_config.permissions.contains(("/var/lib/azimuth/telemetry", "755")))
  
  // 测试Windows下的NTFS配置
  let windows_ntfs_config = get_filesystem_config(FileSystemType::NTFS, OSType::Windows)
  assert_eq(windows_ntfs_config.telemetry_dir, "C:\\ProgramData\\Azimuth\\Telemetry")
  assert_eq(windows_ntfs_config.log_dir, "C:\\ProgramData\\Azimuth\\Logs")
  assert_eq(windows_ntfs_config.temp_dir, "C:\\Temp\\Azimuth")
  assert_eq(windows_ntfs_config.config_dir, "C:\\ProgramData\\Azimuth\\Config")
}