// Azimuth Premium Cross-Platform Compatibility Tests
// 高级跨平台兼容性测试用例，专注于遥测系统在不同平台和环境中的兼容性

// 测试 1: 操作系统兼容性
test "operating system compatibility" {
  // 定义操作系统类型
  enum OperatingSystem {
    Windows
    Linux
    MacOS
    FreeBSD
    Unknown
  }
  
  // 定义平台特性
  type PlatformFeatures = {
    file_separator: String,
    line_separator: String,
    path_separator: String,
    case_sensitive: Bool,
    max_path_length: Int,
    temp_directory: String
  }
  
  // 创建平台检测器
  let create_platform_detector = fn() {
    // 检测操作系统
    let detect_os = fn() {
      // 简化的操作系统检测（实际实现中应使用系统API）
      let os_info = "Linux"  // 模拟当前系统
      
      match os_info {
        "Windows" => OperatingSystem::Windows
        "Linux" => OperatingSystem::Linux
        "MacOS" => OperatingSystem::MacOS
        "FreeBSD" => OperatingSystem::FreeBSD
        _ => OperatingSystem::Unknown
      }
    }
    
    // 获取平台特性
    let get_platform_features = fn(os: OperatingSystem) {
      match os {
        OperatingSystem::Windows => {
          {
            file_separator: "\\",
            line_separator: "\r\n",
            path_separator: ";",
            case_sensitive: false,
            max_path_length: 260,
            temp_directory: "C:\\Temp"
          }
        }
        OperatingSystem::Linux => {
          {
            file_separator: "/",
            line_separator: "\n",
            path_separator: ":",
            case_sensitive: true,
            max_path_length: 4096,
            temp_directory: "/tmp"
          }
        }
        OperatingSystem::MacOS => {
          {
            file_separator: "/",
            line_separator: "\n",
            path_separator: ":",
            case_sensitive: true,
            max_path_length: 1024,
            temp_directory: "/tmp"
          }
        }
        OperatingSystem::FreeBSD => {
          {
            file_separator: "/",
            line_separator: "\n",
            path_separator: ":",
            case_sensitive: true,
            max_path_length: 1024,
            temp_directory: "/tmp"
          }
        }
        OperatingSystem::Unknown => {
          {
            file_separator: "/",
            line_separator: "\n",
            path_separator: ":",
            case_sensitive: true,
            max_path_length: 1024,
            temp_directory: "/tmp"
          }
        }
      }
    }
    
    // 规范化路径
    let normalize_path = fn(path: String, features: PlatformFeatures) {
      // 替换文件分隔符
      let normalized = path.replace("\\", features.file_separator).replace("/", features.file_separator)
      
      // 处理大小写敏感性
      if features.case_sensitive {
        normalized
      } else {
        normalized.to_lowercase()
      }
    }
    
    {
      detect_os,
      get_platform_features,
      normalize_path
    }
  }
  
  // 测试操作系统兼容性
  let detector = create_platform_detector()
  
  // 检测操作系统
  let os = detector.detect_os()
  
  // 获取平台特性
  let features = detector.get_platform_features(os)
  
  // 验证Linux特性
  if os == OperatingSystem::Linux {
    assert_eq(features.file_separator, "/")
    assert_eq(features.line_separator, "\n")
    assert_eq(features.path_separator, ":")
    assert_true(features.case_sensitive)
    assert_eq(features.max_path_length, 4096)
    assert_eq(features.temp_directory, "/tmp")
  }
  
  // 测试路径规范化
  let test_path = "C:\\Users\\test\\file.txt"
  let normalized_path = detector.normalize_path(test_path, features)
  
  if os == OperatingSystem::Linux {
    assert_eq(normalized_path, "c:/users/test/file.txt")
  }
  
  // 测试Windows特性
  let windows_features = detector.get_platform_features(OperatingSystem::Windows)
  assert_eq(windows_features.file_separator, "\\")
  assert_eq(windows_features.line_separator, "\r\n")
  assert_eq(windows_features.path_separator, ";")
  assert_false(windows_features.case_sensitive)
  assert_eq(windows_features.max_path_length, 260)
  assert_eq(windows_features.temp_directory, "C:\\Temp")
  
  // 测试MacOS特性
  let macos_features = detector.get_platform_features(OperatingSystem::MacOS)
  assert_eq(macos_features.file_separator, "/")
  assert_eq(macos_features.line_separator, "\n")
  assert_eq(macos_features.path_separator, ":")
  assert_true(macos_features.case_sensitive)
  assert_eq(macos_features.max_path_length, 1024)
  assert_eq(macos_features.temp_directory, "/tmp")
}

// 测试 2: 架构兼容性
test "architecture compatibility" {
  // 定义处理器架构
  enum ProcessorArchitecture {
    X86
    X64
    ARM
    ARM64
    Unknown
  }
  
  // 定义字节序
  enum Endianness {
    LittleEndian
    BigEndian
  }
  
  // 定义架构特性
  type ArchitectureFeatures = {
    architecture: ProcessorArchitecture,
    endianness: Endianness,
    pointer_size: Int,
    max_alignment: Int,
    atomic_operations: Bool
  }
  
  // 创建架构检测器
  let create_architecture_detector = fn() {
    // 检测处理器架构
    let detect_architecture = fn() {
      // 简化的架构检测（实际实现中应使用系统API）
      let arch_info = "x86_64"  // 模拟当前架构
      
      match arch_info {
        "x86" => ProcessorArchitecture::X86
        "x86_64" | "amd64" => ProcessorArchitecture::X64
        "arm" => ProcessorArchitecture::ARM
        "arm64" | "aarch64" => ProcessorArchitecture::ARM64
        _ => ProcessorArchitecture::Unknown
      }
    }
    
    // 检测字节序
    let detect_endianness = fn() {
      // 简化的字节序检测
      let test_value = 0x12345678
      let first_byte = test_value & 0xFF
      
      if first_byte == 0x78 {
        Endianness::LittleEndian
      } else {
        Endianness::BigEndian
      }
    }
    
    // 获取架构特性
    let get_architecture_features = fn(arch: ProcessorArchitecture, endianness: Endianness) {
      match arch {
        ProcessorArchitecture::X86 => {
          {
            architecture: ProcessorArchitecture::X86,
            endianness,
            pointer_size: 4,
            max_alignment: 16,
            atomic_operations: true
          }
        }
        ProcessorArchitecture::X64 => {
          {
            architecture: ProcessorArchitecture::X64,
            endianness,
            pointer_size: 8,
            max_alignment: 16,
            atomic_operations: true
          }
        }
        ProcessorArchitecture::ARM => {
          {
            architecture: ProcessorArchitecture::ARM,
            endianness,
            pointer_size: 4,
            max_alignment: 8,
            atomic_operations: true
          }
        }
        ProcessorArchitecture::ARM64 => {
          {
            architecture: ProcessorArchitecture::ARM64,
            endianness,
            pointer_size: 8,
            max_alignment: 16,
            atomic_operations: true
          }
        }
        ProcessorArchitecture::Unknown => {
          {
            architecture: ProcessorArchitecture::Unknown,
            endianness,
            pointer_size: 8,
            max_alignment: 16,
            atomic_operations: false
          }
        }
      }
    }
    
    // 检查兼容性
    let check_compatibility = fn(required: ProcessorArchitecture, current: ProcessorArchitecture) {
      match (required, current) {
        // X64应用可以在X64系统上运行
        (ProcessorArchitecture::X64, ProcessorArchitecture::X64) => true
        // X86应用可以在X64系统上运行（通过兼容层）
        (ProcessorArchitecture::X86, ProcessorArchitecture::X64) => true
        // ARM64应用可以在ARM64系统上运行
        (ProcessorArchitecture::ARM64, ProcessorArchitecture::ARM64) => true
        // ARM应用可以在ARM64系统上运行（通过兼容层）
        (ProcessorArchitecture::ARM, ProcessorArchitecture::ARM64) => true
        // 相同架构总是兼容
        (a, b) if a == b => true
        // 其他情况不兼容
        _ => false
      }
    }
    
    {
      detect_architecture,
      detect_endianness,
      get_architecture_features,
      check_compatibility
    }
  }
  
  // 测试架构兼容性
  let detector = create_architecture_detector()
  
  // 检测架构
  let arch = detector.detect_architecture()
  let endianness = detector.detect_endianness()
  
  // 获取架构特性
  let features = detector.get_architecture_features(arch, endianness)
  
  // 验证X64特性
  if arch == ProcessorArchitecture::X64 {
    assert_eq(features.architecture, ProcessorArchitecture::X64)
    assert_eq(features.pointer_size, 8)
    assert_eq(features.max_alignment, 16)
    assert_true(features.atomic_operations)
  }
  
  // 测试兼容性检查
  assert_true(detector.check_compatibility(ProcessorArchitecture::X86, ProcessorArchitecture::X64))
  assert_true(detector.check_compatibility(ProcessorArchitecture::X64, ProcessorArchitecture::X64))
  assert_false(detector.check_compatibility(ProcessorArchitecture::X64, ProcessorArchitecture::X86))
  
  assert_true(detector.check_compatibility(ProcessorArchitecture::ARM, ProcessorArchitecture::ARM64))
  assert_true(detector.check_compatibility(ProcessorArchitecture::ARM64, ProcessorArchitecture::ARM64))
  assert_false(detector.check_compatibility(ProcessorArchitecture::ARM64, ProcessorArchitecture::ARM))
  
  // 测试不同架构的特性
  let x86_features = detector.get_architecture_features(ProcessorArchitecture::X86, Endianness::LittleEndian)
  assert_eq(x86_features.pointer_size, 4)
  
  let arm64_features = detector.get_architecture_features(ProcessorArchitecture::ARM64, Endianness::LittleEndian)
  assert_eq(arm64_features.pointer_size, 8)
}

// 测试 3: 运行时环境兼容性
test "runtime environment compatibility" {
  // 定义运行时类型
  enum RuntimeType {
    NodeJS
    Browser
    Deno
    Bun
    WebAssembly
    Unknown
  }
  
  // 定义运行时特性
  type RuntimeFeatures = {
    runtime: RuntimeType,
    version: String,
    supports_fs: Bool,
    supports_network: Bool,
    supports_worker: Bool,
    supports_async: Bool,
    max_memory: Int
  }
  
  // 创建运行时检测器
  let create_runtime_detector = fn() {
    // 检测运行时环境
    let detect_runtime = fn() {
      // 简化的运行时检测（实际实现中应使用全局变量检查）
      let has_process = true  // 模拟检查
      let has_window = false   // 模拟检查
      let has_deno = false     // 模拟检查
      let has_bun = false      // 模拟检查
      
      if has_window {
        RuntimeType::Browser
      } else if has_deno {
        RuntimeType::Deno
      } else if has_bun {
        RuntimeType::Bun
      } else if has_process {
        RuntimeType::NodeJS
      } else {
        RuntimeType::Unknown
      }
    }
    
    // 获取运行时版本
    let get_runtime_version = fn(runtime: RuntimeType) {
      match runtime {
        RuntimeType::NodeJS => "18.17.0"
        RuntimeType::Browser => "Chrome/120.0"
        RuntimeType::Deno => "1.38.0"
        RuntimeType::Bun => "1.0.0"
        RuntimeType::WebAssembly => "1.0"
        RuntimeType::Unknown => "Unknown"
      }
    }
    
    // 获取运行时特性
    let get_runtime_features = fn(runtime: RuntimeType, version: String) {
      match runtime {
        RuntimeType::NodeJS => {
          {
            runtime: RuntimeType::NodeJS,
            version,
            supports_fs: true,
            supports_network: true,
            supports_worker: true,
            supports_async: true,
            max_memory: 2048  // MB
          }
        }
        RuntimeType::Browser => {
          {
            runtime: RuntimeType::Browser,
            version,
            supports_fs: false,
            supports_network: true,
            supports_worker: true,
            supports_async: true,
            max_memory: 1024  // MB
          }
        }
        RuntimeType::Deno => {
          {
            runtime: RuntimeType::Deno,
            version,
            supports_fs: true,
            supports_network: true,
            supports_worker: true,
            supports_async: true,
            max_memory: 2048  // MB
          }
        }
        RuntimeType::Bun => {
          {
            runtime: RuntimeType::Bun,
            version,
            supports_fs: true,
            supports_network: true,
            supports_worker: true,
            supports_async: true,
            max_memory: 4096  // MB
          }
        }
        RuntimeType::WebAssembly => {
          {
            runtime: RuntimeType::WebAssembly,
            version,
            supports_fs: false,
            supports_network: false,
            supports_worker: false,
            supports_async: false,
            max_memory: 512  // MB
          }
        }
        RuntimeType::Unknown => {
          {
            runtime: RuntimeType::Unknown,
            version,
            supports_fs: false,
            supports_network: false,
            supports_worker: false,
            supports_async: false,
            max_memory: 256  // MB
          }
        }
      }
    }
    
    // 检查功能兼容性
    let check_feature_support = fn(required_features: Array<String>, features: RuntimeFeatures) {
      let mut supported = true
      
      for feature in required_features {
        match feature {
          "fs" => {
            if not features.supports_fs {
              supported = false
            }
          }
          "network" => {
            if not features.supports_network {
              supported = false
            }
          }
          "worker" => {
            if not features.supports_worker {
              supported = false
            }
          }
          "async" => {
            if not features.supports_async {
              supported = false
            }
          }
          _ => {
            // 未知功能，假设不支持
            supported = false
          }
        }
      }
      
      supported
    }
    
    {
      detect_runtime,
      get_runtime_version,
      get_runtime_features,
      check_feature_support
    }
  }
  
  // 测试运行时环境兼容性
  let detector = create_runtime_detector()
  
  // 检测运行时
  let runtime = detector.detect_runtime()
  let version = detector.get_runtime_version(runtime)
  
  // 获取运行时特性
  let features = detector.get_runtime_features(runtime, version)
  
  // 验证NodeJS特性
  if runtime == RuntimeType::NodeJS {
    assert_eq(features.runtime, RuntimeType::NodeJS)
    assert_true(features.supports_fs)
    assert_true(features.supports_network)
    assert_true(features.supports_worker)
    assert_true(features.supports_async)
    assert_eq(features.max_memory, 2048)
  }
  
  // 测试功能兼容性检查
  let nodejs_features = detector.get_runtime_features(RuntimeType::NodeJS, "18.17.0")
  let browser_features = detector.get_runtime_features(RuntimeType::Browser, "Chrome/120.0")
  
  // NodeJS支持所有功能
  assert_true(detector.check_feature_support(["fs", "network", "worker", "async"], nodejs_features))
  
  // 浏览器不支持文件系统
  assert_false(detector.check_feature_support(["fs"], browser_features))
  assert_true(detector.check_feature_support(["network", "worker", "async"], browser_features))
  
  // 测试不同运行时的特性
  let deno_features = detector.get_runtime_features(RuntimeType::Deno, "1.38.0")
  assert_true(deno_features.supports_fs)
  assert_true(deno_features.supports_network)
  
  let wasm_features = detector.get_runtime_features(RuntimeType::WebAssembly, "1.0")
  assert_false(wasm_features.supports_fs)
  assert_false(wasm_features.supports_network)
  assert_false(wasm_features.supports_worker)
  assert_false(wasm_features.supports_async)
}

// 测试 4: API兼容性
test "API compatibility" {
  // 定义API版本
  type APIVersion = {
    major: Int,
    minor: Int,
    patch: Int
  }
  
  // 定义API特性
  type APIFeatures = {
    version: APIVersion,
    deprecated: Array[String],
    added: Array[String>,
    removed: Array[String],
    breaking_changes: Array[String]
  }
  
  // 创建API兼容性检查器
  let create_api_compatibility_checker = fn() {
    // 比较API版本
    let compare_api_versions = fn(v1: APIVersion, v2: APIVersion) {
      if v1.major > v2.major {
        1
      } else if v1.major < v2.major {
        -1
      } else if v1.minor > v2.minor {
        1
      } else if v1.minor < v2.minor {
        -1
      } else if v1.patch > v2.patch {
        1
      } else if v1.patch < v2.patch {
        -1
      } else {
        0
      }
    }
    
    // 检查API兼容性
    let check_api_compatibility = fn(required: APIVersion, available: APIVersion, features: APIFeatures) {
      // 主版本号必须相同
      if required.major != available.major {
        return {
          compatible: false,
          issues: ["主版本号不匹配"]
        }
      }
      
      let mut issues = []
      
      // 检查已移除的API
      for removed in features.removed {
        if compare_api_versions(required, { major: available.major, minor: available.minor - 1, patch: 0 }) >= 0 {
          issues = issues.push("API已移除: " + removed)
        }
      }
      
      // 检查重大变更
      for breaking in features.breaking_changes {
        issues = issues.push("重大变更: " + breaking)
      }
      
      {
        compatible: issues.length() == 0,
        issues
      }
    }
    
    // 获取API版本信息
    let get_api_version_info = fn(version: String) {
      let parts = version.split(".")
      if parts.length() >= 2 {
        {
          major: parts[0].to_int(),
          minor: parts[1].to_int(),
          patch: if parts.length() > 2 { parts[2].to_int() } else { 0 }
        }
      } else {
        {
          major: 1,
          minor: 0,
          patch: 0
        }
      }
    }
    
    {
      compare_api_versions,
      check_api_compatibility,
      get_api_version_info
    }
  }
  
  // 测试API兼容性
  let checker = create_api_compatibility_checker()
  
  // 定义API版本
  let v1_0 = { major: 1, minor: 0, patch: 0 }
  let v1_1 = { major: 1, minor: 1, patch: 0 }
  let v1_2 = { major: 1, minor: 2, patch: 0 }
  let v2_0 = { major: 2, minor: 0, patch: 0 }
  
  // 测试版本比较
  assert_eq(checker.compare_api_versions(v1_0, v1_1), -1)
  assert_eq(checker.compare_api_versions(v1_1, v1_0), 1)
  assert_eq(checker.compare_api_versions(v1_1, v1_1), 0)
  
  // 测试API兼容性
  let features_v1_2 = {
    version: v1_2,
    deprecated: ["old_method"],
    added: ["new_method", "enhanced_feature"],
    removed: [],
    breaking_changes: []
  }
  
  let compatible_result = checker.check_api_compatibility(v1_1, v1_2, features_v1_2)
  assert_true(compatible_result.compatible)
  
  // 测试不兼容的API
  let features_v2_0 = {
    version: v2_0,
    deprecated: [],
    added: ["completely_new_api"],
    removed: ["legacy_method"],
    breaking_changes: ["parameter_order_changed"]
  }
  
  let incompatible_result = checker.check_api_compatibility(v1_2, v2_0, features_v2_0)
  assert_false(incompatible_result.compatible)
  assert_true(incompatible_result.issues.length() > 0)
  
  // 测试版本字符串解析
  let parsed_version = checker.get_api_version_info("1.2.3")
  assert_eq(parsed_version.major, 1)
  assert_eq(parsed_version.minor, 2)
  assert_eq(parsed_version.patch, 3)
}

// 测试 5: 数据格式兼容性
test "data format compatibility" {
  // 定义数据格式版本
  enum DataFormatVersion {
    V1_0
    V1_1
    V2_0
    Unknown
  }
  
  // 定义格式转换器
  type FormatConverter = {
    from_version: DataFormatVersion,
    to_version: DataFormatVersion,
    convert: (String) -> String
  }
  
  // 创建数据格式兼容性管理器
  let create_format_compatibility_manager = fn() {
    // 检测数据格式版本
    let detect_format_version = fn(data: String) {
      if data.contains("\"version\":\"1.0\"") {
        DataFormatVersion::V1_0
      } else if data.contains("\"version\":\"1.1\"") {
        DataFormatVersion::V1_1
      } else if data.contains("\"version\":\"2.0\"") {
        DataFormatVersion::V2_0
      } else {
        DataFormatVersion::Unknown
      }
    }
    
    // 检查版本兼容性
    let is_version_compatible = fn(from: DataFormatVersion, to: DataFormatVersion) {
      match (from, to) {
        // 相同版本总是兼容
        (a, b) if a == b => true
        // 1.1向后兼容1.0
        (DataFormatVersion::V1_0, DataFormatVersion::V1_1) => true
        (DataFormatVersion::V1_1, DataFormatVersion::V1_0) => true
        // 2.0不向后兼容1.x
        (DataFormatVersion::V1_0, DataFormatVersion::V2_0) => false
        (DataFormatVersion::V1_1, DataFormatVersion::V2_0) => false
        (DataFormatVersion::V2_0, DataFormatVersion::V1_0) => false
        (DataFormatVersion::V2_0, DataFormatVersion::V1_1) => false
        // 未知版本不兼容
        _ => false
      }
    }
    
    // 转换数据格式
    let convert_format = fn(data: String, from: DataFormatVersion, to: DataFormatVersion) {
      if from == to {
        return data
      }
      
      match (from, to) {
        // 1.0转1.1：添加新字段
        (DataFormatVersion::V1_0, DataFormatVersion::V1_1) => {
          data.replace("\"version\":\"1.0\"", "\"version\":\"1.1\"")
            .replace("}", ",\"new_field\":\"default_value\"}")
        }
        // 1.1转1.0：移除新字段
        (DataFormatVersion::V1_1, DataFormatVersion::V1_0) => {
          data.replace("\"version\":\"1.1\"", "\"version\":\"1.0\"")
            .replace(",\"new_field\":\"default_value\"", "")
        }
        // 1.0转2.0：重大结构变更
        (DataFormatVersion::V1_0, DataFormatVersion::V2_0) => {
          // 简化的转换逻辑
          "{\"version\":\"2.0\",\"data\":" + data + "}"
        }
        // 1.1转2.0：重大结构变更
        (DataFormatVersion::V1_1, DataFormatVersion::V2_0) => {
          // 简化的转换逻辑
          "{\"version\":\"2.0\",\"data\":" + data + "}"
        }
        // 其他转换不支持
        _ => {
          "Error: Unsupported conversion from " + format_version_to_string(from) + " to " + format_version_to_string(to)
        }
      }
    }
    
    // 版本转字符串
    let format_version_to_string = fn(version: DataFormatVersion) {
      match version {
        DataFormatVersion::V1_0 => "1.0"
        DataFormatVersion::V1_1 => "1.1"
        DataFormatVersion::V2_0 => "2.0"
        DataFormatVersion::Unknown => "unknown"
      }
    }
    
    // 字符串转版本
    let string_to_format_version = fn(version_str: String) {
      match version_str {
        "1.0" => DataFormatVersion::V1_0
        "1.1" => DataFormatVersion::V1_1
        "2.0" => DataFormatVersion::V2_0
        _ => DataFormatVersion::Unknown
      }
    }
    
    {
      detect_format_version,
      is_version_compatible,
      convert_format,
      format_version_to_string,
      string_to_format_version
    }
  }
  
  // 测试数据格式兼容性
  let manager = create_format_compatibility_manager()
  
  // 创建测试数据
  let data_v1_0 = "{\"version\":\"1.0\",\"name\":\"test\",\"value\":123}"
  let data_v1_1 = "{\"version\":\"1.1\",\"name\":\"test\",\"value\":123,\"new_field\":\"default_value\"}"
  
  // 检测格式版本
  let detected_v1_0 = manager.detect_format_version(data_v1_0)
  let detected_v1_1 = manager.detect_format_version(data_v1_1)
  
  assert_eq(detected_v1_0, DataFormatVersion::V1_0)
  assert_eq(detected_v1_1, DataFormatVersion::V1_1)
  
  // 测试版本兼容性
  assert_true(manager.is_version_compatible(DataFormatVersion::V1_0, DataFormatVersion::V1_1))
  assert_true(manager.is_version_compatible(DataFormatVersion::V1_1, DataFormatVersion::V1_0))
  assert_false(manager.is_version_compatible(DataFormatVersion::V1_0, DataFormatVersion::V2_0))
  
  // 测试格式转换
  let converted_v1_0_to_v1_1 = manager.convert_format(data_v1_0, DataFormatVersion::V1_0, DataFormatVersion::V1_1)
  assert_true(converted_v1_0_to_v1_1.contains("\"version\":\"1.1\""))
  assert_true(converted_v1_0_to_v1_1.contains("\"new_field\":\"default_value\""))
  
  let converted_v1_1_to_v1_0 = manager.convert_format(data_v1_1, DataFormatVersion::V1_1, DataFormatVersion::V1_0)
  assert_true(converted_v1_1_to_v1_0.contains("\"version\":\"1.0\""))
  assert_false(converted_v1_1_to_v1_0.contains("\"new_field\""))
  
  let converted_v1_0_to_v2_0 = manager.convert_format(data_v1_0, DataFormatVersion::V1_0, DataFormatVersion::V2_0)
  assert_true(converted_v1_0_to_v2_0.contains("\"version\":\"2.0\""))
  assert_true(converted_v1_0_to_v2_0.contains("\"data\":"))
  
  // 测试版本字符串转换
  assert_eq(manager.format_version_to_string(DataFormatVersion::V1_0), "1.0")
  assert_eq(manager.format_version_to_string(DataFormatVersion::V2_0), "2.0")
  
  assert_eq(manager.string_to_format_version("1.1"), DataFormatVersion::V1_1)
  assert_eq(manager.string_to_format_version("unknown"), DataFormatVersion::Unknown)
}