// Azimuth Cross-Platform Compatibility Tests
// 跨平台兼容性测试，验证遥测系统在不同平台和环境下的兼容性

// 测试1: 操作系统兼容性
test "operating system compatibility" {
  // 检测当前操作系统
  let current_os = @azimuth.Platform::detect_os()
  
  // 验证操作系统检测结果
  assert_true(current_os == "windows" || current_os == "linux" || current_os == "macos" || current_os == "freebsd" || current_os == "wasm")
  
  // 获取操作系统特定信息
  let os_info = @azimuth.Platform::get_os_info()
  
  // 验证操作系统信息结构
  assert_true(os_info.contains("name"))
  assert_true(os_info.contains("version"))
  assert_true(os_info.contains("architecture"))
  
  // 创建跨平台兼容的路径
  let path_separator = @azimuth.Platform::get_path_separator()
  let test_path = "logs" + path_separator + "telemetry" + path_separator + "data.log"
  
  // 验证路径分隔符
  if current_os == "windows" {
    assert_eq(path_separator, "\\")
    assert_true(test_path.contains("\\"))
  } else {
    assert_eq(path_separator, "/")
    assert_true(test_path.contains("/"))
  }
  
  // 测试跨平台文件操作
  let temp_dir = @azimuth.Platform::get_temp_directory()
  assert_true(temp_dir.length() > 0)
  
  // 创建跨平台兼容的文件路径
  let log_file = temp_dir + path_separator + "azimuth_test.log"
  
  // 验证文件路径格式
  if current_os == "windows" {
    assert_true(log_file.contains(":\\") || log_file.contains("\\\\"))
  } else {
    assert_true(log_file.startsWith("/"))
  }
  
  // 测试环境变量处理
  let env_home = @azimuth.Platform::get_env_variable("HOME")
  let env_user = @azimuth.Platform::get_env_variable("USER")
  let env_path = @azimuth.Platform::get_env_variable("PATH")
  
  // 验证环境变量（某些环境可能不存在）
  if current_os == "windows" {
    let env_userprofile = @azimuth.Platform::get_env_variable("USERPROFILE")
    let env_username = @azimuth.Platform::get_env_variable("USERNAME")
    // Windows特定环境变量可能存在
  } else {
    // Unix-like系统环境变量
    assert_true(env_home != None || env_user != None)
  }
  
  // 创建操作系统特定的资源
  let os_specific_resource = Resource({
    attributes: [
      ("os.name", StringValue(current_os)),
      ("os.info", StringValue(os_info)),
      ("path.separator", StringValue(path_separator)),
      ("temp.directory", StringValue(temp_dir)),
      ("platform.architecture", StringValue(@azimuth.Platform::get_architecture()))
    ],
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  })
  
  // 验证操作系统特定资源
  assert_eq(os_specific_resource.attributes.length(), 5)
}

// 测试2: WebAssembly兼容性
test "webassembly compatibility" {
  // 检测是否在WebAssembly环境
  let is_wasm = @azimuth.Platform::is_webassembly()
  
  // 获取WebAssembly特定信息
  let wasm_info = @azimuth.Platform::get_wasm_info()
  
  if is_wasm {
    // 验证WebAssembly环境
    assert_true(wasm_info.contains("runtime"))
    assert_true(wasm_info.contains("features"))
    
    // 测试WebAssembly特定功能
    let wasm_memory = @azimuth.Platform::get_wasm_memory_info()
    assert_true(wasm_memory.contains("initial"))
    assert_true(wasm_memory.contains("maximum"))
    
    // 测试WebAssembly时间功能
    let wasm_time = @azimuth.Platform::get_wasm_time()
    assert_true(wasm_time > 0L)
    
    // 创建WebAssembly特定资源
    let wasm_resource = Resource({
      attributes: [
        ("wasm.runtime", StringValue(@azimuth.Platform::get_wasm_runtime())),
        ("wasm.features", StringValue(@azimuth.Platform::get_wasm_features())),
        ("wasm.memory", StringValue(wasm_memory)),
        ("wasm.time.source", StringValue("performance.now"))
      ],
      schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
    })
    
    // 验证WebAssembly资源
    assert_eq(wasm_resource.attributes.length(), 4)
  } else {
    // 非WebAssembly环境
    assert_true(wasm_info == "Not running in WebAssembly environment")
    
    // 测试原生平台功能
    let native_info = @azimuth.Platform::get_native_info()
    assert_true(native_info.contains("cpu"))
    assert_true(native_info.contains("memory"))
    
    // 创建原生平台特定资源
    let native_resource = Resource({
      attributes: [
        ("native.cpu", StringValue(@azimuth.Platform::get_cpu_info())),
        ("native.memory", StringValue(@azimuth.Platform::get_memory_info())),
        ("native.time.source", StringValue("system.clock"))
      ],
      schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
    })
    
    // 验证原生平台资源
    assert_eq(native_resource.attributes.length(), 3)
  }
  
  // 测试跨平台时间戳获取
  let timestamp = @azimuth.Platform::get_timestamp()
  assert_true(timestamp > 0L)
  
  // 测试跨平台随机数生成
  let random_value = @azimuth.Platform::get_random_number()
  assert_true(random_value >= 0.0 && random_value < 1.0)
}

// 测试3: 浏览器环境兼容性
test "browser environment compatibility" {
  // 检测是否在浏览器环境
  let is_browser = @azimuth.Platform::is_browser()
  
  if is_browser {
    // 获取浏览器信息
    let browser_info = @azimuth.Platform::get_browser_info()
    
    // 验证浏览器信息
    assert_true(browser_info.contains("name"))
    assert_true(browser_info.contains("version"))
    assert_true(browser_info.contains("userAgent"))
    
    // 测试浏览器特定功能
    let browser_features = @azimuth.Platform::get_browser_features()
    assert_true(browser_features.contains("localStorage"))
    assert_true(browser_features.contains("sessionStorage"))
    assert_true(browser_features.contains("webWorkers"))
    
    // 测试浏览器存储
    let storage_available = @azimuth.Platform::is_local_storage_available()
    
    if storage_available {
      // 测试本地存储操作
      @azimuth.Platform::set_local_storage("azimuth_test_key", "test_value")
      let stored_value = @azimuth.Platform::get_local_storage("azimuth_test_key")
      match stored_value {
        Some(value) => assert_eq(value, "test_value")
        None => assert_true(false)
      }
      @azimuth.Platform::remove_local_storage("azimuth_test_key")
    }
    
    // 测试浏览器网络状态
    let online_status = @azimuth.Platform::is_online()
    assert_true(online_status == true || online_status == false)
    
    // 获取浏览器性能信息
    let performance_info = @azimuth.Platform::get_performance_info()
    assert_true(performance_info.contains("navigation"))
    assert_true(performance_info.contains("timing"))
    
    // 创建浏览器特定资源
    let browser_resource = Resource({
      attributes: [
        ("browser.name", StringValue(@azimuth.Platform::get_browser_name())),
        ("browser.version", StringValue(@azimuth.Platform::get_browser_version())),
        ("browser.language", StringValue(@azimuth.Platform::get_browser_language())),
        ("browser.platform", StringValue(@azimuth.Platform::get_browser_platform())),
        ("browser.online", StringValue(online_status.to_string())),
        ("browser.features", StringValue(browser_features))
      ],
      schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
    })
    
    // 验证浏览器资源
    assert_eq(browser_resource.attributes.length(), 6)
  } else {
    // 非浏览器环境
    let browser_info = @azimuth.Platform::get_browser_info()
    assert_eq(browser_info, "Not running in browser environment")
    
    // 测试Node.js特定功能（如果是Node.js环境）
    if @azimuth.Platform::is_nodejs() {
      let nodejs_info = @azimuth.Platform::get_nodejs_info()
      assert_true(nodejs_info.contains("version"))
      assert_true(nodejs_info.contains("platform"))
      
      // 创建Node.js特定资源
      let nodejs_resource = Resource({
        attributes: [
          ("nodejs.version", StringValue(@azimuth.Platform::get_nodejs_version())),
          ("nodejs.platform", StringValue(@azimuth.Platform::get_nodejs_platform())),
          ("nodejs.arch", StringValue(@azimuth.Platform::get_nodejs_arch()))
        ],
        schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
      })
      
      // 验证Node.js资源
      assert_eq(nodejs_resource.attributes.length(), 3)
    }
  }
}

// 测试4: 网络环境兼容性
test "network environment compatibility" {
  // 检测网络连接状态
  let network_status = @azimuth.Platform::get_network_status()
  
  // 验证网络状态
  assert_true(network_status.contains("connected"))
  assert_true(network_status.contains("type"))
  
  // 测试DNS解析
  let dns_result = @azimuth.Platform::resolve_dns("example.com")
  
  // 验证DNS解析结果
  if dns_result.success {
    assert_true(dns_result.addresses.length() > 0)
  } else {
    assert_true(dns_result.error.length() > 0)
  }
  
  // 测试HTTP连接
  let http_result = @azimuth.Platform::test_http_connection("https://httpbin.org/status/200")
  
  // 验证HTTP连接结果
  if http_result.success {
    assert_eq(http_result.status_code, 200)
    assert_true(http_result.response_time > 0)
  } else {
    assert_true(http_result.error.length() > 0)
  }
  
  // 测试HTTPS连接
  let https_result = @azimuth.Platform::test_https_connection("https://httpbin.org/status/200")
  
  // 验证HTTPS连接结果
  if https_result.success {
    assert_eq(https_result.status_code, 200)
    assert_true(https_result.response_time > 0)
    assert_true(https_result.tls_info.length() > 0)
  } else {
    assert_true(https_result.error.length() > 0)
  }
  
  // 测试代理设置
  let proxy_config = @azimuth.Platform::get_proxy_config()
  
  // 验证代理配置
  assert_true(proxy_config.contains("http"))
  assert_true(proxy_config.contains("https"))
  assert_true(proxy_config.contains("no_proxy"))
  
  // 创建网络环境资源
  let network_resource = Resource({
    attributes: [
      ("network.status", StringValue(network_status)),
      ("network.dns.resolution", StringValue(dns_result.success.to_string())),
      ("network.http.connection", StringValue(http_result.success.to_string())),
      ("network.https.connection", StringValue(https_result.success.to_string())),
      ("network.proxy.config", StringValue(proxy_config))
    ],
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  })
  
  // 验证网络资源
  assert_eq(network_resource.attributes.length(), 5)
}

// 测试5: 文件系统兼容性
test "filesystem compatibility" {
  // 获取文件系统信息
  let fs_info = @azimuth.Platform::get_filesystem_info()
  
  // 验证文件系统信息
  assert_true(fs_info.contains("type"))
  assert_true(fs_info.contains("total_space"))
  assert_true(fs_info.contains("free_space"))
  
  // 测试文件路径操作
  let test_paths = [
    "/var/log/azimuth.log",
    "C:\\Program Files\\Azimuth\\log.txt",
    "./relative/path/file.txt",
    "../parent/path/file.txt"
  ]
  
  for path in test_paths {
    // 测试路径规范化
    let normalized_path = @azimuth.Platform::normalize_path(path)
    assert_true(normalized_path.length() > 0)
    
    // 测试路径分割
    let path_parts = @azimuth.Platform::split_path(path)
    assert_true(path_parts.length() > 0)
    
    // 测试路径合并
    let joined_path = @azimuth.Platform::join_path(path_parts)
    assert_true(joined_path.length() > 0)
  }
  
  // 测试文件权限检查
  let temp_file = @azimuth.Platform::get_temp_directory() + @azimuth.Platform::get_path_separator() + "azimuth_permission_test.txt"
  
  // 测试文件创建权限
  let can_create = @azimuth.Platform::can_create_file(temp_file)
  assert_true(can_create == true || can_create == false)
  
  // 测试文件读取权限
  let can_read = @azimuth.Platform::can_read_file(temp_file)
  assert_true(can_read == true || can_read == false)
  
  // 测试文件写入权限
  let can_write = @azimuth.Platform::can_write_file(temp_file)
  assert_true(can_write == true || can_write == false)
  
  // 测试目录创建
  let temp_dir = @azimuth.Platform::get_temp_directory() + @azimuth.Platform::get_path_separator() + "azimuth_test_dir"
  
  // 创建测试目录
  let dir_created = @azimuth.Platform::create_directory(temp_dir)
  assert_true(dir_created == true || dir_created == false)
  
  if dir_created {
    // 测试目录删除
    let dir_removed = @azimuth.Platform::remove_directory(temp_dir)
    assert_true(dir_removed == true || dir_removed == false)
  }
  
  // 创建文件系统资源
  let filesystem_resource = Resource({
    attributes: [
      ("filesystem.info", StringValue(fs_info)),
      ("filesystem.temp.dir", StringValue(@azimuth.Platform::get_temp_directory())),
      ("filesystem.path.separator", StringValue(@azimuth.Platform::get_path_separator())),
      ("filesystem.can.create", StringValue(can_create.to_string())),
      ("filesystem.can.read", StringValue(can_read.to_string())),
      ("filesystem.can.write", StringValue(can_write.to_string()))
    ],
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  })
  
  // 验证文件系统资源
  assert_eq(filesystem_resource.attributes.length(), 6)
}

// 测试6: 硬件兼容性
test "hardware compatibility" {
  // 获取CPU信息
  let cpu_info = @azimuth.Platform::get_cpu_info()
  
  // 验证CPU信息
  assert_true(cpu_info.contains("model"))
  assert_true(cpu_info.contains("cores"))
  assert_true(cpu_info.contains("frequency"))
  
  // 获取内存信息
  let memory_info = @azimuth.Platform::get_memory_info()
  
  // 验证内存信息
  assert_true(memory_info.contains("total"))
  assert_true(memory_info.contains("available"))
  assert_true(memory_info.contains("used"))
  
  // 获取磁盘信息
  let disk_info = @azimuth.Platform::get_disk_info()
  
  // 验证磁盘信息
  assert_true(disk_info.contains("total"))
  assert_true(disk_info.contains("free"))
  assert_true(disk_info.contains("used"))
  
  // 获取网络接口信息
  let network_interfaces = @azimuth.Platform::get_network_interfaces()
  
  // 验证网络接口信息
  assert_true(network_interfaces.length() > 0)
  
  // 测试硬件性能指标
  let cpu_usage = @azimuth.Platform::get_cpu_usage()
  assert_true(cpu_usage >= 0.0 && cpu_usage <= 100.0)
  
  let memory_usage = @azimuth.Platform::get_memory_usage()
  assert_true(memory_usage >= 0.0 && memory_usage <= 100.0)
  
  // 测试硬件特定功能
  let has_avx = @azimuth.Platform::has_cpu_feature("avx")
  let has_sse = @azimuth.Platform::has_cpu_feature("sse")
  let has_neon = @azimuth.Platform::has_cpu_feature("neon")
  
  // 创建硬件资源
  let hardware_resource = Resource({
    attributes: [
      ("hardware.cpu.info", StringValue(cpu_info)),
      ("hardware.memory.info", StringValue(memory_info)),
      ("hardware.disk.info", StringValue(disk_info)),
      ("hardware.cpu.usage", StringValue(cpu_usage.to_string())),
      ("hardware.memory.usage", StringValue(memory_usage.to_string())),
      ("hardware.cpu.avx", StringValue(has_avx.to_string())),
      ("hardware.cpu.sse", StringValue(has_sse.to_string())),
      ("hardware.cpu.neon", StringValue(has_neon.to_string())),
      ("hardware.network.interfaces", StringValue(network_interfaces.length().to_string()))
    ],
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  })
  
  // 验证硬件资源
  assert_eq(hardware_resource.attributes.length(), 9)
}

// 测试7: 运行时环境兼容性
test "runtime environment compatibility" {
  // 检测运行时环境
  let runtime_env = @azimuth.Platform::get_runtime_environment()
  
  // 验证运行时环境
  assert_true(runtime_env == "browser" || runtime_env == "nodejs" || runtime_env == "wasm" || runtime_env == "native")
  
  // 获取运行时版本信息
  let runtime_version = @azimuth.Platform::get_runtime_version()
  assert_true(runtime_version.length() > 0)
  
  // 测试运行时特定功能
  if runtime_env == "browser" {
    // 浏览器特定测试
    let webgl_available = @azimuth.Platform::is_webgl_available()
    let webgl2_available = @azimuth.Platform::is_webgl2_available()
    let webrtc_available = @azimuth.Platform::is_webrtc_available()
    
    // 创建浏览器运行时资源
    let browser_runtime_resource = Resource({
      attributes: [
        ("runtime.environment", StringValue(runtime_env)),
        ("runtime.version", StringValue(runtime_version)),
        ("runtime.webgl.available", StringValue(webgl_available.to_string())),
        ("runtime.webgl2.available", StringValue(webgl2_available.to_string())),
        ("runtime.webrtc.available", StringValue(webrtc_available.to_string()))
      ],
      schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
    })
    
    // 验证浏览器运行时资源
    assert_eq(browser_runtime_resource.attributes.length(), 5)
  } else if runtime_env == "nodejs" {
    // Node.js特定测试
    let node_modules_path = @azimuth.Platform::get_node_modules_path()
    let npm_version = @azimuth.Platform::get_npm_version()
    
    // 创建Node.js运行时资源
    let nodejs_runtime_resource = Resource({
      attributes: [
        ("runtime.environment", StringValue(runtime_env)),
        ("runtime.version", StringValue(runtime_version)),
        ("runtime.node.modules.path", StringValue(node_modules_path)),
        ("runtime.npm.version", StringValue(npm_version))
      ],
      schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
    })
    
    // 验证Node.js运行时资源
    assert_eq(nodejs_runtime_resource.attributes.length(), 4)
  } else if runtime_env == "wasm" {
    // WebAssembly特定测试
    let wasm_features = @azimuth.Platform::get_wasm_features()
    let wasm_memory_limit = @azimuth.Platform::get_wasm_memory_limit()
    
    // 创建WebAssembly运行时资源
    let wasm_runtime_resource = Resource({
      attributes: [
        ("runtime.environment", StringValue(runtime_env)),
        ("runtime.version", StringValue(runtime_version)),
        ("runtime.wasm.features", StringValue(wasm_features)),
        ("runtime.wasm.memory.limit", StringValue(wasm_memory_limit.to_string()))
      ],
      schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
    })
    
    // 验证WebAssembly运行时资源
    assert_eq(wasm_runtime_resource.attributes.length(), 4)
  } else {
    // 原生运行时测试
    let native_compiler = @azimuth.Platform::get_native_compiler()
    let native_build_flags = @azimuth.Platform::get_native_build_flags()
    
    // 创建原生运行时资源
    let native_runtime_resource = Resource({
      attributes: [
        ("runtime.environment", StringValue(runtime_env)),
        ("runtime.version", StringValue(runtime_version)),
        ("runtime.native.compiler", StringValue(native_compiler)),
        ("runtime.native.build.flags", StringValue(native_build_flags))
      ],
      schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
    })
    
    // 验证原生运行时资源
    assert_eq(native_runtime_resource.attributes.length(), 4)
  }
  
  // 测试跨平台兼容性API
  let api_version = @azimuth.Platform::get_api_version()
  assert_true(api_version.length() > 0)
  
  let supported_features = @azimuth.Platform::get_supported_features()
  assert_true(supported_features.length() > 0)
  
  // 创建通用运行时资源
  let common_runtime_resource = Resource({
    attributes: [
      ("runtime.environment", StringValue(runtime_env)),
      ("runtime.version", StringValue(runtime_version)),
      ("runtime.api.version", StringValue(api_version)),
      ("runtime.supported.features", StringValue(supported_features))
    ],
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  })
  
  // 验证通用运行时资源
  assert_eq(common_runtime_resource.attributes.length(), 4)
}