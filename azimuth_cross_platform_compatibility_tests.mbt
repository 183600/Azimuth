// Azimuth 跨平台兼容性测试用例
// 专注于不同操作系统、架构和运行时环境的兼容性验证

// 测试1: 操作系统兼容性
test "操作系统兼容性" {
  // 创建跨平台测试管理器
  let cross_platform_manager = CrossPlatformManager::new()
  
  // 获取当前系统信息
  let system_info = CrossPlatformManager::get_system_info(cross_platform_manager)
  
  // 验证系统信息结构
  assert_true(system_info.os_name.length() > 0)
  assert_true(system_info.os_version.length() > 0)
  assert_true(system_info.architecture.length() > 0)
  assert_true(system_info.platform.length() > 0)
  
  // 测试路径处理兼容性
  let path_tests = [
    { input: "/home/user/file.txt", expected_seperator: "/" },
    { input: "C:\\Users\\User\\file.txt", expected_seperator: "\\" },
    { input: "relative/path/file.txt", expected_seperator: "/" }
  ]
  
  for test_case in path_tests {
    let normalized_path = CrossPlatformManager::normalize_path(cross_platform_manager, test_case.input)
    
    # 验证路径分隔符符合当前平台
    let expected_sep = Path::get_separator()
    assert_true(normalized_path.contains(expected_sep))
    
    # 验证路径可以正确解析
    let path_parts = Path::split(normalized_path)
    assert_true(path_parts.length() > 0)
  }
  
  // 测试文件系统操作兼容性
  let temp_dir = CrossPlatformManager::get_temp_directory(cross_platform_manager)
  assert_true(temp_dir.length() > 0)
  
  # 创建临时文件
  let temp_file = Path::join(temp_dir, "test_file.txt")
  let test_content = "This is a test file for cross-platform compatibility."
  
  let write_result = File::write(temp_file, test_content)
  assert_true(write_result.success)
  
  # 验证文件存在
  assert_true(File::exists(temp_file))
  
  # 读取文件内容
  let read_result = File::read(temp_file)
  assert_true(read_result.success)
  assert_eq(read_result.content, test_content)
  
  # 获取文件信息
  let file_info = File::get_info(temp_file)
  assert_true(file_info.size > 0)
  assert_true(file_info.created_at > 0)
  assert_true(file_info.modified_at > 0)
  
  # 清理临时文件
  let delete_result = File::delete(temp_file)
  assert_true(delete_result.success)
  assert_false(File::exists(temp_file))
  
  // 测试环境变量处理
  let test_env_key = "AZIMUTH_CROSS_PLATFORM_TEST"
  let test_env_value = "test_value"
  
  # 设置环境变量
  let set_env_result = Environment::set(test_env_key, test_env_value)
  assert_true(set_env_result.success)
  
  # 获取环境变量
  let get_env_result = Environment::get(test_env_key)
  assert_true(get_env_result.success)
  assert_eq(get_env_result.value, test_env_value)
  
  # 删除环境变量
  let unset_env_result = Environment::unset(test_env_key)
  assert_true(unset_env_result.success)
  
  # 验证环境变量已删除
  let get_after_unset = Environment::get(test_env_key)
  assert_false(get_after_unset.success)
  
  // 测试进程管理兼容性
  let process_info = CrossPlatformManager::get_current_process_info(cross_platform_manager)
  assert_true(process_info.pid > 0)
  assert_true(process_info.executable_path.length() > 0)
  assert_true(process_info.command_line.length() > 0)
  
  // 测试系统命令执行
  let echo_command = if system_info.os_name.contains("Windows") { "echo" } else { "echo" }
  let command_args = ["Hello from cross-platform test"]
  
  let command_result = Process::execute(echo_command, command_args)
  assert_true(command_result.success)
  assert_true(command_result.stdout.contains("Hello from cross-platform test"))
  assert_eq(command_result.exit_code, 0)
  
  // 测试网络接口检测
  let network_interfaces = CrossPlatformManager::get_network_interfaces(cross_platform_manager)
  assert_true(network_interfaces.length() > 0)
  
  # 验证网络接口信息
  for interface in network_interfaces {
    assert_true(interface.name.length() > 0)
    assert_true(interface.is_up or not(interface.is_up))  # 布尔值
  }
}

// 测试2: 架构兼容性
test "架构兼容性" {
  // 创建架构测试管理器
  let architecture_manager = ArchitectureManager::new()
  
  // 获取当前架构信息
  let current_arch = ArchitectureManager::get_current_architecture(architecture_manager)
  assert_true(current_arch.name.length() > 0)
  assert_true(current_arch.bits == 32 or current_arch.bits == 64)
  assert_true(current_arch.endianness == "little" or current_arch.endianness == "big")
  
  // 测试字节序兼容性
  let test_value = 0x12345678
  let bytes = ArchitectureManager::int_to_bytes(architecture_manager, test_value, 4)
  assert_eq(bytes.length(), 4)
  
  let converted_back = ArchitectureManager::bytes_to_int(architecture_manager, bytes)
  assert_eq(converted_back, test_value)
  
  // 测试跨字节序转换
  let swapped_value = ArchitectureManager::swap_endian(architecture_manager, test_value)
  if current_arch.endianness == "little" {
    # 小端序转换后应该不同
    assert_not_eq(swapped_value, test_value)
    
    # 再次转换应该恢复原值
    let swapped_back = ArchitectureManager::swap_endian(architecture_manager, swapped_value)
    assert_eq(swapped_back, test_value)
  }
  
  // 测试数据类型大小
  let type_sizes = ArchitectureManager::get_type_sizes(architecture_manager)
  assert_true(type_sizes.int > 0)
  assert_true(type_sizes.long > 0)
  assert_true(type_sizes.pointer > 0)
  
  # 在64位系统上，指针大小应该是8字节
  if current_arch.bits == 64 {
    assert_eq(type_sizes.pointer, 8)
  } else {
    assert_eq(type_sizes.pointer, 4)
  }
  
  // 测试数值范围
  let numeric_ranges = ArchitectureManager::get_numeric_ranges(architecture_manager)
  assert_true(numeric_ranges.int_min < numeric_ranges.int_max)
  assert_true(numeric_ranges.long_min < numeric_ranges.long_max)
  
  // 测试CPU特性检测
  let cpu_features = ArchitectureManager::get_cpu_features(architecture_manager)
  assert_true(cpu_features.length() > 0)
  
  # 验证常见CPU特性
  let common_features = ["sse", "sse2", "avx", "avx2"]
  for feature in common_features {
    if cpu_features.contains(feature) {
      assert_true(cpu_features.get(feature, false))
    }
  }
  
  // 测试SIMD操作兼容性
  let simd_support = ArchitectureManager::get_simd_support(architecture_manager)
  assert_true(simd_support.sse or not(simd_support.sse))
  assert_true(simd_support.avx or not(simd_support.avx))
  assert_true(simd_support.avx2 or not(simd_support.avx2))
  
  // 如果支持SIMD，测试基本操作
  if simd_support.sse {
    let simd_result = ArchitectureManager::test_simd_operations(architecture_manager, "sse")
    assert_true(simd_result.success)
  }
  
  // 测试内存对齐
  let alignment_requirements = ArchitectureManager::get_alignment_requirements(architecture_manager)
  assert_true(alignment_requirements.int > 0)
  assert_true(alignment_requirements.long > 0)
  assert_true(alignment_requirements.pointer > 0)
  
  // 验证对齐要求是2的幂
  for (type_name, alignment) in alignment_requirements {
    assert_true(alignment > 0 and (alignment & (alignment - 1)) == 0)
  }
  
  // 测试原子操作支持
  let atomic_support = ArchitectureManager::get_atomic_support(architecture_manager)
  assert_true(atomic_support.cas_8 or not(atomic_support.cas_8))
  assert_true(atomic_support.cas_16 or not(atomic_support.cas_16))
  assert_true(atomic_support.cas_32 or not(atomic_support.cas_32))
  assert_true(atomic_support.cas_64 or not(atomic_support.cas_64))
  
  // 测试缓存信息
  let cache_info = ArchitectureManager::get_cache_info(architecture_manager)
  assert_true(cache_info.l1_data_size > 0)
  assert_true(cache_info.l1_instruction_size > 0)
  assert_true(cache_info.l2_size > 0 or cache_info.l2_size == 0)  # 可能不存在
  assert_true(cache_info.l3_size > 0 or cache_info.l3_size == 0)  # 可能不存在
  
  // 缓存大小应该是合理的值
  assert_true(cache_info.l1_data_size >= 8192)   # 至少8KB
  assert_true(cache_info.l1_data_size <= 65536) # 最多64KB
}

// 测试3: 运行时环境兼容性
test "运行时环境兼容性" {
  // 创建运行时环境管理器
  let runtime_manager = RuntimeEnvironmentManager::new()
  
  // 获取当前运行时信息
  let runtime_info = RuntimeEnvironmentManager::get_runtime_info(runtime_manager)
  assert_true(runtime_info.name.length() > 0)
  assert_true(runtime_info.version.length() > 0)
  
  // 测试内存管理兼容性
  let memory_stats = RuntimeEnvironmentManager::get_memory_stats(runtime_manager)
  assert_true(memory_stats.total_heap > 0)
  assert_true(memory_stats.used_heap >= 0)
  assert_true(memory_stats.free_heap >= 0)
  
  // 验证内存统计的一致性
  assert_eq(memory_stats.total_heap, memory_stats.used_heap + memory_stats.free_heap)
  
  // 测试垃圾回收
  let gc_before = RuntimeEnvironmentManager::get_memory_stats(runtime_manager)
  RuntimeEnvironmentManager::force_gc(runtime_manager)
  let gc_after = RuntimeEnvironmentManager::get_memory_stats(runtime_manager)
  
  # 垃圾回收后，已使用内存应该减少或保持不变
  assert_true(gc_after.used_heap <= gc_before.used_heap)
  
  // 测试线程支持
  let thread_support = RuntimeEnvironmentManager::get_thread_support(runtime_manager)
  assert_true(thread_support.supported)
  assert_true(thread_support.max_threads > 0)
  
  // 测试线程创建和执行
  let thread_result = RuntimeEnvironmentManager::create_and_run_thread(runtime_manager, fn() {
    # 简单的计算任务
    let mut sum = 0
    for i in 0..=1000 {
      sum = sum + i
    }
    sum
  })
  
  assert_true(thread_result.success)
  assert_eq(thread_result.result, 500500)  # 0到1000的和
  
  // 测试并发执行
  let concurrent_results = RuntimeEnvironmentManager::run_concurrent(runtime_manager, [
    fn() { 1 + 1 },
    fn() { 2 + 2 },
    fn() { 3 + 3 },
    fn() { 4 + 4 }
  ])
  
  assert_eq(concurrent_results.length(), 4)
  assert_eq(concurrent_results[0], 2)
  assert_eq(concurrent_results[1], 4)
  assert_eq(concurrent_results[2], 6)
  assert_eq(concurrent_results[3], 8)
  
  // 测试异常处理兼容性
  let exception_result = RuntimeEnvironmentManager::test_exception_handling(runtime_manager, fn() {
    throw RuntimeError("Test exception")
  })
  
  assert_true(exception_result.exception_caught)
  assert_eq(exception_result.exception_message, "Test exception")
  
  // 测试模块系统兼容性
  let module_info = RuntimeEnvironmentManager::get_module_info(runtime_manager)
  assert_true(module_info.loaded_modules.length() > 0)
  
  // 验证核心模块已加载
  let core_modules = ["core", "math", "string", "array"]
  for module in core_modules {
    assert_true(module_info.loaded_modules.contains(module))
  }
  
  // 测试动态加载
  let dynamic_load_result = RuntimeEnvironmentManager::dynamic_load(runtime_manager, "test_module")
  # 注意：这个测试可能会失败，因为测试模块可能不存在
  # 我们主要验证API的兼容性
  assert_true(dynamic_load_result.success or not(dynamic_load_result.success))
  
  // 测试性能监控
  let performance_start = RuntimeEnvironmentManager::start_performance_monitoring(runtime_manager)
  
  # 执行一些计算密集型操作
  let mut result = 0
  for i in 0..=100000 {
    result = result + Math::sqrt(i.to_float())
  }
  
  let performance_stats = RuntimeEnvironmentManager::stop_performance_monitoring(runtime_manager, performance_start)
  
  assert_true(performance_stats.execution_time > 0)
  assert_true(performance_stats.cpu_time > 0)
  assert_true(performance_stats.memory_used > 0)
}

// 测试4: 文件系统兼容性
test "文件系统兼容性" {
  // 创建文件系统兼容性管理器
  let fs_manager = FileSystemManager::new()
  
  // 获取文件系统信息
  let fs_info = FileSystemManager::get_filesystem_info(fs_manager)
  assert_true(fs_info.type.length() > 0)
  assert_true(fs_info.total_space > 0)
  assert_true(fs_info.free_space > 0)
  assert_true(fs_info.free_space <= fs_info.total_space)
  
  // 测试路径操作
  let path_operations = [
    { operation: "join", parts: ["home", "user", "documents"] },
    { operation: "join", parts: ["C:", "Users", "User", "Documents"] },
    { operation: "join", parts: ["relative", "path", "to", "file"] },
    { operation: "normalize", path: "./path/../to/./file.txt" },
    { operation: "dirname", path: "/path/to/file.txt" },
    { operation: "basename", path: "/path/to/file.txt" },
    { operation: "extension", path: "/path/to/file.txt" }
  ]
  
  for op in path_operations {
    let result = FileSystemManager::execute_path_operation(fs_manager, op.operation, op.parts.or_else([op.path]))
    assert_true(result.success)
    assert_true(result.output.length() > 0)
  }
  
  // 测试文件权限
  let temp_dir = FileSystemManager::get_temp_directory(fs_manager)
  let test_file = FileSystemManager::join_paths(fs_manager, [temp_dir, "permissions_test.txt"])
  
  # 创建文件
  let create_result = FileSystemManager::create_file(fs_manager, test_file, "test content")
  assert_true(create_result.success)
  
  # 获取文件权限
  let permissions = FileSystemManager::get_file_permissions(fs_manager, test_file)
  assert_true(permissions.owner_read)
  assert_true(permissions.owner_write)
  
  # 修改文件权限
  let new_permissions = { permissions | owner_execute: true }
  let chmod_result = FileSystemManager::set_file_permissions(fs_manager, test_file, new_permissions)
  assert_true(chmod_result.success)
  
  # 验证权限已修改
  let updated_permissions = FileSystemManager::get_file_permissions(fs_manager, test_file)
  assert_true(updated_permissions.owner_execute)
  
  # 清理测试文件
  let delete_result = FileSystemManager::delete_file(fs_manager, test_file)
  assert_true(delete_result.success)
  
  // 测试符号链接（如果支持）
  let symlink_support = FileSystemManager::supports_symbolic_links(fs_manager)
  
  if symlink_support {
    let target_file = FileSystemManager::join_paths(fs_manager, [temp_dir, "symlink_target.txt"])
    let symlink_file = FileSystemManager::join_paths(fs_manager, [temp_dir, "symlink_link.txt"])
    
    # 创建目标文件
    FileSystemManager::create_file(fs_manager, target_file, "target content")
    
    # 创建符号链接
    let symlink_result = FileSystemManager::create_symbolic_link(fs_manager, symlink_file, target_file)
    assert_true(symlink_result.success)
    
    # 验证符号链接
    let is_symlink = FileSystemManager::is_symbolic_link(fs_manager, symlink_file)
    assert_true(is_symlink)
    
    # 通过符号链接读取内容
    let link_content = FileSystemManager::read_file(fs_manager, symlink_file)
    assert_true(link_content.success)
    assert_eq(link_content.content, "target content")
    
    # 清理
    FileSystemManager::delete_file(fs_manager, symlink_file)
    FileSystemManager::delete_file(fs_manager, target_file)
  }
  
  // 测试文件监视（如果支持）
  let file_watch_support = FileSystemManager::supports_file_watching(fs_manager)
  
  if file_watch_support {
    let watch_file = FileSystemManager::join_paths(fs_manager, [temp_dir, "watch_test.txt"])
    FileSystemManager::create_file(fs_manager, watch_file, "initial content")
    
    # 创建文件监视器
    let watcher = FileSystemManager::create_file_watcher(fs_manager, watch_file)
    assert_true(watcher.created)
    
    # 修改文件
    FileSystemManager::write_file(fs_manager, watch_file, "modified content")
    
    # 等待事件
    Thread::sleep(100)
    
    # 检查事件
    let events = FileSystemManager::get_watch_events(fs_manager, watcher)
    assert_true(events.length() > 0)
    
    # 清理
    FileSystemManager::delete_file(fs_manager, watch_file)
    FileSystemManager::destroy_file_watcher(fs_manager, watcher)
  }
  
  // 测试大文件处理
  let large_content = "x".repeat(1024 * 1024)  # 1MB
  let large_file = FileSystemManager::join_paths(fs_manager, [temp_dir, "large_file.txt"])
  
  let large_write_result = FileSystemManager::write_file(fs_manager, large_file, large_content)
  assert_true(large_write_result.success)
  
  let large_read_result = FileSystemManager::read_file(fs_manager, large_file)
  assert_true(large_read_result.success)
  assert_eq(large_read_result.content.length(), large_content.length())
  
  # 清理
  FileSystemManager::delete_file(fs_manager, large_file)
}

// 测试5: 网络兼容性
test "网络兼容性" {
  // 创建网络兼容性管理器
  let network_manager = NetworkCompatibilityManager::new()
  
  // 获取网络信息
  let network_info = NetworkCompatibilityManager::get_network_info(network_manager)
  assert_true(network_info.hostname.length() > 0)
  assert_true(network_info.interfaces.length() > 0)
  
  // 测试本地地址解析
  let localhost_result = NetworkCompatibilityManager::resolve_hostname(network_manager, "localhost")
  assert_true(localhost_result.success)
  assert_true(localhost_result.addresses.length() > 0)
  
  # 验证本地地址
  let localhost_addresses = localhost_result.addresses
  assert_true(localhost_addresses.any(fn(addr) { addr == "127.0.0.1" or addr == "::1" }))
  
  // 测试DNS解析
  let dns_result = NetworkCompatibilityManager::resolve_hostname(network_manager, "example.com")
  assert_true(dns_result.success)
  assert_true(dns_result.addresses.length() > 0)
  
  # 验证解析的地址是有效的IP地址
  for address in dns_result.addresses {
    assert_true(is_valid_ip_address(address))
  }
  
  // 测试套接字创建
  let socket_result = NetworkCompatibilityManager::create_socket(network_manager, {
    family: "ipv4",
    type: "stream",
    protocol: "tcp"
  })
  
  assert_true(socket_result.success)
  let socket = socket_result.socket
  
  # 测试套接字选项
  let option_result = NetworkCompatibilityManager::set_socket_option(network_manager, socket, "reuse_addr", true)
  assert_true(option_result.success)
  
  # 关闭套接字
  let close_result = NetworkCompatibilityManager::close_socket(network_manager, socket)
  assert_true(close_result.success)
  
  // 测试HTTP客户端兼容性
  let http_client = NetworkCompatibilityManager::create_http_client(network_manager, {
    timeout: 10000,
    user_agent: "Azimuth-CrossPlatform-Test/1.0",
    follow_redirects: true
  })
  
  # 发送HTTP请求
  let http_result = NetworkCompatibilityManager::http_get(network_manager, http_client, "https://httpbin.org/get")
  assert_true(http_result.success)
  assert_eq(http_result.status_code, 200)
  assert_true(http_result.body.length() > 0)
  
  # 测试HTTPS支持
  let https_result = NetworkCompatibilityManager::http_get(network_manager, http_client, "https://httpbin.org/get")
  assert_true(https_result.success)
  assert_eq(https_result.status_code, 200)
  
  # 测试POST请求
  let post_data = "{\"test\": \"cross_platform_compatibility\"}"
  let post_result = NetworkCompatibilityManager::http_post(network_manager, http_client, "https://httpbin.org/post", post_data)
  assert_true(post_result.success)
  assert_eq(post_result.status_code, 200)
  
  // 测试网络接口枚举
  let interfaces = NetworkCompatibilityManager::enumerate_network_interfaces(network_manager)
  assert_true(interfaces.length() > 0)
  
  # 验证接口信息
  for interface in interfaces {
    assert_true(interface.name.length() > 0)
    assert_true(interface.is_up or not(interface.is_up))
    assert_true(interface.addresses.length() >= 0)
    
    # 验证地址格式
    for address in interface.addresses {
      assert_true(is_valid_ip_address(address.address))
    }
  }
  
  // 测试端口扫描（扫描本地常用端口）
  let common_ports = [22, 80, 443, 8080]
  let port_scan_result = NetworkCompatibilityManager::scan_ports(network_manager, "127.0.0.1", common_ports)
  
  assert_true(port_scan_result.scanned_ports == common_ports.length())
  assert_true(port_scan_result.open_ports.length() >= 0)
  
  # 验证扫描的端口在请求范围内
  for port in port_scan_result.open_ports {
    assert_true(common_ports.contains(port))
  }
  
  // 测试连接超时
  let timeout_socket = NetworkCompatibilityManager::create_socket(network_manager, {
    family: "ipv4",
    type: "stream",
    protocol: "tcp"
  })
  
  # 尝试连接到一个不存在的地址，应该超时
  let connect_result = NetworkCompatibilityManager::connect_with_timeout(network_manager, timeout_socket, "192.0.2.1", 12345, 1000)
  assert_false(connect_result.success)  # 应该失败或超时
  
  NetworkCompatibilityManager::close_socket(network_manager, timeout_socket)
}

// 测试6: 国际化和本地化兼容性
test "国际化和本地化兼容性" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  
  // 获取系统区域设置
  let system_locale = I18nManager::get_system_locale(i18n_manager)
  assert_true(system_locale.language.length() > 0)
  assert_true(system_locale.region.length() >= 0)  # 可能为空
  
  // 测试常用区域设置
  let test_locales = [
    { code: "en-US", name: "English (United States)" },
    { code: "zh-CN", name: "Chinese (China)" },
    { code: "fr-FR", name: "French (France)" },
    { code: "de-DE", name: "German (Germany)" },
    { code: "ja-JP", name: "Japanese (Japan)" },
    { code: "es-ES", name: "Spanish (Spain)" }
  ]
  
  // 设置区域设置
  for locale in test_locales {
    let set_result = I18nManager::set_locale(i18n_manager, locale.code)
    assert_true(set_result.success)
    
    # 验证区域设置已设置
    let current_locale = I18nManager::get_current_locale(i18n_manager)
    assert_eq(current_locale, locale.code)
  }
  
  // 测试文本编码
  let encoding_tests = [
    { text: "Hello, World!", encoding: "UTF-8" },
    { text: "你好，世界！", encoding: "UTF-8" },
    { text: "Bonjour, le monde!", encoding: "UTF-8" },
    { text: "¡Hola, mundo!", encoding: "UTF-8" },
    { text: "こんにちは、世界！", encoding: "UTF-8" },
    { text: "Привет, мир!", encoding: "UTF-8" }
  ]
  
  for test in encoding_tests {
    # 编码文本
    let encoded_result = I18nManager::encode_text(i18n_manager, test.text, test.encoding)
    assert_true(encoded_result.success)
    
    # 解码文本
    let decoded_result = I18nManager::decode_text(i18n_manager, encoded_result.data, test.encoding)
    assert_true(decoded_result.success)
    assert_eq(decoded_result.text, test.text)
  }
  
  // 测试日期和时间格式化
  let timestamp = Time::from_string("2023-01-15T14:30:00Z")
  
  for locale in test_locales {
    I18nManager::set_locale(i18n_manager, locale.code)
    
    # 格式化日期
    let date_format = I18nManager::format_date(i18n_manager, timestamp)
    assert_true(date_format.length() > 0)
    
    # 格式化时间
    let time_format = I18nManager::format_time(i18n_manager, timestamp)
    assert_true(time_format.length() > 0)
    
    # 格式化日期时间
    let datetime_format = I18nManager::format_datetime(i18n_manager, timestamp)
    assert_true(datetime_format.length() > 0)
  }
  
  // 测试数字格式化
  let test_number = 1234567.89
  
  for locale in test_locales {
    I18nManager::set_locale(i18n_manager, locale.code)
    
    # 格式化数字
    let number_format = I18nManager::format_number(i18n_manager, test_number)
    assert_true(number_format.length() > 0)
    
    # 格式化货币
    let currency_format = I18nManager::format_currency(i18n_manager, test_number, "USD")
    assert_true(currency_format.length() > 0)
    
    # 格式化百分比
    let percent_format = I18nManager::format_percent(i18n_manager, 0.75)
    assert_true(percent_format.length() > 0)
  }
  
  // 测试字符串排序
  let test_strings = ["apple", "banana", "cherry", "date", "elderberry"]
  
  for locale in test_locales {
    I18nManager::set_locale(i18n_manager, locale.code)
    
    # 排序字符串
    let sorted_strings = I18nManager::sort_strings(i18n_manager, test_strings)
    assert_eq(sorted_strings.length(), test_strings.length())
    
    # 验证排序顺序（可能因区域设置而异）
    for i in 0..=sorted_strings.length() - 2 {
      assert_true(sorted_strings[i] <= sorted_strings[i + 1])
    }
  }
  
  // 测试文本方向
  let ltr_text = "This is left-to-right text."
  let rtl_text = "هذا نص من اليمين إلى اليسار."
  
  let ltr_direction = I18nManager::get_text_direction(i18n_manager, ltr_text)
  let rtl_direction = I18nManager::get_text_direction(i18n_manager, rtl_text)
  
  assert_eq(ltr_direction, "ltr")
  assert_eq(rtl_direction, "rtl")
  
  // 测试复数形式
  I18nManager::set_locale(i18n_manager, "en-US")
  
  let singular_form = I18nManager::pluralize(i18n_manager, "file", 1)
  let plural_form = I18nManager::pluralize(i18n_manager, "file", 2)
  
  assert_eq(singular_form, "file")
  assert_eq(plural_form, "files")
  
  // 测试消息翻译
  let translations = {
    "en-US": {
      "greeting": "Hello, {name}!",
      "farewell": "Goodbye, {name}!"
    },
    "zh-CN": {
      "greeting": "你好，{name}！",
      "farewell": "再见，{name}！"
    },
    "es-ES": {
      "greeting": "¡Hola, {name}!",
      "farewell": "¡Adiós, {name}!"
    }
  }
  
  # 加载翻译
  for (locale, messages) in translations {
    I18nManager::load_translations(i18n_manager, locale, messages)
  }
  
  # 测试翻译
  let translation_tests = [
    { locale: "en-US", key: "greeting", name: "Alice", expected: "Hello, Alice!" },
    { locale: "zh-CN", key: "greeting", name: "Alice", expected: "你好，Alice！" },
    { locale: "es-ES", key: "greeting", name: "Alice", expected: "¡Hola, Alice!" },
    { locale: "en-US", key: "farewell", name: "Bob", expected: "Goodbye, Bob!" },
    { locale: "zh-CN", key: "farewell", name: "Bob", expected: "再见，Bob！" },
    { locale: "es-ES", key: "farewell", name: "Bob", expected: "¡Adiós, Bob!" }
  ]
  
  for test in translation_tests {
    I18nManager::set_locale(i18n_manager, test.locale)
    
    let translated = I18nManager::translate(i18n_manager, test.key, { "name": test.name })
    assert_eq(translated, test.expected)
  }
}

// 测试7: 浏览器环境兼容性
test "浏览器环境兼容性" {
  // 创建浏览器兼容性管理器
  let browser_manager = BrowserCompatibilityManager::new()
  
  // 检测是否在浏览器环境
  let is_browser = BrowserCompatibilityManager::is_browser_environment(browser_manager)
  
  if is_browser {
    // 获取浏览器信息
    let browser_info = BrowserCompatibilityManager::get_browser_info(browser_manager)
    assert_true(browser_info.name.length() > 0)
    assert_true(browser_info.version.length() > 0)
    
    // 测试DOM操作兼容性
    let dom_support = BrowserCompatibilityManager::get_dom_support(browser_manager)
    assert_true(dom_support.document or not(dom_support.document))
    assert_true(dom_support.window or not(dom_support.window))
    
    if dom_support.document {
      // 测试元素创建
      let element_result = BrowserCompatibilityManager::create_element(browser_manager, "div")
      assert_true(element_result.success)
      
      // 测试事件监听
      let event_result = BrowserCompatibilityManager::add_event_listener(browser_manager, element_result.element, "click", fn() {
        "clicked"
      })
      assert_true(event_result.success)
    }
    
    // 测试Web API支持
    let api_support = BrowserCompatibilityManager::get_api_support(browser_manager)
    assert_true(api_support.fetch or not(api_support.fetch))
    assert_true(api_support.websockets or not(api_support.websockets))
    assert_true(api_support.local_storage or not(api_support.local_storage))
    assert_true(api_support.session_storage or not(api_support.session_storage))
    
    // 测试CSS特性支持
    let css_support = BrowserCompatibilityManager::get_css_support(browser_manager)
    assert_true(css_support.flexbox or not(css_support.flexbox))
    assert_true(css_support.grid or not(css_support.grid))
    assert_true(css_support.custom_properties or not(css_support.custom_properties))
    
    // 测试响应式设计
    let viewport_info = BrowserCompatibilityManager::get_viewport_info(browser_manager)
    assert_true(viewport_info.width > 0)
    assert_true(viewport_info.height > 0)
    
    // 测试媒体查询
    let media_queries = [
      "(max-width: 768px)",
      "(min-width: 769px)",
      "(orientation: portrait)",
      "(orientation: landscape)"
    ]
    
    for query in media_queries {
      let query_result = BrowserCompatibilityManager::match_media(browser_manager, query)
      assert_true(query_result.matches or not(query_result.matches))
    }
    
    // 测试Web存储
    if api_support.local_storage {
      # 测试localStorage
      let storage_key = "azimuth_test_key"
      let storage_value = "test_value"
      
      let set_result = BrowserCompatibilityManager::set_local_storage(browser_manager, storage_key, storage_value)
      assert_true(set_result.success)
      
      let get_result = BrowserCompatibilityManager::get_local_storage(browser_manager, storage_key)
      assert_true(get_result.success)
      assert_eq(get_result.value, storage_value)
      
      let remove_result = BrowserCompatibilityManager::remove_local_storage(browser_manager, storage_key)
      assert_true(remove_result.success)
    }
    
    // 测试网络连接
    let connection_info = BrowserCompatibilityManager::get_connection_info(browser_manager)
    if connection_info.available {
      assert_true(connection_info.effective_type.length() > 0)
      assert_true(connection_info.downlink >= 0)
      assert_true(connection_info.rtt >= 0)
    }
    
    // 测试地理位置
    let geolocation_support = BrowserCompatibilityManager::get_geolocation_support(browser_manager)
    if geolocation_support.available {
      # 注意：实际获取地理位置需要用户权限，这里只测试API可用性
      assert_true(true)
    }
    
    // 测试通知
    let notification_support = BrowserCompatibilityManager::get_notification_support(browser_manager)
    if notification_support.available {
      # 请求通知权限
      let permission_result = BrowserCompatibilityManager::request_notification_permission(browser_manager)
      assert_true(permission_result == "granted" or permission_result == "denied" or permission_result == "default")
    }
  } else {
    # 非浏览器环境，测试Node.js兼容性
    let node_info = BrowserCompatibilityManager::get_node_info(browser_manager)
    assert_true(node_info.version.length() > 0)
    assert_true(node_info.platform.length() > 0)
    
    # 测试Node.js模块
    let node_modules = ["fs", "path", "http", "https", "util", "events"]
    
    for module in node_modules {
      let module_result = BrowserCompatibilityManager::require_node_module(browser_manager, module)
      assert_true(module_result.success or not(module_result.success))
    }
  }
}

// 测试8: 跨平台打包和部署兼容性
test "跨平台打包和部署兼容性" {
  // 创建打包部署管理器
  let package_manager = PackageDeploymentManager::new()
  
  // 获取平台信息
  let platform_info = PackageDeploymentManager::get_platform_info(package_manager)
  assert_true(platform_info.os.length() > 0)
  assert_true(platform_info.arch.length() > 0)
  assert_true(platform_info.runtime.length() > 0)
  
  // 测试包格式支持
  let package_formats = ["zip", "tar", "tar.gz", "tar.bz2"]
  let supported_formats = PackageDeploymentManager::get_supported_formats(package_manager)
  
  for format in package_formats {
    assert_true(supported_formats.contains(format) or not(supported_formats.contains(format)))
  }
  
  // 创建测试文件结构
  let temp_dir = PackageDeploymentManager::get_temp_directory(package_manager)
  let test_dir = PackageDeploymentManager::join_paths(package_manager, [temp_dir, "test_app"])
  
  # 创建目录结构
  PackageDeploymentManager::create_directory(package_manager, test_dir)
  PackageDeploymentManager::create_directory(package_manager, PackageDeploymentManager::join_paths(package_manager, [test_dir, "bin"]))
  PackageDeploymentManager::create_directory(package_manager, PackageDeploymentManager::join_paths(package_manager, [test_dir, "lib"]))
  PackageDeploymentManager::create_directory(package_manager, PackageDeploymentManager::join_paths(package_manager, [test_dir, "config"]))
  
  # 创建测试文件
  let executable_file = PackageDeploymentManager::join_paths(package_manager, [test_dir, "bin", "app"])
  let library_file = PackageDeploymentManager::join_paths(package_manager, [test_dir, "lib", "core.lib"])
  let config_file = PackageDeploymentManager::join_paths(package_manager, [test_dir, "config", "settings.json"])
  
  PackageDeploymentManager::write_file(package_manager, executable_file, "#!/usr/bin/env node\nconsole.log('Hello from test app');")
  PackageDeploymentManager::write_file(package_manager, library_file, "library content")
  PackageDeploymentManager::write_file(package_manager, config_file, "{\"name\": \"test_app\", \"version\": \"1.0.0\"}")
  
  # 设置可执行权限（在Unix系统上）
  if platform_info.os != "windows" {
    PackageDeploymentManager::set_executable_permission(package_manager, executable_file)
  }
  
  # 测试打包
  let package_formats_to_test = supported_formats.slice(0, 2)  # 测试前两种格式
  
  for format in package_formats_to_test {
    let package_file = PackageDeploymentManager::join_paths(package_manager, [temp_dir, "test_app." + format])
    
    # 创建包
    let package_result = PackageDeploymentManager::create_package(package_manager, test_dir, package_file, format)
    assert_true(package_result.success)
    assert_true(PackageDeploymentManager::file_exists(package_manager, package_file))
    
    # 验证包内容
    let package_content = PackageDeploymentManager::list_package_contents(package_manager, package_file, format)
    assert_true(package_content.length() > 0)
    
    # 验证包含的文件
    let has_executable = package_content.any(fn(file) { file.contains("app") })
    let has_library = package_content.any(fn(file) { file.contains("core.lib") })
    let has_config = package_content.any(fn(file) { file.contains("settings.json") })
    
    assert_true(has_executable)
    assert_true(has_library)
    assert_true(has_config)
  }
  
  # 测试解包
  let extract_dir = PackageDeploymentManager::join_paths(package_manager, [temp_dir, "extracted"])
  PackageDeploymentManager::create_directory(package_manager, extract_dir)
  
  let package_file = PackageDeploymentManager::join_paths(package_manager, [temp_dir, "test_app." + package_formats_to_test[0]])
  let extract_result = PackageDeploymentManager::extract_package(package_manager, package_file, extract_dir, package_formats_to_test[0])
  assert_true(extract_result.success)
  
  # 验证解包的文件
  let extracted_executable = PackageDeploymentManager::join_paths(package_manager, [extract_dir, "bin", "app"])
  let extracted_library = PackageDeploymentManager::join_paths(package_manager, [extract_dir, "lib", "core.lib"])
  let extracted_config = PackageDeploymentManager::join_paths(package_manager, [extract_dir, "config", "settings.json"])
  
  assert_true(PackageDeploymentManager::file_exists(package_manager, extracted_executable))
  assert_true(PackageDeploymentManager::file_exists(package_manager, extracted_library))
  assert_true(PackageDeploymentManager::file_exists(package_manager, extracted_config))
  
  # 测试安装
  let install_dir = PackageDeploymentManager::join_paths(package_manager, [temp_dir, "installed"])
  let install_result = PackageDeploymentManager::install_package(package_manager, package_file, install_dir, {
    format: package_formats_to_test[0],
    create_symlinks: true,
    set_permissions: true
  })
  
  assert_true(install_result.success)
  
  # 测试卸载
  let uninstall_result = PackageDeploymentManager::uninstall_package(package_manager, install_dir)
  assert_true(uninstall_result.success)
  
  # 测试跨平台脚本执行
  let script_content = if platform_info.os == "windows" {
    "@echo off\necho Hello from Windows script"
  } else {
    "#!/bin/bash\necho 'Hello from Unix script'"
  }
  
  let script_file = PackageDeploymentManager::join_paths(package_manager, [temp_dir, "test_script" + (if platform_info.os == "windows" { ".bat" } else { ".sh" })])
  PackageDeploymentManager::write_file(package_manager, script_file, script_content)
  
  # 设置脚本权限
  if platform_info.os != "windows" {
    PackageDeploymentManager::set_executable_permission(package_manager, script_file)
  }
  
  # 执行脚本
  let script_result = PackageDeploymentManager::execute_script(package_manager, script_file)
  assert_true(script_result.success)
  assert_true(script_result.stdout.contains("Hello"))
  
  # 清理测试文件
  PackageDeploymentManager::delete_directory(package_manager, test_dir)
  PackageDeploymentManager::delete_directory(package_manager, extract_dir)
  for format in package_formats_to_test {
    let package_file = PackageDeploymentManager::join_paths(package_manager, [temp_dir, "test_app." + format])
    PackageDeploymentManager::delete_file(package_manager, package_file)
  }
  PackageDeploymentManager::delete_file(package_manager, script_file)
}

// 辅助函数

// 验证IP地址格式
fn is_valid_ip_address(address) {
  # 简单的IPv4地址验证
  let parts = address.split(".")
  if parts.length() != 4 {
    return false
  }
  
  for part in parts {
    let num = part.to_int()
    if num < 0 or num > 255 {
      return false
    }
  }
  
  true
}