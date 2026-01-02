// Azimuth Cross-Platform Compatibility Tests
// 跨平台兼容性测试用例 - 专注于不同操作系统和架构的兼容性

// Test 1: 操作系统特定功能测试
test "operating system specific functionality" {
  let platform_detector = PlatformDetector::new()
  let current_os = PlatformDetector::detect_os(platform_detector)
  
  // 测试基本操作系统检测
  assert_true(current_os != "")
  assert_true(["linux", "windows", "macos", "freebsd", "openbsd"].contains(current_os))
  
  // 测试操作系统特定功能
  match current_os {
    "linux" => {
      // Linux特定功能测试
      let linux_features = LinuxFeatures::new()
      assert_true(LinuxFeatures::supports_inotify(linux_features))
      assert_true(LinuxFeatures::supports_epoll(linux_features))
      
      // 测试文件系统特性
      let fs_features = LinuxFeatures::get_filesystem_features(linux_features, "/")
      assert_true(fs_features.supports_symlinks)
      assert_true(fs_features.supports_hardlinks)
      
      // 测试进程管理
      let process_info = LinuxFeatures::get_process_info(linux_features)
      assert_true(process_info.pid > 0)
      assert_true(process_info.ppid >= 0)
      assert_true(process_info.memory_usage > 0)
    }
    "windows" => {
      // Windows特定功能测试
      let windows_features = WindowsFeatures::new()
      assert_true(WindowsFeatures::supports_registry(windows_features))
      assert_true(WindowsFeatures::supports_services(windows_features))
      
      // 测试Windows文件系统特性
      let fs_features = WindowsFeatures::get_filesystem_features(windows_features, "C:\\")
      assert_true(fs_features.supports_ntfs)
      assert_true(fs_features.supports_alternate_streams)
      
      // 测试Windows API
      let system_info = WindowsFeatures::get_system_info(windows_features)
      assert_true(system_info.processor_count > 0)
      assert_true(system_info.page_size > 0)
    }
    "macos" => {
      // macOS特定功能测试
      let macos_features = MacOSFeatures::new()
      assert_true(MacOSFeatures::supports_fsevents(macos_features))
      assert_true(MacOSFeatures::supports_launchd(macos_features))
      
      // 测试macOS文件系统特性
      let fs_features = MacOSFeatures::get_filesystem_features(macos_features, "/")
      assert_true(fs_features.supports_case_insensitive)
      assert_true(fs_features.supports_resource_forks)
      
      // 测试macOS系统信息
      let system_info = MacOSFeatures::get_system_info(macos_features)
      assert_true(system_info.kernel_version != "")
      assert_true(system_info.hardware_model != "")
    }
    _ => {
      // 其他操作系统的基本测试
      assert_true(true) // 至少检测到了操作系统
    }
  }
  
  // 测试跨平台兼容性抽象层
  let cross_platform = CrossPlatformAPI::new()
  let temp_dir = CrossPlatformAPI::get_temp_dir(cross_platform)
  assert_true(temp_dir != "")
  
  let path_separator = CrossPlatformAPI::get_path_separator(cross_platform)
  assert_true(path_separator == "/" || path_separator == "\\")
  
  // 测试文件路径处理
  let test_path = CrossPlatformAPI::join_path(cross_platform, [temp_dir, "azimuth_test"])
  assert_true(String::contains(test_path, temp_dir))
  assert_true(String::contains(test_path, "azimuth_test"))
}

// Test 2: 架构特定功能测试
test "architecture specific functionality" {
  let arch_detector = ArchitectureDetector::new()
  let current_arch = ArchitectureDetector::detect_architecture(arch_detector)
  
  // 测试基本架构检测
  assert_true(current_arch != "")
  assert_true(["x86_64", "x86", "arm64", "arm", "riscv64", "wasm32"].contains(current_arch))
  
  // 测试架构特定功能
  match current_arch {
    "x86_64" => {
      // x86_64特定功能测试
      let x86_features = X86Features::new()
      assert_true(X86Features::supports_64bit(x86_features))
      assert_true(X86Features::supports_sse(x86_features))
      
      // 测试CPU特性检测
      let cpu_info = X86Features::get_cpu_info(x86_features)
      assert_true(cpu_info.vendor != "")
      assert_true(cpu_info.model != "")
      assert_true(cpu_info.frequency > 0)
      
      // 测试原子操作支持
      assert_true(X86Features::supports_atomic_operations(x86_features))
    }
    "arm64" => {
      // ARM64特定功能测试
      let arm_features = ARMFeatures::new()
      assert_true(ARMFeatures::supports_64bit(arm_features))
      assert_true(ARMFeatures::supports_neon(arm_features))
      
      // 测试ARM特性
      let cpu_info = ARMFeatures::get_cpu_info(arm_features)
      assert_true(cpu_info.implementer != "")
      assert_true(cpu_info.architecture != "")
      assert_true(cpu_info.variant != "")
      
      // 测试ARM特定优化
      assert_true(ARMFeatures::supports_crypto_extensions(arm_features))
    }
    "wasm32" => {
      // WebAssembly特定功能测试
      let wasm_features = WASMFeatures::new()
      assert_true(WASMFeatures::supports_32bit(wasm_features))
      
      // 测试WASM环境
      let wasm_info = WASMFeatures::get_environment_info(wasm_features)
      assert_true(wasm_info.memory_size > 0)
      assert_true(wasm_features.supports_bulk_memory)
    }
    _ => {
      // 其他架构的基本测试
      assert_true(true) // 至少检测到了架构
    }
  }
  
  // 测试字节序处理
  let endian_detector = EndianDetector::new()
  let current_endian = EndianDetector::detect_endianess(endian_detector)
  assert_true(current_endian == "little" || current_endian == "big")
  
  // 测试跨平台数据序列化
  let serializer = CrossPlatformSerializer::new()
  let test_data = { "value": 42, "name": "test", "floating": 3.14 }
  let serialized = CrossPlatformSerializer::serialize(serializer, test_data)
  let deserialized = CrossPlatformSerializer::deserialize(serializer, serialized)
  
  assert_eq(deserialized.value, test_data.value)
  assert_eq(deserialized.name, test_data.name)
  assert_eq(deserialized.floating, test_data.floating)
}

// Test 3: 文件系统兼容性测试
test "filesystem compatibility testing" {
  let fs_compatibility = FileSystemCompatibility::new()
  
  // 测试基本文件操作
  let test_file = FileSystemCompatibility::get_temp_path(fs_compatibility, "azimuth_fs_test.txt")
  let test_content = "Azimuth filesystem compatibility test content"
  
  // 写入文件
  let write_result = FileSystemCompatibility::write_file(fs_compatibility, test_file, test_content)
  match write_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 读取文件
  let read_result = FileSystemCompatibility::read_file(fs_compatibility, test_file)
  match read_result {
    Ok(content) => assert_eq(content, test_content)
    Err(_) => assert_true(false)
  }
  
  // 测试文件属性
  let file_attrs = FileSystemCompatibility::get_file_attributes(fs_compatibility, test_file)
  assert_true(file_attrs.exists)
  assert_true(file_attrs.size > 0)
  assert_true(file_attrs.created_time > 0)
  assert_true(file_attrs.modified_time > 0)
  
  // 测试目录操作
  let test_dir = FileSystemCompatibility::get_temp_path(fs_compatibility, "azimuth_test_dir")
  let create_dir_result = FileSystemCompatibility::create_directory(fs_compatibility, test_dir)
  match create_dir_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 测试目录列表
  let dir_list = FileSystemCompatibility::list_directory(fs_compatibility, test_dir)
  assert_true(dir_list.length() >= 0)
  
  // 测试路径操作
  let joined_path = FileSystemCompatibility::join_paths(fs_compatibility, [test_dir, "subdir", "file.txt"])
  assert_true(String::contains(joined_path, test_dir))
  
  let normalized_path = FileSystemCompatibility::normalize_path(fs_compatibility, joined_path)
  assert_true(normalized_path != "")
  
  // 测试文件权限
  let permission_result = FileSystemCompatibility::set_permissions(fs_compatibility, test_file, 0o644)
  match permission_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(true) // 某些平台可能不支持权限设置
  }
  
  // 清理测试文件和目录
  FileSystemCompatibility::delete_file(fs_compatibility, test_file)
  FileSystemCompatibility::delete_directory(fs_compatibility, test_dir)
}

// Test 4: 网络兼容性测试
test "network compatibility testing" {
  let network_compatibility = NetworkCompatibility::new()
  
  // 测试本地回环连接
  let loopback_result = NetworkCompatibility::test_loopback_connection(network_compatibility, 8080)
  match loopback_result {
    Ok(success) => assert_true(success)
    Err(_) => assert_true(true) // 端口可能被占用
  }
  
  // 测试DNS解析
  let dns_result = NetworkCompatibility::resolve_hostname(network_compatibility, "localhost")
  match dns_result {
    Ok(addresses) => assert_true(addresses.length() > 0)
    Err(_) => assert_true(false)
  }
  
  // 测试网络接口检测
  let interfaces = NetworkCompatibility::get_network_interfaces(network_compatibility)
  assert_true(interfaces.length() > 0)
  
  // 验证至少有一个回环接口
  let has_loopback = interfaces.any(fn(interface) { 
    String::contains(interface.name, "lo") || interface.is_loopback 
  })
  assert_true(has_loopback)
  
  // 测试Socket创建
  let socket_result = NetworkCompatibility::create_socket(network_compatibility, SocketType::TCP)
  match socket_result {
    Ok(socket) => {
      // 测试Socket选项
      let option_result = NetworkCompatibility::set_socket_option(socket, "timeout", 5000)
      match option_result {
        Ok(_) => assert_true(true)
        Err(_) => assert_true(true) // 某些选项可能不支持
      }
      
      NetworkCompatibility::close_socket(socket)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试HTTP客户端兼容性
  let http_client = NetworkCompatibility::create_http_client(network_compatibility)
  let http_result = NetworkCompatibility::http_get(http_client, "http://httpbin.org/get")
  match http_result {
    Ok(response) => {
      assert_true(response.status_code >= 200 && response.status_code < 300)
      assert_true(response.body.length() > 0)
    }
    Err(_) => assert_true(true) // 网络可能不可用
  }
}

// Test 5: 时间和时区兼容性测试
test "time and timezone compatibility testing" {
  let time_compatibility = TimeCompatibility::new()
  
  // 测试基本时间获取
  let current_time = TimeCompatibility::get_current_time(time_compatibility)
  assert_true(current_time.unix_timestamp > 0)
  assert_true(current_time.year >= 2020)
  assert_true(current_time.month >= 1 && current_time.month <= 12)
  assert_true(current_time.day >= 1 && current_time.day <= 31)
  assert_true(current_time.hour >= 0 && current_time.hour <= 23)
  assert_true(current_time.minute >= 0 && current_time.minute <= 59)
  assert_true(current_time.second >= 0 && current_time.second <= 59)
  
  // 测试时区检测
  let timezone_info = TimeCompatibility::get_timezone_info(time_compatibility)
  assert_true(timezone_info.name != "")
  assert_true(timezone_info.offset_hours >= -12 && timezone_info.offset_hours <= 14)
  assert_true(timezone_info.offset_minutes >= 0 && timezone_info.offset_minutes <= 59)
  
  // 测试时区转换
  let utc_time = TimeCompatibility::to_utc(time_compatibility, current_time)
  let local_time = TimeCompatibility::from_utc(time_compatibility, utc_time)
  
  assert_eq(utc_time.unix_timestamp, current_time.unix_timestamp - timezone_info.offset_hours * 3600 - timezone_info.offset_minutes * 60)
  
  // 测试时间格式化
  let iso_format = TimeCompatibility::format_iso8601(time_compatibility, current_time)
  assert_true(iso_format.length() > 0)
  assert_true(String::contains(iso_format, "T"))
  
  let rfc_format = TimeCompatibility::format_rfc2822(time_compatibility, current_time)
  assert_true(rfc_format.length() > 0)
  
  // 测试时间解析
  let parsed_time = TimeCompatibility::parse_iso8601(time_compatibility, iso_format)
  match parsed_time {
    Ok(time) => assert_eq(time.unix_timestamp, current_time.unix_timestamp)
    Err(_) => assert_true(false)
  }
  
  // 测试高精度时间
  let high_precision_time = TimeCompatibility::get_high_precision_time(time_compatibility)
  assert_true(high_precision_time.seconds > 0)
  assert_true(high_precision_time.nanoseconds >= 0 && high_precision_time.nanoseconds < 1000000000)
  
  // 测试时间计算
  let future_time = TimeCompatibility::add_seconds(time_compatibility, current_time, 3600) // +1小时
  assert_true(future_time.unix_timestamp > current_time.unix_timestamp)
  assert_eq(future_time.unix_timestamp, current_time.unix_timestamp + 3600)
  
  let duration = TimeCompatibility::calculate_duration(time_compatibility, current_time, future_time)
  assert_eq(duration.seconds, 3600)
}

// Test 6: 线程和并发兼容性测试
test "threading and concurrency compatibility testing" {
  let concurrency_compatibility = ConcurrencyCompatibility::new()
  
  // 测试线程创建
  let thread_result = ConcurrencyCompatibility::create_thread(concurrency_compatibility, fn() {
    ConcurrentProcessor::sleep(100)
    "thread_result"
  })
  
  match thread_result {
    Ok(thread) => {
      // 等待线程完成
      let join_result = ConcurrencyCompatibility::join_thread(thread)
      match join_result {
        Ok(result) => assert_eq(result, "thread_result")
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试互斥锁
  let mutex = ConcurrencyCompatibility::create_mutex(concurrency_compatibility)
  let shared_data = ConcurrencyCompatibility::create_shared(concurrency_compatibility, 0)
  
  // 创建多个线程并发访问共享数据
  let threads = []
  for i in 0..<10 {
    let thread = ConcurrencyCompatibility::create_thread(concurrency_compatibility, fn() {
      ConcurrencyCompatibility::lock(mutex)
      let current = ConcurrencyCompatibility::get_shared(shared_data)
      ConcurrencyCompatibility::set_shared(shared_data, current + 1)
      ConcurrencyCompatibility::unlock(mutex)
    })
    threads = threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    ConcurrencyCompatibility::join_thread(thread)
  }
  
  // 验证结果
  let final_value = ConcurrencyCompatibility::get_shared(shared_data)
  assert_eq(final_value, 10)
  
  // 测试原子操作
  let atomic_counter = ConcurrencyCompatibility::create_atomic_counter(concurrency_compatibility, 0)
  
  let atomic_threads = []
  for i in 0::<100 {
    let thread = ConcurrencyCompatibility::create_thread(concurrency_compatibility, fn() {
      ConcurrencyCompatibility::atomic_increment(atomic_counter)
    })
    atomic_threads = atomic_threads.push(thread)
  }
  
  for thread in atomic_threads {
    ConcurrencyCompatibility::join_thread(thread)
  }
  
  let atomic_value = ConcurrencyCompatibility::get_atomic_value(atomic_counter)
  assert_eq(atomic_value, 100)
  
  // 测试条件变量
  let condition = ConcurrencyCompatibility::create_condition(concurrency_compatibility)
  let condition_mutex = ConcurrencyCompatibility::create_mutex(concurrency_compatibility)
  let ready = ConcurrencyCompatibility::create_shared(concurrency_compatibility, false)
  
  // 创建等待线程
  let waiter = ConcurrencyCompatibility::create_thread(concurrency_compatibility, fn() {
    ConcurrencyCompatibility::lock(condition_mutex)
    while !ConcurrencyCompatibility::get_shared(ready) {
      ConcurrencyCompatibility::wait(condition, condition_mutex)
    }
    ConcurrencyCompatibility::unlock(condition_mutex)
  })
  
  // 创建通知线程
  let notifier = ConcurrencyCompatibility::create_thread(concurrency_compatibility, fn() {
    ConcurrentProcessor::sleep(200)
    ConcurrencyCompatibility::lock(condition_mutex)
    ConcurrencyCompatibility::set_shared(ready, true)
    ConcurrencyCompatibility::notify_all(condition)
    ConcurrencyCompatibility::unlock(condition_mutex)
  })
  
  ConcurrencyCompatibility::join_thread(waiter)
  ConcurrencyCompatibility::join_thread(notifier)
  
  // 验证条件变量工作正常
  assert_true(ConcurrencyCompatibility::get_shared(ready))
}

// Test 7: 环境变量和系统配置兼容性测试
test "environment variables and system configuration compatibility" {
  let env_compatibility = EnvironmentCompatibility::new()
  
  // 测试环境变量操作
  let test_key = "AZIMUTH_TEST_VAR"
  let test_value = "test_value_12345"
  
  // 设置环境变量
  let set_result = EnvironmentCompatibility::set_env(env_compatibility, test_key, test_value)
  match set_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 获取环境变量
  let get_result = EnvironmentCompatibility::get_env(env_compatibility, test_key)
  match get_result {
    Some(value) => assert_eq(value, test_value)
    None => assert_true(false)
  }
  
  // 测试环境变量列表
  let all_env = EnvironmentCompatibility::get_all_env(env_compatibility)
  assert_true(all_env.length() > 0)
  
  // 验证我们设置的变量存在
  let has_test_var = all_env.any(fn(pair) { 
    let (key, value) = pair
    key == test_key && value == test_value
  })
  assert_true(has_test_var)
  
  // 删除测试环境变量
  let unset_result = EnvironmentCompatibility::unset_env(env_compatibility, test_key)
  match unset_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证变量已删除
  let deleted_result = EnvironmentCompatibility::get_env(env_compatibility, test_key)
  match deleted_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试系统信息获取
  let system_info = EnvironmentCompatibility::get_system_info(env_compatibility)
  assert_true(system_info.hostname != "")
  assert_true(system_info.username != "")
  assert_true(system_info.home_dir != "")
  assert_true(system_info.temp_dir != "")
  
  // 测试路径展开
  let home_path = EnvironmentCompatibility::expand_path(env_compatibility, "~/test")
  assert_true(String::contains(home_path, system_info.home_dir))
  
  let env_path = EnvironmentCompatibility::expand_path(env_compatibility, "$HOME/test")
  assert_true(String::contains(env_path, system_info.home_dir))
  
  // 测试命令行参数
  let args = EnvironmentCompatibility::get_command_line_args(env_compatibility)
  assert_true(args.length() >= 0)
  
  // 测试工作目录
  let current_dir = EnvironmentCompatibility::get_current_directory(env_compatibility)
  assert_true(current_dir != "")
  
  let temp_dir = EnvironmentCompatibility::get_temp_directory(env_compatibility)
  assert_true(temp_dir != "")
  
  // 测试目录更改
  let original_dir = current_dir
  let change_result = EnvironmentCompatibility::set_current_directory(env_compatibility, temp_dir)
  match change_result {
    Ok(_) => {
      let new_dir = EnvironmentCompatibility::get_current_directory(env_compatibility)
      assert_eq(new_dir, temp_dir)
      
      // 恢复原始目录
      EnvironmentCompatibility::set_current_directory(env_compatibility, original_dir)
    }
    Err(_) => assert_true(false)
  }
}

// Test 8: 内存管理兼容性测试
test "memory management compatibility testing" {
  let memory_compatibility = MemoryCompatibility::new()
  
  // 测试内存分配
  let alloc_result = MemoryCompatibility::allocate(memory_compatibility, 1024)
  match alloc_result {
    Ok(ptr) => {
      // 测试内存写入
      let write_result = MemoryCompatibility::write_memory(ptr, [1, 2, 3, 4])
      match write_result {
        Ok(_) => assert_true(true)
        Err(_) => assert_true(false)
      }
      
      // 测试内存读取
      let read_result = MemoryCompatibility::read_memory(ptr, 4)
      match read_result {
        Ok(data) => assert_eq(data, [1, 2, 3, 4])
        Err(_) => assert_true(false)
      }
      
      // 释放内存
      MemoryCompatibility::deallocate(memory_compatibility, ptr)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试内存信息
  let memory_info = MemoryCompatibility::get_memory_info(memory_compatibility)
  assert_true(memory_info.total_memory > 0)
  assert_true(memory_info.available_memory > 0)
  assert_true(memory_info.used_memory >= 0)
  
  // 测试内存对齐
  let alignment = MemoryCompatibility::get_memory_alignment(memory_compatibility)
  assert_true(alignment > 0 && (alignment & (alignment - 1)) == 0) // 必须是2的幂
  
  let aligned_ptr = MemoryCompatibility::allocate_aligned(memory_compatibility, 1024, alignment)
  match aligned_ptr {
    Ok(ptr) => {
      let address = MemoryCompatibility::get_pointer_address(ptr)
      assert_true(address % alignment == 0) // 验证对齐
      MemoryCompatibility::deallocate(memory_compatibility, ptr)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试内存映射文件
  let test_file = MemoryCompatibility::get_temp_path(memory_compatibility, "azimuth_mmap_test.bin")
  let test_data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // 创建测试文件
  let file_result = MemoryCompatibility::create_file_with_data(memory_compatibility, test_file, test_data)
  match file_result {
    Ok(_) => {
      // 映射文件
      let mmap_result = MemoryCompatibility::memory_map_file(memory_compatibility, test_file)
      match mmap_result {
        Ok(mmap) => {
          // 验证映射内容
          let mapped_data = MemoryCompatibility::get_mapped_data(mmap)
          assert_eq(mapped_data, test_data)
          
          // 修改映射数据
          MemoryCompatibility::write_mapped_data(mmap, 0, [10, 9, 8, 7])
          
          // 取消映射
          MemoryCompatibility::memory_unmap(mmap)
          
          // 验证文件内容已更改
          let updated_data = MemoryCompatibility::read_file_data(memory_compatibility, test_file)
          assert_eq(updated_data[0], 10)
          assert_eq(updated_data[1], 9)
          assert_eq(updated_data[2], 8)
          assert_eq(updated_data[3], 7)
        }
        Err(_) => assert_true(true) // 某些平台可能不支持内存映射
      }
      
      // 清理测试文件
      MemoryCompatibility::delete_file(memory_compatibility, test_file)
    }
    Err(_) => assert_true(false)
  }
}

// Test 9: 系统调用兼容性测试
test "system call compatibility testing" {
  let syscall_compatibility = SystemCallCompatibility::new()
  
  // 测试文件系统系统调用
  let test_file = SystemCallCompatibility::get_temp_path(syscall_compatibility, "azimuth_syscall_test.txt")
  
  // 使用系统调用创建文件
  let create_result = SystemCallCompatibility::syscall_create(syscall_compatibility, test_file, 0o644)
  match create_result {
    Ok(fd) => {
      // 使用系统调用写入数据
      let write_data = "System call compatibility test"
      let write_result = SystemCallCompatibility::syscall_write(syscall_compatibility, fd, String::to_bytes(write_data))
      match write_result {
        Ok(bytes_written) => assert_eq(bytes_written, write_data.length())
        Err(_) => assert_true(false)
      }
      
      // 关闭文件描述符
      SystemCallCompatibility::syscall_close(syscall_compatibility, fd)
    }
    Err(_) => assert_true(false)
  }
  
  // 使用系统调用读取文件
  let open_result = SystemCallCompatibility::syscall_open(syscall_compatibility, test_file, 0)
  match open_result {
    Ok(fd) => {
      // 获取文件大小
      let stat_result = SystemCallCompatibility::syscall_stat(syscall_compatibility, test_file)
      match stat_result {
        Ok(file_stat) => assert_true(file_stat.size > 0)
        Err(_) => assert_true(false)
      }
      
      // 读取文件内容
      let read_result = SystemCallCompatibility::syscall_read(syscall_compatibility, fd, 1024)
      match read_result {
        Ok(data) => {
          let content = String::from_bytes(data)
          assert_eq(content, write_data)
        }
        Err(_) => assert_true(false)
      }
      
      SystemCallCompatibility::syscall_close(syscall_compatibility, fd)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试进程系统调用
  let pid = SystemCallCompatibility::syscall_getpid(syscall_compatibility)
  assert_true(pid > 0)
  
  let ppid = SystemCallCompatibility::syscall_getppid(syscall_compatibility)
  assert_true(ppid >= 0)
  
  // 测试时间系统调用
  let syscall_time = SystemCallCompatibility::syscall_gettime(syscall_compatibility)
  assert_true(syscall_time > 0)
  
  // 清理测试文件
  SystemCallCompatibility::syscall_unlink(syscall_compatibility, test_file)
}

// Test 10: 平台特定优化测试
test "platform-specific optimization testing" {
  let platform_optimizer = PlatformOptimizer::new()
  
  // 检测当前平台特性
  let platform_features = PlatformOptimizer::detect_features(platform_optimizer)
  
  // 测试SIMD支持
  if platform_features.has_simd {
    let simd_test_data = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
    let simd_result = PlatformOptimizer::simd_vector_add(platform_optimizer, simd_test_data, simd_test_data)
    
    assert_eq(simd_result.length(), simd_test_data.length())
    for i in 0..<simd_result.length() {
      assert_eq(simd_result[i], simd_test_data[i] * 2.0)
    }
  }
  
  // 测试多核优化
  if platform_features.cpu_count > 1 {
    let parallel_data = []
    for i in 0..<1000 {
      parallel_data = parallel_data.push(i.to_float())
    }
    
    let parallel_start = Time::now()
    let parallel_result = PlatformOptimizer::parallel_map(platform_optimizer, parallel_data, fn(x) { x * 2.0 })
    let parallel_time = Time::now() - parallel_start
    
    let sequential_start = Time::now()
    let sequential_result = []
    for x in parallel_data {
      sequential_result = sequential_result.push(x * 2.0)
    }
    let sequential_time = Time::now() - sequential_start
    
    // 并行版本应该更快或相等
    assert_true(parallel_time <= sequential_time * 1.2) // 允许20%的误差
    
    // 验证结果一致
    assert_eq(parallel_result.length(), sequential_result.length())
    for i in 0..<parallel_result.length() {
      assert_eq(parallel_result[i], sequential_result[i])
    }
  }
  
  // 测试缓存优化
  let cache_test_data = []
  for i in 0..<10000 {
    cache_test_data = cache_test_data.push(i.to_float())
  }
  
  let cache_optimized_result = PlatformOptimizer::cache_friendly_sum(platform_optimizer, cache_test_data)
  let regular_sum = cache_test_data.reduce(fn(acc, x) { acc + x }, 0.0)
  
  assert_eq(cache_optimized_result, regular_sum)
  
  // 测试内存预取优化
  if platform_features.supports_prefetch {
    let prefetch_data = []
    for i in 0..<1000 {
      prefetch_data = prefetch_data.push([i.to_float(), (i + 1).to_float(), (i + 2).to_float(), (i + 3).to_float()])
    }
    
    let prefetch_result = PlatformOptimizer::prefetch_optimized_process(platform_optimizer, prefetch_data)
    assert_true(prefetch_result.processing_time > 0)
    assert_true(prefetch_result.cache_hit_rate > 0.5) // 至少50%的缓存命中率
  }
  
  // 测试平台特定的字符串操作优化
  let test_string = "Azimuth platform optimization test string with repeated content"
  let optimized_length = PlatformOptimizer::optimized_string_length(platform_optimizer, test_string)
  assert_eq(optimized_length, test_string.length())
  
  let optimized_copy = PlatformOptimizer::optimized_string_copy(platform_optimizer, test_string)
  assert_eq(optimized_copy, test_string)
  
  // 测试平台特定的数学运算优化
  let math_test_data = []
  for i in 0..<1000 {
    math_test_data = math_test_data.push(i.to_float())
  }
  
  let optimized_sqrt = PlatformOptimizer::optimized_vector_sqrt(platform_optimizer, math_test_data)
  assert_eq(optimized_sqrt.length(), math_test_data.length())
  
  for i in 0..<optimized_sqrt.length() {
    let expected = (math_test_data[i]).sqrt()
    assert_true((optimized_sqrt[i] - expected).abs() < 0.0001) // 允许浮点误差
  }
}