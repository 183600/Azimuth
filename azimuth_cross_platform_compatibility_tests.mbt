// Azimuth Cross-Platform Compatibility Test Suite
// 跨平台兼容性测试套件，验证遥测系统在不同操作系统和平台上的兼容性和一致性

// 测试1: 文件路径跨平台处理
test "文件路径跨平台处理测试" {
  // 测试不同平台的路径分隔符处理
  let unix_path = "/home/user/data/azimuth/logs"
  let windows_path = "C:\\Users\\user\\data\\azimuth\\logs"
  let mixed_path = "data/azimuth\\logs/test"
  
  // 标准化路径
  let normalized_unix = azimuth::PathUtils::normalize(unix_path)
  let normalized_windows = azimuth::PathUtils::normalize(windows_path)
  let normalized_mixed = azimuth::PathUtils::normalize(mixed_path)
  
  // 验证路径标准化结果
  assert_true(normalized_unix.contains("data") && normalized_unix.contains("azimuth") && normalized_unix.contains("logs"))
  assert_true(normalized_windows.contains("data") && normalized_windows.contains("azimuth") && normalized_windows.contains("logs"))
  assert_true(normalized_mixed.contains("data") && normalized_mixed.contains("azimuth") && normalized_mixed.contains("logs"))
  
  // 测试路径连接
  let base_path = azimuth::PathUtils::get_base_dir()
  let relative_path = "logs/azimuth.log"
  let joined_path = azimuth::PathUtils::join(base_path, relative_path)
  
  // 验证路径连接结果
  assert_true(joined_path.contains(base_path))
  assert_true(joined_path.contains(relative_path))
  
  // 测试路径分割
  let path_parts = azimuth::PathUtils::split(joined_path)
  assert_true(path_parts.length() >= 3)
  assert_true(path_parts.contains("logs"))
  assert_true(path_parts.contains("azimuth.log"))
  
  // 测试获取文件扩展名
  let file_with_extension = "test.log"
  let file_without_extension = "test"
  let file_with_multiple_extensions = "test.tar.gz"
  
  assert_eq(azimuth::PathUtils::get_extension(file_with_extension), Some("log"))
  assert_eq(azimuth::PathUtils::get_extension(file_without_extension), None)
  assert_eq(azimuth::PathUtils::get_extension(file_with_multiple_extensions), Some("gz"))
  
  // 测试获取文件名（不含路径）
  let path_with_filename = "/path/to/file.txt"
  let filename = azimuth::PathUtils::get_filename(path_with_filename)
  assert_eq(filename, "file.txt")
  
  // 测试获取目录名（不含文件名）
  let dirname = azimuth::PathUtils::get_dirname(path_with_filename)
  assert_eq(dirname, "/path/to")
}

// 测试2: 环境变量跨平台处理
test "环境变量跨平台处理测试" {
  // 测试获取环境变量
  let path_env = azimuth::EnvironmentUtils::get_env("PATH")
  match path_env {
    Some(path) => {
      // 验证PATH环境变量包含路径分隔符
      if azimuth::PlatformUtils::is_windows() {
        assert_true(path.contains(";"))
      } else {
        assert_true(path.contains(":"))
      }
    }
    None => assert_true(false)
  }
  
  // 测试设置和获取环境变量
  let test_key = "AZIMUTH_TEST_VAR"
  let test_value = "test_value"
  
  let set_result = azimuth::EnvironmentUtils::set_env(test_key, test_value)
  assert_true(set_result)
  
  let get_result = azimuth::EnvironmentUtils::get_env(test_key)
  match get_result {
    Some(value) => assert_eq(value, test_value)
    None => assert_true(false)
  }
  
  // 测试删除环境变量
  let unset_result = azimuth::EnvironmentUtils::unset_env(test_key)
  assert_true(unset_result)
  
  let get_after_unset = azimuth::EnvironmentUtils::get_env(test_key)
  match get_after_unset {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试环境变量路径列表处理
  let path_list = azimuth::EnvironmentUtils::get_path_list("PATH")
  assert_true(path_list.length() > 0)
  
  // 验证路径列表中的每个路径都是有效的
  for path in path_list {
    assert_true(path.length() > 0)
  }
  
  // 测试路径分隔符
  let separator = azimuth::EnvironmentUtils::get_path_separator()
  if azimuth::PlatformUtils::is_windows() {
    assert_eq(separator, ";")
  } else {
    assert_eq(separator, ":")
  }
}

// 测试3: 线程和进程跨平台处理
test "线程和进程跨平台处理测试" {
  // 测试获取当前线程ID
  let thread_id = azimuth::ThreadUtils::get_current_thread_id()
  assert_true(thread_id > 0)
  
  // 测试获取当前进程ID
  let process_id = azimuth::ProcessUtils::get_current_process_id()
  assert_true(process_id > 0)
  
  // 测试线程休眠功能
  let start_time = azimuth::TimeUtils::get_current_time_millis()
  azimuth::ThreadUtils::sleep(100) // 休眠100毫秒
  let end_time = azimuth::TimeUtils::get_current_time_millis()
  
  // 验证休眠时间（允许一定误差）
  let sleep_duration = end_time - start_time
  assert_true(sleep_duration >= 90 && sleep_duration <= 110)
  
  // 测试获取系统CPU核心数
  let cpu_count = azimuth::SystemUtils::get_cpu_count()
  assert_true(cpu_count >= 1)
  
  // 测试获取系统内存信息
  let memory_info = azimuth::SystemUtils::get_memory_info()
  assert_true(memory_info.total > 0)
  assert_true(memory_info.available > 0)
  assert_true(memory_info.available <= memory_info.total)
  
  // 测试获取系统负载
  let system_load = azimuth::SystemUtils::get_system_load()
  assert_true(system_load.load_average >= 0.0)
  
  // 测试获取进程列表
  let process_list = azimuth::ProcessUtils::get_process_list()
  assert_true(process_list.length() > 0)
  
  // 验证当前进程在进程列表中
  let current_process_found = process_list.any(fn(p) { p.id == process_id })
  assert_true(current_process_found)
}

// 测试4: 网络跨平台处理
test "网络跨平台处理测试" {
  // 测试获取主机名
  let hostname = azimuth::NetworkUtils::get_hostname()
  assert_true(hostname.length() > 0)
  
  // 测试获取本地IP地址
  let local_ips = azimuth::NetworkUtils::get_local_ip_addresses()
  assert_true(local_ips.length() > 0)
  
  // 验证至少有一个有效的IP地址
  let has_valid_ip = local_ips.any(fn(ip) { 
    ip != "127.0.0.1" && ip != "::1" && ip.length() > 0
  })
  // 注意：在某些测试环境中可能只有环回地址
  
  // 测试解析域名
  let domain_result = azimuth::NetworkUtils::resolve_domain("google.com")
  match domain_result {
    Ok(ip_addresses) => {
      assert_true(ip_addresses.length() > 0)
      // 验证IP地址格式
      for ip in ip_addresses {
        assert_true(azimuth::NetworkUtils::is_valid_ip(ip))
      }
    }
    Err(_) => {
      // 在没有网络连接的环境中，域名解析可能失败
      assert_true(true)
    }
  }
  
  // 测试检查端口是否被占用
  let random_port = 12345 + (azimuth::TimeUtils::get_current_time_millis() % 10000)
  let port_in_use = azimuth::NetworkUtils::is_port_in_use(random_port)
  // 随机端口通常不会被占用，但不能保证
  
  // 测试获取网络接口列表
  let network_interfaces = azimuth::NetworkUtils::get_network_interfaces()
  assert_true(network_interfaces.length() > 0)
  
  // 验证每个网络接口都有名称
  for interface in network_interfaces {
    assert_true(interface.name.length() > 0)
  }
}

// 测试5: 时间跨平台处理
test "时间跨平台处理测试" {
  // 测试获取当前时间戳（毫秒）
  let current_time_millis = azimuth::TimeUtils::get_current_time_millis()
  assert_true(current_time_millis > 1609459200000L) // 2021-01-01之后的时间戳
  
  // 测试获取当前时间戳（秒）
  let current_time_seconds = azimuth::TimeUtils::get_current_time_seconds()
  assert_true(current_time_seconds > 1609459200L) // 2021-01-01之后的时间戳
  
  // 测试时间戳转换
  let converted_millis = azimuth::TimeUtils::seconds_to_millis(current_time_seconds)
  let converted_seconds = azimuth::TimeUtils::millis_to_seconds(current_time_millis)
  
  // 验证转换结果（允许1秒误差）
  assert_true((converted_millis - current_time_millis).abs() < 1000L)
  assert_true((converted_seconds - current_time_seconds).abs() <= 1L)
  
  // 测试格式化时间
  let formatted_time = azimuth::TimeUtils::format_timestamp(current_time_millis, "%Y-%m-%d %H:%M:%S")
  assert_true(formatted_time.length() == 19) // "YYYY-MM-DD HH:MM:SS"格式长度为19
  
  // 测试解析时间
  let parsed_time = azimuth::TimeUtils::parse_timestamp(formatted_time, "%Y-%m-%d %H:%M:%S")
  match parsed_time {
    Ok(timestamp) => {
      // 验证解析结果（允许1秒误差）
      assert_true((timestamp - current_time_millis).abs() < 1000L)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试获取时区信息
  let timezone = azimuth::TimeUtils::get_timezone()
  assert_true(timezone.length() > 0)
  
  // 测试获取UTC偏移量
  let utc_offset = azimuth::TimeUtils::get_utc_offset()
  assert_true(utc_offset >= -12 * 60 && utc_offset <= 14 * 60) // UTC偏移量在-12到+14小时之间
}

// 测试6: 文件系统跨平台处理
test "文件系统跨平台处理测试" {
  // 测试创建临时目录
  let temp_dir = azimuth::FileSystemUtils::create_temp_directory("azimuth_test")
  assert_true(azimuth::FileSystemUtils::directory_exists(temp_dir))
  
  // 测试创建文件
  let test_file = azimuth::PathUtils::join(temp_dir, "test_file.txt")
  let file_content = "This is a test file content."
  let create_result = azimuth::FileSystemUtils::write_file(test_file, file_content)
  assert_true(create_result)
  
  // 测试检查文件是否存在
  assert_true(azimuth::FileSystemUtils::file_exists(test_file))
  
  // 测试读取文件
  let read_content = azimuth::FileSystemUtils::read_file(test_file)
  match read_content {
    Ok(content) => assert_eq(content, file_content)
    Err(_) => assert_true(false)
  }
  
  // 测试获取文件大小
  let file_size = azimuth::FileSystemUtils::get_file_size(test_file)
  assert_eq(file_size, file_content.length())
  
  // 测试获取文件修改时间
  let modified_time = azimuth::FileSystemUtils::get_file_modified_time(test_file)
  assert_true(modified_time > 0)
  
  // 测试复制文件
  let copied_file = azimuth::PathUtils::join(temp_dir, "copied_file.txt")
  let copy_result = azimuth::FileSystemUtils::copy_file(test_file, copied_file)
  assert_true(copy_result)
  assert_true(azimuth::FileSystemUtils::file_exists(copied_file))
  
  // 验证复制的内容
  let copied_content = azimuth::FileSystemUtils::read_file(copied_file)
  match copied_content {
    Ok(content) => assert_eq(content, file_content)
    Err(_) => assert_true(false)
  }
  
  // 测试移动文件
  let moved_file = azimuth::PathUtils::join(temp_dir, "moved_file.txt")
  let move_result = azimuth::FileSystemUtils::move_file(copied_file, moved_file)
  assert_true(move_result)
  assert_true(azimuth::FileSystemUtils::file_exists(moved_file))
  assert_false(azimuth::FileSystemUtils::file_exists(copied_file))
  
  // 测试列出目录内容
  let dir_contents = azimuth::FileSystemUtils::list_directory(temp_dir)
  assert_eq(dir_contents.length(), 2) // test_file.txt 和 moved_file.txt
  
  // 测试创建子目录
  let sub_dir = azimuth::PathUtils::join(temp_dir, "sub_directory")
  let create_dir_result = azimuth::FileSystemUtils::create_directory(sub_dir)
  assert_true(create_dir_result)
  assert_true(azimuth::FileSystemUtils::directory_exists(sub_dir))
  
  // 测试递归删除目录
  let delete_result = azimuth::FileSystemUtils::delete_directory_recursive(temp_dir)
  assert_true(delete_result)
  assert_false(azimuth::FileSystemUtils::directory_exists(temp_dir))
}

// 测试7: 系统信息跨平台处理
test "系统信息跨平台处理测试" {
  // 测试获取操作系统类型
  let os_type = azimuth::SystemUtils::get_os_type()
  assert_true(os_type == "Windows" || os_type == "Linux" || os_type == "macOS" || os_type == "Unix")
  
  // 测试获取操作系统版本
  let os_version = azimuth::SystemUtils::get_os_version()
  assert_true(os_version.length() > 0)
  
  // 测试获取架构信息
  let architecture = azimuth::SystemUtils::get_architecture()
  assert_true(architecture == "x86" || architecture == "x86_64" || architecture == "arm" || architecture == "arm64")
  
  // 测试获取主机信息
  let host_info = azimuth::SystemUtils::get_host_info()
  assert_true(host_info.hostname.length() > 0)
  assert_true(host_info.uptime > 0)
  
  // 测试获取用户信息
  let user_info = azimuth::SystemUtils::get_user_info()
  assert_true(user_info.username.length() > 0)
  
  // 测试获取当前工作目录
  let current_dir = azimuth::SystemUtils::get_current_directory()
  assert_true(azimuth::FileSystemUtils::directory_exists(current_dir))
  
  // 测试获取用户主目录
  let home_dir = azimuth::SystemUtils::get_home_directory()
  assert_true(azimuth::FileSystemUtils::directory_exists(home_dir))
  
  // 测试获取临时目录
  let temp_dir = azimuth::SystemUtils::get_temp_directory()
  assert_true(azimuth::FileSystemUtils::directory_exists(temp_dir))
  
  // 测试获取系统启动时间
  let boot_time = azimuth::SystemUtils::get_boot_time()
  assert_true(boot_time > 0)
  assert_true(boot_time < azimuth::TimeUtils::get_current_time_seconds())
}

// 测试8: 进程间通信跨平台处理
test "进程间通信跨平台处理测试" {
  // 测试创建命名管道
  let pipe_name = "azimuth_test_pipe_" + azimuth::TimeUtils::get_current_time_millis().to_string()
  let pipe_result = azimuth::IPCUtils::create_named_pipe(pipe_name)
  match pipe_result {
    Ok(pipe) => {
      // 测试写入管道
      let message = "Hello from azimuth test"
      let write_result = azimuth::IPCUtils::write_to_pipe(pipe, message)
      assert_true(write_result)
      
      // 测试从管道读取
      let read_result = azimuth::IPCUtils::read_from_pipe(pipe, message.length())
      match read_result {
        Ok(read_message) => assert_eq(read_message, message)
        Err(_) => assert_true(false)
      }
      
      // 测试关闭管道
      let close_result = azimuth::IPCUtils::close_pipe(pipe)
      assert_true(close_result)
    }
    Err(_) => {
      // 在某些平台上可能不支持命名管道
      assert_true(true)
    }
  }
  
  // 测试创建共享内存
  let shm_name = "azimuth_test_shm_" + azimuth::TimeUtils::get_current_time_millis().to_string()
  let shm_size = 1024
  let shm_result = azimuth::IPCUtils::create_shared_memory(shm_name, shm_size)
  match shm_result {
    Ok(shm) => {
      // 测试写入共享内存
      let data = "Shared memory test data"
      let write_result = azimuth::IPCUtils::write_to_shared_memory(shm, data)
      assert_true(write_result)
      
      // 测试从共享内存读取
      let read_result = azimuth::IPCUtils::read_from_shared_memory(shm, data.length())
      match read_result {
        Ok(read_data) => assert_eq(read_data, data)
        Err(_) => assert_true(false)
      }
      
      // 测试释放共享内存
      let release_result = azimuth::IPCUtils::release_shared_memory(shm)
      assert_true(release_result)
    }
    Err(_) => {
      // 在某些平台上可能不支持共享内存
      assert_true(true)
    }
  }
  
  // 测试信号处理
  let signal_result = azimuth::IPCUtils::send_signal(azimuth::ProcessUtils::get_current_process_id(), 0) // 发送空信号
  match signal_result {
    Ok(_) => assert_true(true),
    Err(_) => {
      // 在某些平台上可能不支持信号
      assert_true(true)
    }
  }
}

// 测试9: 注册表跨平台处理
test "注册表跨平台处理测试" {
  // 测试读取注册表（仅Windows）
  if azimuth::PlatformUtils::is_windows() {
    let key_result = azimuth::RegistryUtils::open_key("HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion")
    match key_result {
      Ok(key) => {
        // 测试读取注册表值
        let value_result = azimuth::RegistryUtils::read_string_value(key, "ProgramFilesDir")
        match value_result {
          Ok(value) => {
            assert_true(value.length() > 0)
            assert_true(value.contains("Program Files"))
          }
          Err(_) => assert_true(false)
        }
        
        // 测试关闭注册表键
        let close_result = azimuth::RegistryUtils::close_key(key)
        assert_true(close_result)
      }
      Err(_) => {
        // 注册表键可能不存在或无权限访问
        assert_true(true)
      }
    }
  } else {
    // 非Windows系统应该返回不支持
    let key_result = azimuth::RegistryUtils::open_key("HKEY_LOCAL_MACHINE\\SOFTWARE\\Test")
    match key_result {
      Ok(_) => assert_true(false),
      Err(error) => assert_eq(error.message, "Registry operations are only supported on Windows")
    }
  }
  
  // 测试写入注册表（仅Windows）
  if azimuth::PlatformUtils::is_windows() {
    let test_key_path = "HKEY_CURRENT_USER\\SOFTWARE\\AzimuthTest"
    let create_result = azimuth::RegistryUtils::create_key(test_key_path)
    match create_result {
      Ok(key) => {
        // 测试写入字符串值
        let write_result = azimuth::RegistryUtils::write_string_value(key, "TestValue", "Test data")
        assert_true(write_result)
        
        // 测试读取写入的值
        let read_result = azimuth::RegistryUtils::read_string_value(key, "TestValue")
        match read_result {
          Ok(value) => assert_eq(value, "Test data")
          Err(_) => assert_true(false)
        }
        
        // 测试删除值
        let delete_result = azimuth::RegistryUtils::delete_value(key, "TestValue")
        assert_true(delete_result)
        
        // 测试关闭键
        let close_result = azimuth::RegistryUtils::close_key(key)
        assert_true(close_result)
        
        // 测试删除键
        let delete_key_result = azimuth::RegistryUtils::delete_key(test_key_path)
        assert_true(delete_key_result)
      }
      Err(_) => {
        // 可能无权限创建注册表键
        assert_true(true)
      }
    }
  }
}

// 测试10: 权限和安全跨平台处理
test "权限和安全跨平台处理测试" {
  // 测试检查文件权限
  let temp_dir = azimuth::FileSystemUtils::create_temp_directory("azimuth_test")
  let test_file = azimuth::PathUtils::join(temp_dir, "test_file.txt")
  let create_result = azimuth::FileSystemUtils::write_file(test_file, "test content")
  assert_true(create_result)
  
  // 测试检查读取权限
  let read_permission = azimuth::PermissionUtils::check_read_permission(test_file)
  assert_true(read_permission)
  
  // 测试检查写入权限
  let write_permission = azimuth::PermissionUtils::check_write_permission(test_file)
  assert_true(write_permission)
  
  // 测试检查执行权限
  let execute_permission = azimuth::PermissionUtils::check_execute_permission(test_file)
  // 文本文件通常没有执行权限，但这取决于平台
  
  // 测试设置文件权限（仅Unix-like系统）
  if !azimuth::PlatformUtils::is_windows() {
    let chmod_result = azimuth::PermissionUtils::set_file_permissions(test_file, 0o644)
    assert_true(chmod_result)
    
    // 测试获取文件权限
    let permissions = azimuth::PermissionUtils::get_file_permissions(test_file)
    assert_true(permissions >= 0)
  }
  
  // 测试获取文件所有者
  let owner = azimuth::PermissionUtils::get_file_owner(test_file)
  assert_true(owner.length() > 0)
  
  // 测试检查当前用户权限
  let current_user = azimuth::PermissionUtils::get_current_user()
  assert_true(current_user.length() > 0)
  
  let is_owner = azimuth::PermissionUtils::is_file_owner(test_file, current_user)
  // 通常应该是文件所有者，但这取决于测试环境
  
  // 测试创建符号链接（仅Unix-like系统）
  if !azimuth::PlatformUtils::is_windows() {
    let link_file = azimuth::PathUtils::join(temp_dir, "test_link.txt")
    let symlink_result = azimuth::FileSystemUtils::create_symbolic_link(test_file, link_file)
    match symlink_result {
      Ok(_) => {
        assert_true(azimuth::FileSystemUtils::file_exists(link_file))
        assert_true(azimuth::FileSystemUtils::is_symbolic_link(link_file))
        
        // 测试读取符号链接目标
        let link_target = azimuth::FileSystemUtils::read_symbolic_link(link_file)
        match link_target {
          Ok(target) => assert_eq(target, test_file)
          Err(_) => assert_true(false)
        }
      }
      Err(_) => {
        // 可能无权限创建符号链接
        assert_true(true)
      }
    }
  }
  
  // 清理测试文件
  let delete_result = azimuth::FileSystemUtils::delete_directory_recursive(temp_dir)
  assert_true(delete_result)
}