// Azimuth Cross-Platform Compatibility Tests
// This file contains test cases for cross-platform compatibility

// Test 1: Operating System Detection and Adaptation
test "operating system detection and adaptation" {
  // Define OS types
  let create_os_info = fn(name, version, architecture) {
    { name = name, version = version, architecture = architecture }
  }
  
  // Simulate OS detection
  let detect_os = fn() {
    // In real implementation, would detect actual OS
    // For testing, we'll simulate different OS environments
    
    // Simulate Linux environment
    let linux_info = create_os_info("linux", "5.15.0", "x86_64")
    
    // Simulate Windows environment
    let windows_info = create_os_info("windows", "10.0.19042", "x86_64")
    
    // Simulate macOS environment
    let macos_info = create_os_info("macos", "12.0.1", "arm64")
    
    [linux_info, windows_info, macos_info]
  }
  
  // Test OS-specific path handling
  let get_platform_path = fn(base_path, os_info) {
    match os_info.name {
      "windows" => base_path.replace("/", "\\")
      _ => base_path
    }
  }
  
  // Test OS-specific command handling
  let get_platform_command = fn(command, os_info) {
    match os_info.name {
      "windows" => command + ".exe"
      "macos" => command + ".app"
      _ => command
    }
  }
  
  // Test OS detection
  let os_list = detect_os()
  assert_eq(os_list.length(), 3)
  
  let linux_os = os_list[0]
  assert_eq(linux_os.name, "linux")
  assert_eq(linux_os.version, "5.15.0")
  assert_eq(linux_os.architecture, "x86_64")
  
  let windows_os = os_list[1]
  assert_eq(windows_os.name, "windows")
  assert_eq(windows_os.version, "10.0.19042")
  assert_eq(windows_os.architecture, "x86_64")
  
  let macos_os = os_list[2]
  assert_eq(macos_os.name, "macos")
  assert_eq(macos_os.version, "12.0.1")
  assert_eq(macos_os.architecture, "arm64")
  
  // Test path handling
  let base_path = "/opt/azimuth/config"
  
  let linux_path = get_platform_path(base_path, linux_os)
  assert_eq(linux_path, "/opt/azimuth/config")
  
  let windows_path = get_platform_path(base_path, windows_os)
  assert_eq(windows_path, "\\opt\\azimuth\\config")
  
  let macos_path = get_platform_path(base_path, macos_os)
  assert_eq(macos_path, "/opt/azimuth/config")
  
  // Test command handling
  let base_command = "azimuth-agent"
  
  let linux_command = get_platform_command(base_command, linux_os)
  assert_eq(linux_command, "azimuth-agent")
  
  let windows_command = get_platform_command(base_command, windows_os)
  assert_eq(windows_command, "azimuth-agent.exe")
  
  let macos_command = get_platform_command(base_command, macos_os)
  assert_eq(macos_command, "azimuth-agent.app")
}

// Test 2: Architecture-Specific Optimizations
test "architecture-specific optimizations" {
  // Define architecture types
  let create_arch_info = fn(name, is_64bit, endian, supports_simd) {
    { name = name, is_64bit = is_64bit, endian = endian, supports_simd = supports_simd }
  }
  
  // Create different architecture infos
  let x86_64_arch = create_arch_info("x86_64", true, "little", true)
  let arm64_arch = create_arch_info("arm64", true, "little", true)
  let x86_arch = create_arch_info("x86", false, "little", false)
  let arm_arch = create_arch_info("arm", false, "little", false)
  
  // Test architecture-specific optimizations
  let get_optimized_hash_algorithm = fn(arch_info) {
    match arch_info.name {
      "x86_64" => "xxhash64"
      "arm64" => "xxhash64"
      "x86" => "murmur3"
      "arm" => "murmur3"
      _ => "default"
    }
  }
  
  let get_optimized_compression = fn(arch_info) {
    if arch_info.supports_simd {
      "lz4_simd"
    } else {
      "lz4"
    }
  }
  
  let get_optimized_serialization = fn(arch_info) {
    if arch_info.is_64bit {
      "protobuf64"
    } else {
      "protobuf32"
    }
  }
  
  // Test x86_64 optimizations
  assert_eq(get_optimized_hash_algorithm(x86_64_arch), "xxhash64")
  assert_eq(get_optimized_compression(x86_64_arch), "lz4_simd")
  assert_eq(get_optimized_serialization(x86_64_arch), "protobuf64")
  
  // Test ARM64 optimizations
  assert_eq(get_optimized_hash_algorithm(arm64_arch), "xxhash64")
  assert_eq(get_optimized_compression(arm64_arch), "lz4_simd")
  assert_eq(get_optimized_serialization(arm64_arch), "protobuf64")
  
  // Test x86 optimizations
  assert_eq(get_optimized_hash_algorithm(x86_arch), "murmur3")
  assert_eq(get_optimized_compression(x86_arch), "lz4")
  assert_eq(get_optimized_serialization(x86_arch), "protobuf32")
  
  // Test ARM optimizations
  assert_eq(get_optimized_hash_algorithm(arm_arch), "murmur3")
  assert_eq(get_optimized_compression(arm_arch), "lz4")
  assert_eq(get_optimized_serialization(arm_arch), "protobuf32")
  
  // Test endianness handling
  let handle_endianness = fn(data, arch_info) {
    if arch_info.endian == "little" {
      data  // No conversion needed for little endian
    } else {
      // Would convert to little endian in real implementation
      "converted_" + data
    }
  }
  
  let test_data = "0x12345678"
  
  let little_endian_result = handle_endianness(test_data, x86_64_arch)
  assert_eq(little_endian_result, "0x12345678")
  
  let big_endian_arch = create_arch_info("ppc64", true, "big", true)
  let big_endian_result = handle_endianness(test_data, big_endian_arch)
  assert_eq(big_endian_result, "converted_0x12345678")
}

// Test 3: Platform-Specific File System Operations
test "platform-specific file system operations" {
  // Define file system info
  let create_filesystem_info = fn(path_separator, case_sensitive, max_path_length) {
    { path_separator = path_separator, case_sensitive = case_sensitive, max_path_length = max_path_length }
  }
  
  // Create different file system infos
  let linux_fs = create_filesystem_info("/", true, 4096)
  let windows_fs = create_filesystem_info("\\", false, 260)
  let macos_fs = create_filesystem_info("/", true, 1024)
  
  // Test path joining
  let join_path = fn(parts, fs_info) {
    let separator = fs_info.path_separator
    let mut joined = ""
    
    for i in 0..<parts.length() {
      if i > 0 {
        joined = joined + separator
      }
      joined = joined + parts[i]
    }
    
    joined
  }
  
  // Test path normalization
  let normalize_path = fn(path, fs_info) {
    if fs_info.path_separator == "\\" {
      path.replace("/", "\\")
    } else {
      path.replace("\\", "/")
    }
  }
  
  // Test case sensitivity handling
  let handle_case_sensitivity = fn(path1, path2, fs_info) {
    if fs_info.case_sensitive {
      path1 == path2
    } else {
      path1.to_lowercase() == path2.to_lowercase()
    }
  }
  
  // Test path joining
  let linux_parts = ["home", "user", "azimuth"]
  let linux_path = join_path(linux_parts, linux_fs)
  assert_eq(linux_path, "/home/user/azimuth")
  
  let windows_parts = ["C:", "Program Files", "Azimuth"]
  let windows_path = join_path(windows_parts, windows_fs)
  assert_eq(windows_path, "C:\\Program Files\\Azimuth")
  
  // Test path normalization
  let mixed_path = "/home\\user/azimuth\\config"
  
  let normalized_linux = normalize_path(mixed_path, linux_fs)
  assert_eq(normalized_linux, "/home/user/azimuth/config")
  
  let normalized_windows = normalize_path(mixed_path, windows_fs)
  assert_eq(normalized_windows, "\\home\\user\\azimuth\\config")
  
  // Test case sensitivity
  let path1 = "/home/user/Azimuth"
  let path2 = "/home/user/azimuth"
  
  assert_true(handle_case_sensitivity(path1, path2, linux_fs))  // Different case, but case sensitive
  assert_false(handle_case_sensitivity(path1, path2, windows_fs))  // Same case-insensitive
  
  let same_case_path1 = "/home/user/azimuth"
  let same_case_path2 = "/home/user/azimuth"
  
  assert_true(handle_case_sensitivity(same_case_path1, same_case_path2, linux_fs))
  assert_true(handle_case_sensitivity(same_case_path1, same_case_path2, windows_fs))
  
  // Test path length validation
  let validate_path_length = fn(path, fs_info) {
    path.length() <= fs_info.max_path_length
  }
  
  let short_path = "/short/path"
  let long_path = "/very/long/path/that/exceeds/the/maximum/allowed/length/on/this/particular/filesystem/and/should/be/rejected/according/to/the/platform/limitations"
  
  assert_true(validate_path_length(short_path, linux_fs))
  assert_true(validate_path_length(short_path, windows_fs))
  assert_false(validate_path_length(long_path, windows_fs))
  assert_true(validate_path_length(long_path, linux_fs))  // Linux allows longer paths
}

// Test 4: Cross-Platform Network Configuration
test "cross-platform network configuration" {
  // Define network configuration
  let create_network_config = fn(default_port, use_ipv6, buffer_size, keep_alive) {
    { default_port = default_port, use_ipv6 = use_ipv6, buffer_size = buffer_size, keep_alive = keep_alive }
  }
  
  // Create platform-specific network configs
  let linux_network = create_network_config(8080, true, 65536, true)
  let windows_network = create_network_config(8080, false, 8192, true)
  let macos_network = create_network_config(8080, true, 32768, true)
  
  // Test platform-specific socket options
  let get_socket_options = fn(network_config, platform) {
    match platform {
      "linux" => {
        {
          so_reuseaddr = true,
          so_reuseport = true,
          tcp_nodelay = true,
          so_keepalive = network_config.keep_alive,
          so_sndbuf = network_config.buffer_size,
          so_rcvbuf = network_config.buffer_size
        }
      }
      "windows" => {
        {
          so_reuseaddr = true,
          so_reuseport = false,  // Not supported on Windows
          tcp_nodelay = true,
          so_keepalive = network_config.keep_alive,
          so_sndbuf = network_config.buffer_size,
          so_rcvbuf = network_config.buffer_size
        }
      }
      "macos" => {
        {
          so_reuseaddr = true,
          so_reuseport = true,
          tcp_nodelay = true,
          so_keepalive = network_config.keep_alive,
          so_sndbuf = network_config.buffer_size,
          so_rcvbuf = network_config.buffer_size
        }
      }
      _ => {
        {
          so_reuseaddr = true,
          so_reuseport = false,
          tcp_nodelay = true,
          so_keepalive = network_config.keep_alive,
          so_sndbuf = network_config.buffer_size,
          so_rcvbuf = network_config.buffer_size
        }
      }
    }
  }
  
  // Test Linux socket options
  let linux_options = get_socket_options(linux_network, "linux")
  assert_true(linux_options.so_reuseaddr)
  assert_true(linux_options.so_reuseport)
  assert_true(linux_options.tcp_nodelay)
  assert_true(linux_options.so_keepalive)
  assert_eq(linux_options.so_sndbuf, 65536)
  assert_eq(linux_options.so_rcvbuf, 65536)
  
  // Test Windows socket options
  let windows_options = get_socket_options(windows_network, "windows")
  assert_true(windows_options.so_reuseaddr)
  assert_false(windows_options.so_reuseport)  // Not supported on Windows
  assert_true(windows_options.tcp_nodelay)
  assert_true(windows_options.so_keepalive)
  assert_eq(windows_options.so_sndbuf, 8192)
  assert_eq(windows_options.so_rcvbuf, 8192)
  
  // Test macOS socket options
  let macos_options = get_socket_options(macos_network, "macos")
  assert_true(macos_options.so_reuseaddr)
  assert_true(macos_options.so_reuseport)
  assert_true(macos_options.tcp_nodelay)
  assert_true(macos_options.so_keepalive)
  assert_eq(macos_options.so_sndbuf, 32768)
  assert_eq(macos_options.so_rcvbuf, 32768)
  
  // Test address formatting
  let format_address = fn(address, network_config) {
    if network_config.use_ipv6 {
      "[" + address + "]"
    } else {
      address
    }
  }
  
  let ipv4_address = "192.168.1.100"
  let ipv6_address = "2001:db8::1"
  
  let linux_ipv4 = format_address(ipv4_address, linux_network)
  assert_eq(linux_ipv4, "[192.168.1.100]")  // Linux uses IPv6 by default
  
  let windows_ipv4 = format_address(ipv4_address, windows_network)
  assert_eq(windows_ipv4, "192.168.1.100")  // Windows uses IPv4 by default
  
  let linux_ipv6 = format_address(ipv6_address, linux_network)
  assert_eq(linux_ipv6, "[2001:db8::1]")
  
  let windows_ipv6 = format_address(ipv6_address, windows_network)
  assert_eq(windows_ipv6, "[2001:db8::1]")  // IPv6 addresses always use brackets
}

// Test 5: Platform-Specific Memory Management
test "platform-specific memory management" {
  // Define memory management configuration
  let create_memory_config = fn(page_size, allocation_granularity, max_heap_size) {
    { page_size = page_size, allocation_granularity = allocation_granularity, max_heap_size = max_heap_size }
  }
  
  // Create platform-specific memory configs
  let linux_memory = create_memory_config(4096, 4096, 1073741824)  // 4KB pages, 1GB max heap
  let windows_memory = create_memory_config(4096, 65536, 2147483648)  // 4KB pages, 64KB allocation granularity, 2GB max heap
  let macos_memory = create_memory_config(16384, 16384, 1073741824)  // 16KB pages, 1GB max heap
  
  // Test memory alignment
  let align_memory = fn(size, memory_config) {
    let remainder = size % memory_config.allocation_granularity
    if remainder == 0 {
      size
    } else {
      size + (memory_config.allocation_granularity - remainder)
    }
  }
  
  // Test memory pool creation
  let create_memory_pool = fn(pool_size, memory_config) {
    let aligned_size = align_memory(pool_size, memory_config)
    if aligned_size <= memory_config.max_heap_size {
      { size = aligned_size, allocated = true, blocks = [] }
    } else {
      { size = 0, allocated = false, blocks = [] }
    }
  }
  
  // Test memory alignment
  let unaligned_size = 5000
  let linux_aligned = align_memory(unaligned_size, linux_memory)
  assert_eq(linux_aligned, 8192)  // Aligned to 4KB
  
  let windows_aligned = align_memory(unaligned_size, windows_memory)
  assert_eq(windows_aligned, 65536)  // Aligned to 64KB
  
  let macos_aligned = align_memory(unaligned_size, macos_memory)
  assert_eq(macos_aligned, 16384)  // Aligned to 16KB
  
  // Test memory pool creation
  let linux_pool = create_memory_pool(1048576, linux_memory)  // 1MB
  assert_true(linux_pool.allocated)
  assert_eq(linux_pool.size, 1048576)  // Already aligned to 4KB
  
  let windows_pool = create_memory_pool(1048576, windows_memory)  // 1MB
  assert_true(windows_pool.allocated)
  assert_eq(windows_pool.size, 1048576)  // Already aligned to 64KB
  
  let large_pool = create_memory_pool(3221225472, linux_memory)  // 3GB
  assert_false(large_pool.allocated)  // Exceeds max heap size
  assert_eq(large_pool.size, 0)
  
  // Test platform-specific memory allocators
  let get_memory_allocator = fn(platform) {
    match platform {
      "linux" => "mmap"
      "windows" => "VirtualAlloc"
      "macos" => "mmap"
      _ => "malloc"
    }
  }
  
  assert_eq(get_memory_allocator("linux"), "mmap")
  assert_eq(get_memory_allocator("windows"), "VirtualAlloc")
  assert_eq(get_memory_allocator("macos"), "mmap")
  assert_eq(get_memory_allocator("unknown"), "malloc")
  
  // Test memory statistics
  let get_memory_stats = fn(memory_config, allocated_pools) {
    let mut total_allocated = 0
    let mut pool_count = 0
    
    for pool in allocated_pools {
      if pool.allocated {
        total_allocated = total_allocated + pool.size
        pool_count = pool_count + 1
      }
    }
    
    {
      total_allocated = total_allocated,
      pool_count = pool_count,
      max_heap_size = memory_config.max_heap_size,
      utilization = (total_allocated.to_float() / memory_config.max_heap_size.to_float()) * 100.0
    }
  }
  
  let test_pools = [linux_pool, windows_pool]
  let stats = get_memory_stats(linux_memory, test_pools)
  assert_eq(stats.total_allocated, 2097152)  // 1MB + 1MB
  assert_eq(stats.pool_count, 2)
  assert_eq(stats.max_heap_size, 1073741824)  // 1GB
  assert_eq(stats.utilization, 0.1953125)  // ~0.2%
}

// Test 6: Platform-Specific Threading and Concurrency
test "platform-specific threading and concurrency" {
  // Define threading configuration
  let create_threading_config = fn(default_stack_size, max_threads, thread_model) {
    { default_stack_size = default_stack_size, max_threads = max_threads, thread_model = thread_model }
  }
  
  // Create platform-specific threading configs
  let linux_threading = create_threading_config(8388608, 32768, "pthread")
  let windows_threading = create_threading_config(1048576, 2048, "win32")
  let macos_threading = create_threading_config(524288, 1024, "pthread")
  
  // Test thread creation
  let create_thread = fn(thread_id, stack_size, threading_config) {
    if stack_size == 0 {
      { id = thread_id, stack_size = threading_config.default_stack_size, created = true }
    } else if stack_size >= threading_config.default_stack_size {
      { id = thread_id, stack_size = stack_size, created = true }
    } else {
      { id = thread_id, stack_size = 0, created = false }
    }
  }
  
  // Test thread pool creation
  let create_thread_pool = fn(pool_size, threading_config) {
    if pool_size <= threading_config.max_threads {
      let mut threads = []
      for i in 0..<pool_size {
        let thread = create_thread(i, 0, threading_config)
        if thread.created {
          threads = threads.push(thread)
        }
      }
      { threads = threads, created = true }
    } else {
      { threads = [], created = false }
    }
  }
  
  // Test Linux threading
  let linux_thread = create_thread(1, 0, linux_threading)
  assert_true(linux_thread.created)
  assert_eq(linux_thread.stack_size, 8388608)  // 8MB default stack size
  
  let linux_small_thread = create_thread(2, 4096, linux_threading)
  assert_false(linux_small_thread.created)  // Stack size too small
  
  let linux_large_thread = create_thread(3, 16777216, linux_threading)
  assert_true(linux_large_thread.created)
  assert_eq(linux_large_thread.stack_size, 16777216)  // 16MB custom stack size
  
  // Test Windows threading
  let windows_thread = create_thread(1, 0, windows_threading)
  assert_true(windows_thread.created)
  assert_eq(windows_thread.stack_size, 1048576)  // 1MB default stack size
  
  // Test thread pool creation
  let linux_pool = create_thread_pool(10, linux_threading)
  assert_true(linux_pool.created)
  assert_eq(linux_pool.threads.length(), 10)
  
  let windows_pool = create_thread_pool(100, windows_threading)
  assert_true(windows_pool.created)
  assert_eq(windows_pool.threads.length(), 100)
  
  let oversized_pool = create_thread_pool(5000, windows_threading)
  assert_false(oversized_pool.created)
  assert_eq(oversized_pool.threads.length(), 0)
  
  // Test thread synchronization primitives
  let get_sync_primitives = fn(threading_config) {
    match threading_config.thread_model {
      "pthread" => {
        {
          mutex = "pthread_mutex",
          condition = "pthread_cond",
          semaphore = "sem_t",
          rwlock = "pthread_rwlock"
        }
      }
      "win32" => {
        {
          mutex = "CRITICAL_SECTION",
          condition = "CONDITION_VARIABLE",
          semaphore = "HANDLE",
          rwlock = "SRWLOCK"
        }
      }
      _ => {
        {
          mutex = "std_mutex",
          condition = "std_condition",
          semaphore = "std_semaphore",
          rwlock = "shared_mutex"
        }
      }
    }
  }
  
  let linux_sync = get_sync_primitives(linux_threading)
  assert_eq(linux_sync.mutex, "pthread_mutex")
  assert_eq(linux_sync.condition, "pthread_cond")
  assert_eq(linux_sync.semaphore, "sem_t")
  assert_eq(linux_sync.rwlock, "pthread_rwlock")
  
  let windows_sync = get_sync_primitives(windows_threading)
  assert_eq(windows_sync.mutex, "CRITICAL_SECTION")
  assert_eq(windows_sync.condition, "CONDITION_VARIABLE")
  assert_eq(windows_sync.semaphore, "HANDLE")
  assert_eq(windows_sync.rwlock, "SRWLOCK")
}

// Test 7: Platform-Specific Logging and Diagnostics
test "platform-specific logging and diagnostics" {
  // Define logging configuration
  let create_logging_config = fn(log_directory, log_format, max_log_size) {
    { log_directory = log_directory, log_format = log_format, max_log_size = max_log_size }
  }
  
  // Create platform-specific logging configs
  let linux_logging = create_logging_config("/var/log/azimuth", "json", 104857600)  // 100MB
  let windows_logging = create_logging_config("C:\\ProgramData\\Azimuth\\Logs", "xml", 52428800)  // 50MB
  let macos_logging = create_logging_config("/Library/Logs/Azimuth", "json", 104857600)  // 100MB
  
  // Test log file path creation
  let create_log_path = fn(log_name, timestamp, logging_config) {
    let date = timestamp.substring(0, 8)  // YYYYMMDD
    let time = timestamp.substring(8, 14)  // HHMMSS
    log_name + "-" + date + "-" + time + "." + logging_config.log_format
  }
  
  // Test log rotation
  let should_rotate_log = fn(current_size, logging_config) {
    current_size >= logging_config.max_log_size
  }
  
  // Test log file creation
  let log_name = "azimuth-agent"
  let timestamp = "20230101120000"  // 2023-01-01 12:00:00
  
  let linux_log_path = create_log_path(log_name, timestamp, linux_logging)
  assert_eq(linux_log_path, "azimuth-agent-20230101-120000.json")
  
  let windows_log_path = create_log_path(log_name, timestamp, windows_logging)
  assert_eq(windows_log_path, "azimuth-agent-20230101-120000.xml")
  
  // Test log rotation
  assert_true(should_rotate_log(104857600, linux_logging))  // Exactly at max size
  assert_true(should_rotate_log(200000000, linux_logging))  // Exceeds max size
  assert_false(should_rotate_log(50000000, linux_logging))  // Below max size
  
  assert_true(should_rotate_log(52428800, windows_logging))  // Exactly at max size
  assert_false(should_rotate_log(25000000, windows_logging))  // Below max size
  
  // Test platform-specific log levels
  let get_log_levels = fn(platform) {
    match platform {
      "linux" => ["DEBUG", "INFO", "WARN", "ERROR", "FATAL"]
      "windows" => ["VERBOSE", "INFO", "WARNING", "ERROR", "CRITICAL"]
      "macos" => ["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"]
      _ => ["DEBUG", "INFO", "WARN", "ERROR"]
    }
  }
  
  let linux_levels = get_log_levels("linux")
  assert_eq(linux_levels, ["DEBUG", "INFO", "WARN", "ERROR", "FATAL"])
  
  let windows_levels = get_log_levels("windows")
  assert_eq(windows_levels, ["VERBOSE", "INFO", "WARNING", "ERROR", "CRITICAL"])
  
  let macos_levels = get_log_levels("macos")
  assert_eq(macos_levels, ["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"])
  
  // Test system integration
  let get_system_logger = fn(platform) {
    match platform {
      "linux" => "syslog"
      "windows" => "Event Log"
      "macos" => "os_log"
      _ => "console"
    }
  }
  
  assert_eq(get_system_logger("linux"), "syslog")
  assert_eq(get_system_logger("windows"), "Event Log")
  assert_eq(get_system_logger("macos"), "os_log")
  assert_eq(get_system_logger("unknown"), "console")
}

// Test 8: Platform-Specific Security Features
test "platform-specific security features" {
  // Define security configuration
  let create_security_config = fn(has_selinux, has_apparmor, has_windows_defender, has_gatekeeper) {
    { 
      has_selinux = has_selinux, 
      has_apparmor = has_apparmor, 
      has_windows_defender = has_windows_defender, 
      has_gatekeeper = has_gatekeeper 
    }
  }
  
  // Create platform-specific security configs
  let linux_security = create_security_config(true, true, false, false)
  let windows_security = create_security_config(false, false, true, false)
  let macos_security = create_security_config(false, false, false, true)
  
  // Test security feature availability
  let get_security_features = fn(security_config) {
    let mut features = []
    
    if security_config.has_selinux {
      features = features.push("SELinux")
    }
    
    if security_config.has_apparmor {
      features = features.push("AppArmor")
    }
    
    if security_config.has_windows_defender {
      features = features.push("Windows Defender")
    }
    
    if security_config.has_gatekeeper {
      features = features.push("Gatekeeper")
    }
    
    features
  }
  
  // Test Linux security features
  let linux_features = get_security_features(linux_security)
  assert_eq(linux_features.length(), 2)
  assert_true(linux_features.contains("SELinux"))
  assert_true(linux_features.contains("AppArmor"))
  assert_false(linux_features.contains("Windows Defender"))
  assert_false(linux_features.contains("Gatekeeper"))
  
  // Test Windows security features
  let windows_features = get_security_features(windows_security)
  assert_eq(windows_features.length(), 1)
  assert_false(windows_features.contains("SELinux"))
  assert_false(windows_features.contains("AppArmor"))
  assert_true(windows_features.contains("Windows Defender"))
  assert_false(windows_features.contains("Gatekeeper"))
  
  // Test macOS security features
  let macos_features = get_security_features(macos_security)
  assert_eq(macos_features.length(), 1)
  assert_false(macos_features.contains("SELinux"))
  assert_false(macos_features.contains("AppArmor"))
  assert_false(macos_features.contains("Windows Defender"))
  assert_true(macos_features.contains("Gatekeeper"))
  
  // Test platform-specific permission models
  let get_permission_model = fn(platform) {
    match platform {
      "linux" => "POSIX permissions + SELinux/AppArmor"
      "windows" => "ACL + UAC"
      "macos" => "POSIX permissions + Gatekeeper + SIP"
      _ => "Basic permissions"
    }
  }
  
  assert_eq(get_permission_model("linux"), "POSIX permissions + SELinux/AppArmor")
  assert_eq(get_permission_model("windows"), "ACL + UAC")
  assert_eq(get_permission_model("macos"), "POSIX permissions + Gatekeeper + SIP")
  assert_eq(get_permission_model("unknown"), "Basic permissions")
  
  // Test secure temp directory creation
  let get_secure_temp_dir = fn(platform) {
    match platform {
      "linux" => "/tmp/azimuth-XXXXXX"
      "windows" => "C:\\Users\\User\\AppData\\Local\\Temp\\azimuth-XXXXXX"
      "macos" => "/tmp/azimuth-XXXXXX"
      _ => "/tmp/azimuth-XXXXXX"
    }
  }
  
  let linux_temp = get_secure_temp_dir("linux")
  assert_eq(linux_temp, "/tmp/azimuth-XXXXXX")
  
  let windows_temp = get_secure_temp_dir("windows")
  assert_eq(windows_temp, "C:\\Users\\User\\AppData\\Local\\Temp\\azimuth-XXXXXX")
  
  let macos_temp = get_secure_temp_dir("macos")
  assert_eq(macos_temp, "/tmp/azimuth-XXXXXX")
}

// Test 9: Platform-Specific Performance Monitoring
test "platform-specific performance monitoring" {
  // Define performance monitoring configuration
  let create_perf_config = fn(has_perf, has_etw, has_xctrace, sampling_rate) {
    { 
      has_perf = has_perf, 
      has_etw = has_etw, 
      has_xctrace = has_xctrace, 
      sampling_rate = sampling_rate 
    }
  }
  
  // Create platform-specific perf configs
  let linux_perf = create_perf_config(true, false, false, 1000)  // 1000Hz
  let windows_perf = create_perf_config(false, true, false, 100)  // 100Hz
  let macos_perf = create_perf_config(false, false, true, 1000)  // 1000Hz
  
  // Test performance monitoring tools
  let get_perf_tools = fn(perf_config) {
    let mut tools = []
    
    if perf_config.has_perf {
      tools = tools.push("perf")
    }
    
    if perf_config.has_etw {
      tools = tools.push("ETW")
    }
    
    if perf_config.has_xctrace {
      tools = tools.push("XCTrace")
    }
    
    tools
  }
  
  // Test Linux performance monitoring
  let linux_tools = get_perf_tools(linux_perf)
  assert_eq(linux_tools.length(), 1)
  assert_eq(linux_tools[0], "perf")
  assert_eq(linux_perf.sampling_rate, 1000)
  
  // Test Windows performance monitoring
  let windows_tools = get_perf_tools(windows_perf)
  assert_eq(windows_tools.length(), 1)
  assert_eq(windows_tools[0], "ETW")
  assert_eq(windows_perf.sampling_rate, 100)
  
  // Test macOS performance monitoring
  let macos_tools = get_perf_tools(macos_perf)
  assert_eq(macos_tools.length(), 1)
  assert_eq(macos_tools[0], "XCTrace")
  assert_eq(macos_perf.sampling_rate, 1000)
  
  // Test platform-specific metrics
  let get_platform_metrics = fn(platform) {
    match platform {
      "linux" => [
        "cpu_usage", "memory_usage", "disk_io", "network_io", 
        "context_switches", "page_faults", "system_calls"
      ]
      "windows" => [
        "cpu_usage", "memory_usage", "disk_io", "network_io",
        "handle_count", "thread_count", "process_count"
      ]
      "macos" => [
        "cpu_usage", "memory_usage", "disk_io", "network_io",
        "power_consumption", "thermal_state", "gpu_usage"
      ]
      _ => [
        "cpu_usage", "memory_usage", "disk_io", "network_io"
      ]
    }
  }
  
  let linux_metrics = get_platform_metrics("linux")
  assert_eq(linux_metrics.length(), 7)
  assert_true(linux_metrics.contains("cpu_usage"))
  assert_true(linux_metrics.contains("context_switches"))
  assert_true(linux_metrics.contains("system_calls"))
  
  let windows_metrics = get_platform_metrics("windows")
  assert_eq(windows_metrics.length(), 7)
  assert_true(windows_metrics.contains("cpu_usage"))
  assert_true(windows_metrics.contains("handle_count"))
  assert_true(windows_metrics.contains("thread_count"))
  
  let macos_metrics = get_platform_metrics("macos")
  assert_eq(macos_metrics.length(), 7)
  assert_true(macos_metrics.contains("cpu_usage"))
  assert_true(macos_metrics.contains("power_consumption"))
  assert_true(macos_metrics.contains("thermal_state"))
  
  // Test performance counter access
  let get_perf_counter_access = fn(platform) {
    match platform {
      "linux" => "/proc/stat"
      "windows" => "PDH API"
      "macos" => "sysctl"
      _ => "custom"
    }
  }
  
  assert_eq(get_perf_counter_access("linux"), "/proc/stat")
  assert_eq(get_perf_counter_access("windows"), "PDH API")
  assert_eq(get_perf_counter_access("macos"), "sysctl")
  assert_eq(get_perf_counter_access("unknown"), "custom")
}

// Test 10: Cross-Platform Compatibility Validation
test "cross-platform compatibility validation" {
  // Define platform compatibility matrix
  let create_compatibility_matrix = fn() {
    {
      features = {
        "telemetry_collection" = ["linux", "windows", "macos"],
        "metrics_export" = ["linux", "windows", "macos"],
        "distributed_tracing" = ["linux", "windows", "macos"],
        "profiling" = ["linux", "macos"],
        "advanced_networking" = ["linux", "windows"],
        "security_hardening" = ["linux", "windows", "macos"],
        "auto_update" = ["windows", "macos"],
        "systemd_integration" = ["linux"],
        "windows_service" = ["windows"],
        "launchd_integration" = ["macos"]
      }
    }
  }
  
  // Check feature compatibility
  let is_feature_supported = fn(feature, platform, compatibility_matrix) {
    match compatibility_matrix.features.get(feature) {
      None => false
      Some(supported_platforms) => supported_platforms.contains(platform)
    }
  }
  
  // Get supported features for platform
  let get_supported_features = fn(platform, compatibility_matrix) {
    let mut supported = []
    
    for (feature, platforms) in compatibility_matrix.features {
      if platforms.contains(platform) {
        supported = supported.push(feature)
      }
    }
    
    supported
  }
  
  // Test compatibility matrix
  let compat_matrix = create_compatibility_matrix()
  
  // Test feature compatibility
  assert_true(is_feature_supported("telemetry_collection", "linux", compat_matrix))
  assert_true(is_feature_supported("telemetry_collection", "windows", compat_matrix))
  assert_true(is_feature_supported("telemetry_collection", "macos", compat_matrix))
  
  assert_true(is_feature_supported("profiling", "linux", compat_matrix))
  assert_false(is_feature_supported("profiling", "windows", compat_matrix))
  assert_true(is_feature_supported("profiling", "macos", compat_matrix))
  
  assert_true(is_feature_supported("systemd_integration", "linux", compat_matrix))
  assert_false(is_feature_supported("systemd_integration", "windows", compat_matrix))
  assert_false(is_feature_supported("systemd_integration", "macos", compat_matrix))
  
  // Test supported features per platform
  let linux_features = get_supported_features("linux", compat_matrix)
  assert_eq(linux_features.length(), 7)
  assert_true(linux_features.contains("telemetry_collection"))
  assert_true(linux_features.contains("systemd_integration"))
  assert_false(linux_features.contains("windows_service"))
  
  let windows_features = get_supported_features("windows", compat_matrix)
  assert_eq(windows_features.length(), 7)
  assert_true(windows_features.contains("telemetry_collection"))
  assert_true(windows_features.contains("windows_service"))
  assert_false(windows_features.contains("systemd_integration"))
  
  let macos_features = get_supported_features("macos", compat_matrix)
  assert_eq(macos_features.length(), 7)
  assert_true(macos_features.contains("telemetry_collection"))
  assert_true(linux_features.contains("launchd_integration"))
  assert_false(macos_features.contains("windows_service"))
  
  // Test platform-specific configuration validation
  let validate_platform_config = fn(platform, config) {
    let mut errors = []
    
    // Check required features
    let required_features = ["telemetry_collection", "metrics_export"]
    for feature in required_features {
      if !is_feature_supported(feature, platform, compat_matrix) {
        errors = errors.push("Required feature '" + feature + "' not supported on " + platform)
      }
    }
    
    // Check platform-specific settings
    if platform == "windows" && !config.contains("windows_service_name") {
      errors = errors.push("Windows requires 'windows_service_name' configuration")
    }
    
    if platform == "linux" && !config.contains("systemd_service_file") {
      errors = errors.push("Linux requires 'systemd_service_file' configuration")
    }
    
    if platform == "macos" && !config.contains("launchd_plist") {
      errors = errors.push("macOS requires 'launchd_plist' configuration")
    }
    
    errors
  }
  
  // Test valid configurations
  let valid_linux_config = {
    "telemetry_collection" = "enabled",
    "metrics_export" = "enabled",
    "systemd_service_file" = "/etc/systemd/system/azimuth.service"
  }
  
  let linux_errors = validate_platform_config("linux", valid_linux_config)
  assert_eq(linux_errors.length(), 0)
  
  let valid_windows_config = {
    "telemetry_collection" = "enabled",
    "metrics_export" = "enabled",
    "windows_service_name" = "AzimuthAgent"
  }
  
  let windows_errors = validate_platform_config("windows", valid_windows_config)
  assert_eq(windows_errors.length(), 0)
  
  // Test invalid configurations
  let invalid_linux_config = {
    "telemetry_collection" = "enabled",
    "metrics_export" = "enabled"
    // Missing systemd_service_file
  }
  
  let invalid_linux_errors = validate_platform_config("linux", invalid_linux_config)
  assert_eq(invalid_linux_errors.length(), 1)
  assert_true(invalid_linux_errors[0].contains("systemd_service_file"))
  
  let invalid_windows_config = {
    "telemetry_collection" = "enabled",
    "metrics_export" = "enabled"
    // Missing windows_service_name
  }
  
  let invalid_windows_errors = validate_platform_config("windows", invalid_windows_config)
  assert_eq(invalid_windows_errors.length(), 1)
  assert_true(invalid_windows_errors[0].contains("windows_service_name"))
}