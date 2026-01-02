// Azimuth 跨平台兼容性综合测试用例
// 测试跨平台兼容性功能，包括操作系统、架构和运行时环境适配

// 测试1: 操作系统兼容性
test "操作系统兼容性功能" {
  // 创建平台检测器
  let platform_detector = PlatformDetector::new()
  
  // 检测当前操作系统
  let current_os = PlatformDetector::detect_os(platform_detector)
  assert_true(current_os == OSType::Linux || 
              current_os == OSType::Windows || 
              current_os == OSType::MacOS || 
              current_os == OSType::Other)
  
  // 创建操作系统适配器
  let os_adapter = OSAdapter::new(current_os)
  
  // 测试路径分隔符适配
  let path_separator = OSAdapter::get_path_separator(os_adapter)
  let expected_separator = match current_os {
    OSType::Windows => "\\",
    _ => "/"
  }
  assert_eq(path_separator, expected_separator)
  
  // 测试路径组合
  let path_parts = ["home", "user", "documents", "telemetry.data"]
  let combined_path = OSAdapter::combine_paths(os_adapter, path_parts)
  
  if current_os == OSType::Windows {
    assert_true(combined_path.contains("\\"))
    assert_false(combined_path.contains("/"))
  } else {
    assert_true(combined_path.contains("/"))
    assert_false(combined_path.contains("\\"))
  }
  
  // 测试文件权限处理
  let test_file = OSAdapter::combine_paths(os_adapter, ["tmp", "azimuth_test.txt"])
  let write_result = OSAdapter::create_file_with_permissions(os_adapter, test_file, "test content", 0o644)
  assert_true(write_result)
  
  let read_result = OSAdapter::read_file(os_adapter, test_file)
  assert_eq(read_result, "test content")
  
  // 测试环境变量处理
  let env_var_name = if current_os == OSType::Windows { "AZIMUTH_TEST" } else { "AZIMUTH_TEST" }
  OSAdapter::set_environment_variable(os_adapter, env_var_name, "test_value")
  
  let env_value = OSAdapter::get_environment_variable(os_adapter, env_var_name)
  assert_eq(env_value, "test_value")
  
  // 测试进程管理
  let process_info = OSAdapter::get_process_info(os_adapter)
  assert_true(process_info.pid > 0)
  assert_true(process_info.memory_usage > 0)
  
  // 测试系统资源监控
  let system_info = OSAdapter::get_system_info(os_adapter)
  assert_true(system_info.cpu_count > 0)
  assert_true(system_info.total_memory > 0)
  assert_true(system_info.available_memory > 0)
  assert_true(system_info.available_memory <= system_info.total_memory)
  
  // 清理测试文件
  OSAdapter::delete_file(os_adapter, test_file)
}

// 测试2: 架构兼容性
test "架构兼容性功能" {
  // 创建架构检测器
  let arch_detector = ArchitectureDetector::new()
  
  // 检测当前架构
  let current_arch = ArchitectureDetector::detect_architecture(arch_detector)
  assert_true(current_arch == ArchType::X86_64 || 
              current_arch == ArchType::ARM64 || 
              current_arch == ArchType::X86 || 
              current_arch == ArchType::ARM || 
              current_arch == ArchType::Other)
  
  // 创建架构适配器
  let arch_adapter = ArchAdapter::new(current_arch)
  
  // 测试字节序处理
  let endianness = ArchAdapter::get_endianness(arch_adapter)
  assert_true(endianness == Endianness::Little || endianness == Endianness::Big)
  
  // 测试整数大小
  let int_size = ArchAdapter::get_int_size(arch_adapter)
  let pointer_size = ArchAdapter::get_pointer_size(arch_adapter)
  
  if current_arch == ArchType::X86_64 || current_arch == ArchType::ARM64 {
    assert_eq(pointer_size, 8) // 64位架构指针大小为8字节
  } else {
    assert_eq(pointer_size, 4) // 32位架构指针大小为4字节
  }
  
  // 测试数据对齐
  let alignment_requirements = ArchAdapter::get_alignment_requirements(arch_adapter)
  assert_true(alignment_requirements.int_alignment > 0)
  assert_true(alignment_requirements.double_alignment > 0)
  assert_true(alignment_requirements.pointer_alignment > 0)
  
  // 测试原子操作支持
  let atomic_support = ArchAdapter::get_atomic_support(arch_adapter)
  assert_true(atomic_support.has_32bit_atomic)
  if current_arch == ArchType::X86_64 || current_arch == ArchType::ARM64 {
    assert_true(atomic_support.has_64bit_atomic)
  }
  
  // 测试CPU特性
  let cpu_features = ArchAdapter::get_cpu_features(arch_adapter)
  assert_true(cpu_features.has_basic_instructions)
  
  // 根据架构测试特定特性
  match current_arch {
    ArchType::X86_64 => {
      assert_true(cpu_features.has_sse || cpu_features.has_avx) // x86_64应该有SIMD支持
    }
    ArchType::ARM64 => {
      assert_true(cpu_features.has_neon) // ARM64应该有NEON
    }
    _ => {} // 其他架构不检查特定特性
  }
  
  // 测试性能计数器
  let perf_counters = ArchAdapter::get_performance_counters(arch_adapter)
  if perf_counters.has_cycle_counter {
    let cycle_count = ArchAdapter::read_cycle_counter(arch_adapter)
    assert_true(cycle_count > 0)
  }
}

// 测试3: 运行时环境兼容性
test "运行时环境兼容性功能" {
  // 创建运行时检测器
  let runtime_detector = RuntimeDetector::new()
  
  // 检测当前运行时
  let current_runtime = RuntimeDetector::detect_runtime(runtime_detector)
  assert_true(current_runtime == RuntimeType::Native || 
              current_runtime == RuntimeType::WebAssembly || 
              current_runtime == RuntimeType::JVM || 
              current_runtime == RuntimeType::Other)
  
  // 创建运行时适配器
  let runtime_adapter = RuntimeAdapter::new(current_runtime)
  
  // 测试内存管理
  let memory_info = RuntimeAdapter::get_memory_info(runtime_adapter)
  assert_true(memory_info.heap_size >= 0)
  assert_true(memory_info.heap_used >= 0)
  assert_true(memory_info.heap_used <= memory_info.heap_size)
  
  // 测试垃圾回收
  if current_runtime == RuntimeType::JVM || current_runtime == RuntimeType::Native {
    let gc_info = RuntimeAdapter::get_gc_info(runtime_adapter)
    assert_true(gc_info.collection_count >= 0)
    assert_true(gc_info.collection_time >= 0)
    
    // 触发垃圾回收
    RuntimeAdapter::trigger_gc(runtime_adapter)
    
    let gc_info_after = RuntimeAdapter::get_gc_info(runtime_adapter)
    assert_true(gc_info_after.collection_count >= gc_info.collection_count)
  }
  
  // 测试线程支持
  let thread_info = RuntimeAdapter::get_thread_info(runtime_adapter)
  assert_true(thread_info.thread_count > 0)
  assert_true(thread_info.current_thread_id >= 0)
  
  // 测试并发支持
  let concurrency_support = RuntimeAdapter::get_concurrency_support(runtime_adapter)
  assert_true(concurrency_support.has_threads || concurrency_support.has_async)
  
  // 测试网络支持
  let network_support = RuntimeAdapter::get_network_support(runtime_adapter)
  assert_true(network_support.has_tcp || network_support.has_http)
  
  // 测试文件系统支持
  let fs_support = RuntimeAdapter::get_filesystem_support(runtime_adapter)
  assert_true(fs_support.has_file_read || fs_support.has_file_write)
  
  // 测试时间精度
  let time_precision = RuntimeAdapter::get_time_precision(runtime_adapter)
  assert_true(time_precision.milliseconds_supported)
  if current_runtime != RuntimeType::WebAssembly {
    assert_true(time_precision.microseconds_supported)
  }
  
  // 测试高精度计时
  let start_time = RuntimeAdapter::get_high_precision_time(runtime_adapter)
  Thread::sleep(10) // 等待10毫秒
  let end_time = RuntimeAdapter::get_high_precision_time(runtime_adapter)
  
  assert_true(end_time > start_time)
  let elapsed = end_time - start_time
  assert_true(elapsed >= 10.0) // 至少经过10毫秒
}

// 测试4: WebAssembly兼容性
test "WebAssembly兼容性功能" {
  // 创建WebAssembly检测器
  let wasm_detector = WASMDetector::new()
  
  // 检测是否在WebAssembly环境
  let is_wasm_env = WASMDetector::is_wasm_environment(wasm_detector)
  
  if is_wasm_env {
    // 创建WebAssembly适配器
    let wasm_adapter = WASMAdapter::new()
    
    // 测试内存访问
    let memory_info = WASMAdapter::get_memory_info(wasm_adapter)
    assert_true(memory_info.memory_pages > 0)
    assert_true(memory_info.memory_size > 0)
    
    // 测试JavaScript互操作
    let js_support = WASMAdapter::get_js_interop_support(wasm_adapter)
    if js_support.available {
      // 测试函数调用
      let test_result = WASMAdapter::call_js_function(wasm_adapter, "console.log", ["WASM test"])
      assert_true(test_result)
      
      // 测试对象访问
      let window_obj = WASMAdapter::get_js_object(wasm_adapter, "window")
      assert_true(window_obj.is_valid)
    }
    
    // 测试WebAssembly特性
    let wasm_features = WASMAdapter::get_wasm_features(wasm_adapter)
    assert_true(wasm_features.has_basic_instructions)
    
    // 测试性能限制
    let perf_limits = WASMAdapter::get_performance_limits(wasm_adapter)
    assert_true(perf_limits.max_memory > 0)
    assert_true(perf_limits.max_table_size > 0)
  } else {
    // 非WebAssembly环境的测试
    assert_true(true) // 跳过WASM特定测试
  }
}

// 测试5: 跨平台数据格式兼容性
test "跨平台数据格式兼容性功能" {
  // 创建跨平台序列化器
  let cross_platform_serializer = CrossPlatformSerializer::new()
  
  // 创建测试数据
  let span_data = SpanData::new("cross.platform.test", 1000, 1100, [
    ("platform", "test"),
    ("timestamp", "2023-01-01T00:00:00Z"),
    ("binary.data", "\x00\x01\x02\x03\xFF\xFE\xFD")
  ])
  
  // 序列化为跨平台格式
  let platform_agnostic_data = CrossPlatformSerializer::serialize(cross_platform_serializer, span_data)
  assert_true(platform_agnostic_data.length() > 0)
  
  // 反序列化
  let deserialized_span = CrossPlatformSerializer::deserialize_span(cross_platform_serializer, platform_agnostic_data)
  assert_eq(SpanData::name(deserialized_span), SpanData::name(span_data))
  
  // 测试字节序转换
  let test_int = 0x12345678
  let little_endian_bytes = CrossPlatformSerializer::int_to_little_endian(cross_platform_serializer, test_int)
  let big_endian_bytes = CrossPlatformSerializer::int_to_big_endian(cross_platform_serializer, test_int)
  
  assert_ne(little_endian_bytes, big_endian_bytes)
  
  let converted_int1 = CrossPlatformSerializer::little_endian_to_int(cross_platform_serializer, little_endian_bytes)
  let converted_int2 = CrossPlatformSerializer::big_endian_to_int(cross_platform_serializer, big_endian_bytes)
  
  assert_eq(converted_int1, test_int)
  assert_eq(converted_int2, test_int)
  
  // 测试浮点数格式转换
  let test_float = 3.14159265359
  let float_bytes = CrossPlatformSerializer::float_to_bytes(cross_platform_serializer, test_float)
  let converted_float = CrossPlatformSerializer::bytes_to_float(cross_platform_serializer, float_bytes)
  
  assert_true((converted_float - test_float).abs() < 0.000001)
  
  // 测试字符串编码转换
  let test_string = "测试中文字符串 🚀 Unicode"
  let utf8_bytes = CrossPlatformSerializer::string_to_utf8(cross_platform_serializer, test_string)
  let utf16_bytes = CrossPlatformSerializer::string_to_utf16(cross_platform_serializer, test_string)
  
  let converted_utf8 = CrossPlatformSerializer::utf8_to_string(cross_platform_serializer, utf8_bytes)
  let converted_utf16 = CrossPlatformSerializer::utf16_to_string(cross_platform_serializer, utf16_bytes)
  
  assert_eq(converted_utf8, test_string)
  assert_eq(converted_utf16, test_string)
  
  // 测试时间戳格式转换
  let timestamp = 1672531200 // 2023-01-01 00:00:00 UTC
  let iso_string = CrossPlatformSerializer::timestamp_to_iso_string(cross_platform_serializer, timestamp)
  let converted_timestamp = CrossPlatformSerializer::iso_string_to_timestamp(cross_platform_serializer, iso_string)
  
  assert_eq(converted_timestamp, timestamp)
  assert_true(iso_string.contains("2023-01-01"))
}

// 测试6: 平台特定优化
test "平台特定优化功能" {
  // 创建平台优化器
  let platform_optimizer = PlatformOptimizer::new()
  
  // 检测当前平台
  let current_platform = platform_optimizer.detect_platform()
  
  // 获取平台特定优化配置
  let optimization_config = PlatformOptimizer::get_optimization_config(platform_optimizer, current_platform)
  
  // 验证优化配置
  assert_true(optimization_config.memory_alignment > 0)
  assert_true(optimization_config.cache_line_size > 0)
  assert_true(optimization_config.vector_size > 0)
  
  // 测试内存对齐优化
  let aligned_data = PlatformOptimizer::allocate_aligned_memory(platform_optimizer, 1024, optimization_config.memory_alignment)
  assert_true(aligned_data.is_valid)
  
  let address = PlatformOptimizer::get_memory_address(platform_optimizer, aligned_data)
  assert_eq(address % optimization_config.memory_alignment, 0) // 验证对齐
  
  // 测试SIMD优化（如果支持）
  let simd_support = PlatformOptimizer::get_simd_support(platform_optimizer)
  if simd_support.available {
    let simd_result = PlatformOptimizer::run_simd_operation(platform_optimizer, 
      fn(data, size) { /* SIMD操作 */ return 42 }, 
      aligned_data, 1024)
    assert_eq(simd_result, 42)
  }
  
  // 测试缓存优化
  let cache_config = PlatformOptimizer::get_cache_config(platform_optimizer)
  assert_true(cache_config.l1_size > 0)
  assert_true(cache_config.l2_size > 0)
  assert_true(cache_config.l3_size >= 0) // L3可能不存在
  
  // 测试预取优化
  let test_array = Array::create(1000, 0)
  PlatformOptimizer::prefetch_memory(platform_optimizer, test_array, 0, 1000)
  
  // 测试线程亲和性优化（如果支持）
  let thread_affinity_support = PlatformOptimizer::get_thread_affinity_support(platform_optimizer)
  if thread_affinity_support.available {
    let current_thread = Thread::current()
    let affinity_result = PlatformOptimizer::set_thread_affinity(platform_optimizer, current_thread, 0)
    assert_true(affinity_result)
  }
  
  // 测试CPU频率优化
  let cpu_info = PlatformOptimizer::get_cpu_info(platform_optimizer)
  assert_true(cpu_info.base_frequency > 0)
  
  if cpu_info.has_turbo_boost {
    assert_true(cpu_info.max_frequency >= cpu_info.base_frequency)
  }
  
  // 测试NUMA优化（如果支持）
  let numa_support = PlatformOptimizer::get_numa_support(platform_optimizer)
  if numa_support.available {
    let node_count = PlatformOptimizer::get_numa_node_count(platform_optimizer)
    assert_true(node_count > 0)
    
    let local_memory = PlatformOptimizer::allocate_numa_local_memory(platform_optimizer, 1024, 0)
    assert_true(local_memory.is_valid)
  }
  
  // 清理
  PlatformOptimizer::free_aligned_memory(platform_optimizer, aligned_data)
}