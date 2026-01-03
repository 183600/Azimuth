// Azimuth 跨平台兼容性测试用例
// 测试遥测系统在不同平台和环境下的兼容性

test "多操作系统兼容性验证" {
  // 创建平台检测器
  let platform_detector = PlatformDetector::new()
  
  // 测试支持的操作系统
  let supported_platforms = [
    PlatformInfo::new("Windows", "10.0", "x86_64"),
    PlatformInfo::new("Windows", "11.0", "x86_64"),
    PlatformInfo::new("Linux", "5.15", "x86_64"),
    PlatformInfo::new("Linux", "5.15", "arm64"),
    PlatformInfo::new("macOS", "12.0", "x86_64"),
    PlatformInfo::new("macOS", "13.0", "arm64"),
    PlatformInfo::new("FreeBSD", "13.0", "x86_64"),
    PlatformInfo::new("OpenBSD", "7.0", "x86_64")
  ]
  
  // 为每个平台创建遥测提供者并验证兼容性
  for platform in supported_platforms {
    PlatformDetector::set_current_platform(platform_detector, platform)
    
    // 创建平台特定的配置
    let config = TelemetryConfiguration::new()
    TelemetryConfiguration::adapt_to_platform(config, platform)
    
    // 验证配置适配
    assert_true(TelemetryConfiguration::is_platform_adapted(config), 
      "配置应该适配到 " + platform.os_name + " 平台")
    
    // 创建遥测提供者
    let provider_result = TracerProvider::builder()
      .with_config(config)
      .with_platform_detector(platform_detector)
      .build()
    
    assert_true(provider_result.success, 
      "应该能够在 " + platform.os_name + " " + platform.version + " 上创建遥测提供者")
    
    let tracer_provider = provider_result.provider
    let tracer = TracerProvider::get_tracer(tracer_provider, "platform.compatibility.test")
    
    // 测试基本功能
    let span = Tracer::start_span(tracer, "platform.test.span")
    Span::set_attribute(span, "platform.os", platform.os_name)
    Span::set_attribute(span, "platform.version", platform.version)
    Span::set_attribute(span, "platform.arch", platform.architecture)
    
    // 测试平台特定的功能
    if platform.os_name == "Windows" {
      Span::set_attribute(span, "windows.feature", "event.log")
      assert_true(PlatformFeatures::supports_windows_event_log(platform), 
        "Windows平台应该支持事件日志")
    } else if platform.os_name == "Linux" {
      Span::set_attribute(span, "linux.feature", "syslog")
      assert_true(PlatformFeatures::supports_syslog(platform), 
        "Linux平台应该支持syslog")
    } else if platform.os_name == "macOS" {
      Span::set_attribute(span, "macos.feature", "osx.log")
      assert_true(PlatformFeatures::supports_osx_log(platform), 
        "macOS平台应该支持OS X日志")
    }
    
    Span::end(span)
    
    // 验证导出功能
    let exported_spans = TracerProvider::get_exported_spans(tracer_provider)
    assert_true(exported_spans.length() > 0, 
      "在 " + platform.os_name + " 上应该能够导出span")
  }
  
  // 验证跨平台数据一致性
  let cross_platform_validator = CrossPlatformValidator::new()
  let platform_data = PlatformDetector::get_all_platform_data(platform_detector)
  
  let consistency_result = CrossPlatformValidator::validate_data_consistency(
    cross_platform_validator, platform_data)
  
  assert_true(consistency_result.is_consistent, "跨平台数据应该保持一致")
  assert_true(consistency_result.inconsistencies.length() == 0, 
    "不应该有跨平台数据不一致")
  
  assert_true(true)
}

test "不同CPU架构兼容性测试" {
  // 测试不同CPU架构的兼容性
  let architectures = [
    ArchitectureInfo::new("x86_64", "little", 64),
    ArchitectureInfo::new("arm64", "little", 64),
    ArchitectureInfo::new("arm32", "little", 32),
    ArchitectureInfo::new("riscv64", "little", 64),
    ArchitectureInfo::new("ppc64", "big", 64),
    ArchitectureInfo::new("s390x", "big", 64)
  ]
  
  for arch in architectures {
    // 创建架构特定的测试环境
    let arch_test_env = ArchitectureTestEnvironment::new(arch)
    
    // 测试数据类型大小一致性
    let type_sizes = ArchitectureTestEnvironment::get_type_sizes(arch_test_env)
    assert_eq(type_sizes.int_size, 4, "int类型应该在所有架构上都是4字节")
    assert_eq(type_sizes.long_size, if arch.bits == 64 { 8 } else { 4 }, 
      "long类型大小应该与架构位数匹配")
    assert_eq(type_sizes.pointer_size, arch.bits / 8, 
      "指针大小应该与架构位数匹配")
    
    // 测试字节序处理
    let test_value = 0x12345678
    let little_endian_bytes = ByteOrder::to_little_endian(test_value)
    let big_endian_bytes = ByteOrder::to_big_endian(test_value)
    
    if arch.endianness == "little" {
      assert_eq(ByteOrder::from_little_endian(little_endian_bytes), test_value, 
        "小端架构应该正确处理小端字节序")
    } else {
      assert_eq(ByteOrder::from_big_endian(big_endian_bytes), test_value, 
        "大端架构应该正确处理大端字节序")
    }
    
    // 测试序列化兼容性
    let serializer = CrossArchSerializer::new(arch)
    
    let test_data = TestData::new(
      42, 
      3.14159, 
      "test string", 
      [1, 2, 3, 4, 5]
    )
    
    let serialized = CrossArchSerializer::serialize(serializer, test_data)
    assert_true(serialized.success, "应该能够在 " + arch.name + " 架构上序列化数据")
    
    // 测试反序列化兼容性
    let deserialized = CrossArchSerializer::deserialize(serializer, serialized.data)
    assert_true(deserialized.success, "应该能够在 " + arch.name + " 架构上反序列化数据")
    
    // 验证数据完整性
    let original_data = deserialized.data
    assert_eq(original_data.int_field, test_data.int_field, "整数字段应该在跨架构后保持一致")
    assert_true((original_data.float_field - test_data.float_field).abs() < 0.0001, 
      "浮点字段应该在跨架构后保持一致")
    assert_eq(original_data.string_field, test_data.string_field, "字符串字段应该在跨架构后保持一致")
    
    // 测试遥测数据兼容性
    let tracer_provider = TracerProvider::builder()
      .with_architecture(arch)
      .build()
    
    let tracer = TracerProvider::get_tracer(tracer_provider, "arch.compatibility.test")
    
    let span = Tracer::start_span(tracer, "arch.test.span")
    Span::set_attribute(span, "architecture", arch.name)
    Span::set_attribute(span, "endianness", arch.endianness)
    Span::set_attribute(span, "bits", arch.bits.to_string())
    
    // 添加架构特定的度量
    let meter_provider = MeterProvider::builder()
      .with_architecture(arch)
      .build()
    
    let meter = MeterProvider::get_meter(meter_provider, "arch.test.meter")
    let counter = Meter::create_counter(meter, "arch.test.counter")
    
    Counter::add_with_attributes(counter, 1.0, [
      ("architecture", arch.name),
      ("endianness", arch.endianness),
      ("bits", arch.bits.to_string())
    ])
    
    Span::end(span)
    
    // 验证导出的数据
    let exported_spans = TracerProvider::get_exported_spans(tracer_provider)
    assert_true(exported_spans.length() > 0, 
      "应该能够在 " + arch.name + " 架构上导出遥测数据")
  }
  
  // 验证跨架构数据一致性
  let arch_validator = CrossArchitectureValidator::new()
  let consistency_report = CrossArchitectureValidator::validate_consistency(arch_validator)
  
  assert_true(consistency_report.overall_consistent, "跨架构数据应该保持一致")
  assert_true(consistency_report.inconsistencies.length() == 0, 
    "不应该有跨架构数据不一致")
  
  assert_true(true)
}

test "WebAssembly平台兼容性" {
  // 测试WebAssembly平台兼容性
  let wasm_platform = PlatformInfo::new("WebAssembly", "1.0", "wasm32")
  let wasm_detector = WasmPlatformDetector::new()
  
  // 验证WebAssembly环境检测
  assert_true(WasmPlatformDetector::is_wasm_environment(wasm_detector), 
    "应该正确检测WebAssembly环境")
  
  // 创建WebAssembly特定的配置
  let wasm_config = TelemetryConfiguration::new()
  TelemetryConfiguration::adapt_to_wasm(wasm_config)
  
  // 验证WebAssembly配置适配
  assert_true(TelemetryConfiguration::is_wasm_adapted(wasm_config), 
    "配置应该适配到WebAssembly平台")
  
  // 测试WebAssembly特定的限制
  assert_true(TelemetryConfiguration::get_max_memory_usage(wasm_config) <= 512 * 1024 * 1024, 
    "WebAssembly平台应该有内存使用限制")
  assert_true(TelemetryConfiguration::get_batch_size(wasm_config) <= 100, 
    "WebAssembly平台应该有批处理大小限制")
  
  // 创建WebAssembly遥测提供者
  let wasm_provider_result = TracerProvider::builder()
    .with_config(wasm_config)
    .with_wasm_detector(wasm_detector)
    .build()
  
  assert_true(wasm_provider_result.success, 
    "应该能够在WebAssembly平台上创建遥测提供者")
  
  let wasm_tracer_provider = wasm_provider_result.provider
  let wasm_tracer = TracerProvider::get_tracer(wasm_tracer_provider, "wasm.compatibility.test")
  
  // 测试基本遥测功能
  let wasm_span = Tracer::start_span(wasm_tracer, "wasm.test.span")
  Span::set_attribute(wasm_span, "platform", "WebAssembly")
  Span::set_attribute(wasm_span, "browser", BrowserDetector::get_browser_name())
  Span::set_attribute(wasm_span, "javascript.version", JSRuntime::get_version())
  
  // 测试WebAssembly特定的功能
  if WasmPlatformDetector::supports_browser_api(wasm_detector) {
    Span::set_attribute(wasm_span, "browser.api", "supported")
    
    // 测试浏览器API集成
    let browser_integration = WasmBrowserIntegration::new()
    let page_load_time = WasmBrowserIntegration::get_page_load_time(browser_integration)
    Span::set_attribute(wasm_span, "page.load.time", page_load_time.to_string())
    
    let memory_usage = WasmBrowserIntegration::get_memory_usage(browser_integration)
    Span::set_attribute(wasm_span, "memory.usage", memory_usage.to_string())
  }
  
  // 测试JavaScript互操作
  let js_interop = WasmJSInterop::new()
  let js_result = WasmJSInterop::call_function(js_interop, "console.log", ["WASM telemetry test"])
  assert_true(js_result.success, "应该能够调用JavaScript函数")
  
  // 测试WebAssembly存储
  let wasm_storage = WasmStorage::new()
  let storage_result = WasmStorage::store_telemetry_data(wasm_storage, "test.key", "test.value")
  assert_true(storage_result.success, "应该能够在WebAssembly存储中保存数据")
  
  let retrieved_data = WasmStorage::retrieve_telemetry_data(wasm_storage, "test.key")
  assert_eq(retrieved_data, "test.value", "应该能够从WebAssembly存储中检索数据")
  
  Span::end(wasm_span)
  
  // 测试WebAssembly性能特征
  let wasm_performance = WasmPerformanceProfiler::new()
  WasmPerformanceProfiler::start_profiling(wasm_performance)
  
  // 执行一些遥测操作
  for i = 0; i < 100; i = i + 1 {
    let perf_span = Tracer::start_span(wasm_tracer, "performance.span." + i.to_string())
    Span::set_attribute(perf_span, "iteration", i.to_string())
    Span::end(perf_span)
  }
  
  WasmPerformanceProfiler::stop_profiling(wasm_performance)
  let perf_report = WasmPerformanceProfiler::get_report(wasm_performance)
  
  // 验证性能指标
  assert_true(perf_report.average_span_creation_time < 10.0, 
    "WebAssembly上的span创建时间应该合理")
  assert_true(perf_report.memory_efficiency_score > 0.7, 
    "WebAssembly应该有良好的内存效率")
  
  // 验证导出的数据
  let wasm_exported_spans = TracerProvider::get_exported_spans(wasm_tracer_provider)
  assert_true(wasm_exported_spans.length() > 0, 
    "应该能够在WebAssembly平台上导出span")
  
  // 测试WebAssembly与其他平台的数据兼容性
  let compatibility_validator = WasmCompatibilityValidator::new()
  let compatibility_result = WasmCompatibilityValidator::validate_cross_platform_data(
    compatibility_validator, wasm_exported_spans)
  
  assert_true(compatibility_result.is_compatible, 
    "WebAssembly数据应该与其他平台兼容")
  
  assert_true(true)
}

test "移动平台兼容性测试" {
  // 测试移动平台兼容性
  let mobile_platforms = [
    MobilePlatformInfo::new("Android", "13", "arm64", "Java"),
    MobilePlatformInfo::new("Android", "12", "arm64", "Kotlin"),
    MobilePlatformInfo::new("iOS", "16.0", "arm64", "Swift"),
    MobilePlatformInfo::new("iOS", "15.0", "arm64", "Objective-C")
  ]
  
  for mobile_platform in mobile_platforms {
    // 创建移动平台特定的配置
    let mobile_config = TelemetryConfiguration::new()
    TelemetryConfiguration::adapt_to_mobile(mobile_config, mobile_platform)
    
    // 验证移动平台配置适配
    assert_true(TelemetryConfiguration::is_mobile_adapted(mobile_config), 
      "配置应该适配到 " + mobile_platform.os_name + " 平台")
    
    // 测试移动平台特定的限制
    assert_true(TelemetryConfiguration::get_battery_optimization_enabled(mobile_config), 
      "移动平台应该启用电池优化")
    assert_true(TelemetryConfiguration::get_network_adaptation_enabled(mobile_config), 
      "移动平台应该启用网络自适应")
    
    // 创建移动平台遥测提供者
    let mobile_provider_result = TracerProvider::builder()
      .with_config(mobile_config)
      .with_mobile_platform(mobile_platform)
      .build()
    
    assert_true(mobile_provider_result.success, 
      "应该能够在 " + mobile_platform.os_name + " 上创建遥测提供者")
    
    let mobile_tracer_provider = mobile_provider_result.provider
    let mobile_tracer = TracerProvider::get_tracer(mobile_tracer_provider, "mobile.compatibility.test")
    
    // 测试移动平台特定的功能
    let mobile_span = Tracer::start_span(mobile_tracer, "mobile.test.span")
    Span::set_attribute(mobile_span, "platform", mobile_platform.os_name)
    Span::set_attribute(mobile_span, "version", mobile_platform.version)
    Span::set_attribute(mobile_span, "architecture", mobile_platform.architecture)
    Span::set_attribute(mobile_span, "runtime", mobile_platform.runtime)
    
    // 测试移动设备信息收集
    if mobile_platform.os_name == "Android" {
      let android_info = AndroidDeviceInfo::collect()
      Span::set_attribute(mobile_span, "device.model", android_info.model)
      Span::set_attribute(mobile_span, "device.manufacturer", android_info.manufacturer)
      Span::set_attribute(mobile_span, "os.version", android_info.os_version)
      Span::set_attribute(mobile_span, "app.version", android_info.app_version)
      
      // 测试Android特定的遥测功能
      let android_telemetry = AndroidTelemetryIntegration::new()
      let battery_level = AndroidTelemetryIntegration::get_battery_level(android_telemetry)
      Span::set_attribute(mobile_span, "battery.level", battery_level.to_string())
      
      let network_type = AndroidTelemetryIntegration::get_network_type(android_telemetry)
      Span::set_attribute(mobile_span, "network.type", network_type)
      
    } else if mobile_platform.os_name == "iOS" {
      let ios_info = IosDeviceInfo::collect()
      Span::set_attribute(mobile_span, "device.model", ios_info.model)
      Span::set_attribute(mobile_span, "device.name", ios_info.name)
      Span::set_attribute(mobile_span, "os.version", ios_info.os_version)
      Span::set_attribute(mobile_span, "app.version", ios_info.app_version)
      
      // 测试iOS特定的遥测功能
      let ios_telemetry = IosTelemetryIntegration::new()
      let battery_level = IosTelemetryIntegration::get_battery_level(ios_telemetry)
      Span::set_attribute(mobile_span, "battery.level", battery_level.to_string())
      
      let network_type = IosTelemetryIntegration::get_network_type(ios_telemetry)
      Span::set_attribute(mobile_span, "network.type", network_type)
    }
    
    // 测试移动网络适应性
    let mobile_network = MobileNetworkAdaptation::new()
    MobileNetworkAdaptation::adapt_to_network_conditions(mobile_network)
    
    let network_quality = MobileNetworkAdaptation::get_network_quality(mobile_network)
    Span::set_attribute(mobile_span, "network.quality", network_quality.to_string())
    
    let adaptation_strategy = MobileNetworkAdaptation::get_adaptation_strategy(mobile_network)
    Span::set_attribute(mobile_span, "adaptation.strategy", adaptation_strategy)
    
    // 测试移动电池优化
    let mobile_battery = MobileBatteryOptimization::new()
    MobileBatteryOptimization::optimize_for_battery(mobile_battery)
    
    let battery_optimization_level = MobileBatteryOptimization::get_optimization_level(mobile_battery)
    Span::set_attribute(mobile_span, "battery.optimization.level", battery_optimization_level.to_string())
    
    // 测试移动存储限制
    let mobile_storage = MobileStorageManager::new()
    let storage_usage = MobileStorageManager::get_storage_usage(mobile_storage)
    Span::set_attribute(mobile_span, "storage.usage", storage_usage.to_string())
    
    // 在存储受限时测试数据压缩
    if storage_usage > 80.0 {
      MobileStorageManager::enable_compression(mobile_storage, true)
      assert_true(MobileStorageManager::is_compression_enabled(mobile_storage), 
        "存储使用率高时应该启用压缩")
    }
    
    Span::end(mobile_span)
    
    // 验证导出的数据
    let mobile_exported_spans = TracerProvider::get_exported_spans(mobile_tracer_provider)
    assert_true(mobile_exported_spans.length() > 0, 
      "应该能够在 " + mobile_platform.os_name + " 上导出span")
    
    // 验证移动平台特定属性存在
    let exported_span = mobile_exported_spans[0]
    assert_true(Span::has_attribute(exported_span, "platform"), 
      "导出的span应该包含平台信息")
    assert_true(Span::has_attribute(exported_span, "network.quality"), 
      "导出的span应该包含网络质量信息")
  }
  
  // 测试跨移动平台数据一致性
  let mobile_validator = CrossMobilePlatformValidator::new()
  let mobile_consistency_report = CrossMobilePlatformValidator::validate_consistency(mobile_validator)
  
  assert_true(mobile_consistency_report.overall_consistent, "跨移动平台数据应该保持一致")
  assert_true(mobile_consistency_report.inconsistencies.length() == 0, 
    "不应该有跨移动平台数据不一致")
  
  assert_true(true)
}

test "容器和虚拟化环境兼容性" {
  // 测试容器和虚拟化环境兼容性
  let virtualization_environments = [
    VirtualizationInfo::new("Docker", "20.10", "container"),
    VirtualizationInfo::new("Kubernetes", "1.24", "orchestration"),
    VirtualizationInfo::new("VMware", "7.0", "virtual-machine"),
    VirtualizationInfo::new("Hyper-V", "10.0", "virtual-machine"),
    VirtualizationInfo::new("KVM", "5.15", "virtual-machine")
  ]
  
  for virt_env in virtualization_environments {
    // 创建虚拟化环境检测器
    let virt_detector = VirtualizationDetector::new()
    VirtualizationDetector::set_environment(virt_detector, virt_env)
    
    // 验证虚拟化环境检测
    assert_true(VirtualizationDetector::is_virtualized(virt_detector), 
      "应该正确检测虚拟化环境")
    assert_eq(VirtualizationDetector::get_virtualization_type(virt_detector), virt_env.type, 
      "应该正确识别虚拟化类型")
    
    // 创建虚拟化环境特定的配置
    let virt_config = TelemetryConfiguration::new()
    TelemetryConfiguration::adapt_to_virtualization(virt_config, virt_env)
    
    // 验证虚拟化配置适配
    assert_true(TelemetryConfiguration::is_virtualization_adapted(virt_config), 
      "配置应该适配到 " + virt_env.name + " 环境")
    
    // 测试虚拟化环境特定的功能
    if virt_env.type == "container" {
      assert_true(TelemetryConfiguration::get_container_mode_enabled(virt_config), 
        "容器环境应该启用容器模式")
      
      // 测试容器特定的资源限制
      let container_resources = ContainerResourceMonitor::new()
      let cpu_limit = ContainerResourceMonitor::get_cpu_limit(container_resources)
      let memory_limit = ContainerResourceMonitor::get_memory_limit(container_resources)
      
      assert_true(cpu_limit > 0, "容器应该有CPU限制")
      assert_true(memory_limit > 0, "容器应该有内存限制")
      
    } else if virt_env.type == "orchestration" {
      assert_true(TelemetryConfiguration::get_kubernetes_enabled(virt_config), 
        "Kubernetes环境应该启用Kubernetes支持")
      
      // 测试Kubernetes特定的功能
      let k8s_integration = KubernetesIntegration::new()
      let pod_name = KubernetesIntegration::get_pod_name(k8s_integration)
      let namespace = KubernetesIntegration::get_namespace(k8s_integration)
      let node_name = KubernetesIntegration::get_node_name(k8s_integration)
      
      assert_true(pod_name.length() > 0, "应该能够获取Pod名称")
      assert_true(namespace.length() > 0, "应该能够获取命名空间")
      assert_true(node_name.length() > 0, "应该能够获取节点名称")
      
    } else if virt_env.type == "virtual-machine" {
      assert_true(TelemetryConfiguration::get_vm_optimization_enabled(virt_config), 
        "虚拟机环境应该启用VM优化")
      
      // 测试虚拟机特定的功能
      let vm_info = VMInfoCollector::new()
      let vm_type = VMInfoCollector::get_vm_type(vm_info)
      let hypervisor = VMInfoCollector::get_hypervisor(vm_info)
      
      assert_true(vm_type.length() > 0, "应该能够获取虚拟机类型")
      assert_true(hypervisor.length() > 0, "应该能够获取虚拟机监控程序信息")
    }
    
    // 创建虚拟化环境遥测提供者
    let virt_provider_result = TracerProvider::builder()
      .with_config(virt_config)
      .with_virtualization_detector(virt_detector)
      .build()
    
    assert_true(virt_provider_result.success, 
      "应该能够在 " + virt_env.name + " 环境中创建遥测提供者")
    
    let virt_tracer_provider = virt_provider_result.provider
    let virt_tracer = TracerProvider::get_tracer(virt_tracer_provider, "virtualization.compatibility.test")
    
    // 测试虚拟化环境特定的遥测功能
    let virt_span = Tracer::start_span(virt_tracer, "virtualization.test.span")
    Span::set_attribute(virt_span, "virtualization.type", virt_env.type)
    Span::set_attribute(virt_span, "virtualization.name", virt_env.name)
    Span::set_attribute(virt_span, "virtualization.version", virt_env.version)
    
    // 添加虚拟化环境特定的属性
    if virt_env.type == "container" {
      let container_info = ContainerInfoCollector::collect()
      Span::set_attribute(virt_span, "container.id", container_info.id)
      Span::set_attribute(virt_span, "container.name", container_info.name)
      Span::set_attribute(virt_span, "container.image", container_info.image)
      
    } else if virt_env.type == "orchestration" {
      let k8s_info = KubernetesInfoCollector::collect()
      Span::set_attribute(virt_span, "pod.name", k8s_info.pod_name)
      Span::set_attribute(virt_span, "pod.namespace", k8s_info.namespace)
      Span::set_attribute(virt_span, "pod.uid", k8s_info.pod_uid)
      Span::set_attribute(virt_span, "service.account", k8s_info.service_account)
      
    } else if virt_env.type == "virtual-machine" {
      let vm_info = VMInfoCollector::collect()
      Span::set_attribute(virt_span, "vm.id", vm_info.id)
      Span::set_attribute(virt_span, "vm.name", vm_info.name)
      Span::set_attribute(virt_span, "hypervisor", vm_info.hypervisor)
    }
    
    // 测试虚拟化环境中的资源监控
    let resource_monitor = VirtualizationResourceMonitor::new()
    let cpu_usage = VirtualizationResourceMonitor::get_cpu_usage(resource_monitor)
    let memory_usage = VirtualizationResourceMonitor::get_memory_usage(resource_monitor)
    let disk_usage = VirtualizationResourceMonitor::get_disk_usage(resource_monitor)
    let network_usage = VirtualizationResourceMonitor::get_network_usage(resource_monitor)
    
    Span::set_attribute(virt_span, "resource.cpu.usage", cpu_usage.to_string())
    Span::set_attribute(virt_span, "resource.memory.usage", memory_usage.to_string())
    Span::set_attribute(virt_span, "resource.disk.usage", disk_usage.to_string())
    Span::set_attribute(virt_span, "resource.network.usage", network_usage.to_string())
    
    // 测试虚拟化环境中的网络拓扑发现
    if virt_env.type == "orchestration" {
      let topology_discoverer = KubernetesTopologyDiscoverer::new()
      let service_topology = KubernetesTopologyDiscoverer::discover_service_topology(topology_discoverer)
      
      Span::set_attribute(virt_span, "topology.services.count", service_topology.services.length().to_string())
      Span::set_attribute(virt_span, "topology.pods.count", service_topology.pods.length().to_string())
      Span::set_attribute(virt_span, "topology.namespaces.count", service_topology.namespaces.length().to_string())
    }
    
    Span::end(virt_span)
    
    // 验证导出的数据
    let virt_exported_spans = TracerProvider::get_exported_spans(virt_tracer_provider)
    assert_true(virt_exported_spans.length() > 0, 
      "应该能够在 " + virt_env.name + " 环境中导出span")
    
    // 验证虚拟化环境特定属性存在
    let exported_span = virt_exported_spans[0]
    assert_true(Span::has_attribute(exported_span, "virtualization.type"), 
      "导出的span应该包含虚拟化类型信息")
    assert_true(Span::has_attribute(exported_span, "resource.cpu.usage"), 
      "导出的span应该包含资源使用信息")
  }
  
  // 测试跨虚拟化环境数据一致性
  let virt_validator = CrossVirtualizationValidator::new()
  let virt_consistency_report = CrossVirtualizationValidator::validate_consistency(virt_validator)
  
  assert_true(virt_consistency_report.overall_consistent, "跨虚拟化环境数据应该保持一致")
  assert_true(virt_consistency_report.inconsistencies.length() == 0, 
    "不应该有跨虚拟化环境数据不一致")
  
  assert_true(true)
}

test "边缘计算设备兼容性" {
  // 测试边缘计算设备兼容性
  let edge_devices = [
    EdgeDeviceInfo::new("Raspberry Pi", "4B", "arm64", "Linux", "1GB"),
    EdgeDeviceInfo::new("NVIDIA Jetson", "Nano", "arm64", "Linux", "4GB"),
    EdgeDeviceInfo::new("Intel NUC", "i7", "x86_64", "Linux", "16GB"),
    EdgeDeviceInfo::new("AWS Snowball", "Edge", "x86_64", "Linux", "32GB"),
    EdgeDeviceInfo::new("Custom IoT", "ESP32", "xtensa", "FreeRTOS", "512KB")
  ]
  
  for edge_device in edge_devices {
    // 创建边缘设备特定的配置
    let edge_config = TelemetryConfiguration::new()
    TelemetryConfiguration::adapt_to_edge_device(edge_config, edge_device)
    
    // 验证边缘设备配置适配
    assert_true(TelemetryConfiguration::is_edge_adapted(edge_config), 
      "配置应该适配到 " + edge_device.model + " 设备")
    
    // 测试边缘设备特定的限制
    assert_true(TelemetryConfiguration::get_offline_mode_enabled(edge_config), 
      "边缘设备应该启用离线模式")
    assert_true(TelemetryConfiguration::get_local_storage_enabled(edge_config), 
      "边缘设备应该启用本地存储")
    
    // 根据设备资源调整配置
    let memory_mb = EdgeDeviceInfo::parse_memory(edge_device.memory)
    if memory_mb < 1024 { // 小于1GB
      assert_true(TelemetryConfiguration::get_batch_size(edge_config) <= 10, 
        "低内存设备应该使用小的批处理大小")
      assert_true(TelemetryConfiguration::get_max_buffer_size(edge_config) <= 1000, 
        "低内存设备应该使用小的缓冲区大小")
    }
    
    // 创建边缘设备遥测提供者
    let edge_provider_result = TracerProvider::builder()
      .with_config(edge_config)
      .with_edge_device(edge_device)
      .build()
    
    assert_true(edge_provider_result.success, 
      "应该能够在 " + edge_device.model + " 设备上创建遥测提供者")
    
    let edge_tracer_provider = edge_provider_result.provider
    let edge_tracer = TracerProvider::get_tracer(edge_tracer_provider, "edge.compatibility.test")
    
    // 测试边缘设备特定的功能
    let edge_span = Tracer::start_span(edge_tracer, "edge.test.span")
    Span::set_attribute(edge_span, "device.model", edge_device.model)
    Span::set_attribute(edge_span, "device.architecture", edge_device.architecture)
    Span::set_attribute(edge_span, "device.os", edge_device.os)
    Span::set_attribute(edge_span, "device.memory", edge_device.memory)
    
    // 测试边缘设备传感器集成
    let sensor_manager = EdgeSensorManager::new()
    let available_sensors = EdgeSensorManager::get_available_sensors(sensor_manager)
    
    for sensor in available_sensors {
      match sensor.type {
        "temperature" => {
          let temperature = EdgeSensorManager::read_temperature(sensor_manager, sensor.id)
          Span::set_attribute(edge_span, "sensor.temperature", temperature.to_string())
        }
        "humidity" => {
          let humidity = EdgeSensorManager::read_humidity(sensor_manager, sensor.id)
          Span::set_attribute(edge_span, "sensor.humidity", humidity.to_string())
        }
        "pressure" => {
          let pressure = EdgeSensorManager::read_pressure(sensor_manager, sensor.id)
          Span::set_attribute(edge_span, "sensor.pressure", pressure.to_string())
        }
        "light" => {
          let light = EdgeSensorManager::read_light(sensor_manager, sensor.id)
          Span::set_attribute(edge_span, "sensor.light", light.to_string())
        }
        _ => {}
      }
    }
    
    // 测试边缘网络连接性
    let edge_network = EdgeNetworkManager::new()
    let connection_status = EdgeNetworkManager::get_connection_status(edge_network)
    Span::set_attribute(edge_span, "network.status", connection_status.status)
    Span::set_attribute(edge_span, "network.type", connection_status.type)
    Span::set_attribute(edge_span, "network.strength", connection_status.signal_strength.to_string())
    
    // 测试边缘设备电源管理
    let power_manager = EdgePowerManager::new()
    let battery_level = EdgePowerManager::get_battery_level(power_manager)
    let power_source = EdgePowerManager::get_power_source(power_manager)
    
    Span::set_attribute(edge_span, "power.battery.level", battery_level.to_string())
    Span::set_attribute(edge_span, "power.source", power_source)
    
    // 测试边缘存储管理
    let storage_manager = EdgeStorageManager::new()
    let storage_usage = EdgeStorageManager::get_storage_usage(storage_manager)
    Span::set_attribute(edge_span, "storage.usage", storage_usage.to_string())
    
    // 在存储空间不足时测试数据清理
    if storage_usage > 90.0 {
      EdgeStorageManager::cleanup_old_data(storage_manager)
      let new_storage_usage = EdgeStorageManager::get_storage_usage(storage_manager)
      assert_true(new_storage_usage < storage_usage, "存储清理应该减少使用率")
    }
    
    // 测试边缘计算特定功能
    let edge_computing = EdgeComputingManager::new()
    
    // 测试本地数据处理
    let local_data = "edge sensor data"
    let processed_data = EdgeComputingManager::process_locally(edge_computing, local_data)
    assert_true(processed_data.length() > 0, "应该能够在边缘设备上本地处理数据")
    
    Span::set_attribute(edge_span, "local.processing.enabled", "true")
    Span::set_attribute(edge_span, "local.processing.data.size", processed_data.length().to_string())
    
    // 测试离线数据缓存
    if connection_status.status == "offline" {
      let offline_cache = EdgeOfflineCache::new()
      EdgeOfflineCache::store_data(offline_cache, processed_data)
      
      let cached_data = EdgeOfflineCache::retrieve_data(offline_cache)
      assert_eq(cached_data, processed_data, "离线缓存应该正确存储和检索数据")
      
      Span::set_attribute(edge_span, "offline.cache.enabled", "true")
      Span::set_attribute(edge_span, "offline.cache.size", cached_data.length().to_string())
    }
    
    // 测试边缘设备同步策略
    let sync_manager = EdgeSyncManager::new()
    let sync_strategy = EdgeSyncManager::get_sync_strategy(sync_manager)
    Span::set_attribute(edge_span, "sync.strategy", sync_strategy)
    
    // 根据网络状况调整同步策略
    if connection_status.status == "online" && connection_status.signal_strength > 0.7 {
      EdgeSyncManager::set_sync_strategy(sync_manager, "immediate")
    } else if connection_status.status == "online" {
      EdgeSyncManager::set_sync_strategy(sync_manager, "batch")
    } else {
      EdgeSyncManager::set_sync_strategy(sync_manager, "offline")
    }
    
    Span::end(edge_span)
    
    // 验证导出的数据
    let edge_exported_spans = TracerProvider::get_exported_spans(edge_tracer_provider)
    assert_true(edge_exported_spans.length() > 0, 
      "应该能够在 " + edge_device.model + " 设备上导出span")
    
    // 验证边缘设备特定属性存在
    let exported_span = edge_exported_spans[0]
    assert_true(Span::has_attribute(exported_span, "device.model"), 
      "导出的span应该包含设备型号信息")
    assert_true(Span::has_attribute(exported_span, "network.status"), 
      "导出的span应该包含网络状态信息")
  }
  
  // 测试跨边缘设备数据一致性
  let edge_validator = CrossEdgeDeviceValidator::new()
  let edge_consistency_report = CrossEdgeDeviceValidator::validate_consistency(edge_validator)
  
  assert_true(edge_consistency_report.overall_consistent, "跨边缘设备数据应该保持一致")
  assert_true(edge_consistency_report.inconsistencies.length() == 0, 
    "不应该有跨边缘设备数据不一致")
  
  assert_true(true)
}