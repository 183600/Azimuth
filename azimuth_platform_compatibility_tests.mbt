// Azimuth 平台兼容性测试用例
// 专注于遥测系统在不同平台和运行环境中的兼容性

// 测试1: WebAssembly平台兼容性
test "WebAssembly平台兼容性测试" {
  // 检测当前运行平台
  let platform = PlatformDetector::detect()
  
  // 创建平台特定的配置
  let config = TelemetryConfig::new()
  
  match platform {
    Platform::WebAssembly => {
      // WebAssembly特定配置
      TelemetryConfig::set_exporter_type(config, "wasm.http")
      TelemetryConfig::set_batch_size(config, 32) // 较小的批次大小适合WASM
      TelemetryConfig::set_max_memory_usage(config, 16 * 1024 * 1024) // 16MB内存限制
      
      // 验证WASM特定功能
      assert_true(WasmPlatform::supports_javascript_interop())
      assert_true(WasmPlatform::has_browser_storage())
    }
    Platform::Linux => {
      // Linux特定配置
      TelemetryConfig::set_exporter_type(config, "otlp.grpc")
      TelemetryConfig::set_batch_size(config, 512)
      TelemetryConfig::set_file_watch_enabled(config, true)
      
      // 验证Linux特定功能
      assert_true(LinuxPlatform::supports_signals())
      assert_true(LinuxPlatform::has_proc_fs())
    }
    Platform::Windows => {
      // Windows特定配置
      TelemetryConfig::set_exporter_type(config, "otlp.http")
      TelemetryConfig::set_batch_size(config, 256)
      TelemetryConfig::set_event_log_enabled(config, true)
      
      // 验证Windows特定功能
      assert_true(WindowsPlatform::supports_registry())
      assert_true(WindowsPlatform::has_event_log())
    }
    Platform::MacOS => {
      // macOS特定配置
      TelemetryConfig::set_exporter_type(config, "otlp.http")
      TelemetryConfig::set_batch_size(config, 256)
      TelemetryConfig::set_system_log_enabled(config, true)
      
      // 验证macOS特定功能
      assert_true(MacPlatform::supports_launchd())
      assert_true(MacPlatform::has_system_log())
    }
  }
  
  // 测试平台特定的遥测功能
  let provider = MeterProvider::with_config(config)
  let meter = MeterProvider::get_meter(provider, "platform.compatibility.test")
  
  // 创建平台感知指标
  let platform_counter = Meter::create_counter(meter, "platform.operations")
  Counter::add(platform_counter, 1.0)
  
  // 添加平台属性
  let platform_attrs = [
    ("platform.name", StringValue(Platform::to_string(platform))),
    ("platform.arch", StringValue(Platform::architecture(platform))),
    ("platform.version", StringValue(Platform::version(platform)))
  ]
  
  let platform_resource = Resource::with_attributes(Resource::new(), platform_attrs)
  
  // 验证平台资源属性
  let platform_name = Resource::get_attribute(platform_resource, "platform.name")
  match platform_name {
    Some(StringValue(name)) => assert_true(name.length() > 0)
    _ => assert_true(false)
  }
}
