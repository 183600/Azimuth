// Azimuth Cross-Platform Telemetry Tests
// This file contains test cases for cross-platform telemetry compatibility

// Test 1: Platform-Specific Telemetry Configuration
test "platform-specific telemetry configuration" {
  // Define supported platforms
  enum Platform {
    Windows
    Linux
    MacOS
    Android
    IOS
    WebAssembly
    Docker
    Kubernetes
  }
  
  // Define platform-specific configuration
  type PlatformConfig = {
    platform: Platform,
    telemetry_endpoint: String,
    batch_size: Int,
    flush_interval_ms: Int,
    compression_enabled: Bool,
    platform_specific_attrs: Array[(String, String)]
  }
  
  // Create configuration for different platforms
  let create_platform_config = fn(platform: Platform) {
    match platform {
      Platform::Windows => {
        {
          platform: Platform::Windows,
          telemetry_endpoint: "https://telemetry.windows.example.com/v1/traces",
          batch_size: 50,
          flush_interval_ms: 5000,
          compression_enabled: true,
          platform_specific_attrs: [
            ("os", "windows"),
            ("arch", "amd64"),
            ("runtime", "dotnet"),
            ("service_framework", "windows_service")
          ]
        }
      }
      Platform::Linux => {
        {
          platform: Platform::Linux,
          telemetry_endpoint: "https://telemetry.linux.example.com/v1/traces",
          batch_size: 100,
          flush_interval_ms: 3000,
          compression_enabled: true,
          platform_specific_attrs: [
            ("os", "linux"),
            ("arch", "x86_64"),
            ("runtime", "native"),
            ("container_runtime", "docker")
          ]
        }
      }
      Platform::MacOS => {
        {
          platform: Platform::MacOS,
          telemetry_endpoint: "https://telemetry.macos.example.com/v1/traces",
          batch_size: 75,
          flush_interval_ms: 4000,
          compression_enabled: true,
          platform_specific_attrs: [
            ("os", "macos"),
            ("arch", "arm64"),
            ("runtime", "native"),
            ("ui_framework", "cocoa")
          ]
        }
      }
      Platform::Android => {
        {
          platform: Platform::Android,
          telemetry_endpoint: "https://telemetry.android.example.com/v1/traces",
          batch_size: 25,
          flush_interval_ms: 10000,
          compression_enabled: false,  // Save battery
          platform_specific_attrs: [
            ("os", "android"),
            ("arch", "arm64-v8a"),
            ("runtime", "art"),
            ("mobile", "true"),
            ("battery_optimized", "true")
          ]
        }
      }
      Platform::IOS => {
        {
          platform: Platform::IOS,
          telemetry_endpoint: "https://telemetry.ios.example.com/v1/traces",
          batch_size: 30,
          flush_interval_ms: 8000,
          compression_enabled: false,  // Save battery
          platform_specific_attrs: [
            ("os", "ios"),
            ("arch", "arm64"),
            ("runtime", "native"),
            ("mobile", "true"),
            ("battery_optimized", "true")
          ]
        }
      }
      Platform::WebAssembly => {
        {
          platform: Platform::WebAssembly,
          telemetry_endpoint: "https://telemetry.wasm.example.com/v1/traces",
          batch_size: 20,
          flush_interval_ms: 15000,
          compression_enabled: true,
          platform_specific_attrs: [
            ("os", "web"),
            ("arch", "wasm32"),
            ("runtime", "browser"),
            ("sandboxed", "true")
          ]
        }
      }
      Platform::Docker => {
        {
          platform: Platform::Docker,
          telemetry_endpoint: "https://telemetry.docker.example.com/v1/traces",
          batch_size: 100,
          flush_interval_ms: 2000,
          compression_enabled: true,
          platform_specific_attrs: [
            ("os", "linux"),
            ("arch", "x86_64"),
            ("runtime", "docker"),
            ("containerized", "true"),
            ("ephemeral", "true")
          ]
        }
      }
      Platform::Kubernetes => {
        {
          platform: Platform::Kubernetes,
          telemetry_endpoint: "https://telemetry.k8s.example.com/v1/traces",
          batch_size: 200,
          flush_interval_ms: 1000,
          compression_enabled: true,
          platform_specific_attrs: [
            ("os", "linux"),
            ("arch", "x86_64"),
            ("runtime", "kubernetes"),
            ("orchestrated", "true"),
            ("cluster", "production"),
            ("namespace", "telemetry")
          ]
        }
      }
    }
  }
  
  // Test platform configuration creation
  let windows_config = create_platform_config(Platform::Windows)
  assert_eq(windows_config.batch_size, 50)
  assert_eq(windows_config.flush_interval_ms, 5000)
  assert_true(windows_config.compression_enabled)
  assert_true(windows_config.platform_specific_attrs.some(fn(a) { a.0 == "os" && a.1 == "windows" }))
  
  let android_config = create_platform_config(Platform::Android)
  assert_eq(android_config.batch_size, 25)
  assert_eq(android_config.flush_interval_ms, 10000)
  assert_false(android_config.compression_enabled)  // Battery optimized
  assert_true(android_config.platform_specific_attrs.some(fn(a) { a.0 == "battery_optimized" && a.1 == "true" }))
  
  let k8s_config = create_platform_config(Platform::Kubernetes)
  assert_eq(k8s_config.batch_size, 200)
  assert_eq(k8s_config.flush_interval_ms, 1000)
  assert_true(k8s_config.platform_specific_attrs.some(fn(a) { a.0 == "orchestrated" && a.1 == "true" }))
  
  // Test platform-specific attribute merging
  let merge_platform_attributes = fn(config: PlatformConfig, custom_attrs: Array[(String, String)]) {
    let merged = config.platform_specific_attrs.concat(custom_attrs)
    
    // Remove duplicates (keep custom values)
    let mut result = []
    let mut seen_keys = []
    
    for attr in merged {
      let (key, value) = attr
      if not(seen_keys.contains(key)) {
        result = result.push(attr)
        seen_keys = seen_keys.push(key)
      } else {
        // Replace with custom value if it exists
        let mut index = 0
        let mut found = false
        while index < result.length() {
          if result[index].0 == key {
            result[index] = attr
            found = true
            break
          }
          index = index + 1
        }
      }
    }
    
    result
  }
  
  let custom_attrs = [
    ("environment", "production"),
    ("service", "payment-api"),
    ("version", "1.2.3")
  ]
  
  let merged_attrs = merge_platform_attributes(windows_config, custom_attrs)
  assert_true(merged_attrs.some(fn(a) { a.0 == "os" && a.1 == "windows" }))
  assert_true(merged_attrs.some(fn(a) { a.0 == "environment" && a.1 == "production" }))
  assert_true(merged_attrs.some(fn(a) { a.0 == "service" && a.1 == "payment-api" }))
}

// Test 2: Cross-Platform Telemetry Data Serialization
test "cross-platform telemetry data serialization" {
  // Define serialization format
  enum SerializationFormat {
    JSON
    Protobuf
    MessagePack
    CBOR
    XML
  }
  
  // Define platform-specific format support
  type FormatSupport = {
    format: SerializationFormat,
    supported_platforms: Array[Platform],
    size_efficiency: Float,  // 0.0 to 1.0 (lower is better)
    parse_speed: Float,      // 0.0 to 1.0 (higher is better)
    human_readable: Bool
  }
  
  // Define format support matrix
  let format_support_matrix = [
    {
      format: SerializationFormat::JSON,
      supported_platforms: [Platform::Windows, Platform::Linux, Platform::MacOS, 
                           Platform::Android, Platform::IOS, Platform::WebAssembly,
                           Platform::Docker, Platform::Kubernetes],
      size_efficiency: 0.7,
      parse_speed: 0.6,
      human_readable: true
    },
    {
      format: SerializationFormat::Protobuf,
      supported_platforms: [Platform::Windows, Platform::Linux, Platform::MacOS,
                           Platform::Android, Platform::IOS, Platform::Docker, Platform::Kubernetes],
      size_efficiency: 0.9,
      parse_speed: 0.9,
      human_readable: false
    },
    {
      format: SerializationFormat::MessagePack,
      supported_platforms: [Platform::Windows, Platform::Linux, Platform::MacOS,
                           Platform::Android, Platform::IOS, Platform::WebAssembly,
                           Platform::Docker, Platform::Kubernetes],
      size_efficiency: 0.85,
      parse_speed: 0.8,
      human_readable: false
    },
    {
      format: SerializationFormat::CBOR,
      supported_platforms: [Platform::Linux, Platform::Android, Platform::WebAssembly,
                           Platform::Docker, Platform::Kubernetes],
      size_efficiency: 0.8,
      parse_speed: 0.7,
      human_readable: false
    },
    {
      format: SerializationFormat::XML,
      supported_platforms: [Platform::Windows, Platform::Linux, Platform::MacOS,
                           Platform::Docker, Platform::Kubernetes],
      size_efficiency: 0.5,
      parse_speed: 0.4,
      human_readable: true
    }
  ]
  
  // Get optimal format for platform
  let get_optimal_format = fn(platform: Platform, priority: String) {
    let supported_formats = format_support_matrix.filter_fn(fs) {
      fs.supported_platforms.some(fn(p) { p == platform })
    }
    
    match priority {
      "size" => {
        supported_formats.sort_by(fn(a, b) { 
          if a.size_efficiency > b.size_efficiency { -1 } 
          else if a.size_efficiency < b.size_efficiency { 1 } 
          else { 0 } 
        })
      }
      "speed" => {
        supported_formats.sort_by(fn(a, b) { 
          if a.parse_speed > b.parse_speed { -1 } 
          else if a.parse_speed < b.parse_speed { 1 } 
          else { 0 } 
        })
      }
      "readable" => {
        supported_formats.filter_fn(fs) { fs.human_readable }
      }
      _ => {
        supported_formats
      }
    }
    
    if supported_formats.length() > 0 {
      supported_formats[0].format
    } else {
      SerializationFormat::JSON  // Default fallback
    }
  }
  
  // Test optimal format selection
  let mobile_optimal = get_optimal_format(Platform::Android, "size")
  match mobile_optimal {
    SerializationFormat::Protobuf => assert_true(true)  // Best size efficiency
    _ => assert_true(false)
  }
  
  let web_optimal = get_optimal_format(Platform::WebAssembly, "speed")
  match web_optimal {
    SerializationFormat::Protobuf => assert_true(true)  // Best parse speed
    _ => assert_true(false)
  }
  
  let readable_optimal = get_optimal_format(Platform::Windows, "readable")
  match readable_optimal {
    SerializationFormat::JSON => assert_true(true)  // Human readable
    _ => assert_true(false)
  }
  
  // Test platform-specific serialization
  let serialize_for_platform = fn(platform: Platform, data: Array[(String, String)], format: Option[SerializationFormat]) {
    let selected_format = match format {
      Some(f) => f
      None => get_optimal_format(platform, "size")
    }
    
    // Simulate serialization based on format
    match selected_format {
      SerializationFormat::JSON => {
        let json_string = "{" + data.map_fn(item) {
          "\"" + item.0 + "\":\"" + item.1 + "\""
        }.join(",") + "}"
        ("json", json_string.length())
      }
      SerializationFormat::Protobuf => {
        // Protobuf is more compact
        let proto_size = data.fold(0, fn(acc, item) { 
          acc + item.0.length() + item.1.length() + 2 
        }) / 2  // Rough estimate
        ("protobuf", proto_size)
      }
      SerializationFormat::MessagePack => {
        let msgpack_size = data.fold(0, fn(acc, item) { 
          acc + item.0.length() + item.1.length() + 3 
        }) / 2  // Rough estimate
        ("msgpack", msgpack_size)
      }
      SerializationFormat::CBOR => {
        let cbor_size = data.fold(0, fn(acc, item) { 
          acc + item.0.length() + item.1.length() + 4 
        }) / 2  // Rough estimate
        ("cbor", cbor_size)
      }
      SerializationFormat::XML => {
        let xml_string = "<data>" + data.map_fn(item) {
          "<" + item.0 + ">" + item.1 + "</" + item.0 + ">"
        }.join("") + "</data>"
        ("xml", xml_string.length())
      }
    }
  }
  
  // Test serialization for different platforms
  let telemetry_data = [
    ("trace_id", "trace-12345"),
    ("span_name", "database_query"),
    ("duration_ms", "250"),
    ("status", "success"),
    ("service", "payment-api")
  ]
  
  let (web_format, web_size) = serialize_for_platform(Platform::WebAssembly, telemetry_data, None)
  let (mobile_format, mobile_size) = serialize_for_platform(Platform::Android, telemetry_data, None)
  let (server_format, server_size) = serialize_for_platform(Platform::Kubernetes, telemetry_data, None)
  
  assert_eq(web_format, "protobuf")
  assert_eq(mobile_format, "protobuf")
  assert_eq(server_format, "protobuf")
  
  // Test explicit format override
  let (json_format, json_size) = serialize_for_platform(Platform::Android, telemetry_data, Some(SerializationFormat::JSON))
  assert_eq(json_format, "json")
  assert_true(json_size > mobile_size)  // JSON is less efficient
}

// Test 3: Cross-Platform Network Transport
test "cross-platform network transport" {
  // Define network transport protocols
  enum TransportProtocol {
    HTTP
    HTTPS
    gRPC
    WebSocket
    TCP
    UDP
    MQTT
  }
  
  // Define platform-specific transport capabilities
  type TransportCapability = {
    protocol: TransportProtocol,
    supported_platforms: Array[Platform],
    reliability: String,  // "high", "medium", "low"
    overhead: Int,        // bytes overhead per request
    battery_impact: String,  // "high", "medium", "low"
    streaming_support: Bool
  }
  
  // Define transport capabilities matrix
  let transport_capabilities = [
    {
      protocol: TransportProtocol::HTTP,
      supported_platforms: [Platform::Windows, Platform::Linux, Platform::MacOS,
                           Platform::Android, Platform::IOS, Platform::WebAssembly,
                           Platform::Docker, Platform::Kubernetes],
      reliability: "medium",
      overhead: 500,
      battery_impact: "medium",
      streaming_support: false
    },
    {
      protocol: TransportProtocol::HTTPS,
      supported_platforms: [Platform::Windows, Platform::Linux, Platform::MacOS,
                           Platform::Android, Platform::IOS, Platform::WebAssembly,
                           Platform::Docker, Platform::Kubernetes],
      reliability: "high",
      overhead: 800,
      battery_impact: "medium",
      streaming_support: false
    },
    {
      protocol: TransportProtocol::gRPC,
      supported_platforms: [Platform::Windows, Platform::Linux, Platform::MacOS,
                           Platform::Android, Platform::IOS, Platform::Docker, Platform::Kubernetes],
      reliability: "high",
      overhead: 300,
      battery_impact: "medium",
      streaming_support: true
    },
    {
      protocol: TransportProtocol::WebSocket,
      supported_platforms: [Platform::Windows, Platform::Linux, Platform::MacOS,
                           Platform::Android, Platform::IOS, Platform::WebAssembly],
      reliability: "medium",
      overhead: 200,
      battery_impact: "low",
      streaming_support: true
    },
    {
      protocol: TransportProtocol::TCP,
      supported_platforms: [Platform::Windows, Platform::Linux, Platform::MacOS,
                           Platform::Docker, Platform::Kubernetes],
      reliability: "high",
      overhead: 100,
      battery_impact: "medium",
      streaming_support: true
    },
    {
      protocol: TransportProtocol::UDP,
      supported_platforms: [Platform::Linux, Platform::Android, Platform::Docker, Platform::Kubernetes],
      reliability: "low",
      overhead: 50,
      battery_impact: "low",
      streaming_support: true
    },
    {
      protocol: TransportProtocol::MQTT,
      supported_platforms: [Platform::Linux, Platform::Android, Platform::IOS,
                           Platform::Docker, Platform::Kubernetes],
      reliability: "medium",
      overhead: 150,
      battery_impact: "low",
      streaming_support: true
    }
  ]
  
  // Get optimal transport for platform and use case
  let get_optimal_transport = fn(platform: Platform, use_case: String) {
    let supported_transports = transport_capabilities.filter_fn(tc) {
      tc.supported_platforms.some(fn(p) { p == platform })
    }
    
    match use_case {
      "high_reliability" => {
        supported_transports.filter_fn(tc) { tc.reliability == "high" }
          .sort_by(fn(a, b) { if a.overhead < b.overhead { -1 } else if a.overhead > b.overhead { 1 } else { 0 } })
      }
      "low_power" => {
        supported_transports.filter_fn(tc) { tc.battery_impact == "low" }
          .sort_by(fn(a, b) { if a.overhead < b.overhead { -1 } else if a.overhead > b.overhead { 1 } else { 0 } })
      }
      "streaming" => {
        supported_transports.filter_fn(tc) { tc.streaming_support }
          .sort_by(fn(a, b) { if a.overhead < b.overhead { -1 } else if a.overhead > b.overhead { 1 } else { 0 } })
      }
      _ => {
        supported_transports.sort_by(fn(a, b) { if a.overhead < b.overhead { -1 } else if a.overhead > b.overhead { 1 } else { 0 } })
      }
    }
    
    if supported_transports.length() > 0 {
      supported_transports[0].protocol
    } else {
      TransportProtocol::HTTP  // Default fallback
    }
  }
  
  // Test optimal transport selection
  let server_reliable = get_optimal_transport(Platform::Kubernetes, "high_reliability")
  match server_reliable {
    TransportProtocol::gRPC => assert_true(true)  // High reliability, low overhead
    _ => assert_true(false)
  }
  
  let mobile_power = get_optimal_transport(Platform::Android, "low_power")
  match mobile_power {
    TransportProtocol::WebSocket => assert_true(true)  // Low power, streaming support
    _ => assert_true(false)
  }
  
  let web_streaming = get_optimal_transport(Platform::WebAssembly, "streaming")
  match web_streaming {
    TransportProtocol::WebSocket => assert_true(true)  // Best streaming support for web
    _ => assert_true(false)
  }
  
  // Test platform-specific transport configuration
  let configure_transport = fn(platform: Platform, protocol: TransportProtocol) {
    let capability = transport_capabilities.find_fn(tc) {
      tc.protocol == protocol && tc.supported_platforms.some(fn(p) { p == platform })
    }
    
    match capability {
      Some(cap) => {
        {
          protocol: protocol,
          platform: platform,
          timeout_ms: match protocol {
            TransportProtocol::HTTP | TransportProtocol::HTTPS => 30000
            TransportProtocol::gRPC => 60000
            TransportProtocol::WebSocket => 120000
            TransportProtocol::TCP => 45000
            TransportProtocol::UDP => 5000
            TransportProtocol::MQTT => 20000
          },
          retry_count: match cap.reliability {
            "high" => 3
            "medium" => 2
            "low" => 1
          },
          batch_size: match platform {
            Platform::Android | Platform::IOS => 25
            Platform::WebAssembly => 20
            _ => 100
          },
          compression: match protocol {
            TransportProtocol::HTTP | TransportProtocol::HTTPS => true
            TransportProtocol::gRPC => true
            TransportProtocol::WebSocket => false
            TransportProtocol::TCP => false
            TransportProtocol::UDP => false
            TransportProtocol::MQTT => true
          }
        }
      }
      None => {
        {
          protocol: protocol,
          platform: platform,
          timeout_ms: 30000,
          retry_count: 1,
          batch_size: 50,
          compression: false
        }
      }
    }
  }
  
  // Test transport configuration
  let android_grpc_config = configure_transport(Platform::Android, TransportProtocol::gRPC)
  assert_eq(android_grpc_config.timeout_ms, 60000)
  assert_eq(android_grpc_config.retry_count, 3)
  assert_eq(android_grpc_config.batch_size, 25)
  assert_true(android_grpc_config.compression)
  
  let k8s_tcp_config = configure_transport(Platform::Kubernetes, TransportProtocol::TCP)
  assert_eq(k8s_tcp_config.timeout_ms, 45000)
  assert_eq(k8s_tcp_config.retry_count, 3)
  assert_eq(k8s_tcp_config.batch_size, 100)
  assert_false(k8s_tcp_config.compression)
  
  let web_ws_config = configure_transport(Platform::WebAssembly, TransportProtocol::WebSocket)
  assert_eq(web_ws_config.timeout_ms, 120000)
  assert_eq(web_ws_config.retry_count, 2)
  assert_eq(web_ws_config.batch_size, 20)
  assert_false(web_ws_config.compression)
}

// Test 4: Cross-Platform Time and Timestamp Handling
test "cross-platform time and timestamp handling" {
  // Define timestamp precision levels
  enum TimestampPrecision {
    Second
    Millisecond
    Microsecond
    Nanosecond
  }
  
  // Define platform-specific time capabilities
  type TimeCapability = {
    platform: Platform,
    supported_precisions: Array[TimestampPrecision],
    timezone_support: Bool,
    monotonic_clock: Bool,
    ntp_sync: Bool,
    precision_overhead: Int  // microseconds
  }
  
  // Define time capabilities matrix
  let time_capabilities = [
    {
      platform: Platform::Windows,
      supported_precisions: [TimestampPrecision::Millisecond, TimestampPrecision::Microsecond],
      timezone_support: true,
      monotonic_clock: true,
      ntp_sync: true,
      precision_overhead: 50
    },
    {
      platform: Platform::Linux,
      supported_precisions: [TimestampPrecision::Second, TimestampPrecision::Millisecond, 
                           TimestampPrecision::Microsecond, TimestampPrecision::Nanosecond],
      timezone_support: true,
      monotonic_clock: true,
      ntp_sync: true,
      precision_overhead: 10
    },
    {
      platform: Platform::MacOS,
      supported_precisions: [TimestampPrecision::Millisecond, TimestampPrecision::Microsecond, 
                           TimestampPrecision::Nanosecond],
      timezone_support: true,
      monotonic_clock: true,
      ntp_sync: true,
      precision_overhead: 20
    },
    {
      platform: Platform::Android,
      supported_precisions: [TimestampPrecision::Millisecond],
      timezone_support: true,
      monotonic_clock: true,
      ntp_sync: true,
      precision_overhead: 100
    },
    {
      platform: Platform::IOS,
      supported_precisions: [TimestampPrecision::Millisecond, TimestampPrecision::Microsecond],
      timezone_support: true,
      monotonic_clock: true,
      ntp_sync: true,
      precision_overhead: 80
    },
    {
      platform: Platform::WebAssembly,
      supported_precisions: [TimestampPrecision::Millisecond],
      timezone_support: false,  // Limited timezone support in browsers
      monotonic_clock: true,
      ntp_sync: false,  // Browser-controlled
      precision_overhead: 200
    },
    {
      platform: Platform::Docker,
      supported_precisions: [TimestampPrecision::Millisecond, TimestampPrecision::Microsecond],
      timezone_support: true,
      monotonic_clock: true,
      ntp_sync: true,
      precision_overhead: 30
    },
    {
      platform: Platform::Kubernetes,
      supported_precisions: [TimestampPrecision::Millisecond, TimestampPrecision::Microsecond],
      timezone_support: true,
      monotonic_clock: true,
      ntp_sync: true,
      precision_overhead: 25
    }
  ]
  
  // Get optimal timestamp precision for platform
  let get_optimal_precision = fn(platform: Platform, use_case: String) {
    let capability = time_capabilities.find_fn(tc) { tc.platform == platform }
    
    match capability {
      Some(cap) => {
        match use_case {
          "high_precision" => {
            // Return highest supported precision
            if cap.supported_precisions.some(fn(p) { p == TimestampPrecision::Nanosecond }) {
              TimestampPrecision::Nanosecond
            } else if cap.supported_precisions.some(fn(p) { p == TimestampPrecision::Microsecond }) {
              TimestampPrecision::Microsecond
            } else if cap.supported_precisions.some(fn(p) { p == TimestampPrecision::Millisecond }) {
              TimestampPrecision::Millisecond
            } else {
              TimestampPrecision::Second
            }
          }
          "low_overhead" => {
            // Return lowest overhead precision
            if cap.supported_precisions.some(fn(p) { p == TimestampPrecision::Second }) {
              TimestampPrecision::Second
            } else if cap.supported_precisions.some(fn(p) { p == TimestampPrecision::Millisecond }) {
              TimestampPrecision::Millisecond
            } else {
              cap.supported_precisions[0]  // First available
            }
          }
          _ => {
            // Default to millisecond if available
            if cap.supported_precisions.some(fn(p) { p == TimestampPrecision::Millisecond }) {
              TimestampPrecision::Millisecond
            } else {
              cap.supported_precisions[0]  // First available
            }
          }
        }
      }
      None => {
        TimestampPrecision::Millisecond  // Default fallback
      }
    }
  }
  
  // Test precision selection
  let linux_high_precision = get_optimal_precision(Platform::Linux, "high_precision")
  match linux_high_precision {
    TimestampPrecision::Nanosecond => assert_true(true)
    _ => assert_true(false)
  }
  
  let mobile_low_overhead = get_optimal_precision(Platform::Android, "low_overhead")
  match mobile_low_overhead {
    TimestampPrecision::Millisecond => assert_true(true)  // Only option on Android
    _ => assert_true(false)
  }
  
  let web_default = get_optimal_precision(Platform::WebAssembly, "default")
  match web_default {
    TimestampPrecision::Millisecond => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test timestamp conversion between precisions
  let convert_timestamp = fn(timestamp: Int, from_precision: TimestampPrecision, to_precision: TimestampPrecision) {
    match (from_precision, to_precision) {
      (TimestampPrecision::Nanosecond, TimestampPrecision::Microsecond) => timestamp / 1000
      (TimestampPrecision::Nanosecond, TimestampPrecision::Millisecond) => timestamp / 1000000
      (TimestampPrecision::Nanosecond, TimestampPrecision::Second) => timestamp / 1000000000
      
      (TimestampPrecision::Microsecond, TimestampPrecision::Nanosecond) => timestamp * 1000
      (TimestampPrecision::Microsecond, TimestampPrecision::Millisecond) => timestamp / 1000
      (TimestampPrecision::Microsecond, TimestampPrecision::Second) => timestamp / 1000000
      
      (TimestampPrecision::Millisecond, TimestampPrecision::Nanosecond) => timestamp * 1000000
      (TimestampPrecision::Millisecond, TimestampPrecision::Microsecond) => timestamp * 1000
      (TimestampPrecision::Millisecond, TimestampPrecision::Second) => timestamp / 1000
      
      (TimestampPrecision::Second, TimestampPrecision::Nanosecond) => timestamp * 1000000000
      (TimestampPrecision::Second, TimestampPrecision::Microsecond) => timestamp * 1000000
      (TimestampPrecision::Second, TimestampPrecision::Millisecond) => timestamp * 1000
      
      _ => timestamp  // Same precision
    }
  }
  
  // Test timestamp conversion
  let nanoseconds = 1640995200123456789
  let microseconds = convert_timestamp(nanoseconds, TimestampPrecision::Nanosecond, TimestampPrecision::Microsecond)
  let milliseconds = convert_timestamp(nanoseconds, TimestampPrecision::Nanosecond, TimestampPrecision::Millisecond)
  let seconds = convert_timestamp(nanoseconds, TimestampPrecision::Nanosecond, TimestampPrecision::Second)
  
  assert_eq(microseconds, 1640995200123456)
  assert_eq(milliseconds, 1640995200123)
  assert_eq(seconds, 1640995200)
  
  // Test round-trip conversion
  let round_trip = convert_timestamp(seconds, TimestampPrecision::Second, TimestampPrecision::Nanosecond)
  assert_eq(round_trip, 1640995200000000000)
  
  // Test platform-specific timestamp generation
  let generate_timestamp = fn(platform: Platform, precision: TimestampPrecision) {
    let capability = time_capabilities.find_fn(tc) { tc.platform == platform }
    
    match capability {
      Some(cap) => {
        if cap.supported_precisions.some(fn(p) { p == precision }) {
          // Simulate timestamp generation based on precision
          let base_timestamp = 1640995200  // 2022-01-01 00:00:00 UTC
          
          match precision {
            TimestampPrecision::Second => base_timestamp
            TimestampPrecision::Millisecond => base_timestamp * 1000 + 123
            TimestampPrecision::Microsecond => base_timestamp * 1000000 + 123456
            TimestampPrecision::Nanosecond => base_timestamp * 1000000000 + 123456789
          }
        } else {
          -1  // Precision not supported
        }
      }
      None => {
        -1  // Platform not supported
      }
    }
  }
  
  // Test timestamp generation
  let linux_ms = generate_timestamp(Platform::Linux, TimestampPrecision::Millisecond)
  assert_eq(linux_ms, 1640995200123)
  
  let android_us = generate_timestamp(Platform::Android, TimestampPrecision::Microsecond)
  assert_eq(android_us, -1)  // Not supported on Android
  
  let web_ns = generate_timestamp(Platform::WebAssembly, TimestampPrecision::Nanosecond)
  assert_eq(web_ns, -1)  // Not supported on WebAssembly
  
  let k8s_us = generate_timestamp(Platform::Kubernetes, TimestampPrecision::Microsecond)
  assert_eq(k8s_us, 1640995200123456)
}