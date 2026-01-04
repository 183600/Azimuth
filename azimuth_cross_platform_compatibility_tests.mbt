// Cross-Platform Compatibility Tests for Azimuth Telemetry System
// This file contains tests for cross-platform compatibility and platform-specific behavior

// Test 1: Platform Detection
test "platform detection and identification" {
  // Define platform types
  enum Platform {
    Windows
    Linux
    MacOS
    FreeBSD
    Unknown
  }
  
  // Define architecture types
  enum Architecture {
    X86
    X64
    ARM
    ARM64
    Unknown
  }
  
  // Define platform info
  type PlatformInfo = {
    platform : Platform
    architecture : Architecture
    version : String
    endianness : String
  }
  
  // Simulate platform detection
  let detect_platform = fn() {
    // In a real implementation, this would check system properties
    // For testing purposes, we'll simulate different platforms
    
    // Simulate Linux x64
    PlatformInfo {
      platform: Platform::Linux,
      architecture: Architecture::X64,
      version: "5.15.0",
      endianness: "little"
    }
  }
  
  // Test platform detection
  let platform_info = detect_platform()
  
  match platform_info.platform {
    Platform::Linux => assert_true(true),
    _ => assert_true(false)
  }
  
  match platform_info.architecture {
    Architecture::X64 => assert_true(true),
    _ => assert_true(false)
  }
  
  assert_eq(platform_info.version, "5.15.0")
  assert_eq(platform_info.endianness, "little")
  
  // Test platform-specific behavior
  let get_path_separator = fn(platform : Platform) {
    match platform {
      Platform::Windows => "\\",
      _ => "/"
    }
  }
  
  let get_line_ending = fn(platform : Platform) {
    match platform {
      Platform::Windows => "\r\n",
      _ => "\n"
    }
  }
  
  let get_executable_extension = fn(platform : Platform) {
    match platform {
      Platform::Windows => ".exe",
      _ => ""
    }
  }
  
  // Test platform-specific values
  assert_eq(get_path_separator(Platform::Windows), "\\")
  assert_eq(get_path_separator(Platform::Linux), "/")
  assert_eq(get_path_separator(Platform::MacOS), "/")
  
  assert_eq(get_line_ending(Platform::Windows), "\r\n")
  assert_eq(get_line_ending(Platform::Linux), "\n")
  assert_eq(get_line_ending(Platform::MacOS), "\n")
  
  assert_eq(get_executable_extension(Platform::Windows), ".exe")
  assert_eq(get_executable_extension(Platform::Linux), "")
  assert_eq(get_executable_extension(Platform::MacOS), "")
}

// Test 2: File System Compatibility
test "file system compatibility across platforms" {
  // Define file system characteristics
  type FileSystemInfo = {
    case_sensitive : Bool
    path_separator : String
    max_path_length : Int
    supported_features : [String]
  }
  
  // Get file system info for platform
  let get_file_system_info = fn(platform : Platform) {
    match platform {
      Platform::Windows => FileSystemInfo {
        case_sensitive: false,
        path_separator: "\\",
        max_path_length: 260,
        supported_features: ["short_paths", "file_streams", "hard_links", "symbolic_links"]
      }
      Platform::Linux => FileSystemInfo {
        case_sensitive: true,
        path_separator: "/",
        max_path_length: 4096,
        supported_features: ["hard_links", "symbolic_links", "extended_attributes", "case_sensitivity"]
      }
      Platform::MacOS => FileSystemInfo {
        case_sensitive: false,  // APFS is case-insensitive by default
        path_separator: "/",
        max_path_length: 1024,
        supported_features: ["hard_links", "symbolic_links", "extended_attributes", "resource_forks"]
      }
      _ => FileSystemInfo {
        case_sensitive: true,
        path_separator: "/",
        max_path_length: 255,
        supported_features: []
      }
    }
  }
  
  // Normalize path for platform
  let normalize_path = fn(path : String, platform : Platform) {
    let separator = match platform {
      Platform::Windows => "\\",
      _ => "/"
    }
    
    // Replace all separators with the platform-specific one
    let mut normalized = ""
    for i in 0..<path.length() {
      let c = path[i]
      if c == '/' || c == '\\' {
        normalized = normalized + separator
      } else {
        normalized = normalized + c.to_string()
      }
    }
    
    normalized
  }
  
  // Test file system info
  let windows_fs = get_file_system_info(Platform::Windows)
  let linux_fs = get_file_system_info(Platform::Linux)
  let macos_fs = get_file_system_info(Platform::MacOS)
  
  assert_false(windows_fs.case_sensitive)
  assert_true(linux_fs.case_sensitive)
  assert_false(macos_fs.case_sensitive)
  
  assert_eq(windows_fs.path_separator, "\\")
  assert_eq(linux_fs.path_separator, "/")
  assert_eq(macos_fs.path_separator, "/")
  
  assert_eq(windows_fs.max_path_length, 260)
  assert_eq(linux_fs.max_path_length, 4096)
  assert_eq(macos_fs.max_path_length, 1024)
  
  // Test path normalization
  let test_path = "folder/subfolder\\file.txt"
  
  let windows_normalized = normalize_path(test_path, Platform::Windows)
  let linux_normalized = normalize_path(test_path, Platform::Linux)
  
  assert_eq(windows_normalized, "folder\\subfolder\\file.txt")
  assert_eq(linux_normalized, "folder/subfolder/file.txt")
  
  // Test path joining
  let join_paths = fn(paths : [String], platform : Platform) {
    let separator = match platform {
      Platform::Windows => "\\",
      _ => "/"
    }
    
    let mut result = ""
    for i in 0..<paths.length() {
      if i > 0 {
        result = result + separator
      }
      result = result + paths[i]
    }
    
    result
  }
  
  let windows_joined = join_paths(["folder", "subfolder", "file.txt"], Platform::Windows)
  let linux_joined = join_paths(["folder", "subfolder", "file.txt"], Platform::Linux)
  
  assert_eq(windows_joined, "folder\\subfolder\\file.txt")
  assert_eq(linux_joined, "folder/subfolder/file.txt")
}

// Test 3: Endianness Compatibility
test "endianness compatibility across platforms" {
  // Define endianness types
  enum Endianness {
    LittleEndian
    BigEndian
    Unknown
  }
  
  // Convert between endianness
  let swap_bytes_16 = fn(value : Int) {
    ((value & 0xFF) << 8) | ((value >> 8) & 0xFF)
  }
  
  let swap_bytes_32 = fn(value : Int) {
    ((value & 0xFF) << 24) |
    (((value >> 8) & 0xFF) << 16) |
    (((value >> 16) & 0xFF) << 8) |
    ((value >> 24) & 0xFF)
  }
  
  // Serialize integer to bytes with specific endianness
  let serialize_int = fn(value : Int, endianness : Endianness) {
    match endianness {
      Endianness::LittleEndian => [
        value & 0xFF,
        (value >> 8) & 0xFF,
        (value >> 16) & 0xFF,
        (value >> 24) & 0xFF
      ]
      Endianness::BigEndian => [
        (value >> 24) & 0xFF,
        (value >> 16) & 0xFF,
        (value >> 8) & 0xFF,
        value & 0xFF
      ]
      Endianness::Unknown => [0, 0, 0, 0]
    }
  }
  
  // Deserialize bytes to integer with specific endianness
  let deserialize_int = fn(bytes : [Int], endianness : Endianness) {
    if bytes.length() < 4 {
      0
    } else {
      match endianness {
        Endianness::LittleEndian => {
          bytes[0] |
          (bytes[1] << 8) |
          (bytes[2] << 16) |
          (bytes[3] << 24)
        }
        Endianness::BigEndian => {
          (bytes[0] << 24) |
          (bytes[1] << 16) |
          (bytes[2] << 8) |
          bytes[3]
        }
        Endianness::Unknown => 0
      }
    }
  }
  
  // Test byte swapping
  let original_16 = 0x1234
  let swapped_16 = swap_bytes_16(original_16)
  assert_eq(swapped_16, 0x3412)
  
  let original_32 = 0x12345678
  let swapped_32 = swap_bytes_32(original_32)
  assert_eq(swapped_32, 0x78563412)
  
  // Test serialization/deserialization
  let test_value = 0x12345678
  
  let little_endian_bytes = serialize_int(test_value, Endianness::LittleEndian)
  let big_endian_bytes = serialize_int(test_value, Endianness::BigEndian)
  
  assert_eq(little_endian_bytes, [0x78, 0x56, 0x34, 0x12])
  assert_eq(big_endian_bytes, [0x12, 0x34, 0x56, 0x78])
  
  // Test deserialization
  let little_endian_value = deserialize_int(little_endian_bytes, Endianness::LittleEndian)
  let big_endian_value = deserialize_int(big_endian_bytes, Endianness::BigEndian)
  
  assert_eq(little_endian_value, test_value)
  assert_eq(big_endian_value, test_value)
  
  // Test cross-endianness (little-endian bytes, big-endian deserialization)
  let cross_endian_value = deserialize_int(little_endian_bytes, Endianness::BigEndian)
  assert_eq(cross_endian_value, 0x78563412)  // Byte-swapped
}

// Test 4: Character Encoding Compatibility
test "character encoding compatibility across platforms" {
  // Define encoding types
  enum Encoding {
    UTF8
    UTF16LE
    UTF16BE
    ASCII
    Latin1
  }
  
  // Convert string to bytes with specific encoding
  let string_to_bytes = fn(str : String, encoding : Encoding) {
    match encoding {
      Encoding::UTF8 => {
        // Simplified UTF-8 encoding (for ASCII characters only)
        let mut bytes = []
        for c in str.to_chars() {
          bytes = bytes.push(c.to_int())
        }
        bytes
      }
      Encoding::UTF16LE => {
        // Simplified UTF-16 Little Endian (for ASCII characters only)
        let mut bytes = []
        for c in str.to_chars() {
          bytes = bytes.push(c.to_int())
          bytes = bytes.push(0)
        }
        bytes
      }
      Encoding::UTF16BE => {
        // Simplified UTF-16 Big Endian (for ASCII characters only)
        let mut bytes = []
        for c in str.to_chars() {
          bytes = bytes.push(0)
          bytes = bytes.push(c.to_int())
        }
        bytes
      }
      Encoding::ASCII => {
        // ASCII encoding (for ASCII characters only)
        let mut bytes = []
        for c in str.to_chars() {
          bytes = bytes.push(c.to_int())
        }
        bytes
      }
      Encoding::Latin1 => {
        // Latin-1 encoding (simplified)
        let mut bytes = []
        for c in str.to_chars() {
          bytes = bytes.push(c.to_int())
        }
        bytes
      }
    }
  }
  
  // Convert bytes to string with specific encoding
  let bytes_to_string = fn(bytes : [Int], encoding : Encoding) {
    match encoding {
      Encoding::UTF8 => {
        // Simplified UTF-8 decoding (for ASCII characters only)
        let mut chars = []
        for byte in bytes {
          chars = chars.push(byte.to_char())
        }
        chars.join("")
      }
      Encoding::UTF16LE => {
        // Simplified UTF-16 Little Endian decoding (for ASCII characters only)
        let mut chars = []
        for i in 0..<bytes.length() step 2 {
          if i + 1 < bytes.length() {
            chars = chars.push(bytes[i].to_char())
          }
        }
        chars.join("")
      }
      Encoding::UTF16BE => {
        // Simplified UTF-16 Big Endian decoding (for ASCII characters only)
        let mut chars = []
        for i in 0..<bytes.length() step 2 {
          if i + 1 < bytes.length() {
            chars = chars.push(bytes[i + 1].to_char())
          }
        }
        chars.join("")
      }
      Encoding::ASCII => {
        // ASCII decoding
        let mut chars = []
        for byte in bytes {
          chars = chars.push(byte.to_char())
        }
        chars.join("")
      }
      Encoding::Latin1 => {
        // Latin-1 decoding (simplified)
        let mut chars = []
        for byte in bytes {
          chars = chars.push(byte.to_char())
        }
        chars.join("")
      }
    }
  }
  
  // Test encoding/decoding
  let test_string = "Hello, Azimuth!"
  
  let utf8_bytes = string_to_bytes(test_string, Encoding::UTF8)
  let utf16le_bytes = string_to_bytes(test_string, Encoding::UTF16LE)
  let utf16be_bytes = string_to_bytes(test_string, Encoding::UTF16BE)
  let ascii_bytes = string_to_bytes(test_string, Encoding::ASCII)
  
  // Verify UTF-8 and ASCII are the same for ASCII characters
  assert_eq(utf8_bytes, ascii_bytes)
  
  // Verify UTF-16 encodings have twice the length
  assert_eq(utf16le_bytes.length(), test_string.length() * 2)
  assert_eq(utf16be_bytes.length(), test_string.length() * 2)
  
  // Verify UTF-16 LE and BE are byte-swapped
  for i in 0..<utf16le_bytes.length() {
    if i % 2 == 0 {
      assert_eq(utf16le_bytes[i], utf16be_bytes[i + 1])
    } else {
      assert_eq(utf16le_bytes[i], utf16be_bytes[i - 1])
    }
  }
  
  // Test decoding
  let utf8_decoded = bytes_to_string(utf8_bytes, Encoding::UTF8)
  let utf16le_decoded = bytes_to_string(utf16le_bytes, Encoding::UTF16LE)
  let utf16be_decoded = bytes_to_string(utf16be_bytes, Encoding::UTF16BE)
  let ascii_decoded = bytes_to_string(ascii_bytes, Encoding::ASCII)
  
  assert_eq(utf8_decoded, test_string)
  assert_eq(utf16le_decoded, test_string)
  assert_eq(utf16be_decoded, test_string)
  assert_eq(ascii_decoded, test_string)
  
  // Test cross-encoding (UTF-8 bytes, UTF-16 decoding)
  let cross_decoded = bytes_to_string(utf8_bytes, Encoding::UTF16LE)
  assert_not_eq(cross_decoded, test_string)  // Should be garbled
}

// Test 5: Thread and Process Compatibility
test "thread and process compatibility across platforms" {
  // Define thread priority levels
  enum ThreadPriority {
    Idle
    Low
    Normal
    High
    Critical
  }
  
  // Define platform-specific thread attributes
  type ThreadAttributes = {
    priority : ThreadPriority
    stack_size : Int
    affinity_mask : Int
    is_daemon : Bool
  }
  
  // Get default thread attributes for platform
  let get_default_thread_attributes = fn(platform : Platform) {
    match platform {
      Platform::Windows => ThreadAttributes {
        priority: ThreadPriority::Normal,
        stack_size: 1048576,  // 1MB
        affinity_mask: 0xFFFFFFFF,  // All processors
        is_daemon: false
      }
      Platform::Linux => ThreadAttributes {
        priority: ThreadPriority::Normal,
        stack_size: 8388608,  // 8MB
        affinity_mask: 0xFFFFFFFF,  // All processors
        is_daemon: false
      }
      Platform::MacOS => ThreadAttributes {
        priority: ThreadPriority::Normal,
        stack_size: 524288,  // 512KB
        affinity_mask: 0xFFFFFFFF,  // All processors
        is_daemon: false
      }
      _ => ThreadAttributes {
        priority: ThreadPriority::Normal,
        stack_size: 1048576,  // 1MB
        affinity_mask: 0xFFFFFFFF,  // All processors
        is_daemon: false
      }
    }
  }
  
  // Convert platform priority to OS-specific value
  let priority_to_os_value = fn(priority : ThreadPriority, platform : Platform) {
    match (priority, platform) {
      (ThreadPriority::Idle, Platform::Windows) => -15,
      (ThreadPriority::Low, Platform::Windows) => -2,
      (ThreadPriority::Normal, Platform::Windows) => 0,
      (ThreadPriority::High, Platform::Windows) => 2,
      (ThreadPriority::Critical, Platform::Windows) => 15,
      
      (ThreadPriority::Idle, Platform::Linux) => 19,
      (ThreadPriority::Low, Platform::Linux) => 10,
      (ThreadPriority::Normal, Platform::Linux) => 0,
      (ThreadPriority::High, Platform::Linux) => -5,
      (ThreadPriority::Critical, Platform::Linux) => -20,
      
      (ThreadPriority::Idle, Platform::MacOS) => 20,
      (ThreadPriority::Low, Platform::MacOS) => 10,
      (ThreadPriority::Normal, Platform::MacOS) => 0,
      (ThreadPriority::High, Platform::MacOS) => -5,
      (ThreadPriority::Critical, Platform::MacOS) => -20,
      
      _ => 0  // Default for unknown platform
    }
  }
  
  // Test default thread attributes
  let windows_thread = get_default_thread_attributes(Platform::Windows)
  let linux_thread = get_default_thread_attributes(Platform::Linux)
  let macos_thread = get_default_thread_attributes(Platform::MacOS)
  
  assert_eq(windows_thread.stack_size, 1048576)
  assert_eq(linux_thread.stack_size, 8388608)
  assert_eq(macos_thread.stack_size, 524288)
  
  // Test priority conversion
  assert_eq(priority_to_os_value(ThreadPriority::High, Platform::Windows), 2)
  assert_eq(priority_to_os_value(ThreadPriority::High, Platform::Linux), -5)
  assert_eq(priority_to_os_value(ThreadPriority::High, Platform::MacOS), -5)
  
  assert_eq(priority_to_os_value(ThreadPriority::Low, Platform::Windows), -2)
  assert_eq(priority_to_os_value(ThreadPriority::Low, Platform::Linux), 10)
  assert_eq(priority_to_os_value(ThreadPriority::Low, Platform::MacOS), 10)
}

// Test 6: Network API Compatibility
test "network API compatibility across platforms" {
  // Define socket types
  enum SocketType {
    Stream    // TCP
    Datagram  // UDP
    Raw
  }
  
  // Define address families
  enum AddressFamily {
    IPv4
    IPv6
    Unix
  }
  
  // Define platform-specific socket options
  type SocketOptions = {
    receive_buffer_size : Int
    send_buffer_size : Int
    keep_alive : Bool
    reuse_address : Bool
    no_delay : Bool
  }
  
  // Get default socket options for platform
  let get_default_socket_options = fn(platform : Platform) {
    match platform {
      Platform::Windows => SocketOptions {
        receive_buffer_size: 8192,
        send_buffer_size: 8192,
        keep_alive: true,
        reuse_address: true,
        no_delay: true
      }
      Platform::Linux => SocketOptions {
        receive_buffer_size: 212992,  // 208KB
        send_buffer_size: 212992,     // 208KB
        keep_alive: true,
        reuse_address: true,
        no_delay: true
      }
      Platform::MacOS => SocketOptions {
        receive_buffer_size: 65536,  // 64KB
        send_buffer_size: 65536,     // 64KB
        keep_alive: true,
        reuse_address: true,
        no_delay: true
      }
      _ => SocketOptions {
        receive_buffer_size: 8192,
        send_buffer_size: 8192,
        keep_alive: true,
        reuse_address: true,
        no_delay: true
      }
    }
  }
  
  // Format IP address for platform
  let format_ip_address = fn(address : String, port : Int, family : AddressFamily) {
    match family {
      AddressFamily::IPv4 => address + ":" + port.to_string(),
      AddressFamily::IPv6 => "[" + address + "]:" + port.to_string(),
      AddressFamily::Unix => address
    }
  }
  
  // Test default socket options
  let windows_socket = get_default_socket_options(Platform::Windows)
  let linux_socket = get_default_socket_options(Platform::Linux)
  let macos_socket = get_default_socket_options(Platform::MacOS)
  
  assert_eq(windows_socket.receive_buffer_size, 8192)
  assert_eq(linux_socket.receive_buffer_size, 212992)
  assert_eq(macos_socket.receive_buffer_size, 65536)
  
  // Test IP address formatting
  let ipv4_address = format_ip_address("192.168.1.1", 8080, AddressFamily::IPv4)
  let ipv6_address = format_ip_address("2001:db8::1", 8080, AddressFamily::IPv6)
  let unix_address = format_ip_address("/tmp/socket", 0, AddressFamily::Unix)
  
  assert_eq(ipv4_address, "192.168.1.1:8080")
  assert_eq(ipv6_address, "[2001:db8::1]:8080")
  assert_eq(unix_address, "/tmp/socket")
}

// Test 7: Time and Date Compatibility
test "time and date compatibility across platforms" {
  // Define time zones
  enum TimeZone {
    UTC
    Local
    Specific(String)
  }
  
  // Define timestamp format
  type Timestamp = {
    seconds : Int
    nanoseconds : Int
    time_zone : TimeZone
  }
  
  // Get current time for platform
  let get_current_time = fn(platform : Platform) {
    // In a real implementation, this would get the actual current time
    // For testing purposes, we'll use a fixed timestamp
    Timestamp {
      seconds: 1609459200,  // 2021-01-01 00:00:00 UTC
      nanoseconds: 0,
      time_zone: TimeZone::UTC
    }
  }
  
  // Format timestamp for platform
  let format_timestamp = fn(timestamp : Timestamp, format : String, platform : Platform) {
    match format {
      "iso8601" => {
        let time_str = "2021-01-01T00:00:00"
        match timestamp.time_zone {
          TimeZone::UTC => time_str + "Z",
          TimeZone::Local => time_str + "+00:00",  // Simplified
          TimeZone::Specific(offset) => time_str + offset
        }
      }
      "unix" => timestamp.seconds.to_string(),
      "platform_specific" => {
        match platform {
          Platform::Windows => "01/01/2021 00:00:00",
          Platform::Linux => "Fri Jan  1 00:00:00 UTC 2021",
          Platform::MacOS => "Fri Jan  1 00:00:00 UTC 2021",
          _ => "2021-01-01 00:00:00"
        }
      }
      _ => "2021-01-01 00:00:00"
    }
  }
  
  // Convert between time zones
  let convert_time_zone = fn(timestamp : Timestamp, new_time_zone : TimeZone) {
    match (timestamp.time_zone, new_time_zone) {
      (TimeZone::UTC, TimeZone::Local) => {
        // Simplified conversion (assuming UTC+0)
        { timestamp | time_zone: new_time_zone }
      }
      (TimeZone::Local, TimeZone::UTC) => {
        // Simplified conversion (assuming UTC+0)
        { timestamp | time_zone: new_time_zone }
      }
      _ => { timestamp | time_zone: new_time_zone }
    }
  }
  
  // Test time functions
  let current_time = get_current_time(Platform::Linux)
  assert_eq(current_time.seconds, 1609459200)
  assert_eq(current_time.nanoseconds, 0)
  match current_time.time_zone {
    TimeZone::UTC => assert_true(true),
    _ => assert_true(false)
  }
  
  // Test timestamp formatting
  let iso8601_format = format_timestamp(current_time, "iso8601", Platform::Linux)
  let unix_format = format_timestamp(current_time, "unix", Platform::Linux)
  let windows_format = format_timestamp(current_time, "platform_specific", Platform::Windows)
  let linux_format = format_timestamp(current_time, "platform_specific", Platform::Linux)
  
  assert_eq(iso8601_format, "2021-01-01T00:00:00Z")
  assert_eq(unix_format, "1609459200")
  assert_eq(windows_format, "01/01/2021 00:00:00")
  assert_eq(linux_format, "Fri Jan  1 00:00:00 UTC 2021")
  
  // Test time zone conversion
  let local_time = convert_time_zone(current_time, TimeZone::Local)
  match local_time.time_zone {
    TimeZone::Local => assert_true(true),
    _ => assert_true(false)
  }
  
  let utc_time = convert_time_zone(local_time, TimeZone::UTC)
  match utc_time.time_zone {
    TimeZone::UTC => assert_true(true),
    _ => assert_true(false)
  }
}

// Test 8: Environment Variable Compatibility
test "environment variable compatibility across platforms" {
  // Define environment variable case sensitivity
  let is_case_sensitive = fn(platform : Platform) {
    match platform {
      Platform::Windows => false,
      _ => true
    }
  }
  
  // Get platform-specific environment variable names
  let get_platform_env_vars = fn(platform : Platform) {
    match platform {
      Platform::Windows => {
        [
          ("PATH", "%SystemRoot%\\system32;%SystemRoot%"),
          ("TEMP", "%SystemRoot%\\TEMP"),
          ("USERPROFILE", "%SystemRoot%\\Users\\%USERNAME%"),
          ("APPDATA", "%USERPROFILE%\\AppData\\Roaming")
        ]
      }
      Platform::Linux => {
        [
          ("PATH", "/usr/local/bin:/usr/bin:/bin"),
          ("TMPDIR", "/tmp"),
          ("HOME", "/home/user"),
          ("XDG_CONFIG_HOME", "$HOME/.config")
        ]
      }
      Platform::MacOS => {
        [
          ("PATH", "/usr/local/bin:/usr/bin:/bin"),
          ("TMPDIR", "/tmp"),
          ("HOME", "/Users/user"),
          ("XDG_CONFIG_HOME", "$HOME/.config")
        ]
      }
      _ => []
    }
  }
  
  // Normalize environment variable name for platform
  let normalize_env_var_name = fn(name : String, platform : Platform) {
    if is_case_sensitive(platform) {
      name
    } else {
      name.to_upper()
    }
  }
  
  // Test case sensitivity
  assert_false(is_case_sensitive(Platform::Windows))
  assert_true(is_case_sensitive(Platform::Linux))
  assert_true(is_case_sensitive(Platform::MacOS))
  
  // Test environment variable normalization
  let windows_normalized = normalize_env_var_name("path", Platform::Windows)
  let linux_normalized = normalize_env_var_name("PATH", Platform::Linux)
  
  assert_eq(windows_normalized, "PATH")
  assert_eq(linux_normalized, "PATH")
  
  // Test platform-specific environment variables
  let windows_env_vars = get_platform_env_vars(Platform::Windows)
  let linux_env_vars = get_platform_env_vars(Platform::Linux)
  let macos_env_vars = get_platform_env_vars(Platform::MacOS)
  
  assert_eq(windows_env_vars.length(), 4)
  assert_eq(linux_env_vars.length(), 4)
  assert_eq(macos_env_vars.length(), 4)
  
  // Verify Windows-specific variables
  assert_eq(windows_env_vars[0].0, "PATH")
  assert_true(windows_env_vars[0].1.contains("%SystemRoot%"))
  
  // Verify Linux-specific variables
  assert_eq(linux_env_vars[0].0, "PATH")
  assert_true(linux_env_vars[0].1.contains("/usr/bin"))
  
  // Verify macOS-specific variables
  assert_eq(macos_env_vars[2].0, "HOME")
  assert_true(macos_env_vars[2].1.contains("/Users"))
}

// Test 9: Library and Dependency Compatibility
test "library and dependency compatibility across platforms" {
  // Define library types
  enum LibraryType {
    Static
    Shared
    Framework
    Package
  }
  
  // Define platform-specific library extensions
  let get_library_extension = fn(lib_type : LibraryType, platform : Platform) {
    match (lib_type, platform) {
      (LibraryType::Static, Platform::Windows) => ".lib",
      (LibraryType::Static, Platform::Linux) => ".a",
      (LibraryType::Static, Platform::MacOS) => ".a",
      
      (LibraryType::Shared, Platform::Windows) => ".dll",
      (LibraryType::Shared, Platform::Linux) => ".so",
      (LibraryType::Shared, Platform::MacOS) => ".dylib",
      
      (LibraryType::Framework, Platform::MacOS) => ".framework",
      
      (LibraryType::Package, Platform::Windows) => ".exe",
      (LibraryType::Package, Platform::Linux) => "",
      (LibraryType::Package, Platform::MacOS) => ".app",
      
      _ => ""
    }
  }
  
  // Get platform-specific library paths
  let get_library_paths = fn(platform : Platform) {
    match platform {
      Platform::Windows => [
        "%SystemRoot%\\system32",
        "%SystemRoot%\\SysWOW64",
        "%ProgramFiles%\\Common Files",
        "%ProgramFiles(x86)%\\Common Files"
      ]
      Platform::Linux => [
        "/usr/lib",
        "/usr/local/lib",
        "/lib",
        "/lib64"
      ]
      Platform::MacOS => [
        "/usr/lib",
        "/usr/local/lib",
        "/System/Library/Frameworks",
        "/Library/Frameworks"
      ]
      _ => []
    }
  }
  
  // Test library extensions
  assert_eq(get_library_extension(LibraryType::Static, Platform::Windows), ".lib")
  assert_eq(get_library_extension(LibraryType::Static, Platform::Linux), ".a")
  assert_eq(get_library_extension(LibraryType::Shared, Platform::Windows), ".dll")
  assert_eq(get_library_extension(LibraryType::Shared, Platform::Linux), ".so")
  assert_eq(get_library_extension(LibraryType::Shared, Platform::MacOS), ".dylib")
  assert_eq(get_library_extension(LibraryType::Framework, Platform::MacOS), ".framework")
  
  // Test library paths
  let windows_paths = get_library_paths(Platform::Windows)
  let linux_paths = get_library_paths(Platform::Linux)
  let macos_paths = get_library_paths(Platform::MacOS)
  
  assert_true(windows_paths.length() > 0)
  assert_true(linux_paths.length() > 0)
  assert_true(macos_paths.length() > 0)
  
  // Verify Windows-specific paths
  assert_true(windows_paths[0].contains("%SystemRoot%"))
  
  // Verify Linux-specific paths
  assert_eq(linux_paths[0], "/usr/lib")
  
  // Verify macOS-specific paths
  assert_true(macos_paths[2].contains("Frameworks"))
}

// Test 10: Performance Characteristics Across Platforms
test "performance characteristics across platforms" {
  // Define performance metrics
  type PerformanceMetrics = {
    cpu_usage : Float
    memory_usage : Int
    disk_io_rate : Int
    network_io_rate : Int
    context_switches : Int
  }
  
  // Get platform-specific performance characteristics
  let get_performance_characteristics = fn(platform : Platform) {
    match platform {
      Platform::Windows => {
        PerformanceMetrics {
          cpu_usage: 0.25,      // 25%
          memory_usage: 1048576, // 1MB
          disk_io_rate: 10485760, // 10MB/s
          network_io_rate: 1048576, // 1MB/s
          context_switches: 1000
        }
      }
      Platform::Linux => {
        PerformanceMetrics {
          cpu_usage: 0.20,      // 20%
          memory_usage: 2097152, // 2MB
          disk_io_rate: 20971520, // 20MB/s
          network_io_rate: 2097152, // 2MB/s
          context_switches: 1500
        }
      }
      Platform::MacOS => {
        PerformanceMetrics {
          cpu_usage: 0.30,      // 30%
          memory_usage: 1572864, // 1.5MB
          disk_io_rate: 15728640, // 15MB/s
          network_io_rate: 1572864, // 1.5MB/s
          context_switches: 1200
        }
      }
      _ => {
        PerformanceMetrics {
          cpu_usage: 0.0,
          memory_usage: 0,
          disk_io_rate: 0,
          network_io_rate: 0,
          context_switches: 0
        }
      }
    }
  }
  
  // Benchmark operation on different platforms
  let benchmark_operation = fn(platform : Platform, operation : String) {
    let start_time = get_current_time_millis()
    
    // Simulate different performance based on platform
    let iterations = match platform {
      Platform::Windows => 100000,
      Platform::Linux => 120000,
      Platform::MacOS => 90000,
      _ => 50000
    }
    
    let mut result = 0
    for i in 0..<iterations {
      result = result + i  // Simple operation
    }
    
    let end_time = get_current_time_millis()
    let duration = end_time - start_time
    
    {
      "platform": platform,
      "operation": operation,
      "iterations": iterations,
      "duration_ms": duration,
      "operations_per_second": if duration > 0 { (iterations.to_float() / duration.to_float()) * 1000.0 } else { 0.0 },
      "result": result
    }
  }
  
  // Test performance characteristics
  let windows_perf = get_performance_characteristics(Platform::Windows)
  let linux_perf = get_performance_characteristics(Platform::Linux)
  let macos_perf = get_performance_characteristics(Platform::MacOS)
  
  // Verify different performance characteristics
  assert_eq(windows_perf.cpu_usage, 0.25)
  assert_eq(linux_perf.cpu_usage, 0.20)
  assert_eq(macos_perf.cpu_usage, 0.30)
  
  assert_eq(windows_perf.memory_usage, 1048576)
  assert_eq(linux_perf.memory_usage, 2097152)
  assert_eq(macos_perf.memory_usage, 1572864)
  
  // Test benchmarking
  let windows_benchmark = benchmark_operation(Platform::Windows, "simple_loop")
  let linux_benchmark = benchmark_operation(Platform::Linux, "simple_loop")
  let macos_benchmark = benchmark_operation(Platform::MacOS, "simple_loop")
  
  // Verify benchmark results
  assert_eq(windows_benchmark["iterations"], 100000)
  assert_eq(linux_benchmark["iterations"], 120000)
  assert_eq(macos_benchmark["iterations"], 90000)
  
  assert_true(windows_benchmark["operations_per_second"] > 0.0)
  assert_true(linux_benchmark["operations_per_second"] > 0.0)
  assert_true(macos_benchmark["operations_per_second"] > 0.0)
  
  // Linux should have more iterations and potentially higher ops/sec
  assert_true(linux_benchmark["iterations"] > windows_benchmark["iterations"])
}

// Helper function to get current time in milliseconds
fn get_current_time_millis() -> Int {
  // Implementation would depend on the available time functions in MoonBit
  // This is a placeholder for the actual implementation
  0
}