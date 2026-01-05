// Azimuth Cross-Platform Compatibility Test Suite
// This file contains high-quality test cases for cross-platform compatibility

// Test 1: Platform Detection and Adaptation
test "platform detection and adaptation" {
  // Define platform types
  enum Platform {
    Windows
    Linux
    MacOS
    Android
    IOS
    WebAssembly
    Unknown
  }
  
  // Define platform info
  type PlatformInfo = {
    platform: Platform,
    arch: String,
    version: String,
    features: Array[String]
  }
  
  // Define telemetry configuration
  type TelemetryConfig = {
    buffer_size: Int,
    batch_size: Int,
    timeout_ms: Int,
    max_concurrent_operations: Int,
    platform_specific: Map[String, String]
  }
  
  // Detect platform (simulated)
  let detect_platform = fn(user_agent: String, system_info: Map[String, String]) {
    if user_agent.contains("Windows") {
      Platform::Windows
    } else if user_agent.contains("Linux") {
      Platform::Linux
    } else if user_agent.contains("Mac") {
      Platform::MacOS
    } else if user_agent.contains("Android") {
      Platform::Android
    } else if user_agent.contains("iOS") {
      Platform::IOS
    } else if user_agent.contains("WASM") {
      Platform::WebAssembly
    } else {
      Platform::Unknown
    }
  }
  
  // Get platform-specific configuration
  let get_platform_config = fn(platform: Platform) {
    match platform {
      Windows => {
        buffer_size: 8192,
        batch_size: 100,
        timeout_ms: 5000,
        max_concurrent_operations: 10,
        platform_specific: [
          ("file.separator", "\\"),
          ("line.separator", "\r\n"),
          ("temp.dir", "C:\\Temp\\")
        ]
      }
      Linux => {
        buffer_size: 4096,
        batch_size: 50,
        timeout_ms: 3000,
        max_concurrent_operations: 20,
        platform_specific: [
          ("file.separator", "/"),
          ("line.separator", "\n"),
          ("temp.dir", "/tmp/")
        ]
      }
      MacOS => {
        buffer_size: 4096,
        batch_size: 75,
        timeout_ms: 4000,
        max_concurrent_operations: 15,
        platform_specific: [
          ("file.separator", "/"),
          ("line.separator", "\n"),
          ("temp.dir", "/tmp/")
        ]
      }
      Android => {
        buffer_size: 2048,
        batch_size: 25,
        timeout_ms: 6000,
        max_concurrent_operations: 5,
        platform_specific: [
          ("file.separator", "/"),
          ("line.separator", "\n"),
          ("temp.dir", "/data/local/tmp/")
        ]
      }
      IOS => {
        buffer_size: 2048,
        batch_size: 30,
        timeout_ms: 5500,
        max_concurrent_operations: 8,
        platform_specific: [
          ("file.separator", "/"),
          ("line.separator", "\n"),
          ("temp.dir", "/tmp/")
        ]
      }
      WebAssembly => {
        buffer_size: 1024,
        batch_size: 10,
        timeout_ms: 10000,
        max_concurrent_operations: 3,
        platform_specific: [
          ("file.separator", "/"),
          ("line.separator", "\n"),
          ("temp.dir", "/tmp/")
        ]
      }
      Unknown => {
        buffer_size: 1024,
        batch_size: 10,
        timeout_ms: 5000,
        max_concurrent_operations: 5,
        platform_specific: []
      }
    }
  }
  
  // Adapt file path for platform
  let adapt_file_path = fn(path: String, platform: Platform) {
    let separator = match platform {
      Windows => "\\"
      _ => "/"
    }
    
    // Replace all forward slashes with platform-specific separator
    let parts = path.split("/")
    parts.join(separator)
  }
  
  // Test platform detection
  let windows_ua = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
  let linux_ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36"
  let mac_ua = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
  let android_ua = "Mozilla/5.0 (Linux; Android 10; SM-G975F) AppleWebKit/537.36"
  let ios_ua = "Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X) AppleWebKit/605.1.15"
  let wasm_ua = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36 Edg/91.0.864.59 WASM"
  
  let windows_platform = detect_platform(windows_ua, [])
  let linux_platform = detect_platform(linux_ua, [])
  let mac_platform = detect_platform(mac_ua, [])
  let android_platform = detect_platform(android_ua, [])
  let ios_platform = detect_platform(ios_ua, [])
  let wasm_platform = detect_platform(wasm_ua, [])
  
  assert_eq(windows_platform, Platform::Windows)
  assert_eq(linux_platform, Platform::Linux)
  assert_eq(mac_platform, Platform::MacOS)
  assert_eq(android_platform, Platform::Android)
  assert_eq(ios_platform, Platform::IOS)
  assert_eq(wasm_platform, Platform::WebAssembly)
  
  // Test platform-specific configuration
  let windows_config = get_platform_config(Platform::Windows)
  let linux_config = get_platform_config(Platform::Linux)
  let android_config = get_platform_config(Platform::Android)
  let wasm_config = get_platform_config(Platform::WebAssembly)
  
  assert_eq(windows_config.buffer_size, 8192)
  assert_eq(linux_config.buffer_size, 4096)
  assert_eq(android_config.buffer_size, 2048)
  assert_eq(wasm_config.buffer_size, 1024)
  
  assert_eq(windows_config.platform_specific[0], ("file.separator", "\\"))
  assert_eq(linux_config.platform_specific[0], ("file.separator", "/"))
  
  // Test file path adaptation
  let unix_path = "logs/telemetry/data.txt"
  
  let windows_path = adapt_file_path(unix_path, Platform::Windows)
  let linux_path = adapt_file_path(unix_path, Platform::Linux)
  
  assert_eq(windows_path, "logs\\telemetry\\data.txt")
  assert_eq(linux_path, "logs/telemetry/data.txt")
}

// Test 2: Endianness Handling
test "endianness handling" {
  // Define endianness
  enum Endianness {
    LittleEndian
    BigEndian
  }
  
  // Define byte order marker
  type ByteOrderMarker = {
    endianness: Endianness,
    bytes: Array[Int]
  }
  
  // Detect system endianness
  let detect_endianness = fn() {
    // Simulate detection by checking byte order of a known value
    let test_bytes = [1, 0, 0, 0]  // Little endian representation of 1
    
    if test_bytes[0] == 1 {
      Endianness::LittleEndian
    } else {
      Endianness::BigEndian
    }
  }
  
  // Convert 32-bit integer to bytes
  let int_to_bytes = fn(value: Int, endianness: Endianness) {
    let bytes = [
      value & 0xFF,
      (value >> 8) & 0xFF,
      (value >> 16) & 0xFF,
      (value >> 24) & 0xFF
    ]
    
    match endianness {
      LittleEndian => bytes
      BigEndian => [bytes[3], bytes[2], bytes[1], bytes[0]]
    }
  }
  
  // Convert bytes to 32-bit integer
  let bytes_to_int = fn(bytes: Array[Int], endianness: Endianness) {
    if bytes.length() < 4 {
      0
    } else {
      match endianness {
        LittleEndian => {
          bytes[0] | (bytes[1] << 8) | (bytes[2] << 16) | (bytes[3] << 24)
        }
        BigEndian => {
          bytes[3] | (bytes[2] << 8) | (bytes[1] << 16) | (bytes[0] << 24)
        }
      }
    }
  }
  
  // Convert byte order
  let convert_byte_order = fn(bytes: Array[Int], from_endianness: Endianness, to_endianness: Endianness) {
    if from_endianness == to_endianness {
      bytes
    } else {
      // Reverse byte order
      let mut reversed = []
      for i in 0..bytes.length() {
        reversed = reversed.push(bytes[bytes.length() - 1 - i])
      }
      reversed
    }
  }
  
  // Test endianness handling
  let system_endianness = detect_endianness()
  
  // Test little endian conversion
  let little_endian_bytes = int_to_bytes(0x12345678, Endianness::LittleEndian)
  assert_eq(little_endian_bytes, [0x78, 0x56, 0x34, 0x12])
  
  let little_endian_value = bytes_to_int(little_endian_bytes, Endianness::LittleEndian)
  assert_eq(little_endian_value, 0x12345678)
  
  // Test big endian conversion
  let big_endian_bytes = int_to_bytes(0x12345678, Endianness::BigEndian)
  assert_eq(big_endian_bytes, [0x12, 0x34, 0x56, 0x78])
  
  let big_endian_value = bytes_to_int(big_endian_bytes, Endianness::BigEndian)
  assert_eq(big_endian_value, 0x12345678)
  
  // Test byte order conversion
  let converted_to_big = convert_byte_order(little_endian_bytes, Endianness::LittleEndian, Endianness::BigEndian)
  assert_eq(converted_to_big, big_endian_bytes)
  
  let converted_to_little = convert_byte_order(big_endian_bytes, Endianness::BigEndian, Endianness::LittleEndian)
  assert_eq(converted_to_little, little_endian_bytes)
  
  // Test cross-platform value consistency
  let test_values = [0, 1, 255, 256, 65535, 65536, 16777215, 16777216, -1, -256]
  
  for i in 0..test_values.length() {
    let value = test_values[i]
    
    // Convert to little endian bytes
    let le_bytes = int_to_bytes(value, Endianness::LittleEndian)
    
    // Convert to big endian bytes
    let be_bytes = int_to_bytes(value, Endianness::BigEndian)
    
    // Convert back from little endian
    let le_value = bytes_to_int(le_bytes, Endianness::LittleEndian)
    
    // Convert back from big endian
    let be_value = bytes_to_int(be_bytes, Endianness::BigEndian)
    
    assert_eq(le_value, value)
    assert_eq(be_value, value)
  }
}

// Test 3: Character Encoding Compatibility
test "character encoding compatibility" {
  // Define encoding types
  enum Encoding {
    UTF8
    UTF16
    UTF32
    ASCII
    Latin1
  }
  
  // Define encoded string
  type EncodedString = {
    encoding: Encoding,
    bytes: Array[Int]
  }
  
  // Convert string to UTF-8 bytes
  let string_to_utf8 = fn(str: String) {
    let mut bytes = []
    let chars = str.to_char_array()
    
    for i in 0..chars.length() {
      let char_code = chars[i].to_int()
      
      if char_code <= 0x7F {
        // 1-byte sequence
        bytes = bytes.push(char_code)
      } else if char_code <= 0x7FF {
        // 2-byte sequence
        bytes = bytes.push(0xC0 | (char_code >> 6))
        bytes = bytes.push(0x80 | (char_code & 0x3F))
      } else if char_code <= 0xFFFF {
        // 3-byte sequence
        bytes = bytes.push(0xE0 | (char_code >> 12))
        bytes = bytes.push(0x80 | ((char_code >> 6) & 0x3F))
        bytes = bytes.push(0x80 | (char_code & 0x3F))
      } else {
        // 4-byte sequence
        bytes = bytes.push(0xF0 | (char_code >> 18))
        bytes = bytes.push(0x80 | ((char_code >> 12) & 0x3F))
        bytes = bytes.push(0x80 | ((char_code >> 6) & 0x3F))
        bytes = bytes.push(0x80 | (char_code & 0x3F))
      }
    }
    
    bytes
  }
  
  // Convert UTF-8 bytes to string
  let utf8_to_string = fn(bytes: Array[Int]) {
    let mut chars = []
    let mut i = 0
    
    while i < bytes.length() {
      let byte1 = bytes[i]
      
      if byte1 <= 0x7F {
        // 1-byte sequence
        chars = chars.push(byte1.to_char())
        i = i + 1
      } else if (byte1 & 0xE0) == 0xC0 and i + 1 < bytes.length() {
        // 2-byte sequence
        let byte2 = bytes[i + 1]
        let char_code = ((byte1 & 0x1F) << 6) | (byte2 & 0x3F)
        chars = chars.push(char_code.to_char())
        i = i + 2
      } else if (byte1 & 0xF0) == 0xE0 and i + 2 < bytes.length() {
        // 3-byte sequence
        let byte2 = bytes[i + 1]
        let byte3 = bytes[i + 2]
        let char_code = ((byte1 & 0x0F) << 12) | ((byte2 & 0x3F) << 6) | (byte3 & 0x3F)
        chars = chars.push(char_code.to_char())
        i = i + 3
      } else if (byte1 & 0xF8) == 0xF0 and i + 3 < bytes.length() {
        // 4-byte sequence
        let byte2 = bytes[i + 1]
        let byte3 = bytes[i + 2]
        let byte4 = bytes[i + 3]
        let char_code = ((byte1 & 0x07) << 18) | ((byte2 & 0x3F) << 12) | ((byte3 & 0x3F) << 6) | (byte4 & 0x3F)
        chars = chars.push(char_code.to_char())
        i = i + 4
      } else {
        // Invalid sequence, skip
        i = i + 1
      }
    }
    
    chars.join("")
  }
  
  // Convert between encodings
  let convert_encoding = fn(source: EncodedString, target_encoding: Encoding) {
    // First decode to string
    let decoded_string = match source.encoding {
      UTF8 => utf8_to_string(source.bytes)
      _ => "unsupported"  // Simplified for this test
    }
    
    // Then encode to target
    match target_encoding {
      UTF8 => {
        encoding: UTF8,
        bytes: string_to_utf8(decoded_string)
      }
      _ => {
        encoding: target_encoding,
        bytes: []  // Simplified for this test
      }
    }
  }
  
  // Test character encoding
  let test_strings = [
    "Hello, World!",
    "Café",
    "Москва",
    "北京",
    "🌍🚀",
    "Mixed: ABCçñß中🌟"
  ]
  
  for i in 0..test_strings.length() {
    let original = test_strings[i]
    
    // Encode to UTF-8
    let utf8_bytes = string_to_utf8(original)
    let utf8_encoded = {
      encoding: UTF8,
      bytes: utf8_bytes
    }
    
    // Decode back to string
    let decoded = utf8_to_string(utf8_bytes)
    
    assert_eq(original, decoded)
    
    // Test encoding conversion
    let converted = convert_encoding(utf8_encoded, UTF8)
    assert_eq(converted.encoding, UTF8)
    assert_eq(converted.bytes, utf8_bytes)
  }
  
  // Test ASCII compatibility
  let ascii_string = "Hello, ASCII!"
  let ascii_bytes = string_to_utf8(ascii_string)
  
  // All bytes should be <= 127 for pure ASCII
  for i in 0..ascii_bytes.length() {
    assert_true(ascii_bytes[i] <= 127)
  }
  
  let ascii_decoded = utf8_to_string(ascii_bytes)
  assert_eq(ascii_string, ascii_decoded)
}

// Test 4: Time Zone and Locale Handling
test "time zone and locale handling" {
  // Define time zone
  type TimeZone = {
    name: String,
    offset_hours: Int,
    offset_minutes: Int,
    dst_observed: Bool
  }
  
  // Define locale
  type Locale = {
    language: String,
    country: String,
    script: Option[String],
    variant: Option[String]
  }
  
  // Define formatted date/time
  type FormattedDateTime = {
    year: Int,
    month: Int,
    day: Int,
    hour: Int,
    minute: Int,
    second: Int,
    formatted: String
  }
  
  // Create common time zones
  let create_time_zones = fn() {
    [
      { name: "UTC", offset_hours: 0, offset_minutes: 0, dst_observed: false },
      { name: "America/New_York", offset_hours: -5, offset_minutes: 0, dst_observed: true },
      { name: "Europe/London", offset_hours: 0, offset_minutes: 0, dst_observed: true },
      { name: "Europe/Paris", offset_hours: 1, offset_minutes: 0, dst_observed: true },
      { name: "Asia/Tokyo", offset_hours: 9, offset_minutes: 0, dst_observed: false },
      { name: "Australia/Sydney", offset_hours: 10, offset_minutes: 0, dst_observed: true }
    ]
  }
  
  // Create common locales
  let create_locales = fn() {
    [
      { language: "en", country: "US", script: None, variant: None },
      { language: "en", country: "GB", script: None, variant: None },
      { language: "fr", country: "FR", script: None, variant: None },
      { language: "de", country: "DE", script: None, variant: None },
      { language: "ja", country: "JP", script: None, variant: None },
      { language: "zh", country: "CN", script: None, variant: None }
    ]
  }
  
  // Convert timestamp to different time zones
  let convert_time_zone = fn(timestamp: Int, from_tz: TimeZone, to_tz: TimeZone) {
    let from_offset_minutes = from_tz.offset_hours * 60 + from_tz.offset_minutes
    let to_offset_minutes = to_tz.offset_hours * 60 + to_tz.offset_minutes
    let offset_diff = to_offset_minutes - from_offset_minutes
    
    timestamp + (offset_diff * 60)
  }
  
  // Format date/time for locale
  let format_date_time = fn(timestamp: Int, locale: Locale, tz: TimeZone) {
    // Simplified formatting
    let days = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
    let months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    
    // Convert timestamp to date components (simplified)
    let day_of_week = (timestamp / 86400) % 7
    let day_of_month = ((timestamp / 86400) % 30) + 1
    let month = ((timestamp / 2592000) % 12) + 1
    let year = 1970 + (timestamp / 31536000)
    let hour = (timestamp / 3600) % 24
    let minute = (timestamp / 60) % 60
    let second = timestamp % 60
    
    // Format based on locale
    let formatted = match locale.language {
      "en" => {
        match locale.country {
          "US" => days[day_of_week] + ", " + months[month - 1] + " " + day_of_month.to_string() + ", " + year.to_string() + " " + hour.to_string() + ":" + minute.to_string().pad_left(2, "0") + ":" + second.to_string().pad_left(2, "0") + " " + tz.name
          "GB" => day_of_month.to_string() + " " + months[month - 1] + " " + year.to_string() + " " + hour.to_string() + ":" + minute.to_string().pad_left(2, "0") + ":" + second.to_string().pad_left(2, "0")
          _ => year.to_string() + "-" + month.to_string().pad_left(2, "0") + "-" + day_of_month.to_string().pad_left(2, "0") + " " + hour.to_string() + ":" + minute.to_string().pad_left(2, "0") + ":" + second.to_string().pad_left(2, "0")
        }
      }
      "fr" => day_of_month.to_string() + " " + ["janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre", "octobre", "novembre", "décembre"][month - 1] + " " + year.to_string() + " à " + hour.to_string() + "h" + minute.to_string()
      "de" => day_of_month.to_string() + ". " + ["Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"][month - 1] + " " + year.to_string() + ", " + hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
      "ja" => year.to_string() + "年" + month.to_string() + "月" + day_of_month.to_string() + "日 " + hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
      "zh" => year.to_string() + "年" + month.to_string() + "月" + day_of_month.to_string() + "日 " + hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
      _ => year.to_string() + "-" + month.to_string().pad_left(2, "0") + "-" + day_of_month.to_string().pad_left(2, "0") + " " + hour.to_string() + ":" + minute.to_string().pad_left(2, "0") + ":" + second.to_string().pad_left(2, "0")
    }
    
    {
      year: year,
      month: month,
      day: day_of_month,
      hour: hour,
      minute: minute,
      second: second,
      formatted: formatted
    }
  }
  
  // Test time zone and locale handling
  let time_zones = create_time_zones()
  let locales = create_locales()
  
  let utc = time_zones[0]
  let new_york = time_zones[1]
  let tokyo = time_zones[4]
  
  let en_us = locales[0]
  let fr_fr = locales[2]
  let ja_jp = locales[4]
  
  // Test timestamp: 2023-01-01 12:00:00 UTC
  let timestamp = 1672574400
  
  // Convert to different time zones
  let ny_timestamp = convert_time_zone(timestamp, utc, new_york)
  let tokyo_timestamp = convert_time_zone(timestamp, utc, tokyo)
  
  // 12:00 UTC = 07:00 EST (UTC-5)
  // 12:00 UTC = 21:00 JST (UTC+9)
  assert_eq(ny_timestamp, timestamp - 5 * 3600)
  assert_eq(tokyo_timestamp, timestamp + 9 * 3600)
  
  // Format for different locales
  let utc_formatted = format_date_time(timestamp, en_us, utc)
  let ny_formatted = format_date_time(ny_timestamp, en_us, new_york)
  let tokyo_formatted = format_date_time(tokyo_timestamp, ja_jp, tokyo)
  let fr_formatted = format_date_time(timestamp, fr_fr, utc)
  
  // Verify formatting
  assert_true(utc_formatted.formatted.contains("2023"))
  assert_true(ny_formatted.formatted.contains("America/New_York"))
  assert_true(tokyo_formatted.formatted.contains("年"))
  assert_true(fr_formatted.formatted.contains("janvier"))
  
  // Verify date components are consistent
  assert_eq(utc_formatted.year, 2023)
  assert_eq(ny_formatted.year, 2022)  // Still December 31 in New York
  assert_eq(tokyo_formatted.year, 2023)  // January 1 in Tokyo
  
  assert_eq(utc_formatted.month, 1)
  assert_eq(ny_formatted.month, 12)
  assert_eq(tokyo_formatted.month, 1)
}

// Test 5: File System Path Handling
test "file system path handling" {
  // Define path components
  type PathComponents = {
    root: Option[String],
    segments: Array[String],
    file: Option[String],
    extension: Option[String]
  }
  
  // Define path separator
  type PathSeparator = {
    value: String,
    pattern: String
  }
  
  // Create path separators for different platforms
  let create_path_separators = fn() {
    [
      { value: "/", pattern: "/" },  // Unix-like
      { value: "\\", pattern: "\\" },  // Windows
      { value: "/", pattern: "/" }  // Web/URL
    ]
  }
  
  // Parse path into components
  let parse_path = fn(path: String, separator: PathSeparator) {
    let parts = path.split(separator.pattern)
    
    let (root_parts, path_parts) = if parts.length() > 0 and parts[0] == "" {
      let root = Some(separator.value)
      let remaining = parts.slice(1)
      (root, remaining)
    } else {
      (None, parts)
    }
    
    let (segments, file_part) = if path_parts.length() > 0 {
      let last_part = path_parts[path_parts.length() - 1]
      
      if last_part.contains(".") {
        let file_segments = last_part.split(".")
        let file_name = file_segments[0]
        let file_ext = if file_segments.length() > 1 {
          Some(file_segments[1])
        } else {
          None
        }
        
        let path_segments = if path_parts.length() > 1 {
          path_parts.slice(0, path_parts.length() - 1)
        } else {
          []
        }
        
        (path_segments, Some((file_name, file_ext)))
      } else {
        (path_parts, None)
      }
    } else {
      ([], None)
    }
    
    let file = match file_part {
      Some((name, _)) => Some(name)
      None => None
    }
    
    let extension = match file_part {
      Some((_, ext)) => ext
      None => None
    }
    
    {
      root: root_parts,
      segments: segments,
      file: file,
      extension: extension
    }
  }
  
  // Build path from components
  let build_path = fn(components: PathComponents, separator: PathSeparator) {
    let mut path_parts = []
    
    match components.root {
      Some(root) => path_parts = path_parts.push(root)
      None => {}
    }
    
    for i in 0..components.segments.length() {
      path_parts = path_parts.push(components.segments[i])
    }
    
    let path_without_file = path_parts.join(separator.value)
    
    match components.file {
      Some(file) => {
        let file_with_ext = match components.extension {
          Some(ext) => file + "." + ext
          None => file
        }
        
        if path_without_file == "" {
          file_with_ext
        } else {
          path_without_file + separator.value + file_with_ext
        }
      }
      None => path_without_file
    }
  }
  
  // Normalize path (remove redundant separators, resolve . and ..)
  let normalize_path = fn(path: String, separator: PathSeparator) {
    let parts = path.split(separator.pattern)
    let mut normalized = []
    
    for i in 0..parts.length() {
      let part = parts[i]
      
      if part == "" or part == "." {
        // Skip empty parts and current directory
        continue
      } else if part == ".." {
        // Go up one directory
        if normalized.length() > 0 {
          normalized = normalized.slice(0, normalized.length() - 1)
        }
      } else {
        normalized = normalized.push(part)
      }
    }
    
    normalized.join(separator.value)
  }
  
  // Test file system path handling
  let separators = create_path_separators()
  let unix_sep = separators[0]
  let windows_sep = separators[1]
  
  // Test Unix path parsing
  let unix_path = "/home/user/documents/telemetry/data.txt"
  let unix_components = parse_path(unix_path, unix_sep)
  
  assert_eq(unix_components.root, Some("/"))
  assert_eq(unix_components.segments, ["home", "user", "documents", "telemetry"])
  assert_eq(unix_components.file, Some("data"))
  assert_eq(unix_components.extension, Some("txt"))
  
  // Test Windows path parsing
  let windows_path = "C:\\Program Files\\Telemetry\\config.json"
  let windows_components = parse_path(windows_path, windows_sep)
  
  assert_eq(windows_components.root, Some("C:\\"))
  assert_eq(windows_components.segments, ["Program Files", "Telemetry"])
  assert_eq(windows_components.file, Some("config"))
  assert_eq(windows_components.extension, Some("json"))
  
  // Test path building
  let rebuilt_unix = build_path(unix_components, unix_sep)
  let rebuilt_windows = build_path(windows_components, windows_sep)
  
  assert_eq(rebuilt_unix, "/home/user/documents/telemetry/data.txt")
  assert_eq(rebuilt_windows, "C:\\Program Files\\Telemetry\\config.json")
  
  // Test path normalization
  let messy_unix_path = "/home//user/./documents/../telemetry/./data.txt"
  let normalized_unix = normalize_path(messy_unix_path, unix_sep)
  assert_eq(normalized_unix, "/home/user/telemetry/data.txt")
  
  let messy_windows_path = "C:\\Program Files\\\\Telemetry\\.\\config.json"
  let normalized_windows = normalize_path(messy_windows_path, windows_sep)
  assert_eq(normalized_windows, "C:\\Program Files\\Telemetry\\config.json")
  
  // Test cross-platform path conversion
  let cross_platform_path = "logs/telemetry/data.csv"
  
  let unix_parsed = parse_path(cross_platform_path, unix_sep)
  let windows_converted = build_path(unix_parsed, windows_sep)
  
  assert_eq(windows_converted, "logs\\telemetry\\data.csv")
}

// Test 6: Network Protocol Compatibility
test "network protocol compatibility" {
  // Define protocol version
  type ProtocolVersion = {
    major: Int,
    minor: Int,
    patch: Int
  }
  
  // Define protocol message
  type ProtocolMessage = {
    version: ProtocolVersion,
    message_type: String,
    headers: Map[String, String],
    body: Array[Int]
  }
  
  // Define protocol compatibility
  enum Compatibility {
    Compatible
    Incompatible
    PartiallyCompatible(Array[String])  // List of incompatible features
  }
  
  // Create protocol versions
  let create_versions = fn() {
    [
      { major: 1, minor: 0, patch: 0 },  // v1.0.0
      { major: 1, minor: 1, patch: 0 },  // v1.1.0
      { major: 1, minor: 2, patch: 0 },  // v1.2.0
      { major: 2, minor: 0, patch: 0 }   // v2.0.0
    ]
  }
  
  // Check version compatibility
  let check_compatibility = fn(client_version: ProtocolVersion, server_version: ProtocolVersion) {
    if client_version.major == server_version.major {
      if client_version.minor <= server_version.minor {
        // Client is older or same minor version - compatible
        Compatible
      } else {
        // Client is newer minor version - partially compatible
        PartiallyCompatible(["new-features"])
      }
    } else {
      // Different major versions - incompatible
      Incompatible
    }
  }
  
  // Serialize message to bytes
  let serialize_message = fn(message: ProtocolMessage) {
    let mut bytes = []
    
    // Serialize version
    bytes = bytes.push(message.version.major)
    bytes = bytes.push(message.version.minor)
    bytes = bytes.push(message.version.patch)
    
    // Serialize message type length and type
    let type_bytes = message.message_type.to_utf8_array()
    bytes = bytes.push(type_bytes.length())
    for i in 0..type_bytes.length() {
      bytes = bytes.push(type_bytes[i])
    }
    
    // Serialize headers count
    bytes = bytes.push(message.headers.length())
    
    // Serialize headers
    for i in 0..message.headers.length() {
      let header = message.headers[i]
      let key_bytes = header.0.to_utf8_array()
      let value_bytes = header.1.to_utf8_array()
      
      bytes = bytes.push(key_bytes.length())
      for j in 0..key_bytes.length() {
        bytes = bytes.push(key_bytes[j])
      }
      
      bytes = bytes.push(value_bytes.length())
      for j in 0..value_bytes.length() {
        bytes = bytes.push(value_bytes[j])
      }
    }
    
    // Serialize body length and body
    bytes = bytes.push(message.body.length())
    for i in 0..message.body.length() {
      bytes = bytes.push(message.body[i])
    }
    
    bytes
  }
  
  // Deserialize message from bytes
  let deserialize_message = fn(bytes: Array[Int]) {
    let mut index = 0
    
    // Deserialize version
    let major = bytes[index]
    index = index + 1
    let minor = bytes[index]
    index = index + 1
    let patch = bytes[index]
    index = index + 1
    
    let version = { major: major, minor: minor, patch: patch }
    
    // Deserialize message type
    let type_length = bytes[index]
    index = index + 1
    
    let mut type_bytes = []
    for i in 0..type_length {
      type_bytes = type_bytes.push(bytes[index])
      index = index + 1
    }
    let message_type = type_bytes.to_ascii_string()
    
    // Deserialize headers
    let headers_count = bytes[index]
    index = index + 1
    
    let mut headers = []
    for i in 0..headers_count {
      // Key
      let key_length = bytes[index]
      index = index + 1
      
      let mut key_bytes = []
      for j in 0..key_length {
        key_bytes = key_bytes.push(bytes[index])
        index = index + 1
      }
      let key = key_bytes.to_ascii_string()
      
      // Value
      let value_length = bytes[index]
      index = index + 1
      
      let mut value_bytes = []
      for j in 0..value_length {
        value_bytes = value_bytes.push(bytes[index])
        index = index + 1
      }
      let value = value_bytes.to_ascii_string()
      
      headers = headers.push((key, value))
    }
    
    // Deserialize body
    let body_length = bytes[index]
    index = index + 1
    
    let mut body = []
    for i in 0..body_length {
      body = body.push(bytes[index])
      index = index + 1
    }
    
    {
      version: version,
      message_type: message_type,
      headers: headers,
      body: body
    }
  }
  
  // Test network protocol compatibility
  let versions = create_versions()
  let v100 = versions[0]
  let v110 = versions[1]
  let v120 = versions[2]
  let v200 = versions[3]
  
  // Test compatibility checks
  let compat_v100_v110 = check_compatibility(v100, v110)
  let compat_v110_v100 = check_compatibility(v110, v100)
  let compat_v100_v200 = check_compatibility(v100, v200)
  let compat_v200_v100 = check_compatibility(v200, v100)
  
  assert_eq(compat_v100_v110, Compatible)  // Older client, newer server (same major)
  assert_eq(compat_v110_v100, PartiallyCompatible(["new-features"]))  // Newer client, older server
  assert_eq(compat_v100_v200, Incompatible)  // Different major versions
  assert_eq(compat_v200_v100, Incompatible)  // Different major versions
  
  // Test message serialization/deserialization
  let original_message = {
    version: v110,
    message_type: "telemetry-data",
    headers: [
      ("trace-id", "0af7651916cd43dd8448eb211c80319c"),
      ("content-type", "application/json")
    ],
    body: [1, 2, 3, 4, 5]
  }
  
  let serialized = serialize_message(original_message)
  let deserialized = deserialize_message(serialized)
  
  assert_eq(deserialized.version.major, original_message.version.major)
  assert_eq(deserialized.version.minor, original_message.version.minor)
  assert_eq(deserialized.version.patch, original_message.version.patch)
  assert_eq(deserialized.message_type, original_message.message_type)
  assert_eq(deserialized.headers, original_message.headers)
  assert_eq(deserialized.body, original_message.body)
}