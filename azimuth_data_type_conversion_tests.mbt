// Azimuth 数据类型转换测试用例
// 包含遥测系统中常用的数据类型转换和验证

// 测试1: 基础数值类型转换
test "基础数值类型转换" {
  // 整数到浮点数转换
  let int_val = 42
  let float_val = int_val as Float
  assert_eq(float_val, 42.0)
  
  // 浮点数到整数转换（截断）
  let float_to_int = 3.7 as Int
  assert_eq(float_to_int, 3)
  
  // 字符串到整数转换
  let str_to_int = fn(s: String) {
    let mut result = 0
    let mut is_negative = false
    let chars = s.to_char_array()
    let mut i = 0
    
    // 处理负号
    if i < chars.length() && chars[i] == '-' {
      is_negative = true
      i = i + 1
    }
    
    // 转换数字
    while i < chars.length() {
      let c = chars[i]
      if c >= '0' and c <= '9' {
        result = result * 10 + (c.to_int() - '0'.to_int())
      }
      i = i + 1
    }
    
    if is_negative { -result } else { result }
  }
  
  assert_eq(str_to_int("123"), 123)
  assert_eq(str_to_int("-456"), -456)
  assert_eq(str_to_int("0"), 0)
  
  // 整数到字符串转换
  let int_to_str = fn(n: Int) {
    if n == 0 {
      return "0"
    }
    
    let mut num = n
    let mut is_neg = false
    if num < 0 {
      is_neg = true
      num = -num
    }
    
    let mut chars = []
    while num > 0 {
      let digit = num % 10
      chars = chars.push((digit + '0'.to_int()).to_char())
      num = num / 10
    }
    
    if is_neg {
      chars = chars.push('-')
    }
    
    // 反转字符数组
    let mut result = ""
    for i in (0..chars.length()).reverse() {
      result = result + chars[i].to_string()
    }
    result
  }
  
  assert_eq(int_to_str(0), "0")
  assert_eq(int_to_str(42), "42")
  assert_eq(int_to_str(-123), "-123")
  
  // 字符串到浮点数转换（简化版）
  let str_to_float = fn(s: String) {
    let parts = s.split(".")
    let int_part = str_to_int(parts[0])
    
    if parts.length() == 1 {
      return int_part as Float
    }
    
    let decimal_str = parts[1]
    let mut decimal_value = 0.0
    let mut divisor = 1.0
    
    for i in 0..decimal_str.length() {
      let c = decimal_str[i]
      if c >= '0' and c <= '9' {
        decimal_value = decimal_value * 10.0 + (c.to_int() - '0'.to_int()) as Float
        divisor = divisor * 10.0
      }
    }
    
    (int_part as Float) + (decimal_value / divisor)
  }
  
  assert_eq(str_to_float("3.14"), 3.14)
  assert_eq(str_to_float("-2.5"), -2.5)
  assert_eq(str_to_float("10"), 10.0)
}

// 测试2: 遥测属性值类型转换
test "遥测属性值类型转换" {
  // 定义属性值类型
  enum AttributeValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    ArrayValue(Array[String])
  }
  
  // 属性值到字符串转换
  let attr_to_string = fn(attr: AttributeValue) {
    match attr {
      AttributeValue::StringValue(s) => s
      AttributeValue::IntValue(i) => i.to_string()
      AttributeValue::FloatValue(f) => f.to_string()
      AttributeValue::BoolValue(b) => if b { "true" } else { "false" }
      AttributeValue::ArrayValue(arr) => "[" + arr.join(",") + "]"
    }
  }
  
  // 测试属性值转换
  assert_eq(attr_to_string(AttributeValue::StringValue("test")), "test")
  assert_eq(attr_to_string(AttributeValue::IntValue(42)), "42")
  assert_eq(attr_to_string(AttributeValue::FloatValue(3.14)), "3.14")
  assert_eq(attr_to_string(AttributeValue::BoolValue(true)), "true")
  assert_eq(attr_to_string(AttributeValue::ArrayValue(["a", "b", "c"])), "[a,b,c]")
  
  // 字符串到属性值转换（自动推断类型）
  let string_to_attr = fn(s: String) {
    // 尝试解析为布尔值
    if s == "true" {
      return AttributeValue::BoolValue(true)
    }
    if s == "false" {
      return AttributeValue::BoolValue(false)
    }
    
    // 尝试解析为整数
    let mut is_int = true
    let mut i = 0
    if s.length() > 0 && s[0] == '-' {
      i = 1
    }
    
    while i < s.length() && is_int {
      if s[i] < '0' or s[i] > '9' {
        is_int = false
      }
      i = i + 1
    }
    
    if is_int && s.length() > 0 {
      return AttributeValue::IntValue(str_to_int(s))
    }
    
    // 尝试解析为浮点数
    let mut is_float = true
    let mut dot_count = 0
    i = 0
    if s.length() > 0 && s[0] == '-' {
      i = 1
    }
    
    while i < s.length() && is_float {
      if s[i] == '.' {
        dot_count = dot_count + 1
        if dot_count > 1 {
          is_float = false
        }
      } else if s[i] < '0' or s[i] > '9' {
        is_float = false
      }
      i = i + 1
    }
    
    if is_float && s.length() > 0 {
      return AttributeValue::FloatValue(str_to_float(s))
    }
    
    // 默认为字符串
    AttributeValue::StringValue(s)
  }
  
  // 测试自动类型推断
  assert_eq(string_to_attr("true"), AttributeValue::BoolValue(true))
  assert_eq(string_to_attr("false"), AttributeValue::BoolValue(false))
  assert_eq(string_to_attr("123"), AttributeValue::IntValue(123))
  assert_eq(string_to_attr("-456"), AttributeValue::IntValue(-456))
  assert_eq(string_to_attr("3.14"), AttributeValue::FloatValue(3.14))
  assert_eq(string_to_attr("-2.5"), AttributeValue::FloatValue(-2.5))
  assert_eq(string_to_attr("hello"), AttributeValue::StringValue("hello"))
  
  // 属性值类型强制转换
  let cast_attr = fn(attr: AttributeValue, target_type: String) {
    match (attr, target_type) {
      (AttributeValue::StringValue(s), "int") => AttributeValue::IntValue(str_to_int(s))
      (AttributeValue::StringValue(s), "float") => AttributeValue::FloatValue(str_to_float(s))
      (AttributeValue::StringValue(s), "bool") => {
        if s == "true" {
          AttributeValue::BoolValue(true)
        } else if s == "false" {
          AttributeValue::BoolValue(false)
        } else {
          AttributeValue::BoolValue(s.length() > 0)
        }
      }
      (AttributeValue::IntValue(i), "float") => AttributeValue::FloatValue(i as Float)
      (AttributeValue::IntValue(i), "string") => AttributeValue::StringValue(i.to_string())
      (AttributeValue::IntValue(i), "bool") => AttributeValue::BoolValue(i != 0)
      (AttributeValue::FloatValue(f), "int") => AttributeValue::IntValue(f as Int)
      (AttributeValue::FloatValue(f), "string") => AttributeValue::StringValue(f.to_string())
      (AttributeValue::FloatValue(f), "bool") => AttributeValue::BoolValue(f != 0.0)
      (AttributeValue::BoolValue(b), "int") => AttributeValue::IntValue(if b { 1 } else { 0 })
      (AttributeValue::BoolValue(b), "float") => AttributeValue::FloatValue(if b { 1.0 } else { 0.0 })
      (AttributeValue::BoolValue(b), "string") => AttributeValue::StringValue(if b { "true" } else { "false" })
      _ => attr  // 不支持的转换，返回原值
    }
  }
  
  // 测试强制类型转换
  let string_attr = AttributeValue::StringValue("123")
  assert_eq(cast_attr(string_attr, "int"), AttributeValue::IntValue(123))
  assert_eq(cast_attr(string_attr, "float"), AttributeValue::FloatValue(123.0))
  assert_eq(cast_attr(string_attr, "bool"), AttributeValue::BoolValue(false))
  
  let int_attr = AttributeValue::IntValue(42)
  assert_eq(cast_attr(int_attr, "float"), AttributeValue::FloatValue(42.0))
  assert_eq(cast_attr(int_attr, "string"), AttributeValue::StringValue("42"))
  assert_eq(cast_attr(int_attr, "bool"), AttributeValue::BoolValue(true))
  
  let float_attr = AttributeValue::FloatValue(3.7)
  assert_eq(cast_attr(float_attr, "int"), AttributeValue::IntValue(3))
  assert_eq(cast_attr(float_attr, "string"), AttributeValue::StringValue("3.7"))
  assert_eq(cast_attr(float_attr, "bool"), AttributeValue::BoolValue(true))
  
  let bool_attr = AttributeValue::BoolValue(true)
  assert_eq(cast_attr(bool_attr, "int"), AttributeValue::IntValue(1))
  assert_eq(cast_attr(bool_attr, "float"), AttributeValue::FloatValue(1.0))
  assert_eq(cast_attr(bool_attr, "string"), AttributeValue::StringValue("true"))
}

// 测试3: 时间戳转换和格式化
test "时间戳转换和格式化" {
  // Unix时间戳（秒）到ISO 8601格式字符串转换（简化版）
  let timestamp_to_iso8601 = fn(timestamp: Int) {
    let days_since_epoch = timestamp / 86400  // 自1970-01-01以来的天数
    let seconds_in_day = timestamp % 86400
    
    let hours = seconds_in_day / 3600
    let minutes = (seconds_in_day % 3600) / 60
    let seconds = seconds_in_day % 60
    
    // 简化的日期计算（仅用于演示）
    let year = 1970 + (days_since_epoch / 365)
    let day_of_year = days_since_epoch % 365
    
    // 格式化为ISO 8601字符串
    let time_part = hours.to_string() + ":" + 
                   minutes.to_string() + ":" + 
                   seconds.to_string()
    
    year.to_string() + "-01-01T" + time_part + "Z"
  }
  
  // 测试时间戳转换
  let timestamp1 = 1640995200  // 2022-01-01 00:00:00 UTC
  let iso1 = timestamp_to_iso8601(timestamp1)
  assert_true(iso1.contains("2022-01-01T00:00:00Z"))
  
  let timestamp2 = 1640998800  // 2022-01-01 01:00:00 UTC
  let iso2 = timestamp_to_iso8601(timestamp2)
  assert_true(iso2.contains("2022-01-01T01:00:00Z"))
  
  // ISO 8601格式字符串到Unix时间戳转换（简化版）
  let iso8601_to_timestamp = fn(iso_string: String) {
    // 提取时间部分 "YYYY-MM-DDTHH:MM:SSZ"
    let time_part = iso_string.split("T")[1].split("Z")[0]
    let time_components = time_part.split(":")
    
    let hours = str_to_int(time_components[0])
    let minutes = str_to_int(time_components[1])
    let seconds = str_to_int(time_components[2])
    
    // 提取日期部分 "YYYY-MM-DD"
    let date_part = iso_string.split("T")[0]
    let date_components = date_part.split("-")
    
    let year = str_to_int(date_components[0])
    let month = str_to_int(date_components[1])
    let day = str_to_int(date_components[2])
    
    // 简化的计算（仅用于演示）
    let days_since_epoch = (year - 1970) * 365 + (month - 1) * 30 + (day - 1)
    let seconds_in_day = hours * 3600 + minutes * 60 + seconds
    
    days_since_epoch * 86400 + seconds_in_day
  }
  
  // 测试ISO 8601转换
  let timestamp3 = iso8601_to_timestamp("2022-01-01T00:00:00Z")
  assert_eq(timestamp3, timestamp1)
  
  let timestamp4 = iso8601_to_timestamp("2022-01-01T01:00:00Z")
  assert_eq(timestamp4, timestamp2)
  
  // 持续时间格式化
  let format_duration = fn(duration_ms: Int) {
    if duration_ms < 1000 {
      duration_ms.to_string() + "ms"
    } else if duration_ms < 60000 {
      let seconds = duration_ms / 1000
      let milliseconds = duration_ms % 1000
      seconds.to_string() + "." + (milliseconds / 100).to_string() + "s"
    } else if duration_ms < 3600000 {
      let minutes = duration_ms / 60000
      let seconds = (duration_ms % 60000) / 1000
      minutes.to_string() + "m" + seconds.to_string() + "s"
    } else {
      let hours = duration_ms / 3600000
      let minutes = (duration_ms % 3600000) / 60000
      hours.to_string() + "h" + minutes.to_string() + "m"
    }
  }
  
  // 测试持续时间格式化
  assert_eq(format_duration(500), "500ms")
  assert_eq(format_duration(1500), "1.5s")
  assert_eq(format_duration(65000), "1m5s")
  assert_eq(format_duration(3665000), "1h1m")
  
  // 字符串持续时间到毫秒转换
  let duration_to_ms = fn(duration_str: String) {
    if duration_str.ends_with("ms") {
      let ms_part = duration_str.substring(0, duration_str.length() - 2)
      str_to_int(ms_part)
    } else if duration_str.ends_with("s") {
      let seconds_part = duration_str.substring(0, duration_str.length() - 1)
      if seconds_part.contains(".") {
        let parts = seconds_part.split(".")
        let seconds = str_to_int(parts[0])
        let tenths = if parts.length() > 1 { str_to_int(parts[1]) } else { 0 }
        seconds * 1000 + tenths * 100
      } else {
        str_to_int(seconds_part) * 1000
      }
    } else if duration_str.ends_with("m") && duration_str.contains("s") {
      // 格式: XmYs
      let parts = duration_str.split("m")
      let minutes = str_to_int(parts[0])
      let seconds_part = parts[1].split("s")[0]
      let seconds = str_to_int(seconds_part)
      minutes * 60000 + seconds * 1000
    } else if duration_str.ends_with("m") {
      // 格式: Xm
      let minutes_part = duration_str.substring(0, duration_str.length() - 1)
      str_to_int(minutes_part) * 60000
    } else if duration_str.ends_with("h") && duration_str.contains("m") {
      // 格式: XhYm
      let parts = duration_str.split("h")
      let hours = str_to_int(parts[0])
      let minutes_part = parts[1].split("m")[0]
      let minutes = str_to_int(minutes_part)
      hours * 3600000 + minutes * 60000
    } else {
      0  // 无法识别的格式
    }
  }
  
  // 测试持续时间转换
  assert_eq(duration_to_ms("500ms"), 500)
  assert_eq(duration_to_ms("1.5s"), 1500)
  assert_eq(duration_to_ms("1m5s"), 65000)
  assert_eq(duration_to_ms("1h1m"), 3660000)
  assert_eq(duration_to_ms("30m"), 1800000)
}

// 测试4: 遥测度量单位转换
test "遥测度量单位转换" {
  // 定义度量单位
  enum Unit {
    Bytes
    Kilobytes
    Megabytes
    Gigabytes
    Milliseconds
    Seconds
    Minutes
    Hours
    Count
    Percent
    Rate
  }
  
  // 单位转换函数
  let convert_unit = fn(value: Float, from: Unit, to: Unit) {
    match (from, to) {
      // 字节单位转换
      (Unit::Bytes, Unit::Kilobytes) => value / 1024.0
      (Unit::Bytes, Unit::Megabytes) => value / (1024.0 * 1024.0)
      (Unit::Bytes, Unit::Gigabytes) => value / (1024.0 * 1024.0 * 1024.0)
      
      (Unit::Kilobytes, Unit::Bytes) => value * 1024.0
      (Unit::Kilobytes, Unit::Megabytes) => value / 1024.0
      (Unit::Kilobytes, Unit::Gigabytes) => value / (1024.0 * 1024.0)
      
      (Unit::Megabytes, Unit::Bytes) => value * 1024.0 * 1024.0
      (Unit::Megabytes, Unit::Kilobytes) => value * 1024.0
      (Unit::Megabytes, Unit::Gigabytes) => value / 1024.0
      
      (Unit::Gigabytes, Unit::Bytes) => value * 1024.0 * 1024.0 * 1024.0
      (Unit::Gigabytes, Unit::Kilobytes) => value * 1024.0 * 1024.0
      (Unit::Gigabytes, Unit::Megabytes) => value * 1024.0
      
      // 时间单位转换
      (Unit::Milliseconds, Unit::Seconds) => value / 1000.0
      (Unit::Milliseconds, Unit::Minutes) => value / (1000.0 * 60.0)
      (Unit::Milliseconds, Unit::Hours) => value / (1000.0 * 60.0 * 60.0)
      
      (Unit::Seconds, Unit::Milliseconds) => value * 1000.0
      (Unit::Seconds, Unit::Minutes) => value / 60.0
      (Unit::Seconds, Unit::Hours) => value / (60.0 * 60.0)
      
      (Unit::Minutes, Unit::Milliseconds) => value * 1000.0 * 60.0
      (Unit::Minutes, Unit::Seconds) => value * 60.0
      (Unit::Minutes, Unit::Hours) => value / 60.0
      
      (Unit::Hours, Unit::Milliseconds) => value * 1000.0 * 60.0 * 60.0
      (Unit::Hours, Unit::Seconds) => value * 60.0 * 60.0
      (Unit::Hours, Unit::Minutes) => value * 60.0
      
      // 相同单位
      (from, to) if from == to => value
      
      // 不支持的转换
      _ => value  // 返回原值
    }
  }
  
  // 测试字节单位转换
  assert_eq(convert_unit(1024.0, Unit::Bytes, Unit::Kilobytes), 1.0)
  assert_eq(convert_unit(1048576.0, Unit::Bytes, Unit::Megabytes), 1.0)
  assert_eq(convert_unit(1073741824.0, Unit::Bytes, Unit::Gigabytes), 1.0)
  
  assert_eq(convert_unit(1.0, Unit::Kilobytes, Unit::Bytes), 1024.0)
  assert_eq(convert_unit(1.0, Unit::Megabytes, Unit::Kilobytes), 1024.0)
  assert_eq(convert_unit(1.0, Unit::Gigabytes, Unit::Megabytes), 1024.0)
  
  // 测试时间单位转换
  assert_eq(convert_unit(1000.0, Unit::Milliseconds, Unit::Seconds), 1.0)
  assert_eq(convert_unit(60000.0, Unit::Milliseconds, Unit::Minutes), 1.0)
  assert_eq(convert_unit(3600000.0, Unit::Milliseconds, Unit::Hours), 1.0)
  
  assert_eq(convert_unit(1.0, Unit::Seconds, Unit::Milliseconds), 1000.0)
  assert_eq(convert_unit(1.0, Unit::Minutes, Unit::Seconds), 60.0)
  assert_eq(convert_unit(1.0, Unit::Hours, Unit::Minutes), 60.0)
  
  // 单位格式化
  let format_unit = fn(value: Float, unit: Unit) {
    let (formatted_value, unit_str) = match unit {
      Unit::Bytes => {
        if value < 1024.0 {
          (value, "B")
        } else if value < 1024.0 * 1024.0 {
          (convert_unit(value, Unit::Bytes, Unit::Kilobytes), "KB")
        } else if value < 1024.0 * 1024.0 * 1024.0 {
          (convert_unit(value, Unit::Bytes, Unit::Megabytes), "MB")
        } else {
          (convert_unit(value, Unit::Bytes, Unit::Gigabytes), "GB")
        }
      }
      Unit::Kilobytes => (value, "KB")
      Unit::Megabytes => (value, "MB")
      Unit::Gigabytes => (value, "GB")
      
      Unit::Milliseconds => {
        if value < 1000.0 {
          (value, "ms")
        } else if value < 60000.0 {
          (convert_unit(value, Unit::Milliseconds, Unit::Seconds), "s")
        } else if value < 3600000.0 {
          (convert_unit(value, Unit::Milliseconds, Unit::Minutes), "m")
        } else {
          (convert_unit(value, Unit::Milliseconds, Unit::Hours), "h")
        }
      }
      Unit::Seconds => (value, "s")
      Unit::Minutes => (value, "m")
      Unit::Hours => (value, "h")
      
      Unit::Count => (value, "count")
      Unit::Percent => (value, "%")
      Unit::Rate => (value, "/s")
    }
    
    // 格式化数值，保留适当的小数位数
    let formatted_num = if formatted_value < 10.0 {
      (formatted_value * 100.0).round() / 100.0
    } else {
      formatted_value.round()
    }
    
    formatted_num.to_string() + unit_str
  }
  
  // 测试单位格式化
  assert_eq(format_unit(512.0, Unit::Bytes), "512B")
  assert_eq(format_unit(1536.0, Unit::Bytes), "1.5KB")
  assert_eq(format_unit(2097152.0, Unit::Bytes), "2MB")
  assert_eq(format_unit(500.0, Unit::Milliseconds), "500ms")
  assert_eq(format_unit(1500.0, Unit::Milliseconds), "1.5s")
  assert_eq(format_unit(125000.0, Unit::Milliseconds), "2.08m")
  assert_eq(format_unit(42.0, Unit::Count), "42count")
  assert_eq(format_unit(75.5, Unit::Percent), "75.5%")
  assert_eq(format_unit(10.25, Unit::Rate), "10.25/s")
}

// 测试5: 遥测数据编码转换
test "遥测数据编码转换" {
  // Base64编码（简化版）
  let base64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  
  let string_to_base64 = fn(input: String) {
    let bytes = input.to_byte_array()
    let mut result = ""
    let mut i = 0
    
    while i < bytes.length() {
      let b1 = if i < bytes.length() { bytes[i] } else { 0 }
      let b2 = if i + 1 < bytes.length() { bytes[i + 1] } else { 0 }
      let b3 = if i + 2 < bytes.length() { bytes[i + 2] } else { 0 }
      
      let combined = (b1 << 16) | (b2 << 8) | b3
      
      let index1 = (combined >> 18) & 0x3F
      let index2 = (combined >> 12) & 0x3F
      let index3 = (combined >> 6) & 0x3F
      let index4 = combined & 0x3F
      
      result = result + base64_chars[index1].to_string()
      result = result + base64_chars[index2].to_string()
      
      if i + 1 < bytes.length() {
        result = result + base64_chars[index3].to_string()
      } else {
        result = result + "="
      }
      
      if i + 2 < bytes.length() {
        result = result + base64_chars[index4].to_string()
      } else {
        result = result + "="
      }
      
      i = i + 3
    }
    
    result
  }
  
  // 测试Base64编码
  assert_eq(string_to_base64("hello"), "aGVsbG8=")
  assert_eq(string_to_base64("azimuth"), "YXppbXV0aA==")
  assert_eq(string_to_base64(""), "")
  
  // URL编码（简化版）
  let url_encode = fn(input: String) {
    let mut result = ""
    let unreserved = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~"
    
    for c in input.to_char_array() {
      if unreserved.contains(c.to_string()) {
        result = result + c.to_string()
      } else {
        let byte_val = c.to_int()
        let hex1 = (byte_val >> 4) & 0xF
        let hex2 = byte_val & 0xF
        let hex_chars = "0123456789ABCDEF"
        result = result + "%" + hex_chars[hex1].to_string() + hex_chars[hex2].to_string()
      }
    }
    
    result
  }
  
  // 测试URL编码
  assert_eq(url_encode("hello world"), "hello%20world")
  assert_eq(url_encode("azimuth/telemetry"), "azimuth%2Ftelemetry")
  assert_eq(url_encode("user@email.com"), "user%40email.com")
  
  // 十六进制编码
  let string_to_hex = fn(input: String) {
    let mut result = ""
    let hex_chars = "0123456789ABCDEF"
    
    for byte in input.to_byte_array() {
      let high = (byte >> 4) & 0xF
      let low = byte & 0xF
      result = result + hex_chars[high].to_string() + hex_chars[low].to_string()
    }
    
    result
  }
  
  // 测试十六进制编码
  assert_eq(string_to_hex("A"), "41")
  assert_eq(string_to_hex("az"), "617A")
  assert_eq(string_to_hex(""), "")
  
  // 哈希计算（简化版）
  let simple_hash = fn(input: String) {
    let mut hash = 5381
    for c in input.to_char_array() {
      hash = ((hash << 5) + hash) + c.to_int()  // hash * 33 + c
    }
    hash
  }
  
  // 测试哈希计算
  let hash1 = simple_hash("azimuth")
  let hash2 = simple_hash("telemetry")
  let hash3 = simple_hash("azimuth")  // 相同输入应产生相同哈希
  
  assert_eq(hash1, hash3)
  assert_true(hash1 != hash2)
  
  // 哈希到不同格式的转换
  let hash_to_hex = fn(hash: Int) {
    let hex_chars = "0123456789ABCDEF"
    let mut result = ""
    let mut value = hash
    
    if value == 0 {
      return "0"
    }
    
    while value > 0 {
      let digit = value & 0xF
      result = hex_chars[digit].to_string() + result
      value = value >> 4
    }
    
    result
  }
  
  let hash_to_base64 = fn(hash: Int) {
    let mut value = hash
    let mut result = ""
    
    while value > 0 {
      let index = value % 64
      result = base64_chars[index].to_string() + result
      value = value / 64
    }
    
    if result == "" {
      "A"
    } else {
      result
    }
  }
  
  // 测试哈希格式转换
  let hash_hex = hash_to_hex(hash1)
  let hash_base64 = hash_to_base64(hash1)
  
  assert_true(hash_hex.length() > 0)
  assert_true(hash_base64.length() > 0)
  
  // 验证转换一致性
  let hash_from_hex = simple_hash(hash_hex)
  let hash_from_base64 = simple_hash(hash_base64)
  
  // 这些哈希值应该不同，因为输入字符串不同
  assert_true(hash_from_hex != hash_from_base64)
}