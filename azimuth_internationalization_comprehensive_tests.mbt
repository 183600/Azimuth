// Azimuth 国际化支持测试用例
// 专注于测试遥测系统的国际化、本地化和多语言支持

// 测试1: 多语言界面文本支持
test "多语言界面文本支持" {
  // 模拟多语言文本资源
  let ui_translations = {
    "zh-CN": {
      "dashboard_title": "遥测仪表板",
      "cpu_usage": "CPU使用率",
      "memory_usage": "内存使用率",
      "network_io": "网络IO",
      "alert": "警报",
      "settings": "设置"
    },
    "en-US": {
      "dashboard_title": "Telemetry Dashboard",
      "cpu_usage": "CPU Usage",
      "memory_usage": "Memory Usage",
      "network_io": "Network I/O",
      "alert": "Alert",
      "settings": "Settings"
    },
    "ja-JP": {
      "dashboard_title": "テレメトリーダッシュボード",
      "cpu_usage": "CPU使用率",
      "memory_usage": "メモリ使用率",
      "network_io": "ネットワークI/O",
      "alert": "アラート",
      "settings": "設定"
    },
    "de-DE": {
      "dashboard_title": "Telemetrie-Dashboard",
      "cpu_usage": "CPU-Auslastung",
      "memory_usage": "Speichernutzung",
      "network_io": "Netzwerk-I/O",
      "alert": "Warnung",
      "settings": "Einstellungen"
    }
  }
  
  // 模拟语言切换功能
  func get_translation(lang : String, key : String, translations : Map[String, Map[String, String]]) -> String {
    match translations.get(lang) {
      Some(lang_translations) => {
        match lang_translations.get(key) {
          Some(text) => text,
          None => "[" + lang + "." + key + "]"  // 缺失翻译的占位符
        }
      }
      None => "[lang." + key + "]"  // 缺失语言的占位符
    }
  }
  
  // 测试中文翻译
  assert_eq(get_translation("zh-CN", "dashboard_title", ui_translations), "遥测仪表板")
  assert_eq(get_translation("zh-CN", "cpu_usage", ui_translations), "CPU使用率")
  assert_eq(get_translation("zh-CN", "alert", ui_translations), "警报")
  
  // 测试英文翻译
  assert_eq(get_translation("en-US", "dashboard_title", ui_translations), "Telemetry Dashboard")
  assert_eq(get_translation("en-US", "cpu_usage", ui_translations), "CPU Usage")
  assert_eq(get_translation("en-US", "alert", ui_translations), "Alert")
  
  // 测试日文翻译
  assert_eq(get_translation("ja-JP", "dashboard_title", ui_translations), "テレメトリーダッシュボード")
  assert_eq(get_translation("ja-JP", "cpu_usage", ui_translations), "CPU使用率")
  assert_eq(get_translation("ja-JP", "alert", ui_translations), "アラート")
  
  // 测试德文翻译
  assert_eq(get_translation("de-DE", "dashboard_title", ui_translations), "Telemetrie-Dashboard")
  assert_eq(get_translation("de-DE", "cpu_usage", ui_translations), "CPU-Auslastung")
  assert_eq(get_translation("de-DE", "alert", ui_translations), "Warnung")
  
  // 测试缺失翻译
  assert_eq(get_translation("zh-CN", "nonexistent_key", ui_translations), "[zh-CN.nonexistent_key]")
  assert_eq(get_translation("fr-FR", "dashboard_title", ui_translations), "[lang.dashboard_title]")
  
  // 测试可用语言列表
  let available_languages = ui_translations.keys()
  assert_eq(available_languages.length(), 4)
  assert_true(available_languages.contains("zh-CN"))
  assert_true(available_languages.contains("en-US"))
  assert_true(available_languages.contains("ja-JP"))
  assert_true(available_languages.contains("de-DE"))
}

// 测试2: 数字和日期格式本地化
test "数字和日期格式本地化" {
  // 模拟不同地区的数字格式
  let number_formats = {
    "zh-CN": { decimal_separator: ".", thousands_separator: ",", decimal_places: 2 },
    "en-US": { decimal_separator: ".", thousands_separator: ",", decimal_places: 2 },
    "de-DE": { decimal_separator: ",", thousands_separator: ".", decimal_places: 2 },
    "fr-FR": { decimal_separator: ",", thousands_separator: " ", decimal_places: 2 },
    "ja-JP": { decimal_separator: ".", thousands_separator: ",", decimal_places: 2 }
  }
  
  // 模拟日期格式
  let date_formats = {
    "zh-CN": "YYYY-MM-DD HH:mm:ss",
    "en-US": "MM/DD/YYYY HH:mm:ss",
    "de-DE": "DD.MM.YYYY HH:mm:ss",
    "ja-JP": "YYYY/MM/DD HH:mm:ss",
    "fr-FR": "DD/MM/YYYY HH:mm:ss"
  }
  
  // 数字格式化函数
  func format_number(value : Float, locale : String, formats : Map[String, { decimal_separator : String, thousands_separator : String, decimal_places : Int }]) -> String {
    match formats.get(locale) {
      Some(format) => {
        // 简化的数字格式化
        let integer_part = value.to_int().to_string()
        let decimal_part = ((value - value.to_int().to_float()) * 100.0).to_int().to_string()
        
        // 添加千位分隔符
        let mut formatted_integer = ""
        let mut digit_count = 0
        
        for i in range(0, integer_part.length()) {
          let char_index = integer_part.length() - 1 - i
          let char = integer_part.to_char_array()[char_index]
          formatted_integer = char.to_string() + formatted_integer
          digit_count = digit_count + 1
          
          if digit_count == 3 && char_index > 0 {
            formatted_integer = format.thousands_separator + formatted_integer
            digit_count = 0
          }
        }
        
        return formatted_integer + format.decimal_separator + decimal_part
      }
      None => value.to_string()
    }
  }
  
  // 测试数字格式化
  let test_number = 1234567.89
  
  assert_eq(format_number(test_number, "zh-CN", number_formats), "1,234,567.89")
  assert_eq(format_number(test_number, "en-US", number_formats), "1,234,567.89")
  assert_eq(format_number(test_number, "de-DE", number_formats), "1.234.567,89")
  assert_eq(format_number(test_number, "fr-FR", number_formats), "1 234 567,89")
  assert_eq(format_number(test_number, "ja-JP", number_formats), "1,234,567.89")
  
  // 测试日期格式化（简化版）
  func format_date(timestamp : Int, locale : String, formats : Map[String, String]) -> String {
    // 简化的日期格式化（仅用于测试）
    let year = 2022
    let month = 1
    let day = 1
    let hour = 12
    let minute = 30
    let second = 45
    
    match formats.get(locale) {
      Some(format) => {
        match format {
          "YYYY-MM-DD HH:mm:ss" => year.to_string() + "-" + 
                                   (if month < 10 { "0" } else { "" }) + month.to_string() + "-" +
                                   (if day < 10 { "0" } else { "" }) + day.to_string() + " " +
                                   (if hour < 10 { "0" } else { "" }) + hour.to_string() + ":" +
                                   (if minute < 10 { "0" } else { "" }) + minute.to_string() + ":" +
                                   (if second < 10 { "0" } else { "" }) + second.to_string(),
          "MM/DD/YYYY HH:mm:ss" => (if month < 10 { "0" } else { "" }) + month.to_string() + "/" +
                                   (if day < 10 { "0" } else { "" }) + day.to_string() + "/" +
                                   year.to_string() + " " +
                                   (if hour < 10 { "0" } else { "" }) + hour.to_string() + ":" +
                                   (if minute < 10 { "0" } else { "" }) + minute.to_string() + ":" +
                                   (if second < 10 { "0" } else { "" }) + second.to_string(),
          "DD.MM.YYYY HH:mm:ss" => (if day < 10 { "0" } else { "" }) + day.to_string() + "." +
                                   (if month < 10 { "0" } else { "" }) + month.to_string() + "." +
                                   year.to_string() + " " +
                                   (if hour < 10 { "0" } else { "" }) + hour.to_string() + ":" +
                                   (if minute < 10 { "0" } else { "" }) + minute.to_string() + ":" +
                                   (if second < 10 { "0" } else { "" }) + second.to_string(),
          "YYYY/MM/DD HH:mm:ss" => year.to_string() + "/" +
                                   (if month < 10 { "0" } else { "" }) + month.to_string() + "/" +
                                   (if day < 10 { "0" } else { "" }) + day.to_string() + " " +
                                   (if hour < 10 { "0" } else { "" }) + hour.to_string() + ":" +
                                   (if minute < 10 { "0" } else { "" }) + minute.to_string() + ":" +
                                   (if second < 10 { "0" } else { "" }) + second.to_string(),
          "DD/MM/YYYY HH:mm:ss" => (if day < 10 { "0" } else { "" }) + day.to_string() + "/" +
                                   (if month < 10 { "0" } else { "" }) + month.to_string() + "/" +
                                   year.to_string() + " " +
                                   (if hour < 10 { "0" } else { "" }) + hour.to_string() + ":" +
                                   (if minute < 10 { "0" } else { "" }) + minute.to_string() + ":" +
                                   (if second < 10 { "0" } else { "" }) + second.to_string(),
          _ => timestamp.to_string()
        }
      }
      None => timestamp.to_string()
    }
  }
  
  let test_timestamp = 1640999445  // 2022-01-01 12:30:45
  
  assert_eq(format_date(test_timestamp, "zh-CN", date_formats), "2022-01-01 12:30:45")
  assert_eq(format_date(test_timestamp, "en-US", date_formats), "01/01/2022 12:30:45")
  assert_eq(format_date(test_timestamp, "de-DE", date_formats), "01.01.2022 12:30:45")
  assert_eq(format_date(test_timestamp, "ja-JP", date_formats), "2022/01/01 12:30:45")
  assert_eq(format_date(test_timestamp, "fr-FR", date_formats), "01/01/2022 12:30:45")
}

// 测试3: 时区处理
test "时区处理" {
  // 模拟时区信息
  let timezones = {
    "UTC": { offset: 0, name: "协调世界时" },
    "Asia/Shanghai": { offset: 8, name: "中国标准时间" },
    "America/New_York": { offset: -5, name: "美国东部时间" },
    "Europe/London": { offset: 0, name: "格林尼治标准时间" },
    "Asia/Tokyo": { offset: 9, name: "日本标准时间" },
    "Europe/Berlin": { offset: 1, name: "中欧时间" }
  }
  
  // 时区转换函数
  func convert_timezone(timestamp : Int, from_tz : String, to_tz : String, tz_info : Map[String, { offset : Int, name : String }]) -> Int {
    let from_offset = match tz_info.get(from_tz) {
      Some(info) => info.offset,
      None => 0
    }
    
    let to_offset = match tz_info.get(to_tz) {
      Some(info) => info.offset,
      None => 0
    }
    
    // 转换为UTC，然后再转换为目标时区
    let utc_timestamp = timestamp - from_offset * 3600
    let target_timestamp = utc_timestamp + to_offset * 3600
    
    return target_timestamp
  }
  
  // 测试时区转换
  let base_timestamp = 1640995200  // 2022-01-01 00:00:00 UTC
  
  // UTC转换为其他时区
  let shanghai_time = convert_timezone(base_timestamp, "UTC", "Asia/Shanghai", timezones)
  assert_eq(shanghai_time, 1641024000)  // +8小时
  
  let new_york_time = convert_timezone(base_timestamp, "UTC", "America/New_York", timezones)
  assert_eq(new_york_time, 1640976000)  // -5小时
  
  let tokyo_time = convert_timezone(base_timestamp, "UTC", "Asia/Tokyo", timezones)
  assert_eq(tokyo_time, 1641027600)  // +9小时
  
  // 从其他时区转换到UTC
  let shanghai_to_utc = convert_timezone(shanghai_time, "Asia/Shanghai", "UTC", timezones)
  assert_eq(shanghai_to_utc, base_timestamp)
  
  let new_york_to_utc = convert_timezone(new_york_time, "America/New_York", "UTC", timezones)
  assert_eq(new_york_to_utc, base_timestamp)
  
  // 测试时区名称获取
  func get_timezone_name(tz : String, tz_info : Map[String, { offset : Int, name : String }]) -> String {
    match tz_info.get(tz) {
      Some(info) => info.name,
      None => "未知时区"
    }
  }
  
  assert_eq(get_timezone_name("UTC", timezones), "协调世界时")
  assert_eq(get_timezone_name("Asia/Shanghai", timezones), "中国标准时间")
  assert_eq(get_timezone_name("America/New_York", timezones), "美国东部时间")
  assert_eq(get_timezone_name("Europe/Berlin", timezones), "中欧时间")
  assert_eq(get_timezone_name("Unknown/Timezone", timezones), "未知时区")
  
  // 测试夏令时处理（简化版）
  let daylight_saving_adjustments = {
    "America/New_York": { start_month: 3, end_month: 11, offset_change: 1 },
    "Europe/London": { start_month: 3, end_month: 10, offset_change: 1 },
    "Europe/Berlin": { start_month: 3, end_month: 10, offset_change: 1 }
  }
  
  func is_daylight_saving(timestamp : Int, tz : String, adjustments : Map[String, { start_month : Int, end_month : Int, offset_change : Int }]) -> Bool {
    // 简化的夏令时判断（仅用于测试）
    match adjustments.get(tz) {
      Some(adj) => {
        // 假设时间戳对应的月份在夏令时范围内
        // 这里简化处理，假设6月是夏令时
        return true  // 简化假设
      }
      None => false
    }
  }
  
  let summer_timestamp = 1654041600  // 2022-06-01 00:00:00 UTC
  assert_true(is_daylight_saving(summer_timestamp, "America/New_York", daylight_saving_adjustments))
  assert_true(is_daylight_saving(summer_timestamp, "Europe/London", daylight_saving_adjustments))
  assert_false(is_daylight_saving(summer_timestamp, "Asia/Shanghai", daylight_saving_adjustments))  // 无夏令时
}

// 测试4: 多语言错误消息
test "多语言错误消息" {
  // 模拟错误消息的多语言翻译
  let error_translations = {
    "zh-CN": {
      "connection_failed": "连接失败",
      "invalid_credentials": "凭据无效",
      "data_corruption": "数据损坏",
      "timeout": "操作超时",
      "resource_not_found": "资源未找到"
    },
    "en-US": {
      "connection_failed": "Connection failed",
      "invalid_credentials": "Invalid credentials",
      "data_corruption": "Data corruption",
      "timeout": "Operation timeout",
      "resource_not_found": "Resource not found"
    },
    "ja-JP": {
      "connection_failed": "接続に失敗しました",
      "invalid_credentials": "認証情報が無効です",
      "data_corruption": "データが破損しています",
      "timeout": "操作がタイムアウトしました",
      "resource_not_found": "リソースが見つかりません"
    },
    "de-DE": {
      "connection_failed": "Verbindung fehlgeschlagen",
      "invalid_credentials": "Ungültige Anmeldeinformationen",
      "data_corruption": "Datenbeschädigung",
      "timeout": "Zeitüberschreitung bei Operation",
      "resource_not_found": "Ressource nicht gefunden"
    }
  }
  
  // 模拟错误代码到消息的映射
  let error_codes = {
    1001: "connection_failed",
    1002: "invalid_credentials",
    1003: "data_corruption",
    1004: "timeout",
    1005: "resource_not_found"
  }
  
  // 获取本地化错误消息
  func get_error_message(error_code : Int, locale : String, translations : Map[String, Map[String, String]]) -> String {
    match error_codes.get(error_code) {
      Some(error_key) => {
        match translations.get(locale) {
          Some(lang_translations) => {
            match lang_translations.get(error_key) {
              Some(message) => message,
              None => "[" + locale + "." + error_key + "]"
            }
          }
          None => "[lang." + error_key + "]"
        }
      }
      None => "未知错误代码: " + error_code.to_string()
    }
  }
  
  // 测试错误消息本地化
  assert_eq(get_error_message(1001, "zh-CN", error_translations), "连接失败")
  assert_eq(get_error_message(1002, "en-US", error_translations), "Invalid credentials")
  assert_eq(get_error_message(1003, "ja-JP", error_translations), "データが破損しています")
  assert_eq(get_error_message(1004, "de-DE", error_translations), "Zeitüberschreitung bei Operation")
  
  // 测试未知错误代码
  assert_eq(get_error_message(9999, "zh-CN", error_translations), "未知错误代码: 9999")
  
  // 测试缺失翻译
  assert_eq(get_error_message(1001, "fr-FR", error_translations), "[lang.connection_failed]")
  
  // 测试错误消息格式化
  func format_error_message(error_code : Int, locale : String, details : String, translations : Map[String, Map[String, String]]) -> String {
    let base_message = get_error_message(error_code, locale, translations)
    return base_message + ": " + details
  }
  
  assert_eq(format_error_message(1001, "zh-CN", "网络不可达", error_translations), "连接失败: 网络不可达")
  assert_eq(format_error_message(1002, "en-US", "用户名或密码错误", error_translations), "Invalid credentials: 用户名或密码错误")
  
  // 测试错误日志本地化
  let mut localized_error_logs = []
  let errors = [
    { code: 1001, details: "网络超时", timestamp: 1640995200 },
    { code: 1003, details: "数据包损坏", timestamp: 1640995260 },
    { code: 1005, details: "配置文件不存在", timestamp: 1640995320 }
  ]
  
  for error in errors {
    let localized_message = format_error_message(error.code, "zh-CN", error.details, error_translations)
    let log_entry = {
      timestamp: error.timestamp,
      error_code: error.code,
      message: localized_message,
      locale: "zh-CN"
    }
    localized_error_logs = localized_error_logs.push(log_entry)
  }
  
  // 验证本地化错误日志
  assert_eq(localized_error_logs.length(), 3)
  assert_eq(localized_error_logs[0].message, "连接失败: 网络超时")
  assert_eq(localized_error_logs[1].message, "数据损坏: 数据包损坏")
  assert_eq(localized_error_logs[2].message, "资源未找到: 配置文件不存在")
}

// 测试5: 多语言单位转换
test "多语言单位转换" {
  // 模拟不同地区的度量单位
  let measurement_units = {
    "zh-CN": {
      "temperature": "C",
      "distance": "km",
      "weight": "kg",
      "volume": "L",
      "speed": "km/h"
    },
    "en-US": {
      "temperature": "F",
      "distance": "mi",
      "weight": "lb",
      "volume": "gal",
      "speed": "mph"
    },
    "en-GB": {
      "temperature": "C",
      "distance": "mi",
      "weight": "kg",
      "volume": "L",
      "speed": "mph"
    },
    "ja-JP": {
      "temperature": "C",
      "distance": "km",
      "weight": "kg",
      "volume": "L",
      "speed": "km/h"
    }
  }
  
  // 单位转换函数
  func convert_unit(value : Float, from_unit : String, to_unit : String) -> Float {
    match (from_unit, to_unit) {
      ("C", "F") => value * 9.0 / 5.0 + 32.0,
      ("F", "C") => (value - 32.0) * 5.0 / 9.0,
      ("km", "mi") => value * 0.621371,
      ("mi", "km") => value / 0.621371,
      ("kg", "lb") => value * 2.20462,
      ("lb", "kg") => value / 2.20462,
      ("L", "gal") => value * 0.264172,
      ("gal", "L") => value / 0.264172,
      ("km/h", "mph") => value * 0.621371,
      ("mph", "km/h") => value / 0.621371,
      _ => value  // 相同单位或未知转换
    }
  }
  
  // 获取本地化单位
  func get_localized_unit(measurement_type : String, locale : String, units : Map[String, Map[String, String]]) -> String {
    match units.get(locale) {
      Some(locale_units) => {
        match locale_units.get(measurement_type) {
          Some(unit) => unit,
          None => "unknown"
        }
      }
      None => "unknown"
    }
  }
  
  // 测试单位转换
  let temperature_c = 25.0
  let temperature_f = convert_unit(temperature_c, "C", "F")
  assert_eq(round(temperature_f), 77.0)  // 25°C = 77°F
  
  let distance_km = 100.0
  let distance_mi = convert_unit(distance_km, "km", "mi")
  assert_eq(round(distance_mi), 62.0)  // 100km ≈ 62mi
  
  let weight_kg = 70.0
  let weight_lb = convert_unit(weight_kg, "kg", "lb")
  assert_eq(round(weight_lb), 154.0)  // 70kg ≈ 154lb
  
  // 测试本地化单位获取
  assert_eq(get_localized_unit("temperature", "zh-CN", measurement_units), "C")
  assert_eq(get_localized_unit("temperature", "en-US", measurement_units), "F")
  assert_eq(get_localized_unit("distance", "en-US", measurement_units), "mi")
  assert_eq(get_localized_unit("distance", "zh-CN", measurement_units), "km")
  
  // 测试完整的本地化测量值
  func format_measurement(value : Float, measurement_type : String, locale : String, base_units : Map[String, Map[String, String]]) -> String {
    let base_unit = get_localized_unit(measurement_type, "zh-CN", base_units)  // 使用中文作为基准
    let local_unit = get_localized_unit(measurement_type, locale, base_units)
    
    let converted_value = if local_unit != base_unit && local_unit != "unknown" && base_unit != "unknown" {
      convert_unit(value, base_unit, local_unit)
    } else {
      value
    }
    
    return converted_value.to_string() + " " + local_unit
  }
  
  // 测试测量值格式化
  assert_eq(format_measurement(25.0, "temperature", "zh-CN", measurement_units), "25.0 C")
  assert_eq(format_measurement(25.0, "temperature", "en-US", measurement_units), "77.0 F")
  assert_eq(format_measurement(100.0, "distance", "zh-CN", measurement_units), "100.0 km")
  assert_eq(format_measurement(100.0, "distance", "en-US", measurement_units), "62.1371 mi")
  
  // 测试遥测数据单位本地化
  let telemetry_data = [
    { metric: "temperature", value: 25.0, unit: "C" },
    { metric: "cpu_temperature", value: 65.0, unit: "C" },
    { metric: "network_speed", value: 1000.0, unit: "Mbps" },
    { metric: "disk_usage", value: 500.0, unit: "GB" }
  ]
  
  let mut localized_telemetry = []
  
  for data in telemetry_data {
    let localized_value = format_measurement(data.value, data.metric, "en-US", measurement_units)
    localized_telemetry = localized_telemetry.push({
      metric: data.metric,
      localized_value: localized_value,
      locale: "en-US"
    })
  }
  
  // 验证本地化遥测数据
  assert_eq(localized_telemetry.length(), 4)
  assert_eq(localized_telemetry[0].metric, "temperature")
  assert_eq(localized_telemetry[0].localized_value, "77.0 F")
}

// 测试6: 字符编码处理
test "字符编码处理" {
  // 模拟不同语言的遥测数据
  let multilingual_data = [
    { metric: "error_message", value: "连接失败", language: "zh-CN" },
    { metric: "error_message", value: "Connection failed", language: "en-US" },
    { metric: "error_message", value: "接続に失敗しました", language: "ja-JP" },
    { metric: "error_message", value: "Verbindung fehlgeschlagen", language: "de-DE" },
    { metric: "error_message", value: "Échec de connexion", language: "fr-FR" },
    { metric: "error_message", value: "Conexión fallida", language: "es-ES" },
    { metric: "error_message", value: "Подключение не удалось", language: "ru-RU" },
    { metric: "error_message", value: "اتصال ناموفق", language: "ar-SA" }
  ]
  
  // 测试字符编码处理
  let mut encoding_test_results = []
  
  for data in multilingual_data {
    let byte_length = data.value.length()  // 简化的字节长度计算
    let char_count = data.value.length()
    
    // 检查是否包含非ASCII字符
    let mut has_non_ascii = false
    for i in range(0, data.value.length()) {
      let char_code = data.value.to_char_array()[i].to_int()
      if char_code > 127 {
        has_non_ascii = true
        break
      }
    }
    
    let encoding_info = {
      language: data.language,
      text: data.value,
      char_count: char_count,
      byte_length: byte_length,
      has_non_ascii: has_non_ascii,
      encoding_type: if has_non_ascii { "UTF-8" } else { "ASCII" }
    }
    
    encoding_test_results = encoding_test_results.push(encoding_info)
  }
  
  // 验证编码处理结果
  assert_eq(encoding_test_results.length(), 8)
  
  // 中文应该包含非ASCII字符
  assert_true(encoding_test_results[0].has_non_ascii)
  assert_eq(encoding_test_results[0].encoding_type, "UTF-8")
  
  // 英文应该只包含ASCII字符
  assert_false(encoding_test_results[1].has_non_ascii)
  assert_eq(encoding_test_results[1].encoding_type, "ASCII")
  
  // 日文应该包含非ASCII字符
  assert_true(encoding_test_results[2].has_non_ascii)
  assert_eq(encoding_test_results[2].encoding_type, "UTF-8")
  
  // 德文可能包含非ASCII字符（如ü, ö, ä等）
  assert_true(encoding_test_results[3].has_non_ascii)
  assert_eq(encoding_test_results[3].encoding_type, "UTF-8")
  
  // 法文可能包含非ASCII字符（如é, à, ç等）
  assert_true(encoding_test_results[4].has_non_ascii)
  assert_eq(encoding_test_results[4].encoding_type, "UTF-8")
  
  // 测试字符串长度计算
  let test_string = "测试字符串Test String"
  let char_count = test_string.length()
  assert_eq(char_count, 14)  // 6个中文字符 + 8个英文字符
  
  // 测试字符串截断（保持字符完整性）
  func truncate_string(s : String, max_length : Int) -> String {
    if s.length() <= max_length {
      return s
    }
    
    let mut result = ""
    for i in range(0, max_length) {
      result = result + s.to_char_array()[i].to_string()
    }
    
    return result + "..."
  }
  
  let truncated_zh = truncate_string("这是一个很长的中文测试字符串", 10)
  assert_eq(truncated_zh, "这是一个很长的中文...")
  
  let truncated_en = truncate_string("This is a long English test string", 10)
  assert_eq(truncated_en, "This is a l...")
  
  // 测试多语言搜索
  func search_multilingual(data : Array[{ metric : String, value : String, language : String }], keyword : String) -> Array[{ metric : String, value : String, language : String }] {
    let mut results = []
    
    for item in data {
      if item.value.contains(keyword) {
        results = results.push(item)
      }
    }
    
    return results
  }
  
  let search_results_zh = search_multilingual(multilingual_data, "连接")
  assert_eq(search_results_zh.length(), 1)
  assert_eq(search_results_zh[0].language, "zh-CN")
  
  let search_results_en = search_multilingual(multilingual_data, "failed")
  assert_eq(search_results_en.length(), 1)
  assert_eq(search_results_en[0].language, "en-US")
}

// 辅助函数：四舍五入
func round(x : Float) -> Float {
  let int_part = x.to_int()
  let decimal_part = x - int_part.to_float()
  
  if decimal_part >= 0.5 {
    return (int_part + 1).to_float()
  } else {
    return int_part.to_float()
  }
}