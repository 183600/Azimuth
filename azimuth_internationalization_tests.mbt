// Azimuth Telemetry System - Internationalization Tests
// This file contains test cases for internationalization functionality

// Test 1: Unicode String Handling
test "unicode string handling" {
  // Test various Unicode characters
  let chinese_text = "遥测系统"
  let arabic_text = "نظام القياس عن بعد"
  let russian_text = "Система телеметрии"
  let japanese_text = "テレメトリシステム"
  let emoji_text = "🔍 📊 📈 📉"
  let mixed_text = "Azimuth 遥测系统 📊"
  
  // Test string length with Unicode
  assert_eq(chinese_text.length(), 4)
  assert_eq(arabic_text.length(), 20)
  assert_eq(russian_text.length(), 19)
  assert_eq(japanese_text.length(), 9)
  assert_eq(emoji_text.length(), 7) // Each emoji counts as one character
  assert_eq(mixed_text.length(), 12)
  
  // Test string concatenation with Unicode
  let combined = chinese_text + " " + japanese_text
  assert_eq(combined, "遥测系统 テレメトリシステム")
  
  // Test substring operations with Unicode
  let chinese_substring = chinese_text.substring(0, 2)
  assert_eq(chinese_substring, "遥测")
  
  let mixed_substring = mixed_text.substring(8, 11)
  assert_eq(mixed_substring, "📊")
}

// Test 2: Locale-aware Date/Time Formatting
test "locale-aware date/time formatting" {
  let timestamp = 1609459200L // 2021-01-01 00:00:00 UTC
  let formatter = DateTimeFormatter::new()
  
  // Test different locales
  DateTimeFormatter::set_locale(formatter, "en-US")
  let us_format = DateTimeFormatter::format(formatter, timestamp, "YYYY-MM-DD HH:mm:ss")
  assert_eq(us_format, "2021-01-01 00:00:00")
  
  DateTimeFormatter::set_locale(formatter, "zh-CN")
  let cn_format = DateTimeFormatter::format(formatter, timestamp, "YYYY年MM月DD日 HH:mm:ss")
  assert_eq(cn_format, "2021年01月01日 00:00:00")
  
  DateTimeFormatter::set_locale(formatter, "ja-JP")
  let jp_format = DateTimeFormatter::format(formatter, timestamp, "YYYY年MM月DD日 HH:mm:ss")
  assert_eq(jp_format, "2021年01月01日 00:00:00")
  
  DateTimeFormatter::set_locale(formatter, "ar-SA")
  let ar_format = DateTimeFormatter::format(formatter, timestamp, "YYYY/MM/DD HH:mm:ss")
  assert_eq(ar_format, "2021/01/01 00:00:00")
  
  // Test timezone handling
  DateTimeFormatter::set_timezone(formatter, "Asia/Shanghai")
  let shanghai_time = DateTimeFormatter::format(formatter, timestamp, "YYYY-MM-DD HH:mm:ss ZZ")
  assert_eq(shanghai_time, "2021-01-01 08:00:00 +0800")
  
  DateTimeFormatter::set_timezone(formatter, "America/New_York")
  let ny_time = DateTimeFormatter::format(formatter, timestamp, "YYYY-MM-DD HH:mm:ss ZZ")
  assert_eq(ny_time, "2020-12-31 19:00:00 -0500")
}

// Test 3: Number Formatting with Locales
test "number formatting with locales" {
  let number_formatter = NumberFormatter::new()
  
  // Test decimal formatting
  let test_number = 1234.5678
  
  NumberFormatter::set_locale(number_formatter, "en-US")
  let us_decimal = NumberFormatter::format_decimal(number_formatter, test_number, 2)
  assert_eq(us_decimal, "1,234.57")
  
  NumberFormatter::set_locale(number_formatter, "de-DE")
  let de_decimal = NumberFormatter::format_decimal(number_formatter, test_number, 2)
  assert_eq(de_decimal, "1.234,57")
  
  NumberFormatter::set_locale(number_formatter, "zh-CN")
  let cn_decimal = NumberFormatter::format_decimal(number_formatter, test_number, 2)
  assert_eq(cn_decimal, "1,234.57")
  
  NumberFormatter::set_locale(number_formatter, "ar-SA")
  let ar_decimal = NumberFormatter::format_decimal(number_formatter, test_number, 2)
  assert_eq(ar_decimal, "1,234.57")
  
  // Test percentage formatting
  let percentage = 0.7543
  
  NumberFormatter::set_locale(number_formatter, "en-US")
  let us_percent = NumberFormatter::format_percentage(number_formatter, percentage, 1)
  assert_eq(us_percent, "75.4%")
  
  NumberFormatter::set_locale(number_formatter, "fr-FR")
  let fr_percent = NumberFormatter::format_percentage(number_formatter, percentage, 1)
  assert_eq(fr_percent, "75,4 %")
  
  // Test currency formatting
  let currency = 1234.56
  
  NumberFormatter::set_locale(number_formatter, "en-US")
  let us_currency = NumberFormatter::format_currency(number_formatter, currency, "USD")
  assert_eq(us_currency, "$1,234.56")
  
  NumberFormatter::set_locale(number_formatter, "ja-JP")
  let jp_currency = NumberFormatter::format_currency(number_formatter, currency, "JPY")
  assert_eq(jp_currency, "¥1,235") // Japanese yen doesn't use decimal places
  
  NumberFormatter::set_locale(number_formatter, "de-DE")
  let de_currency = NumberFormatter::format_currency(number_formatter, currency, "EUR")
  assert_eq(de_currency, "1.234,56 €")
}

// Test 4: Message Localization
test "message localization" {
  let localizer = MessageLocalizer::new()
  
  // Load message bundles for different locales
  let en_messages = {
    "telemetry.started": "Telemetry collection started",
    "telemetry.stopped": "Telemetry collection stopped",
    "telemetry.error": "Telemetry error: {0}",
    "metric.recorded": "Metric {0} recorded with value {1}",
    "span.created": "Span {0} created",
    "span.ended": "Span {0} ended"
  }
  
  let zh_messages = {
    "telemetry.started": "遥测收集已启动",
    "telemetry.stopped": "遥测收集已停止",
    "telemetry.error": "遥测错误：{0}",
    "metric.recorded": "指标 {0} 已记录，值为 {1}",
    "span.created": "跨度 {0} 已创建",
    "span.ended": "跨度 {0} 已结束"
  }
  
  let ja_messages = {
    "telemetry.started": "テレメトリ収集が開始されました",
    "telemetry.stopped": "テレメトリ収集が停止されました",
    "telemetry.error": "テレメトリエラー: {0}",
    "metric.recorded": "メトリック {0} が値 {1} で記録されました",
    "span.created": "スパン {0} が作成されました",
    "span.ended": "スパン {0} が終了しました"
  }
  
  MessageLocalizer::load_bundle(localizer, "en", en_messages)
  MessageLocalizer::load_bundle(localizer, "zh", zh_messages)
  MessageLocalizer::load_bundle(localizer, "ja", ja_messages)
  
  // Test English localization
  MessageLocalizer::set_locale(localizer, "en")
  assert_eq(MessageLocalizer::get_message(localizer, "telemetry.started"), "Telemetry collection started")
  assert_eq(MessageLocalizer::get_message(localizer, "telemetry.stopped"), "Telemetry collection stopped")
  assert_eq(MessageLocalizer::format_message(localizer, "telemetry.error", ["connection failed"]), "Telemetry error: connection failed")
  assert_eq(MessageLocalizer::format_message(localizer, "metric.recorded", ["cpu_usage", "75.5"]), "Metric cpu_usage recorded with value 75.5")
  
  // Test Chinese localization
  MessageLocalizer::set_locale(localizer, "zh")
  assert_eq(MessageLocalizer::get_message(localizer, "telemetry.started"), "遥测收集已启动")
  assert_eq(MessageLocalizer::get_message(localizer, "telemetry.stopped"), "遥测收集已停止")
  assert_eq(MessageLocalizer::format_message(localizer, "telemetry.error", ["连接失败"]), "遥测错误：连接失败")
  assert_eq(MessageLocalizer::format_message(localizer, "metric.recorded", ["cpu使用率", "75.5"]), "指标 cpu使用率 已记录，值为 75.5")
  
  // Test Japanese localization
  MessageLocalizer::set_locale(localizer, "ja")
  assert_eq(MessageLocalizer::get_message(localizer, "telemetry.started"), "テレメトリ収集が開始されました")
  assert_eq(MessageLocalizer::get_message(localizer, "telemetry.stopped"), "テレメトリ収集が停止されました")
  assert_eq(MessageLocalizer::format_message(localizer, "telemetry.error", ["接続に失敗しました"]), "テレメトリエラー: 接続に失敗しました")
  assert_eq(MessageLocalizer::format_message(localizer, "metric.recorded", ["CPU使用率", "75.5"]), "メトリック CPU使用率 が値 75.5 で記録されました")
  
  // Test fallback to default locale
  MessageLocalizer::set_locale(localizer, "fr") // French not loaded
  assert_eq(MessageLocalizer::get_message(localizer, "telemetry.started"), "Telemetry collection started") // Falls back to English
}

// Test 5: Right-to-Left Text Support
test "right-to-left text support" {
  let text_processor = TextProcessor::new()
  
  // Test Arabic text (RTL)
  let arabic_text = "نظام القياس عن بعد"
  TextProcessor::set_direction(text_processor, "rtl")
  let processed_arabic = TextProcessor::process(text_processor, arabic_text)
  assert_eq(processed_arabic, "نظام القياس عن بعد")
  
  // Test mixed LTR/RTL text
  let mixed_text = "Azimuth نظام القياس"
  let processed_mixed = TextProcessor::process(text_processor, mixed_text)
  // Should preserve both LTR and RTL parts correctly
  assert_true(processed_mixed.contains("Azimuth"))
  assert_true(processed_mixed.contains("نظام القياس"))
  
  // Test Hebrew text (RTL)
  let hebrew_text = "מערכת טלמטריה"
  let processed_hebrew = TextProcessor::process(text_processor, hebrew_text)
  assert_eq(processed_hebrew, "מערכת טלמטריה")
  
  // Test text direction detection
  assert_eq(TextProcessor::detect_direction(text_processor, arabic_text), "rtl")
  assert_eq(TextProcessor::detect_direction(text_processor, hebrew_text), "rtl")
  assert_eq(TextProcessor::detect_direction(text_processor, "English text"), "ltr")
  assert_eq(TextProcessor::detect_direction(text_processor, "中文文本"), "ltr") // Chinese is LTR
}

// Test 6: Telemetry Data with International Characters
test "telemetry data with international characters" {
  // Create spans with international names
  let chinese_span_ctx = SpanContext::new("中文追踪ID", "中文跨度ID", true, "")
  let chinese_span = Span::new("中文跨度名称", Internal, chinese_span_ctx)
  Span::add_event(chinese_span, "中文事件", Some([("中文属性", StringValue("中文值"))]))
  
  let arabic_span_ctx = SpanContext::new("معرف التتبع", "معرف الامتداد", true, "")
  let arabic_span = Span::new("اسم الامتداد", Internal, arabic_span_ctx)
  Span::add_event(arabic_span, "حدث", Some([("سمة", StringValue("قيمة"))]))
  
  let japanese_span_ctx = SpanContext::new("トレースID", "スパンID", true, "")
  let japanese_span = Span::new("スパン名", Internal, japanese_span_ctx)
  Span::add_event(japanese_span, "イベント", Some([("属性", StringValue("値"))]))
  
  // Test that international data is preserved
  assert_eq(Span::name(chinese_span), "中文跨度名称")
  assert_eq(SpanContext::trace_id(chinese_span_ctx), "中文追踪ID")
  
  assert_eq(Span::name(arabic_span), "اسم الامتداد")
  assert_eq(SpanContext::trace_id(arabic_span_ctx), "معرف التتبع")
  
  assert_eq(Span::name(japanese_span), "スパン名")
  assert_eq(SpanContext::trace_id(japanese_span_ctx), "トレースID")
  
  // Test serialization and deserialization with international data
  let serialized_chinese = SpanSerializer::serialize(chinese_span)
  let deserialized_chinese = SpanSerializer::deserialize(serialized_chinese)
  
  assert_eq(Span::name(deserialized_chinese), "中文跨度名称")
  
  let serialized_arabic = SpanSerializer::serialize(arabic_span)
  let deserialized_arabic = SpanSerializer::deserialize(serialized_arabic)
  
  assert_eq(Span::name(deserialized_arabic), "اسم الامتداد")
  
  let serialized_japanese = SpanSerializer::serialize(japanese_span)
  let deserialized_japanese = SpanSerializer::deserialize(serialized_japanese)
  
  assert_eq(Span::name(deserialized_japanese), "スパン名")
}

// Test 7: Localized Error Messages
test "localized error messages" {
  let error_localizer = ErrorLocalizer::new()
  
  // Load error messages for different locales
  let en_errors = {
    "connection.failed": "Connection failed: {0}",
    "data.invalid": "Invalid data: {0}",
    "permission.denied": "Permission denied",
    "resource.not_found": "Resource not found: {0}",
    "timeout.exceeded": "Operation timed out after {0} seconds"
  }
  
  let zh_errors = {
    "connection.failed": "连接失败：{0}",
    "data.invalid": "无效数据：{0}",
    "permission.denied": "权限被拒绝",
    "resource.not_found": "资源未找到：{0}",
    "timeout.exceeded": "操作在 {0} 秒后超时"
  }
  
  let ja_errors = {
    "connection.failed": "接続に失敗しました: {0}",
    "data.invalid": "無効なデータ: {0}",
    "permission.denied": "アクセスが拒否されました",
    "resource.not_found": "リソースが見つかりません: {0}",
    "timeout.exceeded": "操作が {0} 秒後にタイムアウトしました"
  }
  
  ErrorLocalizer::load_bundle(error_localizer, "en", en_errors)
  ErrorLocalizer::load_bundle(error_localizer, "zh", zh_errors)
  ErrorLocalizer::load_bundle(error_localizer, "ja", ja_errors)
  
  // Test English error messages
  ErrorLocalizer::set_locale(error_localizer, "en")
  assert_eq(ErrorLocalizer::get_error_message(error_localizer, "permission.denied"), "Permission denied")
  assert_eq(ErrorLocalizer::format_error_message(error_localizer, "connection.failed", ["network unreachable"]), "Connection failed: network unreachable")
  
  // Test Chinese error messages
  ErrorLocalizer::set_locale(error_localizer, "zh")
  assert_eq(ErrorLocalizer::get_error_message(error_localizer, "permission.denied"), "权限被拒绝")
  assert_eq(ErrorLocalizer::format_error_message(error_localizer, "connection.failed", ["网络不可达"]), "连接失败：网络不可达")
  
  // Test Japanese error messages
  ErrorLocalizer::set_locale(error_localizer, "ja")
  assert_eq(ErrorLocalizer::get_error_message(error_localizer, "permission.denied"), "アクセスが拒否されました")
  assert_eq(ErrorLocalizer::format_error_message(error_localizer, "connection.failed", ["ネットワークに到達できません"]), "接続に失敗しました: ネットワークに到達できません")
}

// Test 8: Localized Log Messages
test "localized log messages" {
  let log_localizer = LogLocalizer::new()
  
  // Load log messages for different locales
  let en_logs = {
    "service.started": "Service {0} started on port {1}",
    "service.stopped": "Service {0} stopped",
    "request.received": "Received {0} request from {1}",
    "response.sent": "Sent {0} response to {1}",
    "error.occurred": "Error occurred: {0}"
  }
  
  let zh_logs = {
    "service.started": "服务 {0} 已在端口 {1} 上启动",
    "service.stopped": "服务 {0} 已停止",
    "request.received": "从 {1} 收到 {0} 请求",
    "response.sent": "向 {1} 发送 {0} 响应",
    "error.occurred": "发生错误：{0}"
  }
  
  let ja_logs = {
    "service.started": "サービス {0} がポート {1} で開始されました",
    "service.stopped": "サービス {0} が停止されました",
    "request.received": "{1} から {0} リクエストを受信しました",
    "response.sent": "{1} に {0} レスポンスを送信しました",
    "error.occurred": "エラーが発生しました: {0}"
  }
  
  LogLocalizer::load_bundle(log_localizer, "en", en_logs)
  LogLocalizer::load_bundle(log_localizer, "zh", zh_logs)
  LogLocalizer::load_bundle(log_localizer, "ja", ja_logs)
  
  // Create localized log records
  LogLocalizer::set_locale(log_localizer, "en")
  let en_log = LogLocalizer::create_log(log_localizer, Info, "service.started", ["telemetry", "8080"])
  assert_eq(LogRecord::severity_number(en_log), Info)
  match LogRecord::body(en_log) {
    Some(body) => assert_eq(body, "Service telemetry started on port 8080")
    None => assert_true(false)
  }
  
  LogLocalizer::set_locale(log_localizer, "zh")
  let zh_log = LogLocalizer::create_log(log_localizer, Info, "service.started", ["遥测", "8080"])
  match LogRecord::body(zh_log) {
    Some(body) => assert_eq(body, "服务 遥测 已在端口 8080 上启动")
    None => assert_true(false)
  }
  
  LogLocalizer::set_locale(log_localizer, "ja")
  let ja_log = LogLocalizer::create_log(log_localizer, Info, "service.started", ["テレメトリ", "8080"])
  match LogRecord::body(ja_log) {
    Some(body) => assert_eq(body, "サービス テレメトリ がポート 8080 で開始されました")
    None => assert_true(false)
  }
}

// Test 9: Localized Metric Names and Descriptions
test "localized metric names and descriptions" {
  let metric_localizer = MetricLocalizer::new()
  
  // Load metric information for different locales
  let en_metrics = {
    "cpu.usage": {
      "name": "CPU Usage",
      "description": "Percentage of CPU utilization",
      "unit": "%"
    },
    "memory.usage": {
      "name": "Memory Usage",
      "description": "Percentage of memory utilization",
      "unit": "%"
    },
    "request.count": {
      "name": "Request Count",
      "description": "Total number of requests",
      "unit": "count"
    }
  }
  
  let zh_metrics = {
    "cpu.usage": {
      "name": "CPU使用率",
      "description": "CPU利用率百分比",
      "unit": "%"
    },
    "memory.usage": {
      "name": "内存使用率",
      "description": "内存利用率百分比",
      "unit": "%"
    },
    "request.count": {
      "name": "请求数量",
      "description": "请求总数",
      "unit": "计数"
    }
  }
  
  let ja_metrics = {
    "cpu.usage": {
      "name": "CPU使用率",
      "description": "CPU利用率のパーセンテージ",
      "unit": "%"
    },
    "memory.usage": {
      "name": "メモリ使用率",
      "description": "メモリ利用率のパーセンテージ",
      "unit": "%"
    },
    "request.count": {
      "name": "リクエスト数",
      "description": "リクエストの総数",
      "unit": "カウント"
    }
  }
  
  MetricLocalizer::load_bundle(metric_localizer, "en", en_metrics)
  MetricLocalizer::load_bundle(metric_localizer, "zh", zh_metrics)
  MetricLocalizer::load_bundle(metric_localizer, "ja", ja_metrics)
  
  // Test English metric information
  MetricLocalizer::set_locale(metric_localizer, "en")
  let en_cpu_info = MetricLocalizer::get_metric_info(metric_localizer, "cpu.usage")
  assert_eq(en_cpu_info.name, "CPU Usage")
  assert_eq(en_cpu_info.description, "Percentage of CPU utilization")
  assert_eq(en_cpu_info.unit, "%")
  
  // Test Chinese metric information
  MetricLocalizer::set_locale(metric_localizer, "zh")
  let zh_cpu_info = MetricLocalizer::get_metric_info(metric_localizer, "cpu.usage")
  assert_eq(zh_cpu_info.name, "CPU使用率")
  assert_eq(zh_cpu_info.description, "CPU利用率百分比")
  assert_eq(zh_cpu_info.unit, "%")
  
  // Test Japanese metric information
  MetricLocalizer::set_locale(metric_localizer, "ja")
  let ja_cpu_info = MetricLocalizer::get_metric_info(metric_localizer, "cpu.usage")
  assert_eq(ja_cpu_info.name, "CPU使用率")
  assert_eq(ja_cpu_info.description, "CPU利用率のパーセンテージ")
  assert_eq(ja_cpu_info.unit, "%")
}

// Test 10: Locale-aware Sorting and Comparison
test "locale-aware sorting and comparison" {
  let string_comparator = StringComparator::new()
  
  // Test strings with different locales
  let strings = ["apple", "banana", "cherry", "date", "elderberry"]
  let chinese_strings = ["苹果", "香蕉", "樱桃", "枣", "接骨木莓"]
  let japanese_strings = ["りんご", "バナナ", "チェリー", "デート", "エルダベリー"]
  
  // Test English sorting
  StringComparator::set_locale(string_comparator, "en-US")
  let sorted_en = StringComparator::sort(string_comparator, strings)
  assert_eq(sorted_en, ["apple", "banana", "cherry", "date", "elderberry"])
  
  // Test Chinese sorting
  StringComparator::set_locale(string_comparator, "zh-CN")
  let sorted_zh = StringComparator::sort(string_comparator, chinese_strings)
  // Note: Actual sorting order depends on Chinese collation rules
  assert_eq(sorted_zh.length(), 5)
  assert_true(sorted_zh.contains("苹果"))
  assert_true(sorted_zh.contains("香蕉"))
  
  // Test Japanese sorting
  StringComparator::set_locale(string_comparator, "ja-JP")
  let sorted_ja = StringComparator::sort(string_comparator, japanese_strings)
  // Note: Actual sorting order depends on Japanese collation rules
  assert_eq(sorted_ja.length(), 5)
  assert_true(sorted_ja.contains("りんご"))
  assert_true(sorted_ja.contains("バナナ"))
  
  // Test locale-aware comparison
  StringComparator::set_locale(string_comparator, "en-US")
  assert_eq(StringComparator::compare(string_comparator, "apple", "banana"), -1)
  assert_eq(StringComparator::compare(string_comparator, "banana", "apple"), 1)
  assert_eq(StringComparator::compare(string_comparator, "apple", "apple"), 0)
  
  // Test case-insensitive comparison
  StringComparator::set_case_sensitive(string_comparator, false)
  assert_eq(StringComparator::compare(string_comparator, "Apple", "apple"), 0)
  assert_eq(StringComparator::compare(string_comparator, "APPLE", "banana"), -1)
  
  // Test accent-sensitive comparison
  StringComparator::set_locale(string_comparator, "fr-FR")
  StringComparator::set_case_sensitive(string_comparator, false)
  assert_eq(StringComparator::compare(string_comparator, "café", "cafe"), 1) // café comes after cafe
}