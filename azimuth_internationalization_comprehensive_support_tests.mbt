// Azimuth 国际化(i18n)综合支持测试用例
// 专注于测试多语言支持、本地化、区域设置和文化适应性

// 测试1: 基础多语言支持
test "基础多语言支持" {
  // 1. 创建国际化管理器
  let i18n_manager = I18nManager::new()
  
  // 2. 加载语言资源
  I18nManager::load_language(i18n_manager, "en", {
    "telemetry.title": "Azimuth Telemetry System",
    "telemetry.description": "Advanced observability and monitoring solution",
    "metrics.cpu": "CPU Usage",
    "metrics.memory": "Memory Usage",
    "metrics.network": "Network Traffic",
    "error.connection_failed": "Connection to server failed",
    "status.active": "Active",
    "status.inactive": "Inactive"
  })
  
  I18nManager::load_language(i18n_manager, "zh", {
    "telemetry.title": "Azimuth 遥测系统",
    "telemetry.description": "高级可观测性和监控解决方案",
    "metrics.cpu": "CPU 使用率",
    "metrics.memory": "内存使用率",
    "metrics.network": "网络流量",
    "error.connection_failed": "连接服务器失败",
    "status.active": "活跃",
    "status.inactive": "非活跃"
  })
  
  I18nManager::load_language(i18n_manager, "ja", {
    "telemetry.title": "Azimuth テレメトリーシステム",
    "telemetry.description": "高度な可観測性と監視ソリューション",
    "metrics.cpu": "CPU使用率",
    "metrics.memory": "メモリ使用率",
    "metrics.network": "ネットワークトラフィック",
    "error.connection_failed": "サーバーへの接続に失敗しました",
    "status.active": "アクティブ",
    "status.inactive": "非アクティブ"
  })
  
  // 3. 测试英文翻译
  I18nManager::set_current_language(i18n_manager, "en")
  assert_eq(I18nManager::translate(i18n_manager, "telemetry.title"), "Azimuth Telemetry System")
  assert_eq(I18nManager::translate(i18n_manager, "metrics.cpu"), "CPU Usage")
  assert_eq(I18nManager::translate(i18n_manager, "status.active"), "Active")
  
  // 4. 测试中文翻译
  I18nManager::set_current_language(i18n_manager, "zh")
  assert_eq(I18nManager::translate(i18n_manager, "telemetry.title"), "Azimuth 遥测系统")
  assert_eq(I18nManager::translate(i18n_manager, "metrics.cpu"), "CPU 使用率")
  assert_eq(I18nManager::translate(i18n_manager, "status.active"), "活跃")
  
  // 5. 测试日文翻译
  I18nManager::set_current_language(i18n_manager, "ja")
  assert_eq(I18nManager::translate(i18n_manager, "telemetry.title"), "Azimuth テレメトリーシステム")
  assert_eq(I18nManager::translate(i18n_manager, "metrics.cpu"), "CPU使用率")
  assert_eq(I18nManager::translate(i18n_manager, "status.active"), "アクティブ")
  
  // 6. 测试缺失键的回退
  I18nManager::set_current_language(i18n_manager, "zh")
  assert_eq(I18nManager::translate(i18n_manager, "nonexistent.key"), "nonexistent.key")
}

// 测试2: 参数化翻译
test "参数化翻译" {
  // 1. 创建国际化管理器
  let i18n_manager = I18nManager::new()
  
  // 2. 加载带参数的语言资源
  I18nManager::load_language(i18n_manager, "en", {
    "telemetry.span_duration": "Span '{name}' completed in {duration}ms",
    "telemetry.metric_value": "Metric '{metric}' has value {value}",
    "error.count_format": "Found {count} errors in the last {timeframe}",
    "user.greeting": "Hello, {name}! Welcome to {system}."
  })
  
  I18nManager::load_language(i18n_manager, "zh", {
    "telemetry.span_duration": "跨度 '{name}' 在 {duration} 毫秒内完成",
    "telemetry.metric_value": "指标 '{metric}' 的值为 {value}",
    "error.count_format": "在过去 {timeframe} 中发现 {count} 个错误",
    "user.greeting": "你好，{name}！欢迎使用 {system}。"
  })
  
  // 3. 测试英文参数化翻译
  I18nManager::set_current_language(i18n_manager, "en")
  let en_span_duration = I18nManager::translate_with_params(
    i18n_manager, 
    "telemetry.span_duration", 
    [("name", "database_query"), ("duration", "250")]
  )
  assert_eq(en_span_duration, "Span 'database_query' completed in 250ms")
  
  let en_metric_value = I18nManager::translate_with_params(
    i18n_manager, 
    "telemetry.metric_value", 
    [("metric", "cpu_usage"), ("value", "75.5%")]
  )
  assert_eq(en_metric_value, "Metric 'cpu_usage' has value 75.5%")
  
  // 4. 测试中文参数化翻译
  I18nManager::set_current_language(i18n_manager, "zh")
  let zh_span_duration = I18nManager::translate_with_params(
    i18n_manager, 
    "telemetry.span_duration", 
    [("name", "database_query"), ("duration", "250")]
  )
  assert_eq(zh_span_duration, "跨度 'database_query' 在 250 毫秒内完成")
  
  let zh_metric_value = I18nManager::translate_with_params(
    i18n_manager, 
    "telemetry.metric_value", 
    [("metric", "cpu_usage"), ("value", "75.5%")]
  )
  assert_eq(zh_metric_value, "指标 'cpu_usage' 的值为 75.5%")
  
  // 5. 测试数字格式化
  I18nManager::set_current_language(i18n_manager, "en")
  let en_error_count = I18nManager::translate_with_params(
    i18n_manager, 
    "error.count_format", 
    [("count", "42"), ("timeframe", "hour")]
  )
  assert_eq(en_error_count, "Found 42 errors in the last hour")
  
  I18nManager::set_current_language(i18n_manager, "zh")
  let zh_error_count = I18nManager::translate_with_params(
    i18n_manager, 
    "error.count_format", 
    [("count", "42"), ("timeframe", "小时")]
  )
  assert_eq(zh_error_count, "在过去 小时 中发现 42 个错误")
}

// 测试3: 复数形式处理
test "复数形式处理" {
  // 1. 创建国际化管理器
  let i18n_manager = I18nManager::new()
  
  // 2. 加载带复数形式的语言资源
  I18nManager::load_language_with_plurals(i18n_manager, "en", {
    "telemetry.span_count": {
      "one": "Found {count} span",
      "other": "Found {count} spans"
    },
    "telemetry.error_count": {
      "zero": "No errors found",
      "one": "Found {count} error",
      "other": "Found {count} errors"
    },
    "telemetry.metric_count": {
      "one": "One metric available",
      "other": "{count} metrics available"
    }
  })
  
  I18nManager::load_language_with_plurals(i18n_manager, "zh", {
    "telemetry.span_count": {
      "other": "发现 {count} 个跨度"
    },
    "telemetry.error_count": {
      "other": "发现 {count} 个错误"
    },
    "telemetry.metric_count": {
      "other": "有 {count} 个指标可用"
    }
  })
  
  // 3. 测试英文复数形式
  I18nManager::set_current_language(i18n_manager, "en")
  
  // 测试单数形式
  let en_span_one = I18nManager::translate_plural(i18n_manager, "telemetry.span_count", 1)
  assert_eq(en_span_one, "Found 1 span")
  
  let en_error_one = I18nManager::translate_plural(i18n_manager, "telemetry.error_count", 1)
  assert_eq(en_error_one, "Found 1 error")
  
  let en_metric_one = I18nManager::translate_plural(i18n_manager, "telemetry.metric_count", 1)
  assert_eq(en_metric_one, "One metric available")
  
  // 测试复数形式
  let en_span_many = I18nManager::translate_plural(i18n_manager, "telemetry.span_count", 5)
  assert_eq(en_span_many, "Found 5 spans")
  
  let en_error_many = I18nManager::translate_plural(i18n_manager, "telemetry.error_count", 5)
  assert_eq(en_error_many, "Found 5 errors")
  
  let en_metric_many = I18nManager::translate_plural(i18n_manager, "telemetry.metric_count", 5)
  assert_eq(en_metric_many, "5 metrics available")
  
  // 测试零形式
  let en_error_zero = I18nManager::translate_plural(i18n_manager, "telemetry.error_count", 0)
  assert_eq(en_error_zero, "No errors found")
  
  // 4. 测试中文复数形式（中文通常不区分单复数）
  I18nManager::set_current_language(i18n_manager, "zh")
  
  let zh_span_one = I18nManager::translate_plural(i18n_manager, "telemetry.span_count", 1)
  assert_eq(zh_span_one, "发现 1 个跨度")
  
  let zh_span_many = I18nManager::translate_plural(i18n_manager, "telemetry.span_count", 5)
  assert_eq(zh_span_many, "发现 5 个跨度")
  
  let zh_error_zero = I18nManager::translate_plural(i18n_manager, "telemetry.error_count", 0)
  assert_eq(zh_error_zero, "发现 0 个错误")
}

// 测试4: 区域设置和格式化
test "区域设置和格式化" {
  // 1. 创建区域设置管理器
  let locale_manager = LocaleManager::new()
  
  // 2. 设置不同的区域设置
  LocaleManager::set_locale(locale_manager, "en-US")
  LocaleManager::configure_number_format(locale_manager, {
    decimal_separator: ".",
    thousands_separator: ",",
    currency_symbol: "$",
    currency_position: "before"
  })
  
  LocaleManager::set_locale(locale_manager, "zh-CN")
  LocaleManager::configure_number_format(locale_manager, {
    decimal_separator: ".",
    thousands_separator: ",",
    currency_symbol: "¥",
    currency_position: "before"
  })
  
  LocaleManager::set_locale(locale_manager, "de-DE")
  LocaleManager::configure_number_format(locale_manager, {
    decimal_separator: ",",
    thousands_separator: ".",
    currency_symbol: "€",
    currency_position: "after"
  })
  
  // 3. 测试数字格式化
  LocaleManager::set_locale(locale_manager, "en-US")
  let en_number = LocaleManager::format_number(locale_manager, 1234567.89)
  assert_eq(en_number, "1,234,567.89")
  
  let en_currency = LocaleManager::format_currency(locale_manager, 1234.56)
  assert_eq(en_currency, "$1,234.56")
  
  // 4. 测试中文数字格式化
  LocaleManager::set_locale(locale_manager, "zh-CN")
  let zh_number = LocaleManager::format_number(locale_manager, 1234567.89)
  assert_eq(zh_number, "1,234,567.89")
  
  let zh_currency = LocaleManager::format_currency(locale_manager, 1234.56)
  assert_eq(zh_currency, "¥1,234.56")
  
  // 5. 测试德文数字格式化
  LocaleManager::set_locale(locale_manager, "de-DE")
  let de_number = LocaleManager::format_number(locale_manager, 1234567.89)
  assert_eq(de_number, "1.234.567,89")
  
  let de_currency = LocaleManager::format_currency(locale_manager, 1234.56)
  assert_eq(de_currency, "1.234,56€")
  
  // 6. 测试日期时间格式化
  LocaleManager::configure_datetime_format(locale_manager, "en-US", "MM/DD/YYYY HH:mm:ss")
  LocaleManager::configure_datetime_format(locale_manager, "zh-CN", "YYYY年MM月DD日 HH:mm:ss")
  LocaleManager::configure_datetime_format(locale_manager, "de-DE", "DD.MM.YYYY HH:mm:ss")
  
  let test_timestamp = 1735689600000000000L // 2025-01-01 00:00:00 UTC
  
  LocaleManager::set_locale(locale_manager, "en-US")
  let en_datetime = LocaleManager::format_datetime(locale_manager, test_timestamp)
  assert_eq(en_datetime, "01/01/2025 00:00:00")
  
  LocaleManager::set_locale(locale_manager, "zh-CN")
  let zh_datetime = LocaleManager::format_datetime(locale_manager, test_timestamp)
  assert_eq(zh_datetime, "2025年01月01日 00:00:00")
  
  LocaleManager::set_locale(locale_manager, "de-DE")
  let de_datetime = LocaleManager::format_datetime(locale_manager, test_timestamp)
  assert_eq(de_datetime, "01.01.2025 00:00:00")
}

// 测试5: 文本方向和RTL支持
test "文本方向和RTL支持" {
  // 1. 创建文本方向管理器
  let text_direction_manager = TextDirectionManager::new()
  
  // 2. 设置不同语言的文本方向
  TextDirectionManager::set_language_direction(text_direction_manager, "en", "LTR")
  TextDirectionManager::set_language_direction(text_direction_manager, "zh", "LTR")
  TextDirectionManager::set_language_direction(text_direction_manager, "ar", "RTL")
  TextDirectionManager::set_language_direction(text_direction_manager, "he", "RTL")
  
  // 3. 测试LTR语言
  TextDirectionManager::set_current_language(text_direction_manager, "en")
  assert_eq(TextDirectionManager::get_direction(text_direction_manager), "LTR")
  assert_eq(TextDirectionManager::get_alignment(text_direction_manager), "left")
  assert_eq(TextDirectionManager::get_text_align(text_direction_manager), "left")
  
  // 4. 测试RTL语言
  TextDirectionManager::set_current_language(text_direction_manager, "ar")
  assert_eq(TextDirectionManager::get_direction(text_direction_manager), "RTL")
  assert_eq(TextDirectionManager::get_alignment(text_direction_manager), "right")
  assert_eq(TextDirectionManager::get_text_align(text_direction_manager), "right")
  
  // 5. 测试混合方向文本处理
  let mixed_text = "Hello مرحبا World"
  let processed_text = TextDirectionManager::process_mixed_direction(text_direction_manager, mixed_text)
  assert_true(processed_text.contains("Hello"))
  assert_true(processed_text.contains("مرحبا"))
  assert_true(processed_text.contains("World"))
  
  // 6. 测试UI布局调整
  let ui_elements = [
    ("navigation", "left"),
    ("sidebar", "left"),
    ("content", "left"),
    ("controls", "right")
  ]
  
  // LTR布局
  TextDirectionManager::set_current_language(text_direction_manager, "en")
  let ltr_layout = TextDirectionManager::adjust_ui_layout(text_direction_manager, ui_elements)
  assert_eq(ltr_layout[0].1, "left")  // navigation
  assert_eq(ltr_layout[1].1, "left")  // sidebar
  assert_eq(ltr_layout[2].1, "left")  // content
  assert_eq(ltr_layout[3].1, "right") // controls
  
  // RTL布局
  TextDirectionManager::set_current_language(text_direction_manager, "ar")
  let rtl_layout = TextDirectionManager::adjust_ui_layout(text_direction_manager, ui_elements)
  assert_eq(rtl_layout[0].1, "right") // navigation
  assert_eq(rtl_layout[1].1, "right") // sidebar
  assert_eq(rtl_layout[2].1, "right") // content
  assert_eq(rtl_layout[3].1, "left")  // controls
}

// 测试6: 动态语言切换
test "动态语言切换" {
  // 1. 创建国际化管理器
  let i18n_manager = I18nManager::new()
  
  // 2. 加载多种语言资源
  I18nManager::load_language(i18n_manager, "en", {
    "ui.dashboard": "Dashboard",
    "ui.metrics": "Metrics",
    "ui.traces": "Traces",
    "ui.logs": "Logs",
    "message.welcome": "Welcome to Azimuth Telemetry"
  })
  
  I18nManager::load_language(i18n_manager, "zh", {
    "ui.dashboard": "仪表板",
    "ui.metrics": "指标",
    "ui.traces": "追踪",
    "ui.logs": "日志",
    "message.welcome": "欢迎使用 Azimuth 遥测系统"
  })
  
  I18nManager::load_language(i18n_manager, "ja", {
    "ui.dashboard": "ダッシュボード",
    "ui.metrics": "メトリクス",
    "ui.traces": "トレース",
    "ui.logs": "ログ",
    "message.welcome": "Azimuth テレメトリーシステムへようこそ"
  })
  
  // 3. 创建UI组件
  let ui_components = [
    ("dashboard_title", "ui.dashboard"),
    ("metrics_title", "ui.metrics"),
    ("traces_title", "ui.traces"),
    ("logs_title", "ui.logs"),
    ("welcome_message", "message.welcome")
  ]
  
  // 4. 测试初始语言（英文）
  I18nManager::set_current_language(i18n_manager, "en")
  let en_ui = I18nManager::translate_components(i18n_manager, ui_components)
  assert_eq(en_ui[0].1, "Dashboard")
  assert_eq(en_ui[1].1, "Metrics")
  assert_eq(en_ui[2].1, "Traces")
  assert_eq(en_ui[3].1, "Logs")
  assert_eq(en_ui[4].1, "Welcome to Azimuth Telemetry")
  
  // 5. 动态切换到中文
  I18nManager::set_current_language(i18n_manager, "zh")
  let zh_ui = I18nManager::translate_components(i18n_manager, ui_components)
  assert_eq(zh_ui[0].1, "仪表板")
  assert_eq(zh_ui[1].1, "指标")
  assert_eq(zh_ui[2].1, "追踪")
  assert_eq(zh_ui[3].1, "日志")
  assert_eq(zh_ui[4].1, "欢迎使用 Azimuth 遥测系统")
  
  // 6. 动态切换到日文
  I18nManager::set_current_language(i18n_manager, "ja")
  let ja_ui = I18nManager::translate_components(i18n_manager, ui_components)
  assert_eq(ja_ui[0].1, "ダッシュボード")
  assert_eq(ja_ui[1].1, "メトリクス")
  assert_eq(ja_ui[2].1, "トレース")
  assert_eq(ja_ui[3].1, "ログ")
  assert_eq(ja_ui[4].1, "Azimuth テレメトリーシステムへようこそ")
  
  // 7. 测试语言切换事件
  let language_change_events = []
  I18nManager::set_language_change_callback(i18n_manager, fn(old_lang, new_lang) {
    language_change_events.push((old_lang, new_lang))
  })
  
  // 8. 触发语言切换
  I18nManager::set_current_language(i18n_manager, "en")
  I18nManager::set_current_language(i18n_manager, "zh")
  I18nManager::set_current_language(i18n_manager, "ja")
  
  // 9. 验证语言切换事件
  assert_eq(language_change_events.length(), 3)
  assert_eq(language_change_events[0], ("en", "zh"))
  assert_eq(language_change_events[1], ("zh", "ja"))
  assert_eq(language_change_events[2], ("ja", "en"))
}

// 测试7: 错误消息本地化
test "错误消息本地化" {
  // 1. 创建错误国际化管理器
  let error_i18n_manager = ErrorI18nManager::new()
  
  // 2. 加载错误消息的多语言版本
  ErrorI18nManager::load_error_messages(error_i18n_manager, "en", {
    "connection.timeout": "Connection timeout after {timeout} seconds",
    "connection.refused": "Connection refused by server",
    "authentication.failed": "Authentication failed: {reason}",
    "resource.not_found": "Resource '{resource}' not found",
    "permission.denied": "Permission denied: {action} requires {permission}"
  })
  
  ErrorI18nManager::load_error_messages(error_i18n_manager, "zh", {
    "connection.timeout": "连接超时，已等待 {timeout} 秒",
    "connection.refused": "服务器拒绝连接",
    "authentication.failed": "身份验证失败：{reason}",
    "resource.not_found": "资源 '{resource}' 未找到",
    "permission.denied": "权限被拒绝：{action} 需要 {permission} 权限"
  })
  
  ErrorI18nManager::load_error_messages(error_i18n_manager, "ja", {
    "connection.timeout": "{timeout}秒後に接続がタイムアウトしました",
    "connection.refused": "サーバーによって接続が拒否されました",
    "authentication.failed": "認証に失敗しました：{reason}",
    "resource.not_found": "リソース '{resource}' が見つかりません",
    "permission.denied": "アクセス拒否：{action}には{permission}権限が必要です"
  })
  
  // 3. 创建错误对象
  let timeout_error = Error::new("connection.timeout", [("timeout", "30")])
  let auth_error = Error::new("authentication.failed", [("reason", "invalid credentials")])
  let resource_error = Error::new("resource.not_found", [("resource", "telemetry_data_123")])
  
  // 4. 测试英文错误消息
  ErrorI18nManager::set_current_language(error_i18n_manager, "en")
  assert_eq(
    ErrorI18nManager::format_error(error_i18n_manager, timeout_error),
    "Connection timeout after 30 seconds"
  )
  assert_eq(
    ErrorI18nManager::format_error(error_i18n_manager, auth_error),
    "Authentication failed: invalid credentials"
  )
  assert_eq(
    ErrorI18nManager::format_error(error_i18n_manager, resource_error),
    "Resource 'telemetry_data_123' not found"
  )
  
  // 5. 测试中文错误消息
  ErrorI18nManager::set_current_language(error_i18n_manager, "zh")
  assert_eq(
    ErrorI18nManager::format_error(error_i18n_manager, timeout_error),
    "连接超时，已等待 30 秒"
  )
  assert_eq(
    ErrorI18nManager::format_error(error_i18n_manager, auth_error),
    "身份验证失败：invalid credentials"
  )
  assert_eq(
    ErrorI18nManager::format_error(error_i18n_manager, resource_error),
    "资源 'telemetry_data_123' 未找到"
  )
  
  // 6. 测试日文错误消息
  ErrorI18nManager::set_current_language(error_i18n_manager, "ja")
  assert_eq(
    ErrorI18nManager::format_error(error_i18n_manager, timeout_error),
    "30秒後に接続がタイムアウトしました"
  )
  assert_eq(
    ErrorI18nManager::format_error(error_i18n_manager, auth_error),
    "認証に失敗しました：invalid credentials"
  )
  assert_eq(
    ErrorI18nManager::format_error(error_i18n_manager, resource_error),
    "リソース 'telemetry_data_123' が見つかりません"
  )
  
  // 7. 测试未知错误类型的回退
  let unknown_error = Error::new("unknown.error.type", [])
  ErrorI18nManager::set_current_language(error_i18n_manager, "en")
  assert_eq(
    ErrorI18nManager::format_error(error_i18n_manager, unknown_error),
    "unknown.error.type"
  )
}

// 测试8: 时区和时间格式本地化
test "时区和时间格式本地化" {
  // 1. 创建时区管理器
  let timezone_manager = TimezoneManager::new()
  
  // 2. 设置不同时区
  TimezoneManager::set_timezone(timezone_manager, "UTC", "UTC+0")
  TimezoneManager::set_timezone(timezone_manager, "America/New_York", "UTC-5")
  TimezoneManager::set_timezone(timezone_manager, "Europe/London", "UTC+0")
  TimezoneManager::set_timezone(timezone_manager, "Asia/Shanghai", "UTC+8")
  TimezoneManager::set_timezone(timezone_manager, "Asia/Tokyo", "UTC+9")
  
  // 3. 设置时间格式
  TimezoneManager::set_time_format(timezone_manager, "en", "12-hour")
  TimezoneManager::set_time_format(timezone_manager, "zh", "24-hour")
  TimezoneManager::set_time_format(timezone_manager, "ja", "24-hour")
  
  // 4. 创建基准时间戳（2025-01-01 12:00:00 UTC）
  let base_timestamp = 1735737600000000000L
  
  // 5. 测试不同时区的时间转换
  TimezoneManager::set_current_timezone(timezone_manager, "UTC")
  let utc_time = TimezoneManager::format_timestamp(timezone_manager, base_timestamp)
  assert_eq(utc_time, "2025-01-01 12:00:00 UTC")
  
  TimezoneManager::set_current_timezone(timezone_manager, "America/New_York")
  let ny_time = TimezoneManager::format_timestamp(timezone_manager, base_timestamp)
  assert_eq(ny_time, "2025-01-01 07:00:00 EST")
  
  TimezoneManager::set_current_timezone(timezone_manager, "Asia/Shanghai")
  let shanghai_time = TimezoneManager::format_timestamp(timezone_manager, base_timestamp)
  assert_eq(shanghai_time, "2025-01-01 20:00:00 CST")
  
  TimezoneManager::set_current_timezone(timezone_manager, "Asia/Tokyo")
  let tokyo_time = TimezoneManager::format_timestamp(timezone_manager, base_timestamp)
  assert_eq(tokyo_time, "2025-01-01 21:00:00 JST")
  
  // 6. 测试时间格式本地化
  TimezoneManager::set_current_timezone(timezone_manager, "America/New_York")
  
  // 12小时格式（英文）
  TimezoneManager::set_current_language(timezone_manager, "en")
  let ny_12hour = TimezoneManager::format_timestamp_with_format(timezone_manager, base_timestamp, "MM/DD/YYYY hh:mm:ss A")
  assert_eq(ny_12hour, "01/01/2025 07:00:00 PM")
  
  // 24小时格式（中文）
  TimezoneManager::set_current_language(timezone_manager, "zh")
  let shanghai_24hour = TimezoneManager::format_timestamp_with_format(timezone_manager, base_timestamp, "YYYY年MM月DD日 HH:mm:ss")
  assert_eq(shanghai_24hour, "2025年01月01日 20:00:00")
  
  // 24小时格式（日文）
  TimezoneManager::set_current_language(timezone_manager, "ja")
  let tokyo_24hour = TimezoneManager::format_timestamp_with_format(timezone_manager, base_timestamp, "YYYY年MM月DD日 HH:mm:ss")
  assert_eq(tokyo_24hour, "2025年01月01日 21:00:00")
  
  // 7. 测试相对时间格式
  let current_time = 1735741200000000000L // 2025-01-01 13:00:00 UTC
  let past_time = 1735734000000000000L     // 2025-01-01 11:00:00 UTC
  let future_time = 1735744800000000000L   // 2025-01-01 14:00:00 UTC
  
  TimezoneManager::set_current_language(timezone_manager, "en")
  assert_eq(TimezoneManager::format_relative_time(timezone_manager, past_time, current_time), "2 hours ago")
  assert_eq(TimezoneManager::format_relative_time(timezone_manager, future_time, current_time), "in 1 hour")
  
  TimezoneManager::set_current_language(timezone_manager, "zh")
  assert_eq(TimezoneManager::format_relative_time(timezone_manager, past_time, current_time), "2小时前")
  assert_eq(TimezoneManager::format_relative_time(timezone_manager, future_time, current_time), "1小时后")
  
  TimezoneManager::set_current_language(timezone_manager, "ja")
  assert_eq(TimezoneManager::format_relative_time(timezone_manager, past_time, current_time), "2時間前")
  assert_eq(TimezoneManager::format_relative_time(timezone_manager, future_time, current_time), "1時間後")
}