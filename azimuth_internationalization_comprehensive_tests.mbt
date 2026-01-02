// Azimuth国际化支持测试用例
// 测试Azimuth遥测系统的国际化和本地化功能

test "多语言错误消息支持" {
  // 创建国际化配置
  let i18n_config = InternationalizationConfig::new()
  InternationalizationConfig::set_default_locale(i18n_config, "en-US")
  
  // 加载不同语言的错误消息
  InternationalizationConfig::load_language_pack(i18n_config, "en-US", [
    ("error.connection.failed", "Connection to server failed"),
    ("error.timeout.exceeded", "Request timeout exceeded"),
    ("error.invalid.credentials", "Invalid authentication credentials"),
    ("error.resource.not.found", "Requested resource not found"),
    ("error.permission.denied", "Permission denied")
  ])
  
  InternationalizationConfig::load_language_pack(i18n_config, "zh-CN", [
    ("error.connection.failed", "连接服务器失败"),
    ("error.timeout.exceeded", "请求超时"),
    ("error.invalid.credentials", "无效的身份验证凭据"),
    ("error.resource.not.found", "未找到请求的资源"),
    ("error.permission.denied", "权限被拒绝")
  ])
  
  InternationalizationConfig::load_language_pack(i18n_config, "ja-JP", [
    ("error.connection.failed", "サーバーへの接続に失敗しました"),
    ("error.timeout.exceeded", "リクエストタイムアウトを超えました"),
    ("error.invalid.credentials", "無効な認証資格情報"),
    ("error.resource.not.found", "要求されたリソースが見つかりません"),
    ("error.permission.denied", "アクセス許可が拒否されました")
  ])
  
  // 测试英文错误消息
  InternationalizationConfig::set_current_locale(i18n_config, "en-US")
  let en_error_message = InternationalizationConfig::get_error_message(i18n_config, "error.connection.failed")
  assert_eq(en_error_message, "Connection to server failed")
  
  let en_timeout_message = InternationalizationConfig::get_error_message(i18n_config, "error.timeout.exceeded")
  assert_eq(en_timeout_message, "Request timeout exceeded")
  
  // 测试中文错误消息
  InternationalizationConfig::set_current_locale(i18n_config, "zh-CN")
  let zh_error_message = InternationalizationConfig::get_error_message(i18n_config, "error.connection.failed")
  assert_eq(zh_error_message, "连接服务器失败")
  
  let zh_timeout_message = InternationalizationConfig::get_error_message(i18n_config, "error.timeout.exceeded")
  assert_eq(zh_timeout_message, "请求超时")
  
  // 测试日文错误消息
  InternationalizationConfig::set_current_locale(i18n_config, "ja-JP")
  let ja_error_message = InternationalizationConfig::get_error_message(i18n_config, "error.connection.failed")
  assert_eq(ja_error_message, "サーバーへの接続に失敗しました")
  
  let ja_timeout_message = InternationalizationConfig::get_error_message(i18n_config, "error.timeout.exceeded")
  assert_eq(ja_timeout_message, "リクエストタイムアウトを超えました")
  
  // 测试回退到默认语言
  InternationalizationConfig::set_current_locale(i18n_config, "fr-FR")  // 未加载的语言
  let fallback_message = InternationalizationConfig::get_error_message(i18n_config, "error.connection.failed")
  assert_eq(fallback_message, "Connection to server failed")  // 应该回退到默认语言
  
  // 测试消息参数化
  InternationalizationConfig::load_language_pack(i18n_config, "en-US", [
    ("error.retry.attempt", "Retry attempt {0} of {1}")
  ])
  
  InternationalizationConfig::load_language_pack(i18n_config, "zh-CN", [
    ("error.retry.attempt", "重试第{0}次，共{1}次")
  ])
  
  InternationalizationConfig::set_current_locale(i18n_config, "en-US")
  let en_retry_message = InternationalizationConfig::get_formatted_error_message(
    i18n_config, 
    "error.retry.attempt", 
    ["2", "5"]
  )
  assert_eq(en_retry_message, "Retry attempt 2 of 5")
  
  InternationalizationConfig::set_current_locale(i18n_config, "zh-CN")
  let zh_retry_message = InternationalizationConfig::get_formatted_error_message(
    i18n_config, 
    "error.retry.attempt", 
    ["2", "5"]
  )
  assert_eq(zh_retry_message, "重试第2次，共5次")
}

test "多语言日志记录支持" {
  // 创建国际化日志配置
  let i18n_logger_config = InternationalizationLoggerConfig::new()
  InternationalizationLoggerConfig::set_default_locale(i18n_logger_config, "en-US")
  
  // 加载不同语言的日志模板
  InternationalizationLoggerConfig::load_log_templates(i18n_logger_config, "en-US", [
    ("log.user.login", "User {0} logged in from {1}"),
    ("log.operation.started", "Operation '{0}' started at {1}"),
    ("log.error.occurred", "Error occurred in {0}: {1}"),
    ("log.performance.metric", "Performance metric {0} recorded value {1}")
  ])
  
  InternationalizationLoggerConfig::load_log_templates(i18n_logger_config, "zh-CN", [
    ("log.user.login", "用户 {0} 从 {1} 登录"),
    ("log.operation.started", "操作 '{0}' 于 {1} 开始"),
    ("log.error.occurred", "在 {0} 中发生错误：{1}"),
    ("log.performance.metric", "性能指标 {0} 记录值 {1}")
  ])
  
  InternationalizationLoggerConfig::load_log_templates(i18n_logger_config, "ja-JP", [
    ("log.user.login", "ユーザー {0} が {1} からログインしました"),
    ("log.operation.started", "操作 '{0}' が {1} に開始されました"),
    ("log.error.occurred", "{0} でエラーが発生しました：{1}"),
    ("log.performance.metric", "パフォーマンスメトリック {0} が値 {1} を記録しました")
  ])
  
  // 创建支持国际化的日志记录器
  let logger = InternationalizationLogger::new(i18n_logger_config)
  
  // 测试英文日志记录
  InternationalizationLogger::set_locale(logger, "en-US")
  let en_log_message = InternationalizationLogger::format_log_message(
    logger, 
    "log.user.login", 
    ["john.doe", "192.168.1.100"]
  )
  assert_eq(en_log_message, "User john.doe logged in from 192.168.1.100")
  
  // 测试中文日志记录
  InternationalizationLogger::set_locale(logger, "zh-CN")
  let zh_log_message = InternationalizationLogger::format_log_message(
    logger, 
    "log.user.login", 
    ["john.doe", "192.168.1.100"]
  )
  assert_eq(zh_log_message, "用户 john.doe 从 192.168.1.100 登录")
  
  // 测试日文日志记录
  InternationalizationLogger::set_locale(logger, "ja-JP")
  let ja_log_message = InternationalizationLogger::format_log_message(
    logger, 
    "log.user.login", 
    ["john.doe", "192.168.1.100"]
  )
  assert_eq(ja_log_message, "ユーザー john.doe が 192.168.1.100 からログインしました")
  
  // 测试国际化日志记录器与标准日志系统的集成
  let standard_logger = LoggerProvider::get_logger(LoggerProvider::default(), "i18n.test")
  
  InternationalizationLogger::set_locale(logger, "en-US")
  let en_log = LogRecord::new(Info, en_log_message)
  LogRecord::add_attribute(en_log, "locale", "en-US")
  Logger::emit(standard_logger, en_log)
  
  InternationalizationLogger::set_locale(logger, "zh-CN")
  let zh_log = LogRecord::new(Info, zh_log_message)
  LogRecord::add_attribute(zh_log, "locale", "zh-CN")
  Logger::emit(standard_logger, zh_log)
  
  InternationalizationLogger::set_locale(logger, "ja-JP")
  let ja_log = LogRecord::new(Info, ja_log_message)
  LogRecord::add_attribute(ja_log, "locale", "ja-JP")
  Logger::emit(standard_logger, ja_log)
  
  assert_true(true)
}

test "本地化时间和日期格式" {
  // 创建本地化时间配置
  let locale_time_config = LocaleTimeConfig::new()
  
  // 测试不同地区的时间格式
  LocaleTimeConfig::set_locale(locale_time_config, "en-US")
  let us_time_format = LocaleTimeConfig::get_time_format(locale_time_config)
  assert_eq(us_time_format, "h:mm:ss a")  // 12小时制，带AM/PM
  
  LocaleTimeConfig::set_locale(locale_time_config, "zh-CN")
  let cn_time_format = LocaleTimeConfig::get_time_format(locale_time_config)
  assert_eq(cn_time_format, "HH:mm:ss")  // 24小时制
  
  LocaleTimeConfig::set_locale(locale_time_config, "ja-JP")
  let jp_time_format = LocaleTimeConfig::get_time_format(locale_time_config)
  assert_eq(jp_time_format, "HH:mm:ss")  // 24小时制
  
  // 测试不同地区的日期格式
  LocaleTimeConfig::set_locale(locale_time_config, "en-US")
  let us_date_format = LocaleTimeConfig::get_date_format(locale_time_config)
  assert_eq(us_date_format, "MM/dd/yyyy")  // 月/日/年
  
  LocaleTimeConfig::set_locale(locale_time_config, "zh-CN")
  let cn_date_format = LocaleTimeConfig::get_date_format(locale_time_config)
  assert_eq(cn_date_format, "yyyy-MM-dd")  // 年-月-日
  
  LocaleTimeConfig::set_locale(locale_time_config, "ja-JP")
  let jp_date_format = LocaleTimeConfig::get_date_format(locale_time_config)
  assert_eq(jp_date_format, "yyyy/MM/dd")  // 年/月/日
  
  // 测试时间戳格式化
  let test_timestamp = 1735689600000000000L  // 2025-01-02T10:00:00Z
  
  LocaleTimeConfig::set_locale(locale_time_config, "en-US")
  let us_formatted_time = LocaleTimeConfig::format_timestamp(locale_time_config, test_timestamp)
  assert_eq(us_formatted_time, "01/02/2025 10:00:00 AM")
  
  LocaleTimeConfig::set_locale(locale_time_config, "zh-CN")
  let cn_formatted_time = LocaleTimeConfig::format_timestamp(locale_time_config, test_timestamp)
  assert_eq(cn_formatted_time, "2025-01-02 10:00:00")
  
  LocaleTimeConfig::set_locale(locale_time_config, "ja-JP")
  let jp_formatted_time = LocaleTimeConfig::format_timestamp(locale_time_config, test_timestamp)
  assert_eq(jp_formatted_time, "2025/01/02 10:00:00")
  
  // 测试相对时间格式化
  let current_timestamp = 1735689600000000000L
  let past_timestamp = current_timestamp - 3600000000000L  // 1小时前
  let future_timestamp = current_timestamp + 86400000000000L  // 1天后
  
  LocaleTimeConfig::set_locale(locale_time_config, "en-US")
  let us_relative_past = LocaleTimeConfig::format_relative_time(locale_time_config, past_timestamp, current_timestamp)
  assert_eq(us_relative_past, "1 hour ago")
  
  let us_relative_future = LocaleTimeConfig::format_relative_time(locale_time_config, future_timestamp, current_timestamp)
  assert_eq(us_relative_future, "in 1 day")
  
  LocaleTimeConfig::set_locale(locale_time_config, "zh-CN")
  let cn_relative_past = LocaleTimeConfig::format_relative_time(locale_time_config, past_timestamp, current_timestamp)
  assert_eq(cn_relative_past, "1小时前")
  
  let cn_relative_future = LocaleTimeConfig::format_relative_time(locale_time_config, future_timestamp, current_timestamp)
  assert_eq(cn_relative_future, "1天后")
  
  LocaleTimeConfig::set_locale(locale_time_config, "ja-JP")
  let jp_relative_past = LocaleTimeConfig::format_relative_time(locale_time_config, past_timestamp, current_timestamp)
  assert_eq(jp_relative_past, "1時間前")
  
  let jp_relative_future = LocaleTimeConfig::format_relative_time(locale_time_config, future_timestamp, current_timestamp)
  assert_eq(jp_relative_future, "1日後")
}

test "多语言度量数据显示" {
  // 创建国际化度量配置
  let i18n_metric_config = InternationalizationMetricConfig::new()
  
  // 加载不同语言的度量名称和描述
  InternationalizationMetricConfig::load_metric_translations(i18n_metric_config, "en-US", [
    ("metric.cpu.usage", "CPU Usage", "Percentage of CPU utilization"),
    ("metric.memory.usage", "Memory Usage", "Percentage of memory utilization"),
    ("metric.request.count", "Request Count", "Total number of requests"),
    ("metric.response.time", "Response Time", "Average response time in milliseconds"),
    ("metric.error.rate", "Error Rate", "Percentage of requests resulting in errors")
  ])
  
  InternationalizationMetricConfig::load_metric_translations(i18n_metric_config, "zh-CN", [
    ("metric.cpu.usage", "CPU使用率", "CPU利用率百分比"),
    ("metric.memory.usage", "内存使用率", "内存利用率百分比"),
    ("metric.request.count", "请求数量", "请求总数"),
    ("metric.response.time", "响应时间", "平均响应时间（毫秒）"),
    ("metric.error.rate", "错误率", "导致错误的请求百分比")
  ])
  
  InternationalizationMetricConfig::load_metric_translations(i18n_metric_config, "ja-JP", [
    ("metric.cpu.usage", "CPU使用率", "CPU使用率の割合"),
    ("metric.memory.usage", "メモリ使用率", "メモリ使用率の割合"),
    ("metric.request.count", "リクエスト数", "リクエストの総数"),
    ("metric.response.time", "応答時間", "平均応答時間（ミリ秒）"),
    ("metric.error.rate", "エラー率", "エラーになったリクエストの割合")
  ])
  
  // 测试英文度量显示
  InternationalizationMetricConfig::set_locale(i18n_metric_config, "en-US")
  let en_cpu_name = InternationalizationMetricConfig::get_metric_name(i18n_metric_config, "metric.cpu.usage")
  let en_cpu_desc = InternationalizationMetricConfig::get_metric_description(i18n_metric_config, "metric.cpu.usage")
  
  assert_eq(en_cpu_name, "CPU Usage")
  assert_eq(en_cpu_desc, "Percentage of CPU utilization")
  
  // 测试中文度量显示
  InternationalizationMetricConfig::set_locale(i18n_metric_config, "zh-CN")
  let zh_cpu_name = InternationalizationMetricConfig::get_metric_name(i18n_metric_config, "metric.cpu.usage")
  let zh_cpu_desc = InternationalizationMetricConfig::get_metric_description(i18n_metric_config, "metric.cpu.usage")
  
  assert_eq(zh_cpu_name, "CPU使用率")
  assert_eq(zh_cpu_desc, "CPU利用率百分比")
  
  // 测试日文度量显示
  InternationalizationMetricConfig::set_locale(i18n_metric_config, "ja-JP")
  let ja_cpu_name = InternationalizationMetricConfig::get_metric_name(i18n_metric_config, "metric.cpu.usage")
  let ja_cpu_desc = InternationalizationMetricConfig::get_metric_description(i18n_metric_config, "metric.cpu.usage")
  
  assert_eq(ja_cpu_name, "CPU使用率")
  assert_eq(ja_cpu_desc, "CPU使用率の割合")
  
  // 测试度量值格式化
  InternationalizationMetricConfig::set_locale(i18n_metric_config, "en-US")
  let en_formatted_value = InternationalizationMetricConfig::format_metric_value(
    i18n_metric_config, 
    "metric.cpu.usage", 
    75.2345
  )
  assert_eq(en_formatted_value, "75.23%")  // 英文使用点作为小数点
  
  InternationalizationMetricConfig::set_locale(i18n_metric_config, "zh-CN")
  let zh_formatted_value = InternationalizationMetricConfig::format_metric_value(
    i18n_metric_config, 
    "metric.cpu.usage", 
    75.2345
  )
  assert_eq(zh_formatted_value, "75.23%")  // 中文也使用点作为小数点
  
  InternationalizationMetricConfig::set_locale(i18n_metric_config, "ja-JP")
  let ja_formatted_value = InternationalizationMetricConfig::format_metric_value(
    i18n_metric_config, 
    "metric.cpu.usage", 
    75.2345
  )
  assert_eq(ja_formatted_value, "75.23%")  // 日文也使用点作为小数点
  
  // 测试大数字格式化
  InternationalizationMetricConfig::set_locale(i18n_metric_config, "en-US")
  let en_large_number = InternationalizationMetricConfig::format_metric_value(
    i18n_metric_config, 
    "metric.request.count", 
    1234567.89
  )
  assert_eq(en_large_number, "1,234,567.89")  // 英文使用逗号作为千位分隔符
  
  InternationalizationMetricConfig::set_locale(i18n_metric_config, "zh-CN")
  let zh_large_number = InternationalizationMetricConfig::format_metric_value(
    i18n_metric_config, 
    "metric.request.count", 
    1234567.89
  )
  assert_eq(zh_large_number, "1,234,567.89")  // 中文也使用逗号作为千位分隔符
  
  InternationalizationMetricConfig::set_locale(i18n_metric_config, "ja-JP")
  let ja_large_number = InternationalizationMetricConfig::format_metric_value(
    i18n_metric_config, 
    "metric.request.count", 
    1234567.89
  )
  assert_eq(ja_large_number, "1,234,567.89")  // 日文也使用逗号作为千位分隔符
}

test "多语言用户界面标签" {
  // 创建国际化UI配置
  let i18n_ui_config = InternationalizationUIConfig::new()
  InternationalizationUIConfig::set_default_locale(i18n_ui_config, "en-US")
  
  // 加载不同语言的UI标签
  InternationalizationUIConfig::load_ui_translations(i18n_ui_config, "en-US", [
    ("ui.dashboard.title", "Telemetry Dashboard"),
    ("ui.metrics.section", "Metrics"),
    ("ui.logs.section", "Logs"),
    ("ui.traces.section", "Traces"),
    ("ui.settings.title", "Settings"),
    ("ui.button.refresh", "Refresh"),
    ("ui.button.export", "Export"),
    ("ui.filter.placeholder", "Search..."),
    ("ui.table.column.timestamp", "Timestamp"),
    ("ui.table.column.service", "Service"),
    ("ui.table.column.operation", "Operation"),
    ("ui.table.column.duration", "Duration")
  ])
  
  InternationalizationUIConfig::load_ui_translations(i18n_ui_config, "zh-CN", [
    ("ui.dashboard.title", "遥测仪表板"),
    ("ui.metrics.section", "度量"),
    ("ui.logs.section", "日志"),
    ("ui.traces.section", "追踪"),
    ("ui.settings.title", "设置"),
    ("ui.button.refresh", "刷新"),
    ("ui.button.export", "导出"),
    ("ui.filter.placeholder", "搜索..."),
    ("ui.table.column.timestamp", "时间戳"),
    ("ui.table.column.service", "服务"),
    ("ui.table.column.operation", "操作"),
    ("ui.table.column.duration", "持续时间")
  ])
  
  InternationalizationUIConfig::load_ui_translations(i18n_ui_config, "ja-JP", [
    ("ui.dashboard.title", "テレメトリーダッシュボード"),
    ("ui.metrics.section", "メトリクス"),
    ("ui.logs.section", "ログ"),
    ("ui.traces.section", "トレース"),
    ("ui.settings.title", "設定"),
    ("ui.button.refresh", "更新"),
    ("ui.button.export", "エクスポート"),
    ("ui.filter.placeholder", "検索..."),
    ("ui.table.column.timestamp", "タイムスタンプ"),
    ("ui.table.column.service", "サービス"),
    ("ui.table.column.operation", "操作"),
    ("ui.table.column.duration", "期間")
  ])
  
  // 测试英文UI标签
  InternationalizationUIConfig::set_locale(i18n_ui_config, "en-US")
  let en_dashboard_title = InternationalizationUIConfig::get_ui_text(i18n_ui_config, "ui.dashboard.title")
  assert_eq(en_dashboard_title, "Telemetry Dashboard")
  
  let en_refresh_button = InternationalizationUIConfig::get_ui_text(i18n_ui_config, "ui.button.refresh")
  assert_eq(en_refresh_button, "Refresh")
  
  // 测试中文UI标签
  InternationalizationUIConfig::set_locale(i18n_ui_config, "zh-CN")
  let zh_dashboard_title = InternationalizationUIConfig::get_ui_text(i18n_ui_config, "ui.dashboard.title")
  assert_eq(zh_dashboard_title, "遥测仪表板")
  
  let zh_refresh_button = InternationalizationUIConfig::get_ui_text(i18n_ui_config, "ui.button.refresh")
  assert_eq(zh_refresh_button, "刷新")
  
  // 测试日文UI标签
  InternationalizationUIConfig::set_locale(i18n_ui_config, "ja-JP")
  let ja_dashboard_title = InternationalizationUIConfig::get_ui_text(i18n_ui_config, "ui.dashboard.title")
  assert_eq(ja_dashboard_title, "テレメトリーダッシュボード")
  
  let ja_refresh_button = InternationalizationUIConfig::get_ui_text(i18n_ui_config, "ui.button.refresh")
  assert_eq(ja_refresh_button, "更新")
  
  // 测试UI标签复数形式
  InternationalizationUIConfig::load_ui_translations(i18n_ui_config, "en-US", [
    ("ui.item.count", "{0} item", "{0} items")  // 单数和复数形式
  ])
  
  InternationalizationUIConfig::load_ui_translations(i18n_ui_config, "zh-CN", [
    ("ui.item.count", "{0} 项")  // 中文没有复数形式
  ])
  
  InternationalizationUIConfig::load_ui_translations(i18n_ui_config, "ja-JP", [
    ("ui.item.count", "{0} 項目")  // 日文没有复数形式
  ])
  
  // 测试英文复数形式
  InternationalizationUIConfig::set_locale(i18n_ui_config, "en-US")
  let en_singular = InternationalizationUIConfig::get_plural_ui_text(i18n_ui_config, "ui.item.count", 1)
  assert_eq(en_singular, "1 item")
  
  let en_plural = InternationalizationUIConfig::get_plural_ui_text(i18n_ui_config, "ui.item.count", 5)
  assert_eq(en_plural, "5 items")
  
  // 测试中文（无复数形式）
  InternationalizationUIConfig::set_locale(i18n_ui_config, "zh-CN")
  let zh_singular = InternationalizationUIConfig::get_plural_ui_text(i18n_ui_config, "ui.item.count", 1)
  assert_eq(zh_singular, "1 项")
  
  let zh_plural = InternationalizationUIConfig::get_plural_ui_text(i18n_ui_config, "ui.item.count", 5)
  assert_eq(zh_plural, "5 项")
  
  // 测试日文（无复数形式）
  InternationalizationUIConfig::set_locale(i18n_ui_config, "ja-JP")
  let ja_singular = InternationalizationUIConfig::get_plural_ui_text(i18n_ui_config, "ui.item.count", 1)
  assert_eq(ja_singular, "1 項目")
  
  let ja_plural = InternationalizationUIConfig::get_plural_ui_text(i18n_ui_config, "ui.item.count", 5)
  assert_eq(ja_plural, "5 項目")
}

test "文化特定的格式化规则" {
  // 创建文化特定格式化配置
  let culture_config = CultureSpecificConfig::new()
  
  // 测试不同文化的数字格式化
  CultureSpecificConfig::set_culture(culture_config, "en-US")
  let us_number_format = CultureSpecificConfig::get_number_format(culture_config)
  assert_eq(us_number_format.decimal_separator, ".")
  assert_eq(us_number_format.thousands_separator, ",")
  
  CultureSpecificConfig::set_culture(culture_config, "de-DE")
  let de_number_format = CultureSpecificConfig::get_number_format(culture_config)
  assert_eq(de_number_format.decimal_separator, ",")
  assert_eq(de_number_format.thousands_separator, ".")
  
  CultureSpecificConfig::set_culture(culture_config, "zh-CN")
  let cn_number_format = CultureSpecificConfig::get_number_format(culture_config)
  assert_eq(cn_number_format.decimal_separator, ".")
  assert_eq(cn_number_format.thousands_separator, ",")
  
  // 测试数字格式化
  let test_number = 1234567.89
  
  CultureSpecificConfig::set_culture(culture_config, "en-US")
  let us_formatted_number = CultureSpecificConfig::format_number(culture_config, test_number)
  assert_eq(us_formatted_number, "1,234,567.89")
  
  CultureSpecificConfig::set_culture(culture_config, "de-DE")
  let de_formatted_number = CultureSpecificConfig::format_number(culture_config, test_number)
  assert_eq(de_formatted_number, "1.234.567,89")
  
  CultureSpecificConfig::set_culture(culture_config, "zh-CN")
  let cn_formatted_number = CultureSpecificConfig::format_number(culture_config, test_number)
  assert_eq(cn_formatted_number, "1,234,567.89")
  
  // 测试货币格式化
  CultureSpecificConfig::set_culture(culture_config, "en-US")
  let us_currency = CultureSpecificConfig::format_currency(culture_config, 1234.56, "USD")
  assert_eq(us_currency, "$1,234.56")
  
  CultureSpecificConfig::set_culture(culture_config, "de-DE")
  let de_currency = CultureSpecificConfig::format_currency(culture_config, 1234.56, "EUR")
  assert_eq(de_currency, "1.234,56 €")
  
  CultureSpecificConfig::set_culture(culture_config, "zh-CN")
  let cn_currency = CultureSpecificConfig::format_currency(culture_config, 1234.56, "CNY")
  assert_eq(cn_currency, "¥1,234.56")
  
  // 测试百分比格式化
  CultureSpecificConfig::set_culture(culture_config, "en-US")
  let us_percentage = CultureSpecificConfig::format_percentage(culture_config, 0.7523)
  assert_eq(us_percentage, "75.23%")
  
  CultureSpecificConfig::set_culture(culture_config, "de-DE")
  let de_percentage = CultureSpecificConfig::format_percentage(culture_config, 0.7523)
  assert_eq(de_percentage, "75,23%")
  
  CultureSpecificConfig::set_culture(culture_config, "zh-CN")
  let cn_percentage = CultureSpecificConfig::format_percentage(culture_config, 0.7523)
  assert_eq(cn_percentage, "75.23%")
  
  // 测试文本方向性（RTL vs LTR）
  CultureSpecificConfig::set_culture(culture_config, "en-US")
  let us_text_direction = CultureSpecificConfig::get_text_direction(culture_config)
  assert_eq(us_text_direction, "ltr")
  
  CultureSpecificConfig::set_culture(culture_config, "ar-SA")
  let ar_text_direction = CultureSpecificConfig::get_text_direction(culture_config)
  assert_eq(ar_text_direction, "rtl")
  
  CultureSpecificConfig::set_culture(culture_config, "he-IL")
  let he_text_direction = CultureSpecificConfig::get_text_direction(culture_config)
  assert_eq(he_text_direction, "rtl")
  
  // 测试文化特定的排序规则
  CultureSpecificConfig::set_culture(culture_config, "en-US")
  let us_sort_order = CultureSpecificConfig::get_sort_order(culture_config)
  assert_eq(us_sort_order, "alphabetical")
  
  CultureSpecificConfig::set_culture(culture_config, "zh-CN")
  let cn_sort_order = CultureSpecificConfig::get_sort_order(culture_config)
  assert_eq(cn_sort_order, "pinyin")
  
  CultureSpecificConfig::set_culture(culture_config, "ja-JP")
  let ja_sort_order = CultureSpecificConfig::get_sort_order(culture_config)
  assert_eq(ja_sort_order, "hiragana")
}