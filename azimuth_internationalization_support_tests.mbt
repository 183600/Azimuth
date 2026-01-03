// 国际化支持测试
// 测试Azimuth遥测系统的国际化支持功能

test "多语言消息本地化" {
  // 测试多语言消息本地化功能
  let localization_manager = LocalizationManager::new()
  
  // 加载语言资源
  let en_messages = MessageBundle::new("en")
  MessageBundle::add_message(en_messages, "telemetry.started", "Telemetry system started")
  MessageBundle::add_message(en_messages, "telemetry.stopped", "Telemetry system stopped")
  MessageBundle::add_message(en_messages, "span.created", "Span '{spanName}' created")
  MessageBundle::add_message(en_messages, "metric.recorded", "Metric '{metricName}' recorded with value {value}")
  MessageBundle::add_message(en_messages, "error.connection.failed", "Connection failed: {reason}")
  
  let zh_messages = MessageBundle::new("zh")
  MessageBundle::add_message(zh_messages, "telemetry.started", "遥测系统已启动")
  MessageBundle::add_message(zh_messages, "telemetry.stopped", "遥测系统已停止")
  MessageBundle::add_message(zh_messages, "span.created", "已创建Span '{spanName}'")
  MessageBundle::add_message(zh_messages, "metric.recorded", "已记录度量 '{metricName}'，值为 {value}")
  MessageBundle::add_message(zh_messages, "error.connection.failed", "连接失败：{reason}")
  
  let ja_messages = MessageBundle::new("ja")
  MessageBundle::add_message(ja_messages, "telemetry.started", "テレメトリシステムが開始されました")
  MessageBundle::add_message(ja_messages, "telemetry.stopped", "テレメトリシステムが停止されました")
  MessageBundle::add_message(ja_messages, "span.created", "Span '{spanName}' が作成されました")
  MessageBundle::add_message(ja_messages, "metric.recorded", "メトリック '{metricName}' が値 {value} で記録されました")
  MessageBundle::add_message(ja_messages, "error.connection.failed", "接続に失敗しました: {reason}")
  
  // 注册语言资源
  LocalizationManager::register_bundle(localization_manager, en_messages)
  LocalizationManager::register_bundle(localization_manager, zh_messages)
  LocalizationManager::register_bundle(localization_manager, ja_messages)
  
  // 设置默认语言
  LocalizationManager::set_default_locale(localization_manager, "en")
  
  // 测试英文本地化
  LocalizationManager::set_locale(localization_manager, "en")
  let en_started = LocalizationManager::get_message(localization_manager, "telemetry.started")
  let en_span_created = LocalizationManager::format_message(localization_manager, "span.created", [("spanName", "user.request")])
  let en_metric_recorded = LocalizationManager::format_message(localization_manager, "metric.recorded", [
    ("metricName", "response.time"),
    ("value", "150ms")
  ])
  
  assert_eq(en_started, "Telemetry system started")
  assert_eq(en_span_created, "Span 'user.request' created")
  assert_eq(en_metric_recorded, "Metric 'response.time' recorded with value 150ms")
  
  // 测试中文本地化
  LocalizationManager::set_locale(localization_manager, "zh")
  let zh_started = LocalizationManager::get_message(localization_manager, "telemetry.started")
  let zh_span_created = LocalizationManager::format_message(localization_manager, "span.created", [("spanName", "user.request")])
  let zh_metric_recorded = LocalizationManager::format_message(localization_manager, "metric.recorded", [
    ("metricName", "response.time"),
    ("value", "150ms")
  ])
  
  assert_eq(zh_started, "遥测系统已启动")
  assert_eq(zh_span_created, "已创建Span 'user.request'")
  assert_eq(zh_metric_recorded, "已记录度量 'response.time'，值为 150ms")
  
  // 测试日文本地化
  LocalizationManager::set_locale(localization_manager, "ja")
  let ja_started = LocalizationManager::get_message(localization_manager, "telemetry.started")
  let ja_span_created = LocalizationManager::format_message(localization_manager, "span.created", [("spanName", "user.request")])
  let ja_metric_recorded = LocalizationManager::format_message(localization_manager, "metric.recorded", [
    ("metricName", "response.time"),
    ("value", "150ms")
  ])
  
  assert_eq(ja_started, "テレメトリシステムが開始されました")
  assert_eq(ja_span_created, "Span 'user.request' が作成されました")
  assert_eq(ja_metric_recorded, "メトリック 'response.time' が値 150ms で記録されました")
  
  // 测试回退到默认语言
  LocalizationManager::set_locale(localization_manager, "fr") // 不存在的语言
  let fallback_message = LocalizationManager::get_message(localization_manager, "telemetry.started")
  assert_eq(fallback_message, "Telemetry system started") // 回退到英文
}

test "区域特定格式化" {
  // 测试区域特定格式化功能
  let locale_formatter = LocaleFormatter::new()
  
  // 测试日期时间格式化
  let timestamp = 1704067200000L // 2024-01-01 00:00:00 UTC
  
  // 美国格式
  let us_datetime = LocaleFormatter::format_datetime(locale_formatter, timestamp, "en-US")
  assert_true(String::contains(us_datetime, "1/1/2024"))
  
  // 中国格式
  let cn_datetime = LocaleFormatter::format_datetime(locale_formatter, timestamp, "zh-CN")
  assert_true(String::contains(cn_datetime, "2024年1月1日"))
  
  // 日本格式
  let jp_datetime = LocaleFormatter::format_datetime(locale_formatter, timestamp, "ja-JP")
  assert_true(String::contains(jp_datetime, "2024/1/1"))
  
  // 德国格式
  let de_datetime = LocaleFormatter::format_datetime(locale_formatter, timestamp, "de-DE")
  assert_true(String::contains(de_datetime, "1.1.2024"))
  
  // 测试数字格式化
  let number = 1234567.89
  
  // 美国格式
  let us_number = LocaleFormatter::format_number(locale_formatter, number, "en-US")
  assert_eq(us_number, "1,234,567.89")
  
  // 中国格式
  let cn_number = LocaleFormatter::format_number(locale_formatter, number, "zh-CN")
  assert_eq(cn_number, "1,234,567.89")
  
  // 德国格式
  let de_number = LocaleFormatter::format_number(locale_formatter, number, "de-DE")
  assert_eq(de_number, "1.234.567,89")
  
  // 法国格式
  let fr_number = LocaleFormatter::format_number(locale_formatter, number, "fr-FR")
  assert_eq(fr_number, "1 234 567,89")
  
  // 印度格式
  let in_number = LocaleFormatter::format_number(locale_formatter, number, "en-IN")
  assert_eq(in_number, "12,34,567.89")
  
  // 测试百分比格式化
  let percentage = 0.7543
  
  // 美国格式
  let us_percentage = LocaleFormatter::format_percentage(locale_formatter, percentage, "en-US")
  assert_eq(us_percentage, "75.43%")
  
  // 中国格式
  let cn_percentage = LocaleFormatter::format_percentage(locale_formatter, percentage, "zh-CN")
  assert_eq(cn_percentage, "75.43%")
  
  // 土耳其格式
  let tr_percentage = LocaleFormatter::format_percentage(locale_formatter, percentage, "tr-TR")
  assert_eq(tr_percentage, "%75,43")
  
  // 测试货币格式化
  let currency = 1234.56
  
  // 美元
  let usd_currency = LocaleFormatter::format_currency(locale_formatter, currency, "en-US", "USD")
  assert_eq(usd_currency, "$1,234.56")
  
  // 人民币
  let cny_currency = LocaleFormatter::format_currency(locale_formatter, currency, "zh-CN", "CNY")
  assert_eq(cny_currency, "¥1,234.56")
  
  // 欧元
  let eur_currency = LocaleFormatter::format_currency(locale_formatter, currency, "de-DE", "EUR")
  assert_eq(eur_currency, "1.234,56 €")
  
  // 日元
  let jpy_currency = LocaleFormatter::format_currency(locale_formatter, currency, "ja-JP", "JPY")
  assert_eq(jpy_currency, "¥1,235") // 日元通常不显示小数
  
  // 测试单位格式化
  let bytes = 1048576 // 1MB
  
  // 美国格式
  let us_bytes = LocaleFormatter::format_bytes(locale_formatter, bytes, "en-US")
  assert_eq(us_bytes, "1 MB")
  
  // 中国格式
  let cn_bytes = LocaleFormatter::format_bytes(locale_formatter, bytes, "zh-CN")
  assert_eq(cn_bytes, "1 MB")
  
  // 测试持续时间格式化
  let duration_ms = 3661500 // 1小时1分1.5秒
  
  // 美国格式
  let us_duration = LocaleFormatter::format_duration(locale_formatter, duration_ms, "en-US")
  assert_eq(us_duration, "1h 1m 1.5s")
  
  // 中国格式
  let cn_duration = LocaleFormatter::format_duration(locale_formatter, duration_ms, "zh-CN")
  assert_eq(cn_duration, "1小时1分钟1.5秒")
}

test "时区处理和转换" {
  // 测试时区处理和转换功能
  let timezone_manager = TimezoneManager::new()
  
  // 创建UTC时间
  let utc_time = 1704067200000L // 2024-01-01 00:00:00 UTC
  
  // 转换到不同时区
  let us_pacific_time = TimezoneManager::convert(utc_time, "UTC", "America/Los_Angeles")
  let us_eastern_time = TimezoneManager::convert(utc_time, "UTC", "America/New_York")
  let china_time = TimezoneManager::convert(utc_time, "UTC", "Asia/Shanghai")
  let japan_time = TimezoneManager::convert(utc_time, "UTC", "Asia/Tokyo")
  let uk_time = TimezoneManager::convert(utc_time, "UTC", "Europe/London")
  
  // 验证时区转换
  // 2024-01-01 00:00:00 UTC
  // 美国太平洋时间：2023-12-31 16:00:00 (UTC-8，考虑夏令时)
  // 美国东部时间：2023-12-31 19:00:00 (UTC-5，考虑夏令时)
  // 中国时间：2024-01-01 08:00:00 (UTC+8)
  // 日本时间：2024-01-01 09:00:00 (UTC+9)
  // 英国时间：2024-01-01 00:00:00 (UTC+0，1月1日不是夏令时)
  
  // 验证转换结果
  assert_true(String::contains(us_pacific_time, "2023-12-31"))
  assert_true(String::contains(us_eastern_time, "2023-12-31"))
  assert_true(String::contains(china_time, "2024-01-01"))
  assert_true(String::contains(japan_time, "2024-01-01"))
  assert_true(String::contains(uk_time, "2024-01-01"))
  
  // 测试时区感知的格式化
  let timezone_formatter = TimezoneFormatter::new()
  
  // 格式化不同时区的时间
  let formatted_utc = TimezoneFormatter::format_with_timezone(timezone_formatter, utc_time, "UTC", "en-US")
  let formatted_pacific = TimezoneFormatter::format_with_timezone(timezone_formatter, utc_time, "America/Los_Angeles", "en-US")
  let formatted_china = TimezoneFormatter::format_with_timezone(timezone_formatter, utc_time, "Asia/Shanghai", "zh-CN")
  
  // 验证格式化结果
  assert_true(String::contains(formatted_utc, "UTC"))
  assert_true(String::contains(formatted_pacific, "PST") || String::contains(formatted_pacific, "PDT"))
  assert_true(String::contains(formatted_china, "CST"))
  
  // 测试时区检测
  let detected_timezone = TimezoneManager::detect_system_timezone()
  assert_true(String::length(detected_timezone) > 0)
  
  // 测试时区列表
  let available_timezones = TimezoneManager::get_available_timezones()
  assert_true(Array::length(available_timezones) > 100)
  assert_true(Array::contains(available_timezones, "UTC"))
  assert_true(Array::contains(available_timezones, "America/New_York"))
  assert_true(Array::contains(available_timezones, "Asia/Shanghai"))
  assert_true(Array::contains(available_timezones, "Europe/London"))
}

test "多语言错误消息和异常处理" {
  // 测试多语言错误消息和异常处理
  let error_localizer = ErrorLocalizer::new()
  
  // 加载错误消息
  let en_errors = ErrorMessageBundle::new("en")
  ErrorMessageBundle::add_error(en_errors, "connection.failed", "Connection failed: {reason}")
  ErrorMessageBundle::add_error(en_errors, "timeout.exceeded", "Operation timed out after {timeout}ms")
  ErrorMessageBundle::add_error(en_errors, "invalid.format", "Invalid format: {details}")
  ErrorMessageBundle::add_error(en_errors, "resource.not.found", "Resource '{resource}' not found")
  ErrorMessageBundle::add_error(en_errors, "permission.denied", "Permission denied: {action}")
  
  let zh_errors = ErrorMessageBundle::new("zh")
  ErrorMessageBundle::add_error(zh_errors, "connection.failed", "连接失败：{reason}")
  ErrorMessageBundle::add_error(zh_errors, "timeout.exceeded", "操作超时，超过 {timeout} 毫秒")
  ErrorMessageBundle::add_error(zh_errors, "invalid.format", "格式无效：{details}")
  ErrorMessageBundle::add_error(zh_errors, "resource.not.found", "资源 '{resource}' 未找到")
  ErrorMessageBundle::add_error(zh_errors, "permission.denied", "权限被拒绝：{action}")
  
  let ja_errors = ErrorMessageBundle::new("ja")
  ErrorMessageBundle::add_error(ja_errors, "connection.failed", "接続に失敗しました: {reason}")
  ErrorMessageBundle::add_error(ja_errors, "timeout.exceeded", "操作が {timeout}ms後にタイムアウトしました")
  ErrorMessageBundle::add_error(ja_errors, "invalid.format", "形式が無効です: {details}")
  ErrorMessageBundle::add_error(ja_errors, "resource.not.found", "リソース '{resource}' が見つかりません")
  ErrorMessageBundle::add_error(ja_errors, "permission.denied", "アクセスが拒否されました: {action}")
  
  // 注册错误消息
  ErrorLocalizer::register_bundle(error_localizer, en_errors)
  ErrorLocalizer::register_bundle(error_localizer, zh_errors)
  ErrorLocalizer::register_bundle(error_localizer, ja_errors)
  
  // 创建多语言异常
  let exception = MultilingualException::new("connection.failed")
  MultilingualException::add_parameter(exception, "reason", "Network unreachable")
  
  // 设置语言为英文
  ErrorLocalizer::set_locale(error_localizer, "en")
  let en_error_message = ErrorLocalizer::get_error_message(error_localizer, exception)
  assert_eq(en_error_message, "Connection failed: Network unreachable")
  
  // 设置语言为中文
  ErrorLocalizer::set_locale(error_localizer, "zh")
  let zh_error_message = ErrorLocalizer::get_error_message(error_localizer, exception)
  assert_eq(zh_error_message, "连接失败：Network unreachable")
  
  // 设置语言为日文
  ErrorLocalizer::set_locale(error_localizer, "ja")
  let ja_error_message = ErrorLocalizer::get_error_message(error_localizer, exception)
  assert_eq(ja_error_message, "接続に失敗しました: Network unreachable")
  
  // 测试不同类型的异常
  let timeout_exception = MultilingualException::new("timeout.exceeded")
  MultilingualException::add_parameter(timeout_exception, "timeout", "5000")
  
  ErrorLocalizer::set_locale(error_localizer, "en")
  let en_timeout_message = ErrorLocalizer::get_error_message(error_localizer, timeout_exception)
  assert_eq(en_timeout_message, "Operation timed out after 5000ms")
  
  ErrorLocalizer::set_locale(error_localizer, "zh")
  let zh_timeout_message = ErrorLocalizer::get_error_message(error_localizer, timeout_exception)
  assert_eq(zh_timeout_message, "操作超时，超过 5000 毫秒")
  
  // 测试错误代码和本地化消息的映射
  let error_code_mapper = ErrorCodeMapper::new()
  ErrorCodeMapper::add_mapping(error_code_mapper, "CONN_001", "connection.failed")
  ErrorCodeMapper::add_mapping(error_code_mapper, "TIME_001", "timeout.exceeded")
  ErrorCodeMapper::add_mapping(error_code_mapper, "FMT_001", "invalid.format")
  
  // 通过错误代码获取本地化消息
  let localized_error = ErrorLocalizer::get_message_by_code(error_localizer, error_code_mapper, "CONN_001", [("reason", "Host not found")])
  
  ErrorLocalizer::set_locale(error_localizer, "en")
  let en_code_message = ErrorLocalizer::format_with_parameters(error_localizer, localized_error)
  assert_eq(en_code_message, "Connection failed: Host not found")
}

test "文化特定的用户界面元素" {
  // 测试文化特定的用户界面元素
  let ui_localizer = UILocalizer::new()
  
  // 加载UI元素
  let en_ui = UIElementBundle::new("en")
  UIElementBundle::add_element(en_ui, "button.save", "Save")
  UIElementBundle::add_element(en_ui, "button.cancel", "Cancel")
  UIElementBundle::add_element(en_ui, "button.delete", "Delete")
  UIElementBundle::add_element(en_ui, "menu.file", "File")
  UIElementBundle::add_element(en_ui, "menu.edit", "Edit")
  UIElementBundle::add_element(en_ui, "menu.view", "View")
  UIElementBundle::add_element(en_ui, "label.username", "Username")
  UIElementBundle::add_element(en_ui, "label.password", "Password")
  UIElementBundle::add_element(en_ui, "label.email", "Email")
  
  let zh_ui = UIElementBundle::new("zh")
  UIElementBundle::add_element(zh_ui, "button.save", "保存")
  UIElementBundle::add_element(zh_ui, "button.cancel", "取消")
  UIElementBundle::add_element(zh_ui, "button.delete", "删除")
  UIElementBundle::add_element(zh_ui, "menu.file", "文件")
  UIElementBundle::add_element(zh_ui, "menu.edit", "编辑")
  UIElementBundle::add_element(zh_ui, "menu.view", "查看")
  UIElementBundle::add_element(zh_ui, "label.username", "用户名")
  UIElementBundle::add_element(zh_ui, "label.password", "密码")
  UIElementBundle::add_element(zh_ui, "label.email", "电子邮件")
  
  let ja_ui = UIElementBundle::new("ja")
  UIElementBundle::add_element(ja_ui, "button.save", "保存")
  UIElementBundle::add_element(ja_ui, "button.cancel", "キャンセル")
  UIElementBundle::add_element(ja_ui, "button.delete", "削除")
  UIElementBundle::add_element(ja_ui, "menu.file", "ファイル")
  UIElementBundle::add_element(ja_ui, "menu.edit", "編集")
  UIElementBundle::add_element(ja_ui, "menu.view", "表示")
  UIElementBundle::add_element(ja_ui, "label.username", "ユーザー名")
  UIElementBundle::add_element(ja_ui, "label.password", "パスワード")
  UIElementBundle::add_element(ja_ui, "label.email", "メールアドレス")
  
  // 注册UI元素
  UILocalizer::register_bundle(ui_localizer, en_ui)
  UILocalizer::register_bundle(ui_localizer, zh_ui)
  UILocalizer::register_bundle(ui_localizer, ja_ui)
  
  // 测试英文UI元素
  UILocalizer::set_locale(ui_localizer, "en")
  let en_save_button = UILocalizer::get_element(ui_localizer, "button.save")
  let en_cancel_button = UILocalizer::get_element(ui_localizer, "button.cancel")
  let en_username_label = UILocalizer::get_element(ui_localizer, "label.username")
  
  assert_eq(en_save_button, "Save")
  assert_eq(en_cancel_button, "Cancel")
  assert_eq(en_username_label, "Username")
  
  // 测试中文UI元素
  UILocalizer::set_locale(ui_localizer, "zh")
  let zh_save_button = UILocalizer::get_element(ui_localizer, "button.save")
  let zh_cancel_button = UILocalizer::get_element(ui_localizer, "button.cancel")
  let zh_username_label = UILocalizer::get_element(ui_localizer, "label.username")
  
  assert_eq(zh_save_button, "保存")
  assert_eq(zh_cancel_button, "取消")
  assert_eq(zh_username_label, "用户名")
  
  // 测试日文UI元素
  UILocalizer::set_locale(ui_localizer, "ja")
  let ja_save_button = UILocalizer::get_element(ui_localizer, "button.save")
  let ja_cancel_button = UILocalizer::get_element(ui_localizer, "button.cancel")
  let ja_username_label = UILocalizer::get_element(ui_localizer, "label.username")
  
  assert_eq(ja_save_button, "保存")
  assert_eq(ja_cancel_button, "キャンセル")
  assert_eq(ja_username_label, "ユーザー名")
  
  // 测试文化特定的布局方向
  let layout_manager = CultureLayoutManager::new()
  
  // 从左到右的语言（英语、中文、日语）
  assert_eq(CultureLayoutManager::get_text_direction(layout_manager, "en"), LTR)
  assert_eq(CultureLayoutManager::get_text_direction(layout_manager, "zh"), LTR)
  assert_eq(CultureLayoutManager::get_text_direction(layout_manager, "ja"), LTR)
  
  // 从右到左的语言（阿拉伯语、希伯来语）
  assert_eq(CultureLayoutManager::get_text_direction(layout_manager, "ar"), RTL)
  assert_eq(CultureLayoutManager::get_text_direction(layout_manager, "he"), RTL)
  
  // 测试文化特定的排序规则
  let sorter = CultureSorter::new()
  
  let names = ["张三", "李四", "王五", "赵六"]
  
  // 中文排序（按拼音）
  CultureSorter::set_locale(sorter, "zh-CN")
  let zh_sorted_names = CultureSorter::sort_strings(sorter, names)
  
  // 英文排序
  let english_names = ["Alice", "Bob", "Charlie", "David"]
  CultureSorter::set_locale(sorter, "en-US")
  let en_sorted_names = CultureSorter::sort_strings(sorter, english_names)
  
  // 验证排序结果
  assert_eq(Array::length(zh_sorted_names), 4)
  assert_eq(Array::length(en_sorted_names), 4)
  
  // 测试文化特定的日期格式
  let date_formatter = CultureDateFormatter::new()
  
  let date = 1704067200000L // 2024-01-01
  
  // 美国日期格式
  let us_date = CultureDateFormatter::format_short_date(date_formatter, date, "en-US")
  assert_eq(us_date, "1/1/2024")
  
  // 中国日期格式
  let cn_date = CultureDateFormatter::format_short_date(date_formatter, date, "zh-CN")
  assert_eq(cn_date, "2024/1/1")
  
  // 日本日期格式
  let jp_date = CultureDateFormatter::format_short_date(date_formatter, date, "ja-JP")
  assert_eq(jp_date, "2024/01/01")
  
  // 英国日期格式
  let uk_date = CultureDateFormatter::format_short_date(date_formatter, date, "en-GB")
  assert_eq(uk_date, "01/01/2024")
}

test "动态语言切换和资源管理" {
  // 测试动态语言切换和资源管理
  let dynamic_localizer = DynamicLocalizer::new()
  
  // 加载资源
  let en_resources = DynamicResourceBundle::new("en")
  DynamicResourceBundle::add_resource(en_resources, "welcome.message", "Welcome to Azimuth Telemetry")
  DynamicResourceBundle::add_resource(en_resources, "dashboard.title", "Telemetry Dashboard")
  DynamicResourceBundle::add_resource(en_resources, "metrics.cpu", "CPU Usage")
  DynamicResourceBundle::add_resource(en_resources, "metrics.memory", "Memory Usage")
  DynamicResourceBundle::add_resource(en_resources, "metrics.network", "Network Traffic")
  
  let zh_resources = DynamicResourceBundle::new("zh")
  DynamicResourceBundle::add_resource(zh_resources, "welcome.message", "欢迎使用Azimuth遥测系统")
  DynamicResourceBundle::add_resource(zh_resources, "dashboard.title", "遥测仪表板")
  DynamicResourceBundle::add_resource(zh_resources, "metrics.cpu", "CPU使用率")
  DynamicResourceBundle::add_resource(zh_resources, "metrics.memory", "内存使用率")
  DynamicResourceBundle::add_resource(zh_resources, "metrics.network", "网络流量")
  
  let ja_resources = DynamicResourceBundle::new("ja")
  DynamicResourceBundle::add_resource(ja_resources, "welcome.message", "Azimuthテレメトリシステムへようこそ")
  DynamicResourceBundle::add_resource(ja_resources, "dashboard.title", "テレメトリダッシュボード")
  DynamicResourceBundle::add_resource(ja_resources, "metrics.cpu", "CPU使用率")
  DynamicResourceBundle::add_resource(ja_resources, "metrics.memory", "メモリ使用率")
  DynamicResourceBundle::add_resource(ja_resources, "metrics.network", "ネットワークトラフィック")
  
  // 注册资源
  DynamicLocalizer::register_bundle(dynamic_localizer, en_resources)
  DynamicLocalizer::register_bundle(dynamic_localizer, zh_resources)
  DynamicLocalizer::register_bundle(dynamic_localizer, ja_resources)
  
  // 设置初始语言
  DynamicLocalizer::set_locale(dynamic_localizer, "en")
  
  // 获取资源
  let welcome_message = DynamicLocalizer::get_resource(dynamic_localizer, "welcome.message")
  assert_eq(welcome_message, "Welcome to Azimuth Telemetry")
  
  // 动态切换语言
  DynamicLocalizer::set_locale(dynamic_localizer, "zh")
  let zh_welcome_message = DynamicLocalizer::get_resource(dynamic_localizer, "welcome.message")
  assert_eq(zh_welcome_message, "欢迎使用Azimuth遥测系统")
  
  // 再次切换语言
  DynamicLocalizer::set_locale(dynamic_localizer, "ja")
  let ja_welcome_message = DynamicLocalizer::get_resource(dynamic_localizer, "welcome.message")
  assert_eq(ja_welcome_message, "Azimuthテレメトリシステムへようこそ")
  
  // 测试资源预加载
  let resource_preloader = ResourcePreloader::new()
  
  // 预加载常用资源
  ResourcePreloader::preload_resource(resource_preloader, "en", "welcome.message")
  ResourcePreloader::preload_resource(resource_preloader, "en", "dashboard.title")
  ResourcePreloader::preload_resource(resource_preloader, "zh", "welcome.message")
  ResourcePreloader::preload_resource(resource_preloader, "zh", "dashboard.title")
  ResourcePreloader::preload_resource(resource_preloader, "ja", "welcome.message")
  ResourcePreloader::preload_resource(resource_preloader, "ja", "dashboard.title")
  
  // 验证预加载状态
  assert_true(ResourcePreloader::is_preloaded(resource_preloader, "en", "welcome.message"))
  assert_true(ResourcePreloader::is_preloaded(resource_preloader, "zh", "welcome.message"))
  assert_true(ResourcePreloader::is_preloaded(resource_preloader, "ja", "welcome.message"))
  assert_false(ResourcePreloader::is_preloaded(resource_preloader, "en", "metrics.cpu")) // 未预加载
  
  // 测试资源缓存
  let resource_cache = ResourceCache::new()
  ResourceCache::set_max_size(resource_cache, 100)
  ResourceCache::set_ttl(resource_cache, 300000) // 5分钟TTL
  
  // 缓存资源
  ResourceCache::put(resource_cache, "en", "welcome.message", "Welcome to Azimuth Telemetry")
  ResourceCache::put(resource_cache, "zh", "welcome.message", "欢迎使用Azimuth遥测系统")
  ResourceCache::put(resource_cache, "ja", "welcome.message", "Azimuthテレメトリシステムへようこそ")
  
  // 从缓存获取资源
  let cached_en_message = ResourceCache::get(resource_cache, "en", "welcome.message")
  let cached_zh_message = ResourceCache::get(resource_cache, "zh", "welcome.message")
  let cached_ja_message = ResourceCache::get(resource_cache, "ja", "welcome.message")
  
  assert_eq(cached_en_message, "Welcome to Azimuth Telemetry")
  assert_eq(cached_zh_message, "欢迎使用Azimuth遥测系统")
  assert_eq(cached_ja_message, "Azimuthテレメトリシステムへようこそ")
  
  // 测试缓存统计
  let cache_stats = ResourceCache::get_stats(resource_cache)
  assert_eq(CacheStats::get_size(cache_stats), 3)
  assert_eq(CacheStats::get_hit_count(cache_stats), 3)
  assert_eq(CacheStats::get_miss_count(cache_stats), 0)
  
  // 测试资源热更新
  let resource_updater = ResourceUpdater::new()
  
  // 设置更新监听器
  ResourceUpdater::add_listener(resource_updater, (locale, key, old_value, new_value) => {
    assert_eq(locale, "zh")
    assert_eq(key, "welcome.message")
    assert_eq(old_value, "欢迎使用Azimuth遥测系统")
    assert_eq(new_value, "欢迎使用Azimuth遥测系统（新版）")
  })
  
  // 更新资源
  ResourceUpdater::update_resource(resource_updater, "zh", "welcome.message", "欢迎使用Azimuth遥测系统（新版）")
  
  // 验证资源已更新
  let updated_zh_message = DynamicLocalizer::get_resource(dynamic_localizer, "welcome.message")
  assert_eq(updated_zh_message, "欢迎使用Azimuth遥测系统（新版）")
}